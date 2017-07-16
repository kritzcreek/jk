{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Solver where

import           Protolude hiding (Constraint, mkInteger)

import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.SemVer (SemVerRange, SemVer)
import qualified Data.SemVer as SemVer
import           Solver.Types
import           Solver.Monad
import           Z3.Monad (Z3, AST)
import qualified Z3.Monad as Z3

-- |
-- Generate an `or` constraint that covers all versions of a package that
-- meet the given range constraint. This does not require the solution
-- to be distinct, that will be addressed elsewere.
getDependency
  :: (PackageName, SemVerRange)
  -> SolverT Z3 (Maybe AST)
getDependency (name, verRange) = do
  pkgs <- gets solverIndex
  case Map.lookup name pkgs of
    Just vers -> do
      -- select at least one package in version range. this will be limited to
      -- one distinct version by other assertion in the same scope
      let somePackage :: [SemVer] -> SolverT Z3 AST
          somePackage xs = do
            let packages = Package name <$> xs
            packages' <- traverse getPackage packages
            case packageVarVar <$> packages' of
              [p] -> pure p
              ps -> Z3.mkOr ps

      case filter (SemVer.matches verRange) (Map.keys vers) of
        [] -> do
          -- Didn't find any matching versions. This branch needs to be pruned
          Just <$> Z3.mkBool False
        xs -> Just <$> somePackage xs

    Nothing -> do
      error ("missing package: " <> show name)

getDependencyNodes
  :: (PackageName, SemVerRange)
  -> SolverT Z3 [(Package, AST)]
getDependencyNodes (name, verRange) = do
  pkgs <- gets solverIndex
  case Map.lookup name pkgs of
    Just vers -> do
      let somePackage :: [SemVer] -> SolverT Z3 [(Package, AST)]
          somePackage xs = do
            let packages = Package name <$> xs
            traverse (\ x -> (,) x . packageVarVar <$> getPackage x) packages

      somePackage $ filter (SemVer.matches verRange) (Map.keys vers)

    Nothing -> do
      liftIO . putStrLn $ "missing package: " ++ show name
      return []

packageVersionMapLookup
  :: Package
  -> Index
  -> Maybe PackageDescription
packageVersionMapLookup package packages = do
  versions <- Map.lookup (packageName package) packages
  Map.lookup (packageVersion package) versions

renderOneLine :: Package -> Text
renderOneLine p = packageName p <> "-" <> SemVer.renderSV (packageVersion p)

getDescription :: PackageDescription -> SolverT Z3 (Maybe AST)
getDescription (PackageDescription dependencies) = do
  deps <- catMaybes <$> traverse getDependency (Map.toList dependencies)
  case deps of
    [] -> pure Nothing
    [x] -> Just <$> pure x
    xs -> Just <$> Z3.mkAnd xs

getPackage
  :: Package
  -> SolverT Z3 PackageVar
getPackage package = do
  modify $ \ s@SolverState{solverDependencies = deps} -> s{solverDependencies = Set.insert (packageName package) deps}
  mcachedVar <- Map.lookup package <$> gets solverPackages
  mpdesc <- packageVersionMapLookup package <$> gets solverIndex
  case (mcachedVar, mpdesc) of
    (Just cachedVar, _) -> return cachedVar
    (Nothing, Nothing) -> error "no matching packages?"
    (_, Just pDesc) -> do
      self <- PackageVar <$> Z3.mkFreshBoolVar (toS (renderOneLine package))
      modify $ \ s@SolverState{solverPackages = pkgs} -> s{solverPackages = Map.insert package self pkgs}
      mdeps <- getDescription pDesc
      traverse_ (Z3.assert <=< Z3.mkImplies (packageVarVar self)) mdeps
      return self

getLatestVersion
  :: PackageName
  -> SolverT Z3 (Maybe (Package, AST))
getLatestVersion pkgName = do
  pkgs <- gets solverIndex
  case Map.lookup pkgName pkgs of
    Nothing -> trace ("whut" :: Text) $ return Nothing
    Just ve ->
      let step [] = return Nothing
          step (pkgVer:ys) = do
            let pkgId = Package pkgName pkgVer
            pkgVar <- packageVarVar <$> getPackage pkgId
            res <- Z3.local $ do
              Z3.assert pkgVar
              Z3.assert =<< getDistinctVersion pkgName
              Z3.check

            case res of
              Z3.Sat -> return $ Just (pkgId, pkgVar)
              _      -> step ys

      -- keys are normally are returned in ascending order
      -- but we want them in descending...
      in step $ Map.foldlWithKey (\xs k _ -> k:xs) [] ve

splitPackageNameAndVersion :: Map Package PackageVar -> Map PackageName (Map SemVer PackageVar)
splitPackageNameAndVersion = Map.fromListWith Map.union . fmap step . Map.toList where
  step (k, v) = (packageName k, Map.singleton (packageVersion k) v)

getDistinctVersion
  :: PackageName
  -> SolverT Z3 AST
getDistinctVersion pkgName = do
  pkgs <- splitPackageNameAndVersion <$> gets solverPackages
  case Map.lookup pkgName pkgs of
    Just k ->
     case Map.elems k of
       [] -> error "cannot make an empty set distinct?"
       [x] -> return $ packageVarVar x
       xs -> do
         z <- Z3.mkInteger 0
         o <- Z3.mkInteger 1
         let distinct l = Z3.mkIte (packageVarVar l) o z
         assigned <- Z3.mkAdd =<< traverse distinct xs
         Z3.mkEq assigned o
    Nothing -> trace ("assertDistinctVersion couldn't find: " ++ show pkgName) Z3.mkFalse 

getInstallationPlan
  :: Set (PackageName, SemVerRange) -- ^ Packages to install
  -> SolverT Z3 (Set Package)
getInstallationPlan desiredPackages = do
  -- pin down the platform and compiler
  -- TODO(Christoph): Use the compiler information in the manifest
  -- assertGlobalFlags platform compiler

  -- add constraints for the desired packages to install
  for_ desiredPackages $ \ dependency ->
    getDependency dependency >>= \x -> case x of
      Just depVar -> Z3.assert depVar
      Nothing -> return ()

  -- select a single version for each package, in priority order
  -- note: this should run in better than O(n*m) time because
  -- we're folding over the partially solved constraint set
  -- ordering <- gets hakeSolverPriorities
  dependencies <- gets solverDependencies
  for_ dependencies $ \name -> do
    Just (_, pkgVar) <- getLatestVersion name
    Z3.assert pkgVar

  solved <- Z3.withModel $ \model -> do
    packages <- gets solverPackages
    filterM (\(_, PackageVar var) -> maybe False identity <$> Z3.evalBool model var) (Map.toList packages)
  case solved of
    (Z3.Sat, Just ps) -> pure (Set.fromList (map fst ps))
    _ -> error "Failed to solve"
