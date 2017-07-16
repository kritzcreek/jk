{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Protolude hiding (Constraint, mkInteger)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.SemVer as SemVer
import           Solver
import           Solver.Types
import           Solver.Monad
import           System.Directory (listDirectory)
import           System.FilePath
import           Unsafe (unsafeFromJust)
import           Z3.Monad (Z3)
import qualified Z3.Monad as Z3

main :: IO ()
main = run

buildIndex :: IO Index
buildIndex = do
  packageNames <- listDirectory "index"
  index <- for packageNames $ \pn -> do
    versions <- listDirectory ("index" </> pn)
    manifests <- for versions $ \v -> do
      manifest <- unsafeFromJust . parseManifest <$> readFile ("index" </> pn </> v </> "manifest.toml")
      pure (PackageDescription (manifestDependencies manifest))
    pure (Map.fromList (zip (map (unsafeFromJust . hush . SemVer.parseSemVer . toS) versions) manifests))
  pure (Map.fromList (zip (map toS packageNames) index))

loadIndex :: SolverT Z3 ()
loadIndex = do
  ix <- liftIO buildIndex
  modify $ \ s -> s{solverIndex = ix}
  pure ()

run :: IO ()
run = do
  env <- Z3.newEnv Nothing Z3.stdOpts
  st <- execHakeSolverT defaultSolverState env loadIndex

  let prog' = do
        Z3.setASTPrintMode Z3.Z3_PRINT_SMTLIB2_COMPLIANT
        getInstallationPlan
          (Set.fromList
            [ ("purescript-either", SemVer.Eq (SemVer.semver 3 1 0))
            , ("purescript-prelude", SemVer.Eq (SemVer.semver 3 0 0))
            ])

  -- let prog = do
  --       Z3.setASTPrintMode Z3.Z3_PRINT_SMTLIB2_COMPLIANT

  --       -- Just x <- getDependency ("purescript-either", SemVer.Eq (SemVer.semver 3 1 0))
  --       -- bs <- Z3.astToString x
  --       -- liftIO $ putStrLn bs

  --       -- _b <- getDependency ("purescript-arrays", SemVer.anyVersion)
  --       bns <- getDependencyNodes ("purescript-either", SemVer.Eq (SemVer.semver 3 0 0))
  --       -- bs' <- Z3.astToString b
  --       -- liftIO $ putStrLn bs'
  --       deps <- gets solverDependencies
  --       distinct <- traverse getDistinctVersion (Set.toList deps)
  --       traverse_ Z3.assert distinct

  --       (res, mmodel) <- Z3.getModel
  --       case mmodel of
  --         Just model -> do
  --           str <- Z3.modelToString model
  --           print bns
  --           for_ bns $ \ (pn, bn) -> do
  --             mmev <- Z3.evalBool model bn
  --             liftIO $ case mmev of
  --               Just True -> putStrLn $ "Ploz install: " ++ show pn
  --               Just False -> putStrLn $ "Skip: " ++ show pn
  --               Nothing -> putStrLn $ "Undef: " ++ show pn
  --           return (res, Just str)
  --         Nothing -> return (res, Nothing)

  (x, _st') <- runLocalHakeSolverT st env prog'

  case x of
    iPlan -> do
      putText "found model"
      traverse_ print (Set.toList iPlan)
