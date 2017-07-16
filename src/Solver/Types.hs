{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Solver.Types where

import Protolude
import           Data.Aeson as Aeson
import           Data.Aeson.Types (parseMaybe)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import           Data.SemVer (SemVerRange, SemVer, parseSemVerRange)
import qualified Text.Toml as Toml
import           Unsafe (unsafeFromJust)
import           Z3.Monad (AST)

type PackageName = Text

data PackageDescription = PackageDescription
    { packageDescriptionDependencies :: !(Map PackageName SemVerRange)
    } deriving (Show, Eq, Ord)

type Index = Map PackageName (Map SemVer PackageDescription)

data Manifest = Manifest
  { manifestDependencies :: Map PackageName SemVerRange
  } deriving (Show)

parseManifest :: Text -> Maybe Manifest
parseManifest = go . Aeson.toJSON <=< hush . Toml.parseTomlDoc "wot"
  where
    go :: Aeson.Value -> Maybe Manifest
    go =
      parseMaybe $ withObject "Manifest" $ \o -> do
        deps <- o .:? "dependencies" .!= HM.empty
        pure (Manifest (Map.fromList (HM.toList (HM.map (unsafeFromJust . hush . parseSemVerRange) deps))))

data Package = Package
  { packageName :: PackageName
  , packageVersion :: SemVer
  } deriving (Show, Eq, Ord)

data PackageVar = PackageVar
  { packageVarPackage :: Package
  , packageVarVar :: AST
  } deriving (Show, Eq, Ord)
