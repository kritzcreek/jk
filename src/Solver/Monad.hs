{-# LANGUAGE Unsafe #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Solver.Monad where

import           Protolude hiding (Constraint, mkInteger)

import           Control.Monad.Trans.Class (MonadTrans)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Solver.Types
import           Z3.Monad ( Z3, Z3Env)
import qualified Z3.Monad as Z3

data SolverState = SolverState
  { solverIndex :: !Index
  , solverPackages :: !(Map Package PackageVar)
  , solverDependencies :: !(Set PackageName)
  }

defaultSolverState :: SolverState
defaultSolverState = SolverState
  { solverIndex = Map.empty
  , solverPackages = Map.empty
  , solverDependencies = Set.empty
  }

newtype SolverT m a = SolverT {unSolverT :: StateT SolverState m a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadTrans, MonadState SolverState)

instance Z3.MonadZ3 m => Z3.MonadZ3 (SolverT m) where
  getSolver = lift Z3.getSolver
  getContext = lift Z3.getContext

runHakeSolverT
  :: SolverState
  -> Z3Env
  -> SolverT Z3 a
  -> IO (a, SolverState)
runHakeSolverT st env app = do
  let script = runStateT (unSolverT app) st
  Z3.evalZ3WithEnv script env

execHakeSolverT
  :: SolverState
  -> Z3Env
  -> SolverT Z3 a
  -> IO SolverState
execHakeSolverT st env app = do
  let script = execStateT (unSolverT app) st
  Z3.evalZ3WithEnv script env

runLocalHakeSolverT
  :: SolverState
  -> Z3Env
  -> SolverT Z3 a
  -> IO (a, SolverState)
runLocalHakeSolverT st env app = do
  let script = runStateT (unSolverT app) st
  Z3.evalZ3WithEnv (Z3.local script) env

execLocalHakeSolverT
  :: SolverState
  -> Z3Env
  -> SolverT Z3 a
  -> IO SolverState
execLocalHakeSolverT st env app = do
  let script = execStateT (unSolverT app) st
  Z3.evalZ3WithEnv (Z3.local script) env
