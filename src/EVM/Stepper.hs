{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EVM.Stepper
  ( Action (..)
  , Stepper
  , exec
  , execFully
  , run
  , runFully
  , wait
  , ask
  , evm
  , evmIO
  , enter
  , interpret
  )
where

-- This module is an abstract definition of EVM steppers.
-- Steppers can be run as TTY debuggers or as CLI test runners.
--
-- The implementation uses the operational monad pattern
-- as the framework for monadic interpretation.

import Control.Monad.Operational (Program, ProgramViewT(..), ProgramView, singleton, view)
import Control.Monad.State.Strict (execStateT, runStateT, get)
import Data.Text (Text)

import EVM qualified
import EVM.Exec qualified
import EVM.Fetch qualified as Fetch
import EVM.Types
import Control.Monad.ST (stToIO, RealWorld)
import Control.Monad.IO.Class
import EVM.Effects

-- | The instruction type of the operational monad
data Action t s a where

  -- | Keep executing until an intermediate result is reached
  Exec :: Action t s (VMResult t s)

  -- | Wait for a query to be resolved
  Wait :: Query t s -> Action t s ()

  -- | Multiple things can happen
  Ask :: Choose s -> Action Symbolic s ()

  -- | Embed a VM state transformation
  EVM  :: EVM t s a -> Action t s a

  -- | Perform an IO action
  IOAct :: IO a -> Action t s a

-- | Type alias for an operational monad of @Action@
type Stepper t s a = Program (Action t s) a

-- Singleton actions

exec :: Stepper t s (VMResult t s)
exec = singleton Exec

run :: Stepper t s (VM t s)
run = exec >> evm get

wait :: Query t s -> Stepper t s ()
wait = singleton . Wait

ask :: Choose s -> Stepper Symbolic s ()
ask = singleton . Ask

evm :: EVM t s a -> Stepper t s a
evm = singleton . EVM

evmIO :: IO a -> Stepper t s a
evmIO = singleton . IOAct

-- | Run the VM until final result, resolving all queries
execFully :: Stepper Concrete s (Either EvmError (Expr Buf))
execFully =
  exec >>= \case
    HandleEffect (Query q) ->
      wait q >> execFully
    VMFailure x ->
      pure (Left x)
    VMSuccess x ->
      pure (Right x)

-- | Run the VM until its final state
runFully :: Stepper t s (VM t s)
runFully = do
  vm <- run
  case vm.result of
    Nothing -> internalError "should not occur"
    Just (HandleEffect (Query q)) ->
      wait q >> runFully
    Just (HandleEffect (Choose q)) ->
      ask q >> runFully
    Just _ ->
      pure vm

enter :: Text -> Stepper t s ()
enter t = evm (EVM.pushTrace (EntryTrace t))

interpret
  :: forall m a . (App m)
  => Fetch.Fetcher Concrete m RealWorld
  -> VM Concrete RealWorld
  -> Stepper Concrete RealWorld a
  -> m a
interpret fetcher vm = eval . view
  where
    eval :: ProgramView (Action Concrete RealWorld) a -> m a
    eval (Return x) = pure x
    eval (action :>>= k) =
      case action of
        Exec -> do
          (r, vm') <- liftIO $ stToIO $ runStateT EVM.Exec.exec vm
          interpret fetcher vm' (k r)
        Wait q -> do
          m <- fetcher q
          vm' <- liftIO $ stToIO $ execStateT m vm
          interpret fetcher vm' (k ())
        IOAct m -> do
          r <- liftIO m
          interpret fetcher vm (k r)
        EVM m -> do
          (r, vm') <- liftIO $ stToIO $ runStateT m vm
          interpret fetcher vm' (k r)
