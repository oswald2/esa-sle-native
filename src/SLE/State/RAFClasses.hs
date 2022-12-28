module SLE.State.RAFClasses
    ( HasRAF(..)
    , getRAF
    , getRAFSTM
    ) where

import           RIO

import           Control.Lens

import           SLE.Data.Common
import           SLE.Data.Handle
import           SLE.State.RAFState

class HasRAF env where
  getRAFs :: Getter env (Vector (RAFVar, SleHandle))
  getRAFVar :: env -> RAFIdx -> RAFVar


getRAF :: (MonadIO m, HasRAF env) => env -> RAFIdx -> m RAF
getRAF env idx = readRAFVarIO (getRAFVar env idx)

getRAFSTM :: (HasRAF env) => env -> RAFIdx -> STM RAF
getRAFSTM env idx = readRAFVar (getRAFVar env idx)
