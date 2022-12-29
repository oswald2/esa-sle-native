module SLE.State.RAFClasses
    ( HasRAF(..)
    , getRAF
    , getRAFSTM
    , getRAFSleHandle
    , getRAFConfig
    ) where

import           RIO                     hiding ( (^.) )

import           Control.Lens

import           SLE.Data.Common
import           SLE.Data.Handle
import           SLE.Data.ProviderConfig
import           SLE.State.RAFState

class HasRAF env where
  getRAFs :: Getter env (Vector RAFVar)
  getRAFVar :: env -> RAFIdx -> RAFVar


getRAF :: (MonadIO m, HasRAF env) => env -> RAFIdx -> m RAF
getRAF env idx = readRAFVarIO (getRAFVar env idx)

getRAFSTM :: (HasRAF env) => env -> RAFIdx -> STM RAF
getRAFSTM env idx = readRAFVar (getRAFVar env idx)

getRAFSleHandle :: (HasRAF env) => env -> RAFIdx -> SleHandle
getRAFSleHandle env idx = getRAFVar env idx ^. rafSleHandle

getRAFConfig :: (HasRAF env) => env -> RAFIdx -> RAFConfig
getRAFConfig env idx = getRAFVar env idx ^. rafVarCfg
