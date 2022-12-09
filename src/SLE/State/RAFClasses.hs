module SLE.State.RAFClasses
    ( HasRAF(..)
    , getRAF
    ) where

import           RIO

import           Control.Lens

import           SLE.State.RAFState

class HasRAF env where
  getRAFs :: Getter env (Vector RAFVar)
  getRAFVar :: env -> Int -> RAFVar


getRAF :: (MonadIO m, HasRAF env) => env -> Int -> m RAF
getRAF env idx = readRAFVarIO (getRAFVar env idx)

