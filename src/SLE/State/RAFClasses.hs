module SLE.State.RAFClasses
    ( HasRAF(..)
    , getRAFVar
    , getRAF
    , getRAFSTM
    , getRAFSleHandle
    , getRAFConfig
    , setRAFServiceState
    ) where

import           RIO                     hiding ( (^.)
                                                , (^?)
                                                )

import           Control.Lens

import           SLE.Data.Common
import           SLE.Data.Handle
import           SLE.Data.ProviderConfig
import           SLE.State.RAFState

class HasRAF env where
  getRAFs :: Getter env (Vector RAFVar)
  getRAFVar' :: env -> RAFIdx -> (Maybe RAFVar)


getRAFVar
    :: (MonadIO m, MonadReader env m, HasRAF env) => RAFIdx -> m (Maybe RAFVar)
getRAFVar idx = do
    env <- ask
    return $ getRAFVar' env idx

getRAF :: (MonadIO m, HasRAF env) => env -> RAFIdx -> m (Maybe RAF)
getRAF env idx =
    maybe (pure Nothing) (fmap Just . readRAFVarIO) (getRAFVar' env idx)

getRAFSTM :: (HasRAF env) => env -> RAFIdx -> STM (Maybe RAF)
getRAFSTM env idx =
    maybe (pure Nothing) (fmap Just . readRAFVar) (getRAFVar' env idx)

getRAFSleHandle :: (HasRAF env) => env -> RAFIdx -> Maybe SleHandle
getRAFSleHandle env idx = case getRAFVar' env idx of
    Nothing  -> Nothing
    Just var -> Just (var ^. rafSleHandle)

getRAFConfig :: (HasRAF env) => env -> RAFIdx -> Maybe RAFConfig
getRAFConfig env idx = case getRAFVar' env idx of
    Nothing  -> Nothing
    Just var -> Just (var ^. rafVarCfg)

setRAFServiceState
    :: (MonadIO m, MonadReader env m, HasRAF env) => RAFIdx -> ServiceState -> m ()
setRAFServiceState idx state = do 
    env <- ask 
    case getRAFVar' env idx of 
        Nothing -> return () 
        Just var -> setRAFState var state
