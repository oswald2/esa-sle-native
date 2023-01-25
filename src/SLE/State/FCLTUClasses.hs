module SLE.State.FCLTUClasses
    ( HasFCLTU(..)
    , getFCLTUVar
    , getFCLTU
    , getFCLTUSTM
    , getFCLTUSleHandle
    , getFCLTUConfig
    , setFCLTUServiceState
    ) where

import           RIO                     hiding ( (^.)
                                                , (^?)
                                                )

import           Control.Lens

import           SLE.Data.Common
import           SLE.Data.Handle
import           SLE.Data.ProviderConfig
import           SLE.State.FCLTUState

class HasFCLTU env where
  getFCLTUs :: Getter env (Vector FCLTUVar)
  getFCLTUVar' :: env -> FCLTUIdx -> (Maybe FCLTUVar)


getFCLTUVar
    :: (MonadIO m, MonadReader env m, HasFCLTU env)
    => FCLTUIdx
    -> m (Maybe FCLTUVar)
getFCLTUVar idx = do
    env <- ask
    return $ getFCLTUVar' env idx

getFCLTU :: (MonadIO m, HasFCLTU env) => env -> FCLTUIdx -> m (Maybe FCLTU)
getFCLTU env idx =
    maybe (pure Nothing) (fmap Just . readFCLTUVarIO) (getFCLTUVar' env idx)

getFCLTUSTM :: (HasFCLTU env) => env -> FCLTUIdx -> STM (Maybe FCLTU)
getFCLTUSTM env idx =
    maybe (pure Nothing) (fmap Just . readFCLTUVar) (getFCLTUVar' env idx)

getFCLTUSleHandle :: (HasFCLTU env) => env -> FCLTUIdx -> Maybe SleHandle
getFCLTUSleHandle env idx = case getFCLTUVar' env idx of
    Nothing  -> Nothing
    Just var -> Just (var ^. fcltuSleHandle)

getFCLTUConfig :: (HasFCLTU env) => env -> FCLTUIdx -> Maybe FCLTUConfig
getFCLTUConfig env idx = case getFCLTUVar' env idx of
    Nothing  -> Nothing
    Just var -> Just (var ^. fcltuVarCfg)

setFCLTUServiceState
    :: (MonadIO m, MonadReader env m, HasFCLTU env)
    => FCLTUIdx
    -> ServiceState
    -> m ()
setFCLTUServiceState idx state = do
    env <- ask
    case getFCLTUVar' env idx of
        Nothing  -> return ()
        Just var -> setFCLTUState var state
