module SLE.Protocol.RAF
    ( runRAF
    ) where

import           RIO

import           SLE.Data.ProviderConfig
import           SLE.Data.RAF
import           SLE.Protocol.SLEProtocol
import           SLE.State.Classes

runRAF
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasEventHandler env
       , HasProviderConfig env
       , HasSleHandle env
       , HasTimer env
       )
    => RAFConfig
    -> RAFVar
    -> m ()
runRAF cfg var = do
    listenSLE (fromIntegral (cfg ^. cfgRAFPort)) (rafStateMachine cfg var)
    runRAF cfg var





