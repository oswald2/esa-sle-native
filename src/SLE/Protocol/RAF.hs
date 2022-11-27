module SLE.Protocol.RAF
    ( runRAF
    , runRAFs
    ) where

import           RIO
import qualified RIO.Vector                    as V

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
       , HasTimer env
       )
    => RAFVar
    -> m ()
runRAF var = do
    let cfg = var ^. rafVarCfg
    listenSLE (var ^. rafSleHandle)
              (fromIntegral (cfg ^. cfgRAFPort))
              (rafStateMachine cfg var)
    runRAF var



runRAFs
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasEventHandler env
       , HasProviderConfig env
       , HasTimer env
       , HasRAF env
       )
    => m ()
runRAFs = do
    env <- ask
    let rafs    = env ^. getRAFs
        threads = V.foldl (\prev raf -> prev <> conc (runRAF raf)) mempty rafs
    runConc threads

