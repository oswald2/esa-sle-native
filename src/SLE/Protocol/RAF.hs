module SLE.Protocol.RAF
    ( runRAF
    , runRAFs
    ) where

import           RIO
import qualified RIO.Vector                    as V

import           SLE.Data.RAF

import           SLE.Protocol.SLEProtocol

import           SLE.State.Classes
import           SLE.State.RAFClasses
import           SLE.State.RAFState

runRAF
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasEventHandler env
       , HasProviderConfig env
       , HasTimer env
       , HasRAF env
       )
    => (Word64 -> IO ())
    -> RAFVar
    -> m ()
runRAF perfFunc var = do
    let cfg = var ^. rafVarCfg
    listenRAF (var ^. rafSleHandle)
              cfg
              (var ^. rafIdx)
              (rafStateMachine cfg var)
              perfFunc
    runRAF perfFunc var



runRAFs
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasEventHandler env
       , HasProviderConfig env
       , HasTimer env
       , HasRAF env
       )
    => (Word64 -> IO ())
    -> m ()
runRAFs perfFunc = do
    env <- ask
    let rafs    = env ^. getRAFs
        threads = V.foldl (\prev raf -> prev <> conc (runRAF perfFunc raf))
                          mempty
                          rafs
    runConc threads

