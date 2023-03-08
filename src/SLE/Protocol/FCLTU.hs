module SLE.Protocol.FCLTU
    ( runFCLTU
    , runFCLTUs
    ) where

import           RIO
import qualified RIO.Vector                    as V

import           SLE.Data.FCLTU

import           SLE.Protocol.SLEProtocol

import           SLE.State.Classes
import           SLE.State.RAFClasses
import           SLE.State.FCLTUClasses
import           SLE.State.FCLTUState



runFCLTU
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasEventHandler env
       , HasProviderConfig env
       , HasTimer env
       , HasRAF env
       , HasFCLTU env
       )
    => (Word64 -> IO ())
    -> ConfigFromApp
    -> FCLTUVar
    -> m ()
runFCLTU perfFunc appCfg var = do
    let cfg = var ^. fcltuVarCfg
    listenFCLTU (var ^. fcltuSleHandle)
                cfg
                (var ^. fcltuIdx)
                (fcltuStateMachine cfg var perfFunc appCfg)
    runFCLTU perfFunc appCfg var



runFCLTUs
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasEventHandler env
       , HasProviderConfig env
       , HasTimer env
       , HasRAF env
       , HasFCLTU env
       )
    => (Word64 -> IO ())
    -> ConfigFromApp
    -> m ()
runFCLTUs perfFunc appCfg = do
    env <- ask
    let fcltus  = env ^. getFCLTUs
        threads = V.foldl
            (\prev fcltu -> prev <> conc (runFCLTU perfFunc appCfg fcltu))
            mempty
            fcltus
    runConc threads

