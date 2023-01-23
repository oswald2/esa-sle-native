module SLE.Data.Ops
    ( protocolAbort
    , peerAbort
    , onServerDisconnect
    , stopTimers
    ) where

import           RIO

import           SLE.Data.Common
import           SLE.Data.Handle
import           SLE.Data.PDU
import           SLE.Data.WriteCmd

import           SLE.State.Classes
import           SLE.State.Events
import           SLE.State.ProviderState
import           SLE.State.RAFClasses

import           Control.Concurrent.Killable



-- | Issue a protocol abort message 
protocolAbort
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasEventHandler env
       , HasRAF env
       )
    => SleHandle
    -> m ()
protocolAbort hdl = do
    logDebug "SLE ProtocolAbort called!"
    setServiceState (hdl ^. sleIdx) ServiceInit
    sleRaiseEvent TMLProtocolAbort
    resetState hdl
    writeSLE hdl SLEAbort

resetState
    :: (MonadIO m, MonadReader env m, HasEventHandler env) => SleHandle -> m ()
resetState hdl = do
    case hdl ^. sleIdx of
        TMRAF idx  -> sleRaiseEvent (SLERafStatus idx ServiceInit)
        TMRCF _idx -> return ()
        TMFirst idx ->
            sleRaiseEvent (SLERafStatus (RAFIdx (fromIntegral idx)) ServiceInit)


peerAbort
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasEventHandler env)
    => SleHandle
    -> PeerAbortDiagnostic
    -> m ()
peerAbort hdl diag = do
    logWarn $ "Peer Abort called with diag: " <> display diag
    let pdu = SlePeerAbort diag
    writeSLE hdl (SLEPdu (SlePduPeerAbort pdu))
    resetState hdl


-- | Called when the server side TML layer looses the connection to the client
onServerDisconnect
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasEventHandler env
       , HasTimer env
       , HasRAF env
       )
    => SleHandle
    -> m ()
onServerDisconnect hdl = do
    logWarn "Server is disconnected..."
    stopTimers
    sleRaiseEvent TMLDisconnect
    setServiceState (hdl ^. sleIdx) ServiceInit
    sleRaiseEvent TMLProtocolAbort
    resetState hdl
    return ()


-- | Stop the hearbeat timers 
stopTimers
    :: (MonadUnliftIO m, MonadReader env m, HasTimer env)
    => m ()
stopTimers = do
    env <- ask
    action <- atomically $ do
        hbTimer  <- readTVar (env ^. getTimerHBT)
        hbrTimer <- readTVar (env ^. getTimerHBR)
        
        writeTVar (env ^. getTimerHBT) Nothing 
        writeTVar (env ^. getTimerHBR) Nothing 
        
        return $ do
            forM_ hbTimer  kill
            forM_ hbrTimer kill

    liftIO action
