module SLE.Protocol.TMLProtocol
    ( processReadTML
    , processPDU
    , processContext
    , protocolAbort
    , peerAbort
    , checkPDU
    , heartBeatMessage
    , startTimers
    , stopTimers
    , onServerDisconnect
    ) where

import           ByteString.StrictBuilder
import           Conduit
import           Control.Concurrent.Killable
import           Data.Conduit.Attoparsec
import           RIO
import qualified RIO.Text                      as T
import           System.Timer.Updatable


import           SLE.Data.CommonConfig
import           SLE.Data.Handle
import           SLE.Data.WriteCmd
import           SLE.Data.TMLConfig
import           SLE.Data.TMLMessage

import           SLE.State.Classes
import           SLE.State.Events

import           Text.Show.Pretty



processReadTML
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasCommonConfig env
       , HasEventHandler env
       , HasLogFunc env
       , HasTimer env
       )
    => SleHandle
    -> ConduitT TMLMessage Void m ()
    -> ConduitT ByteString Void m ()
processReadTML hdl processor = do
    conduitParserEither tmlPduParser .| worker .| processPDU hdl .| processor
  where
    worker = do
        x <- await
        case x of
            Nothing  -> return ()
            Just val -> do
                case val of
                    Left err -> do
                        env <- ask
                        sleRaiseEvent
                            (TMLParseError (T.pack (errorMessage err)))
                        runRIO env (protocolAbort hdl)
                    Right (_, pdu) -> do
                        logDebug $ "Received PDU: " <> fromString (ppShow pdu)
                        lift restartHBRTimer
                        -- process pdu
                        yield pdu
                        worker


processPDU
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasEventHandler env
       , HasLogFunc env
       , HasCommonConfig env
       , HasTimer env
       )
    => SleHandle
    -> ConduitT TMLPDU TMLMessage m ()
processPDU hdl = do
    awaitForever $ \case
        TMLPDUHeartBeat -> do
            logDebug "Received heartbeat."
        TMLPDUCtxt ctxt -> do
            logDebug $ "Received context message: " <> displayShow ctxt
            lift $ processContext hdl ctxt
        TMLPDUMessage msg -> do
            lift $ logDebug $ "Yielding PDU: " <> fromString (ppShow msg)
            yield msg


processContext
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasTimer env
       , HasLogFunc env
       , HasCommonConfig env
       , HasEventHandler env
       )
    => SleHandle
    -> TMLContextMsgRead
    -> m ()
processContext hdl msg = do
    cfg <- view (commonCfg . cfgTML)
    case chkContextMsg cfg msg of
        Right _ -> do
            stopTimers
            startTimersWith hdl
                            (_tmlCtxHeartbeatInterval msg)
                            (_tmlCtxDeadFactor msg)
        Left err -> do
            logError $ "Received illegal context message: " <> displayShow err
            peerAbort hdl



heartBeatMessage :: ByteString
heartBeatMessage = builderBytes $ tmlHeartBeatMsgBuilder TMLHeartBeatMessage




-- | Start the heartbeat timers 
startTimers
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasCommonConfig env
       , HasLogFunc env
       , HasEventHandler env
       , HasTimer env
       )
    => SleHandle
    -> m ()
startTimers hdl = do
    env <- ask
    let cfg = env ^. commonCfg . cfgTML
    startTimersWith hdl (cfgHeartbeat cfg) (cfgDeadFactor cfg)


-- | start the heartbeat timers with the given values for heartbeat time and 
-- the dead factor 
startTimersWith
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasEventHandler env
       , HasLogFunc env
       , HasTimer env
       )
    => SleHandle
    -> Word16
    -> Word16
    -> m ()
startTimersWith hdl hbTime' deadFactor = do
    env <- ask
    let hbrTime = fromIntegral hbTime' * fromIntegral deadFactor * 1_000_000

    logDebug "Starting timers..."
    hbrTimer <- liftIO
        $ replacer (runRIO env (heartBeatReceiveTimeOut hdl)) hbrTime

    atomically $ do
        writeTVar (env ^. getTimerHBR) (Just hbrTimer)

    return ()


-- | Stop the hearbeat timers 
stopTimers
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasTimer env)
    => m ()
stopTimers = do
    env <- ask
    logDebug "Stopping timers..."
    action <- atomically $ do
        hbTimer  <- readTVar (env ^. getTimerHBT)
        hbrTimer <- readTVar (env ^. getTimerHBR)
        return $ do
            forM_ hbTimer  kill
            forM_ hbrTimer kill

    liftIO action


-- | Restart the heartbeat timers 
restartHBRTimer
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasCommonConfig env
       , HasLogFunc env
       , HasTimer env
       )
    => m ()
restartHBRTimer = do
    env <- ask
    let cfg = env ^. commonCfg . cfgTML
        hbTime =
            fromIntegral (cfgHeartbeat cfg)
                * fromIntegral (cfgDeadFactor cfg)
                * 1_000_000
    logDebug "Restarting HeartBeat Reception Timer"
    atomically $ do
        let tvar = env ^. getTimerHBR
        timer <- readTVar tvar
        case timer of
            Nothing -> return ()
            Just t  -> renew t hbTime


-- | The heartbeat timeout for hte receiver has timed out 
heartBeatReceiveTimeOut
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasEventHandler env)
    => SleHandle
    -> m ()
heartBeatReceiveTimeOut hdl = do
    logWarn "heartBeatReceiveTimeout"
    protocolAbort hdl


-- | Issue a protocol abort message 
protocolAbort
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasEventHandler env)
    => SleHandle
    -> m ()
protocolAbort hdl = do
    logDebug "ProtocolAbort called!"
    sleRaiseEvent TMLProtocolAbort
    writeSLE hdl SLEAbort


-- | Issue a peer abort message
peerAbort
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasEventHandler env)
    => SleHandle
    -> m ()
peerAbort hdl = do
    logDebug "PeerAbort!"
    sleRaiseEvent TMLPeerAbort
    sleRaiseEvent TMLProtocolAbort
    writeSLE hdl SLEAbort


-- | Check a incoming 'TMLPDU' for errors
checkPDU :: TMLConfig -> TMLPDU -> Either Text TMLContextMsgRead
checkPDU _cfg TMLPDUMessage{} = Left "Received PDU, expected Context message"
checkPDU _cfg TMLPDUHeartBeat =
    Left "Received HeartBeat, expected Context message"
checkPDU cfg (TMLPDUCtxt msg) = case chkContextMsg cfg msg of
    Left IllegalHeartBeat ->
        Left "Context message contained illegal hearbeat value"
    Left IllegalDeadFactor ->
        Left "Context message contained illegal dead-factor"
    Left  IllegalProtocol -> Left "Context message contained illegal protocol"
    Left  IllegalVersion  -> Left "Context message contained illegal version"
    Right _               -> Right msg


-- | Called when the server side TML layer looses the connection to the client
onServerDisconnect
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasEventHandler env)
    => m ()
onServerDisconnect = do
    logWarn "Server is disconnecting..."
    sleRaiseEvent TMLDisconnect
    return ()


