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
import           Conduit.SocketReconnector
import           Control.Concurrent.Killable
import           Data.Conduit.Attoparsec
import           Data.Conduit.List
import           Data.Conduit.Network
import           Network.Socket                 ( PortNumber )
import           RIO
import qualified RIO.ByteString.Lazy           as BL
import qualified RIO.Text                      as T
--import           Data.Conduit.TQueue
import           System.Timer.Updatable


import           SLE.Data.Bind
import           SLE.Data.Common
import           SLE.Data.CommonConfig
import           SLE.Data.DEL
import           SLE.Data.Handle
import           SLE.Data.Input
import           SLE.Data.PDU
import           SLE.Data.TMLConfig
import           SLE.Data.TMLMessage

import           SLE.State.Classes
import           SLE.State.Events

import           Data.ASN1.BinaryEncoding
import           Data.ASN1.Encoding

import           Text.Show.Pretty

import           Text.Builder                  as TB


processReadTML
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasCommonConfig env
       , HasEventHandler env
       , HasLogFunc env
       , HasSleHandle env
       , HasTimer env
       )
    => ConduitT TMLMessage Void m ()
    -> ConduitT ByteString Void m ()
processReadTML processor = do
    conduitParserEither tmlPduParser .| worker .| processPDU .| processor
  where
    worker = do
        x <- await
        case x of
            Nothing  -> return ()
            Just val -> do
                case val of
                    Left err -> do
                        env <- ask
                        liftIO $ do
                            sleRaiseEvent
                                env
                                (TMLParseError (T.pack (errorMessage err)))
                            runRIO env protocolAbort
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
       , HasSleHandle env
       , HasTimer env
       )
    => ConduitT TMLPDU TMLMessage m ()
processPDU = do
    awaitForever $ \case
        TMLPDUHeartBeat -> do
            logDebug "Received heartbeat."
        TMLPDUCtxt ctxt -> do
            logDebug $ "Received context message: " <> displayShow ctxt
            lift $ processContext ctxt
        TMLPDUMessage msg -> do
            lift $ logDebug $ "Yielding PDU: " <> fromString (ppShow msg)
            yield msg


processContext
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasTimer env
       , HasLogFunc env
       , HasCommonConfig env
       , HasSleHandle env
       , HasEventHandler env
       )
    => TMLContextMsgRead
    -> m ()
processContext msg = do
    cfg <- view (commonCfg . cfgTML)
    case chkContextMsg cfg msg of
        Right _ -> do
            stopTimers
            startTimersWith (_tmlCtxHeartbeatInterval msg)
                            (_tmlCtxDeadFactor msg)
        Left err -> do
            logError $ "Received illegal context message: " <> displayShow err
            peerAbort



heartBeatMessage :: ByteString
heartBeatMessage = builderBytes $ tmlHeartBeatMsgBuilder TMLHeartBeatMessage




-- | Start the heartbeat timers 
startTimers
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasCommonConfig env
       , HasLogFunc env
       , HasSleHandle env
       , HasEventHandler env
       , HasTimer env
       )
    => m ()
startTimers = do
    env <- ask
    let cfg = env ^. commonCfg . cfgTML
    startTimersWith (cfgHeartbeat cfg) (cfgDeadFactor cfg)


-- | start the heartbeat timers with the given values for heartbeat time and 
-- the dead factor 
startTimersWith
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasEventHandler env
       , HasLogFunc env
       , HasSleHandle env
       , HasTimer env
       )
    => Word16
    -> Word16
    -> m ()
startTimersWith hbTime' deadFactor = do
    env <- ask
    let hbrTime = fromIntegral hbTime' * fromIntegral deadFactor * 1_000_000

    logDebug "Starting timers..."
    hbrTimer <- liftIO $ replacer (runRIO env heartBeatReceiveTimeOut) hbrTime

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
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasSleHandle env
       , HasEventHandler env
       )
    => m ()
heartBeatReceiveTimeOut = do
    logWarn "heartBeatReceiveTimeout"
    protocolAbort


-- | Issue a protocol abort message 
protocolAbort
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasEventHandler env
       , HasSleHandle env
       )
    => m ()
protocolAbort = do
    logDebug "ProtocolAbort called!"
    env <- ask
    liftIO $ sleRaiseEvent env TMLProtocolAbort
    atomically $ writeTBQueue (env ^. getHandle . sleInput) SLEAbort


-- | Issue a peer abort message
peerAbort
    :: ( MonadIO m
       , MonadReader env m
       , HasLogFunc env
       , HasSleHandle env
       , HasEventHandler env
       )
    => m ()
peerAbort = do
    logDebug "PeerAbort!"
    env <- ask
    liftIO $ sleRaiseEvent env TMLPeerAbort
    liftIO $ sleRaiseEvent env TMLProtocolAbort
    atomically $ writeTBQueue (env ^. getHandle . sleInput) SLEAbort


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
    env <- ask
    sleRaiseEvent env TMLDisconnect
    return ()


