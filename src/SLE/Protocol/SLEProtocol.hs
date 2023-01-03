module SLE.Protocol.SLEProtocol
    ( listenRAF
    , connectSLE
    ) where

import           RIO
import qualified RIO.ByteString.Lazy           as BL
import qualified RIO.Text                      as T

import           ByteString.StrictBuilder

import           Conduit
import           Conduit.SocketReconnector

import           Data.Conduit.Attoparsec
import           Data.Conduit.List
import           Data.Conduit.Network

import           SLE.Data.Common
import           SLE.Data.CommonConfig
import           SLE.Data.DEL
import           SLE.Data.Handle
import           SLE.Data.PDU
import           SLE.Data.PDUParser
import           SLE.Data.ProviderConfig
import           SLE.Data.TMLConfig
import           SLE.Data.TMLMessage
import           SLE.Data.WriteCmd

import           SLE.State.Classes
import           SLE.State.Events
import           SLE.State.RAFClasses
import           SLE.State.RAFState

import           SLE.Protocol.TMLProtocol

import           Data.ASN1.BinaryEncoding
import           Data.ASN1.Encoding
import           Data.STM.TimedBuffer

import           Text.Show.Pretty

import           Text.Builder                  as TB



connectSLE
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasEventHandler env
       , HasUserConfig env
       , HasTimer env
       )
    => SleHandle
    -> ConnectAddr
    -> m ()
connectSLE hdl addr = do
    runGeneralTCPReconnectClient
        (clientSettings (fromIntegral (port addr)) (encodeUtf8 (host addr)))
        200000
        (processConnect hdl)
        onDisconnect


processConnect
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasEventHandler env
       , HasLogFunc env
       , HasCommonConfig env
       , HasTimer env
       )
    => SleHandle
    -> AppData
    -> m ()
processConnect hdl appData = do
    env <- ask
    logInfo "SLE: Connected to provider."
    sleRaiseEvent TMLConnect

    -- now send the context 
    let msg = TMLCtxtMessage { _tmlCtxHbt   = cfgHeartbeat cfg
                             , _tmlCtxDeadf = cfgDeadFactor cfg
                             }
        cfg    = env ^. commonCfg . cfgTML

        encMsg = builderBytes $ tmlContextMsgBuilder msg

    logDebug "Sending context message..."
    runConduitRes $ sourceList [encMsg] .| appSink appData
    logDebug "Starting timers..."
    startTimers hdl

    logDebug "Running chains..."
    race_
        (runConduitRes (appSource appData .| processReadTML hdl processSLEMsg))
        (runConduitRes (processWriteTML hdl .| appSink appData))

    return ()

onDisconnect
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasEventHandler env
       , HasLogFunc env
       , HasTimer env
       )
    => m ()
onDisconnect = do
    logInfo "SLE: Disconnected from provider"
    stopTimers
    sleRaiseEvent TMLDisconnect
    return ()

processSLEMsg
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
    => ConduitT TMLMessage Void m ()
processSLEMsg = do
    awaitForever $ \msg -> do
        let encSle = msg ^. tmlMsgData
        case decodeASN1 DER (BL.fromStrict encSle) of
            Left err ->
                logError $ "Error decoding ASN1 message: " <> displayShow err
            Right ls -> do
                lift $ logDebug $ "Received ASN1: " <> fromString (ppShow ls)
                let result = parseASN1 slePduParser ls
                case result of
                    Left err ->
                        logError $ "Error decoding SLE message: " <> display err
                    Right pdu -> do
                        -- lift $ processSleBind bind
                        logDebug $ "SLE Message:\n" <> fromString (ppShow pdu)


processServerSLEMsg
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env)
    => (SlePdu -> m ())
    -> ConduitT TMLMessage Void m ()
processServerSLEMsg processSlePdu = awaitForever $ \tlm -> do
    let encSle = tlm ^. tmlMsgData
    case decodeASN1 DER (BL.fromStrict encSle) of
        Left err ->
            logError $ "Error decoding ASN1 message: " <> displayShow err
        Right ls -> do
            lift $ logDebug $ "Received ASN1: " <> fromString (ppShow ls)
            let result = parseASN1 slePduParser ls
            case result of
                Left err ->
                    logError $ "Error decoding SLE PDU: " <> display err
                Right pdu -> do
                    lift $ logDebug $ "Received SLE PDU: " <> fromString
                        (ppShow pdu)
                    lift $ processSlePdu pdu


-- | Read input from the queue from the 'SleHandle'. If there is nothing received 
-- within the configured timeout (send heartbeat), returns 'Nothing'. This indicates
-- that a heartbeat message should be sent
readSLEInput
    :: (MonadIO m, MonadReader env m, HasTimer env)
    => SleHandle
    -> m (Maybe SleWrite)
readSLEInput hdl = do
    env   <- ask
    val   <- liftIO $ readTVarIO (env ^. hbt)
    delay <- registerDelay (fromIntegral val)
    atomically
        $   Just
        <$> readSLEHandle hdl
        <|> (readTVar delay >>= checkSTM >> pure Nothing)



-- | Processes the SleWrite and yields a 'ByteString' which is the encoded message 
-- if there is one. Returns 'True' if the loop should terminate and 'False' otherwise.
processSLEInput
    :: (MonadUnliftIO m, MonadReader env m, HasCommonConfig env, HasLogFunc env)
    => SleWrite
    -> ConduitT () ByteString m Bool
processSLEInput SLEAbort      = return True
processSLEInput SLEAbortPeer  = return True
processSLEInput SLEStopListen = return True
processSLEInput (SLEMsg msg)  = do
    logDebug $ "processSLEInput: " <> fromString (ppShow msg)
    yield $ builderBytes $ tmlMessageBuilder msg
    return False
processSLEInput (SLEPdu pdu) = do
    logDebug $ "processSLEInput: SLE PDU: " <> fromString (ppShow pdu)
    cfg    <- view commonCfg
    encPdu <- liftIO $ encodePDU cfg pdu
    let tlmMsg    = tmlSleMsg encPdu
        encTlmMsg = builderBytes $ tmlMessageBuilder tlmMsg
    logDebug $ "processSLEInput: sending TLM Message: " <> fromString
        (ppShow tlmMsg)
    yield encTlmMsg
    return False

-- | Listen on the queue in the 'SleHandle'. If it returns 'Nothing', this means that 
-- a timeout occured and a heartbeat message is yielded. Otherwise, 'processSLEInput'
-- is called.
processWriteTML
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasCommonConfig env
       , HasTimer env
       , HasLogFunc env
       )
    => SleHandle
    -> ConduitT () ByteString m ()
processWriteTML hdl = go
  where
    go = do
        val <- readSLEInput hdl
        case val of
            Just inp -> do
                logDebug $ "Sending SLE Input: " <> fromString (ppShow inp)
                terminate <- processSLEInput inp
                if terminate
                    then
                        logDebug
                            "Received termination request, terminating write chain"
                    else go
            Nothing -> do
                logDebug "Sending heartbeat message..."
                yield heartBeatMessage
                go

-- | Runs a server socket and listens for incoming connections. Calls 'onServerDisconnect'
-- in case the client disconnects and calls 'processServerReadSLE' and 
-- 'processServerSendSLE' in parallel threads for reading/writing the socket
listenRAF
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasEventHandler env
       , HasProviderConfig env
       , HasTimer env
       , HasRAF env
       )
    => SleHandle
    -> RAFConfig
    -> RAFIdx
    -> (SlePdu -> m ())
    -> m ()
listenRAF hdl cfg idx process = do
    void
        $ runGeneralTCPServer
              (serverSettings (fromIntegral (cfg ^. cfgRAFPort)) "*")
        $ \app -> do
              logInfo
                  $  "SLE RAF "
                  <> display (cfg ^. cfgRAFSII)
                  <> ": new connection on server socket: "
                  <> display (cfg ^. cfgRAFPort)
              let netThreads = race_ (processServerReadSLE hdl process app)
                                     (processServerSendSLE hdl app)
              race_ netThreads (processSleTransferBuffer hdl cfg idx)
              logInfo
                  $  "SLE RAF "
                  <> display (cfg ^. cfgRAFSII)
                  <> " disconnect on server socket: "
                  <> display (cfg ^. cfgRAFPort)
    onServerDisconnect

-- | Reads from the socket and forwards the data to the TML Message parser conduit.
-- On the first run, it is waitet the configured time for a TML Context Message. 
-- If one is received, regular processing of TML messages start. Otherwise, a protocol
-- abort is sent.
processServerReadSLE
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasEventHandler env
       , HasProviderConfig env
       , HasTimer env
       )
    => SleHandle
    -> (SlePdu -> m ())
    -> AppData
    -> m ()
processServerReadSLE hdl process app = do
    env <- ask
    let initChain = appSource app .| conduitParserEither tmlPduParser .| sink
        initTime =
            fromIntegral (cfgServerInitTime (env ^. commonCfg . cfgTML))
                + 1_000_000

    -- first, run initial waiting for context message
    result <- race (runConduitRes initChain) (threadDelay initTime)
    case result of
        Right _ -> do
            logDebug "Timeout waiting for context message, aborting"
            protocolAbort hdl
        Left (Left err) -> do
            logDebug (display err)
            protocolAbort hdl
        Left (Right Nothing) -> do -- we should terminate
            logDebug "Conduit told us to terminate..."
            protocolAbort hdl
        Left (Right (Just ctxtMsg)) -> processContext hdl ctxtMsg

    -- now, if we are still here, start the normal processing
    runConduitRes $ appSource app .| processReadTML
        hdl
        (processServerSLEMsg (lift . process))

  where
    sink = do
        x <- await
        case x of
            Nothing  -> return $ Right Nothing
            Just val -> do
                env <- ask
                case val of
                    Left  err      -> return $ Left (T.pack (errorMessage err))
                    Right (_, pdu) -> do
                        logDebug $ "Received PDU: " <> fromString (ppShow pdu)
                        -- process pdu
                        case checkPDU (env ^. commonCfg . cfgTML) pdu of
                            Left  err     -> return $ Left err
                            Right ctxtMsg -> return $ Right (Just ctxtMsg)


-- | Processes the writing side of the socket. This function uses 'processWriteTML' 
-- internally, which is also used for the client side.
processServerSendSLE
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProviderConfig env
       , HasTimer env
       )
    => SleHandle
    -> AppData
    -> m ()
processServerSendSLE hdl app =
    runConduitRes $ processWriteTML hdl .| appSink app


processSleTransferBuffer
    :: (MonadUnliftIO m, MonadReader env m, HasRAF env)
    => SleHandle
    -> RAFConfig
    -> RAFIdx
    -> m ()
processSleTransferBuffer hdl cfg idx = do
    env <- ask
    loop env

  where
    loop env = do
        raf <- getRAF env idx
        case raf ^. rafState of
            ServiceActive -> do
                pdus <- readFrameOrNotifications
                    hdl
                    (Timeout (cfg ^. cfgRAFLatency))
                writeSLE hdl (SLEPdu (SlePduRafTranserBuffer pdus))
            _ -> atomically $ do
                r <- getRAFSTM env idx
                when (r ^. rafState /= ServiceActive) retrySTM
        loop env
