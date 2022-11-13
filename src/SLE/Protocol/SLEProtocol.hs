module SLE.Protocol.SLEProtocol
    ( listenSLE
    , connectSLE
    ) where

import           RIO
import qualified RIO.ByteString.Lazy           as BL
import qualified RIO.Text                      as T

import           ByteString.StrictBuilder

import           Conduit
import           Conduit.SocketReconnector

import           Control.Concurrent.Killable

import           Data.Conduit.Attoparsec
import           Data.Conduit.List
import           Data.Conduit.Network
import           Network.Socket                 ( PortNumber )
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

import           SLE.State.Events
import           SLE.State.Classes

import           SLE.Protocol.TMLProtocol

import           Data.ASN1.BinaryEncoding
import           Data.ASN1.Encoding

import           Text.Show.Pretty

import           Text.Builder                  as TB



connectSLE
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasEventHandler env
       , HasUserConfig env
       , HasSleHandle env
       , HasTimer env
       )
    => ConnectAddr
    -> m ()
connectSLE addr = do
    runGeneralTCPReconnectClient
        (clientSettings (fromIntegral (port addr)) (encodeUtf8 (host addr)))
        200000
        processConnect
        onDisconnect


processConnect
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasEventHandler env
       , HasLogFunc env
       , HasCommonConfig env
       , HasSleHandle env
       , HasTimer env
       )
    => AppData
    -> m ()
processConnect appData = do
    env <- ask
    logInfo "SLE: Connected to provider."
    liftIO $ sleRaiseEvent env TMLConnect

    -- now send the context 
    let msg = TMLCtxtMessage { _tmlCtxHbt   = cfgHeartbeat cfg
                             , _tmlCtxDeadf = cfgDeadFactor cfg
                             }
        cfg    = env ^. commonCfg . cfgTML

        encMsg = builderBytes $ tmlContextMsgBuilder msg

    logDebug "Sending context message..."
    runConduitRes $ sourceList [encMsg] .| appSink appData
    logDebug "Starting timers..."
    startTimers

    logDebug "Running chains..."
    race_
        (runConduitRes (appSource appData .| processReadTML processSLEMsg))
        (runConduitRes (processWriteTML .| appSink appData))

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
    env <- ask
    logInfo "SLE: Disconnected from provider"
    stopTimers
    liftIO $ sleRaiseEvent env TMLDisconnect
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



processServerSLEMsg
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasEventHandler env
       , HasLogFunc env
       , HasSleHandle env
       , HasProviderConfig env
       )
    => ConduitT TMLMessage Void m ()
processServerSLEMsg = do
    processSLEBind
    processSLEMsg




processSLEBind
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasEventHandler env
       , HasSleHandle env
       , HasCommonConfig env
       )
    => ConduitT TMLMessage Void m ()
processSLEBind = do
    x <- await
    case x of
        Nothing  -> return ()
        Just msg -> do
            let encSle = msg ^. tmlMsgData
            case decodeASN1 DER (BL.fromStrict encSle) of
                Left err ->
                    logError
                        $  "Error decoding ASN1 message: "
                        <> displayShow err
                Right ls -> do
                    lift $ logDebug $ "Received ASN1: " <> fromString
                        (ppShow ls)
                    let result = parseASN1 parseSleBind ls
                    case result of
                        Left err ->
                            logError
                                $  "Error decoding SLE BIND message: "
                                <> display err
                        Right bind -> do
                            lift $ processSleBind bind



processSleBind
    :: ( MonadIO m
       , MonadReader env m
       , HasLogFunc env
       , HasCommonConfig env
       , HasEventHandler env
       , HasSleHandle env
       )
    => SleBindInvocation
    -> m ()
processSleBind msg = do
    logDebug $ "Received SLE Bind Invocation: " <> fromString (ppShow msg)
    env <- ask
    liftIO $ sleRaiseEvent env (SLEBindReceived msg)

    -- TODO: for now, we just send a positive result back 
    let cfg = env ^. commonCfg
    let ret = SleBindReturn { _sleBindRetCredentials = Nothing
                            , _sleBindRetResponderID = cfg ^. cfgResponder
                            , _sleBindRetResult      = Left (VersionNumber 3)
                            }
        pdu = SlePduBindReturn ret

    -- send the bind response
    writeSLEInput (env ^. getHandle) (SLEPdu pdu)

    return ()





-- | Read input from the queue from the 'SleHandle'. If there is nothing received 
-- within the configured timeout (send heartbeat), returns 'Nothing'. This indicates
-- that a heartbeat message should be sent
readSLEInput
    :: (MonadIO m, MonadReader env m, HasTimer env, HasSleHandle env)
    => m (Maybe SleInput)
readSLEInput = do
    env   <- ask
    val   <- liftIO $ readTVarIO (env ^. hbt)
    delay <- registerDelay (fromIntegral val)
    atomically
        $   Just
        <$> readTBQueue (env ^. getHandle . sleInput)
        <|> (readTVar delay >>= checkSTM >> pure Nothing)



-- | Processes the SleInput and yields a 'ByteString' which is the encoded message 
-- if there is one. Returns 'True' if the loop should terminate and 'False' otherwise.
processSLEInput
    :: (MonadUnliftIO m, MonadReader env m, HasCommonConfig env, HasLogFunc env)
    => SleInput
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
    logDebug
        $  "processSLEInput: sending TLM Message: "
        <> fromString (ppShow tlmMsg)
        <> "\n Encoded: "
        <> display (TB.run (hexData encTlmMsg))
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
       , HasSleHandle env
       , HasLogFunc env
       )
    => ConduitT () ByteString m ()
processWriteTML = go
  where
    go = do
        val <- readSLEInput
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
listenSLE
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasEventHandler env
       , HasProviderConfig env
       , HasSleHandle env
       , HasTimer env
       )
    => PortNumber
    -> m ()
listenSLE serverPort = do
    void
        $ runGeneralTCPServer (serverSettings (fromIntegral serverPort) "*")
        $ \app -> race_ (processServerReadSLE app) (processServerSendSLE app)
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
       , HasSleHandle env
       , HasTimer env
       )
    => AppData
    -> m ()
processServerReadSLE app = do
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
            protocolAbort
        Left (Left err) -> do
            logDebug (display err)
            protocolAbort
        Left (Right Nothing) -> do -- we should terminate
            logDebug "Conduit told us to terminate..."
            protocolAbort
        Left (Right (Just ctxtMsg)) -> processContext ctxtMsg

    -- now, if we are still here, start the normal processing
    runConduitRes $ appSource app .| processReadTML processServerSLEMsg

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
       , HasSleHandle env
       , HasTimer env
       )
    => AppData
    -> m ()
processServerSendSLE app = runConduitRes $ processWriteTML .| appSink app
