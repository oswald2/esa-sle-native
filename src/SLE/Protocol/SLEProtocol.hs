module SLE.Protocol.SLEProtocol
    ( listenRAF
    , listenFCLTU
    , connectSLE
    ) where

import           RIO
import qualified RIO.ByteString                as B
import qualified RIO.ByteString.Lazy           as BL
import qualified RIO.Text                      as T

import           ByteString.StrictBuilder

import           Conduit
import           Conduit.SocketReconnector

import           Data.Conduit.Attoparsec
import           Data.Conduit.List
import           Data.Conduit.Network

import           SLE.Data.Bind
import           SLE.Data.Common
import           SLE.Data.CommonConfig
import           SLE.Data.DEL
import           SLE.Data.Handle
import           SLE.Data.Ops
import           SLE.Data.PDU
import           SLE.Data.PDUParser
import           SLE.Data.ProviderConfig
import           SLE.Data.TMLConfig
import           SLE.Data.TMLMessage
import           SLE.Data.WriteCmd

import           SLE.State.Classes
import           SLE.State.Events
import           SLE.State.FCLTUClasses
import           SLE.State.RAFClasses
import           SLE.State.RAFState

import           SLE.Protocol.TMLProtocol

import           Data.ASN1.BinaryEncoding
import           Data.ASN1.Encoding
import           Data.STM.TimedBuffer

import           Text.Show.Pretty

-- import           Text.Builder                  as TB



connectSLE
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasEventHandler env
       , HasUserConfig env
       , HasTimer env
       , HasRAF env
       , HasFCLTU env
       )
    => ApplicationIdentifier
    -> SleHandle
    -> ConnectAddr
    -> (Word64 -> IO ())
    -> m ()
connectSLE appID hdl addr perfFunc = do
    runGeneralTCPReconnectClient
        (clientSettings (fromIntegral (port addr)) (encodeUtf8 (host addr)))
        200000
        (processConnect appID hdl perfFunc)
        onDisconnect


processConnect
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasEventHandler env
       , HasLogFunc env
       , HasCommonConfig env
       , HasTimer env
       , HasRAF env
       , HasFCLTU env
       )
    => ApplicationIdentifier
    -> SleHandle
    -> (Word64 -> IO ())
    -> AppData
    -> m ()
processConnect appID hdl perfFunc appData = do
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
        (runConduitRes
            (appSource appData .| processReadTML hdl (processSLEMsg appID))
        )
        (runConduitRes (processWriteTML hdl perfFunc .| appSink appData))

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
    => ApplicationIdentifier
    -> ConduitT TMLMessage Void m ()
processSLEMsg appId = do
    awaitForever $ \msg -> do
        let encSle = msg ^. tmlMsgData
        case decodeASN1 DER (BL.fromStrict encSle) of
            Left err ->
                logError $ "Error decoding ASN1 message: " <> displayShow err
            Right ls -> do
                lift $ logDebug $ "Received ASN1: " <> fromString (ppShow ls)
                let result = parseASN1 (slePduParser appId) ls
                case result of
                    Left err ->
                        logError $ "Error decoding SLE message: " <> display err
                    Right pdu -> do
                        -- lift $ processSleBind bind
                        logDebug $ "SLE Message:\n" <> fromString (ppShow pdu)


processServerSLEMsg
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasCommonConfig env)
    => ApplicationIdentifier
    -> (SlePdu -> m ())
    -> ConduitT TMLMessage Void m ()
processServerSLEMsg appID processSlePdu = do
    await >>= \case
        Nothing  -> return ()
        Just tlm -> do
            let encSle = tlm ^. tmlMsgData
            case decodeASN1 DER (BL.fromStrict encSle) of
                Left err ->
                    logError
                        $  "Error decoding ASN1 message: "
                        <> displayShow err
                Right ls -> do
                    logDebug $ "Received ASN1: " <> fromString (ppShow ls)
                    let result = parseASN1 (slePduParser appID) ls
                    case result of
                        Left err ->
                            logError $ "Error decoding SLE PDU: " <> display err
                        Right pdu -> do
                            logDebug $ "Received SLE PDU: " <> fromString
                                (ppShow pdu)
                            lift $ processSlePdu pdu
                            processServerSLEMsg appID processSlePdu


-- | Processes the SleWrite and yields a 'ByteString' which is the encoded message 
-- if there is one. Returns 'True' if the loop should terminate and 'False' otherwise.
processSLEInput
    :: (MonadUnliftIO m, MonadReader env m, HasCommonConfig env, HasLogFunc env)
    => (Word64 -> IO ())
    -> SleWrite
    -> ConduitT () ByteString m Bool
processSLEInput _        SLEAbort      = return True
processSLEInput _        SLEAbortPeer  = return True
processSLEInput _        SLEStopListen = return True
processSLEInput perfFunc (SLEMsg msg)  = do
    logDebug $ "processSLEInput: " <> fromString (ppShow msg)
    let dat = builderBytes $ tmlMessageBuilder msg
    yield dat
    liftIO $ perfFunc (fromIntegral (B.length dat))
    return False
processSLEInput perfFunc (SLEPdu pdu) = do
    logDebug $ "processSLEInput: SLE PDU: " <> fromString (ppShow pdu)
    cfg    <- view commonCfg
    encPdu <- liftIO $ encodePDU cfg pdu
    let tlmMsg    = tmlSleMsg encPdu
        encTlmMsg = builderBytes $ tmlMessageBuilder tlmMsg
    logDebug $ "processSLEInput: sending TLM Message: " <> fromString
        (ppShow tlmMsg)
    yield encTlmMsg
    when (isTransfer pdu) $ liftIO $ perfFunc
        (fromIntegral (B.length encTlmMsg))
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
    -> (Word64 -> IO ())
    -> ConduitT () ByteString m ()
processWriteTML hdl perfFunc = go
  where
    go = do
        val <- readSLE hdl
        case val of
            Just inp -> do
                logDebug $ "Sending SLE Input: " <> fromString (ppShow inp)
                terminate <- processSLEInput perfFunc inp
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
       , HasFCLTU env
       )
    => SleHandle
    -> RAFConfig
    -> RAFIdx
    -> (SlePdu -> m ())
    -> (Word64 -> IO ())
    -> m ()
listenRAF hdl cfg idx process perfFunc = do
    void
        $ runGeneralTCPServer
              (serverSettings (fromIntegral (cfg ^. cfgRAFPort)) "*")
        $ \app -> do
              logInfo
                  $  "SLE RAF "
                  <> display (cfg ^. cfgRAFSII)
                  <> ": new connection on server socket: "
                  <> display (cfg ^. cfgRAFPort)
              let netThreads = race_
                      (processServerReadSLE RtnAllFrames hdl process app)
                      (processServerSendSLE hdl perfFunc app)
              res :: Either SomeException () <- try
                  $ race_ netThreads (processSleTransferBuffer hdl cfg idx)
              onServerDisconnect hdl
              logWarn
                  $  "SLE RAF "
                  <> display (cfg ^. cfgRAFSII)
                  <> " disconnect on server socket: "
                  <> display (cfg ^. cfgRAFPort)
                  <> ": "
                  <> fromString (show res)

    -- loop over, we need to start again on disconnect
    listenRAF hdl cfg idx process perfFunc

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
       , HasRAF env
       , HasFCLTU env
       )
    => ApplicationIdentifier
    -> SleHandle
    -> (SlePdu -> m ())
    -> AppData
    -> m ()
processServerReadSLE appID hdl process app = do
    env <- ask
    let initChain = appSource app .| conduitParserEither tmlPduParser .| sink
        initTime =
            fromIntegral (cfgServerInitTime (env ^. commonCfg . cfgTML))
                + 1_000_000

    -- first, run initial waiting for context message
    result <- race (runConduitRes initChain) (threadDelay initTime)
    case result of
        Right _ -> do
            logError "Timeout waiting for SLE TML context message, aborting"
            protocolAbort hdl
        Left (Left err) -> do
            logError
                $  "Error waiting for SLE TML context message: "
                <> display err
            protocolAbort hdl
        Left (Right Nothing) -> do -- we should terminate
            logDebug "Conduit told us to terminate..."
            protocolAbort hdl
        Left (Right (Just ctxtMsg)) -> do
            processContext hdl ctxtMsg
            -- now, if we are still here, start the normal processing
            (res :: Either SomeException ()) <-
                try $ runConduitRes $ appSource app .| processReadTML
                    hdl
                    (processServerSLEMsg appID (lift . process))
            case res of
                Left err ->
                    logWarn $ "processServerReadSLE leaves with: " <> fromString
                        (show err)
                Right () -> return ()

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
    -> (Word64 -> IO ())
    -> AppData
    -> m ()
processServerSendSLE hdl perfFunc app = do
    res :: Either SomeException () <-
        try $ runConduitRes $ processWriteTML hdl perfFunc .| appSink app
    case res of
        Left err ->
            logWarn $ "processServerSendSLE leaves " <> fromString (show err)
        Right _ -> return ()


processSleTransferBuffer
    :: (MonadUnliftIO m, MonadReader env m, HasRAF env, HasLogFunc env)
    => SleHandle
    -> RAFConfig
    -> RAFIdx
    -> m ()
processSleTransferBuffer hdl cfg idx = do
    env                            <- ask
    res :: Either SomeException () <- try $ loop env
    case res of
        Left err ->
            logWarn $ "processSleTransferBuffer leaves" <> fromString (show err)
        Right _ -> return ()

  where
    loop :: (MonadUnliftIO m, MonadReader env m, HasRAF env) => env -> m ()
    loop env = do
        raf' <- getRAF env idx
        forM_ raf' $ \raf -> do
            case raf ^. rafState of
                ServiceActive -> do
                    pdus <- readFrameOrNotifications
                        hdl
                        (Timeout (cfg ^. cfgRAFLatency))
                    unless (null pdus) $ writeSLE
                        hdl
                        (SLEPdu (SlePduRafTransferBuffer pdus))
                _ -> atomically $ do
                    r' <- getRAFSTM env idx
                    forM_ r' $ \r ->
                        when (r ^. rafState /= ServiceActive) retrySTM
        loop env



listenFCLTU
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasEventHandler env
       , HasProviderConfig env
       , HasTimer env
       , HasRAF env
       , HasFCLTU env
       )
    => SleHandle
    -> FCLTUConfig
    -> FCLTUIdx
    -> (SlePdu -> m ())
    -> m ()
listenFCLTU hdl cfg idx process = do
    void
        $ runGeneralTCPServer
              (serverSettings (fromIntegral (cfg ^. cfgFCLTUPort)) "*")
        $ \app -> do
              logInfo
                  $  "SLE FCLTU "
                  <> display (cfg ^. cfgFCLTUSII)
                  <> ": new connection on server socket: "
                  <> display (cfg ^. cfgFCLTUPort)
              res :: Either SomeException () <- try $ race_
                  (processServerReadSLE FwdCltu hdl process app)
                  (processServerSendSLE hdl (\_ -> return ()) app)
              onServerDisconnect hdl
              logWarn
                  $  "SLE FCLTU "
                  <> display (cfg ^. cfgFCLTUSII)
                  <> " disconnect on server socket: "
                  <> display (cfg ^. cfgFCLTUPort)
                  <> ": "
                  <> fromString (show res)

    -- loop over, we need to start again on disconnect
    listenFCLTU hdl cfg idx process
