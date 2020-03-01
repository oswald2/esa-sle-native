module Data.SLE.TMLProtocol
  ( connectSLE
  )
where

import           RIO
import qualified RIO.Text as T
import           Conduit 
import           Conduit.SocketReconnector
import           Data.Conduit.Network
import           Data.Conduit.List 
import           Data.Conduit.Attoparsec
--import           Data.Conduit.TQueue
import           System.Timer.Updatable
import           Control.Concurrent.Killable
import           ByteString.StrictBuilder

import           Data.SLE.TMLConfig
import           Data.SLE.TMLMessage
import           Data.SLE.SLEInput

import           State.SLEEvents
import           State.Classes

connectSLE
  :: (MonadUnliftIO m
    , MonadReader env m
    , HasLogFunc env
    , HasEventHandler env
    , HasConfig env
    , HasSleInput env 
    , HasTimer env)
  => ConnectAddr
  -> m ()
connectSLE addr = do
  runGeneralTCPReconnectClient
    (clientSettings (fromIntegral (port addr)) (encodeUtf8 (host addr)))
    200000
    processConnect
    onDisconnect 


processConnect
  :: (MonadUnliftIO m
    , MonadReader env m
    , HasEventHandler env
    , HasLogFunc env 
    , HasConfig env
    , HasSleInput env 
    , HasTimer env)
  => AppData
  -> m ()
processConnect appData = do
  env <- ask
  logInfo "SLE: Connected to provider."
  liftIO $ sleRaiseEvent env TMLConnect

  -- now send the context 
  let msg = TMLCtxtMessage {
        _tmlCtxHbt = (cfgHeartbeat cfg)
        , _tmlCtxDeadf = (cfgDeadFactor cfg)
        }
      cfg = env ^. getTMLConfig

      encMsg = builderBytes $ tmlContextMsgBuilder msg 

  logDebug "Sending context message..."
  runConduitRes $ sourceList [encMsg] .| appSink appData
  logDebug "Starting timers..."
  startTimers

  logDebug "Running chains..."
  race_ (runConduitRes (appSource appData .| processReadTML))
        (runConduitRes (processWriteTML .| appSink appData))

  return ()

onDisconnect
  :: (MonadUnliftIO m
    , MonadReader env m
    , HasEventHandler env
    , HasLogFunc env 
    , HasTimer env)
  => m ()
onDisconnect = do
  env <- ask
  logInfo "SLE: Disconnected from provider"
  stopTimers
  liftIO $ sleRaiseEvent env TMLDisconnect
  return ()



processReadTML :: (MonadUnliftIO m
    , MonadReader env m
    , HasConfig env 
    , HasEventHandler env 
    , HasLogFunc env 
    , HasSleInput env
    , HasTimer env)
    => ConduitT ByteString Void m () 
processReadTML = do 
  conduitParserEither tmlPduParser .| worker .| processPDU .| Conduit.sinkNull
  where 
    worker = do 
      x <- await  
      case x of 
        Nothing -> return () 
        Just val -> do 
          case val of 
            Left err -> do
              env <- ask 
              liftIO $ do
                sleRaiseEvent env (TMLParseError (T.pack (errorMessage err)))
                runRIO env protocolAbort
            Right (_, pdu) -> do 
              logDebug $ "Received PDU: " <> displayShow pdu
              lift $ restartHBRTimer
              -- process pdu
              yield pdu 
              worker 
        

processPDU :: (MonadUnliftIO m
    , MonadReader env m
    , HasEventHandler env 
    , HasLogFunc env 
    , HasSleInput env 
    , HasTimer env)
    => ConduitT TMLPDU TMLMessage m () 
processPDU = do 
  awaitForever $ \case 
    TMLPDUHeartBeat -> do
      logDebug "Received heartbeat."
    TMLPDUCtxt ctxt -> do 
      logDebug $ "Received context message: " <> displayShow ctxt
      lift $ processContext ctxt 
    TMLPDUMessage msg -> do 
      lift $ logDebug $ "Yielding PDU:" <> displayShow msg
      yield msg 


processContext :: (MonadUnliftIO m
  , MonadReader env m
  , HasTimer env
  , HasLogFunc env 
  , HasSleInput env 
  , HasEventHandler env) 
    => TMLContextMsgRead -> m () 
processContext msg = do
  if chkContextMsg msg 
    then do
      stopTimers 
      startTimersWith (_tmlCtxHeartbeatInterval msg) (_tmlCtxDeadFactor msg)
    else do 
      peerAbort



heartBeatMessage :: ByteString 
heartBeatMessage = builderBytes $ tmlHeartBeatMsgBuilder TMLHeartBeatMessage

processWriteTML :: (MonadUnliftIO m
  , MonadReader env m 
  , HasTimer env
  , HasSleInput env
  , HasLogFunc env) => ConduitT () ByteString m () 
processWriteTML = go 
  where 
    go = do 
      val <- readSLEInput 
      case val of 
        Just inp -> do 
          terminate <- processSLEInput inp  
          if terminate 
            then logDebug "Received termination request, terminating write chain"
            else go 
        Nothing -> do
          logDebug "Sending heartbeat message..."
          yield heartBeatMessage
          go 


readSLEInput :: (MonadIO m
  , MonadReader env m
  , HasTimer env
  , HasSleInput env) => m (Maybe SLEInput)
readSLEInput = do 
  env <- ask 
  val <- liftIO $ readTVarIO (env ^. hbt)
  delay <- registerDelay (fromIntegral val)
  atomically $ 
    Just <$> readTBQueue (env ^. getInput)
    <|> (readTVar delay >>= checkSTM >> pure Nothing)


processSLEInput :: (MonadUnliftIO m
  , MonadReader env m 
  , HasTimer env
  , HasSleInput env
  , HasLogFunc env) => SLEInput -> ConduitT () ByteString m Bool
processSLEInput SLEAbort = return True 
processSLEInput SLEAbortPeer = return True 
processSLEInput (SLEMsg msg) = do 
  yield $ builderBytes $ tmlMessageBuilder msg 
  return False 



startTimers :: (MonadUnliftIO m
  , MonadReader env m
  , HasConfig env
  , HasLogFunc env 
  , HasSleInput env 
  , HasEventHandler env 
  , HasTimer env) => m () 
startTimers = do 
  env <- ask 
  let cfg = env ^. getTMLConfig
  startTimersWith (cfgHeartbeat cfg) (cfgDeadFactor cfg)


startTimersWith :: (MonadUnliftIO m
  , MonadReader env m
  , HasEventHandler env 
  , HasLogFunc env 
  , HasSleInput env
  , HasTimer env) => Word16 -> Word16 -> m () 
startTimersWith hbTime' deadFactor = do 
  env <- ask
  let --hbTime = fromIntegral hbTime' * 1_000_000
      hbrTime = fromIntegral hbTime' * fromIntegral deadFactor * 1_000_000

  logDebug "Starting timers..."
  --hbTimer <- liftIO $ replacer (runRIO env heartBeatTimeOut) hbTime
  hbrTimer <- liftIO $ replacer (runRIO env heartBeatReceiveTimeOut) hbrTime

  atomically $ do 
    --writeTVar (env ^. getTimerHBT) (Just hbTimer)
    writeTVar (env ^. getTimerHBR) (Just hbrTimer)

  return () 



stopTimers :: (MonadUnliftIO m
  , MonadReader env m 
  , HasLogFunc env 
  , HasTimer env) => m () 
stopTimers = do 
  env <- ask 
  logDebug "Stopping timers..."
  action <- atomically $ do 
    hbTimer <- readTVar  (env ^. getTimerHBT)
    hbrTimer <- readTVar (env ^. getTimerHBR)
    return $ do 
      forM_ hbTimer kill 
      forM_ hbrTimer kill 

  liftIO action 




-- restartHBTTimer :: (MonadUnliftIO m
--   , MonadReader env m
--   , HasConfig env
--   , HasLogFunc env 
--   , HasTimer env) => m () 
-- restartHBTTimer = do 
--   env <- ask 
--   let cfg = env ^. getTMLConfig
--       hbTime = fromIntegral (cfgHeartbeat cfg) * 1_000_000
--   logDebug "Restarting HeartBeat Timer"
--   atomically $ do 
--     let tvar = env ^. getTimerHBT
--     timer <- readTVar tvar
--     case timer of 
--       Nothing -> return () 
--       Just t -> renew t hbTime 


restartHBRTimer :: (MonadUnliftIO m
  , MonadReader env m
  , HasConfig env
  , HasLogFunc env 
  , HasTimer env) => m () 
restartHBRTimer = do 
  env <- ask 
  let cfg = env ^. getTMLConfig
      hbTime = fromIntegral (cfgHeartbeat cfg) * fromIntegral (cfgDeadFactor cfg) * 1_000_000
  logDebug "Restarting HeartBeat Reception Timer"
  atomically $ do 
    let tvar = env ^. getTimerHBR
    timer <- readTVar tvar
    case timer of 
      Nothing -> return () 
      Just t -> renew t hbTime 



-- heartBeatTimeOut :: (HasLogFunc env) => RIO env () 
-- heartBeatTimeOut = do 
--   logWarn "heartBeatTimeOut"


heartBeatReceiveTimeOut :: (HasLogFunc env, HasSleInput env, HasEventHandler env) => RIO env () 
heartBeatReceiveTimeOut = do 
  logWarn "heartBeatReceiveTimeout"
  protocolAbort  


protocolAbort :: (HasLogFunc env, HasEventHandler env, HasSleInput env) => RIO env () 
protocolAbort = do 
  logDebug "ProtocolAbort!"
  env <- ask
  liftIO $ sleRaiseEvent env TMLProtocolAbort
  atomically $ writeTBQueue (env ^. getInput) SLEAbort 


peerAbort :: (MonadIO m
  , MonadReader env m
  , HasLogFunc env
  , HasSleInput env
  , HasEventHandler env) => m () 
peerAbort = do 
  logDebug "PeerAbort!"
  env <- ask
  liftIO $ sleRaiseEvent env TMLPeerAbort
  liftIO $ sleRaiseEvent env TMLProtocolAbort
  atomically $ writeTBQueue (env ^. getInput) SLEAbort 
