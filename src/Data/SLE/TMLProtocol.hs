module Data.SLE.TMLProtocol
  ( connectSLE
  )
where

import           RIO
import qualified Data.Text.IO as T 
import           Conduit 
import           Conduit.SocketReconnector
import           Data.Conduit.Network
import           Data.Conduit.List 
import           System.Timer.Updatable
import           Control.Concurrent.Killable
import           ByteString.StrictBuilder

import           Data.SLE.TMLConfig
import           Data.SLE.TMLMessage

import           State.SLEEvents
import           State.Classes

connectSLE
  :: (MonadUnliftIO m
    , MonadReader env m
    , HasLogFunc env
    , HasEventHandler env
    , HasConfig env
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
    , HasConfig env
    , HasTimer env)
  => AppData
  -> m ()
processConnect appData = do
  env <- ask
  liftIO $ sleRaiseEvent env TMLConnect

  -- now send the context 
  let msg = TMLCtxtMessage {
        _tmlCtxHbt = (cfgHeartbeat cfg)
        , _tmlCtxDeadf = (cfgDeadFactor cfg)
        }
      cfg = env ^. getTMLConfig

      encMsg = builderBytes $ tmlContextMsgBuilder msg 

  runConduitRes $ sourceList [encMsg] .| appSink appData
  startTimers

  race_ (runConduitRes (appSource appData .| processReadTML))
        (runConduitRes (processWriteTML .| appSink appData))

  return ()

onDisconnect
  :: (MonadUnliftIO m
    , MonadReader env m
    , HasEventHandler env
    , HasTimer env)
  => m ()
onDisconnect = do
  env <- ask
  stopTimers
  liftIO $ sleRaiseEvent env TMLDisconnect
  return ()



processReadTML :: (MonadUnliftIO m
    , MonadReader env m
    , HasConfig env 
    , HasTimer env)
    => ConduitT ByteString Void m () 
processReadTML = awaitForever $ \_ -> do 
  lift $ restartHBTTimer


processWriteTML :: ConduitT () ByteString m () 
processWriteTML = return ()


startTimers :: (MonadUnliftIO m
  , MonadReader env m
  , HasConfig env
  , HasEventHandler env 
  , HasTimer env) => m () 
startTimers = do 
  env <- ask 
  let cfg = env ^. getTMLConfig
      hbTime = fromIntegral (cfgHeartbeat cfg) * 1_000_000
      hbrTime = fromIntegral (cfgHeartbeat cfg) * fromIntegral (cfgDeadFactor cfg) * 1_000_000

  hbTimer <- liftIO $ replacer heartBeatTimeOut hbTime
  hbrTimer <- liftIO $ replacer (heartBeatReceiveTimeOut env) hbrTime

  atomically $ do 
    writeTVar (env ^. getTimerHBT) (Just hbTimer)
    writeTVar (env ^. getTimerHBR) (Just hbrTimer)

  return () 



stopTimers :: (MonadUnliftIO m
  , MonadReader env m
  , HasTimer env) => m () 
stopTimers = do 
  env <- ask 
  action <- atomically $ do 
    hbTimer <- readTVar  (env ^. getTimerHBT)
    hbrTimer <- readTVar (env ^. getTimerHBR)
    return $ do 
      forM_ hbTimer kill 
      forM_ hbrTimer kill 

  liftIO action 




restartHBTTimer :: (MonadUnliftIO m
  , MonadReader env m
  , HasConfig env
  , HasTimer env) => m () 
restartHBTTimer = do 
  env <- ask 
  let cfg = env ^. getTMLConfig
      hbTime = fromIntegral (cfgHeartbeat cfg) * 1_000_000

  atomically $ do 
    let tvar = env ^. getTimerHBT
    timer <- readTVar tvar
    case timer of 
      Nothing -> return () 
      Just t -> renew t hbTime 




heartBeatTimeOut :: IO () 
heartBeatTimeOut = do 
  T.putStrLn "heartBeatTimeOut"


heartBeatReceiveTimeOut :: (HasEventHandler env) => env -> IO () 
heartBeatReceiveTimeOut env = do 
  sleRaiseEvent env TMLProtocolAbort
