module Data.SLE.Api
  ( SleHandle
  , newSleHandle
  , startClient
  , startClientRIO
  , startServer
  , startServerRIO
  , bind
  )
where

import           RIO
--import qualified RIO.Text                      as T
--import qualified Data.Text.IO                  as T
import           Data.SLE.TMLConfig
import           Data.SLE.TMLProtocol
import           Data.SLE.Input
import           Data.SLE.Bind
import           Data.SLE.Config
import           Data.SLE.Handle
import           Data.SLE.ServiceInstanceID
import           Data.SLE.PDU

import           State.Events
import           State.AppState

import           Network.Socket                 ( PortNumber )
import           Text.Builder



startClient :: ConnectAddr -> SleEventHandler -> SleHandle -> IO ()
startClient addr eventHandler hdl = do
  defLogOptions <- logOptionsHandle stdout True
  let logOptions = setLogMinLevel LevelDebug defLogOptions
  withLogFunc logOptions $ \logFunc -> do
    state <- initialState Data.SLE.Config.defaultConfig logFunc eventHandler hdl

    runRIO state $ do
      connectSLE addr


startClientRIO :: ConnectAddr -> RIO AppState ()
startClientRIO = connectSLE



startServer :: SleEventHandler -> SleHandle -> IO ()
startServer eventHandler hdl = do
  defLogOptions <- logOptionsHandle stdout True
  let logOptions = setLogMinLevel LevelDebug defLogOptions
  withLogFunc logOptions $ \logFunc -> do
    state <- initialState Data.SLE.Config.defaultConfig logFunc eventHandler hdl

    runRIO state $ do
      listenSLE (hdl ^. slePort)


startServerRIO :: SleHandle -> RIO AppState ()
startServerRIO hdl = listenSLE (hdl ^. slePort)



bind
  :: (MonadIO m)
  => Config
  -> SleHandle
  -> ApplicationIdentifier
  -> [ServiceInstanceAttribute]
  -> m ()
bind cfg hdl appID attrs = do
  -- create an SLE Bind Invocation
  let bnd = mkSleBindInvocation (cfg ^. cfgInitiator)
              (PortID (run (decimal (hdl ^. slePort))))
              appID
              (VersionNumber 2)
              (ServiceInstanceIdentifier attrs)
  -- send it to the lower layers
  writeSLEInput hdl (SLEPdu (SlePduBind bnd))



unbind :: (Monad m) => SleHandle -> m ()
unbind = undefined


sendFrame :: (Monad m) => SleHandle -> ByteString -> m ()
sendFrame = undefined

sendOCF :: (Monad m) => SleHandle -> Word32 -> m ()
sendOCF = undefined


receiveFrame :: (Monad m) => SleHandle -> m ByteString
receiveFrame = undefined

receiveOCF :: (Monad m) => SleHandle -> m Word32
receiveOCF = undefined



