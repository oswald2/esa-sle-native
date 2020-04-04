module Data.SLE.Api
  ( SleHandle
  , newSleHandle
  , startClient
  , startClientRIO
  , startServer
  , startServerRIO
  )
where

import           RIO
--import qualified RIO.Text                      as T
--import qualified Data.Text.IO                  as T
import           Data.SLE.TMLConfig
import           Data.SLE.TMLProtocol
--import           Data.SLE.SLEInput
import           Data.SLE.Bind
import           Data.SLE.Config
import           Data.SLE.Handle

import           State.SLEEvents
import           State.AppState

import           Network.Socket                 ( PortNumber )




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



startServer :: PortNumber -> SleEventHandler -> SleHandle -> IO ()
startServer port eventHandler hdl = do
  defLogOptions <- logOptionsHandle stdout True
  let logOptions = setLogMinLevel LevelDebug defLogOptions
  withLogFunc logOptions $ \logFunc -> do
    state <- initialState Data.SLE.Config.defaultConfig
                          logFunc
                          eventHandler
                          hdl

    runRIO state $ do
      listenSLE port


startServerRIO :: PortNumber -> RIO AppState ()
startServerRIO = listenSLE



bind :: (Monad m) => Config -> SleHandle -> [SleAttributes] -> m ()
bind cfg hdl attrs = undefined

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



