module Data.SLE.Api
  ( startClient
  , startClientRIO
  , startServer
  , startServerRIO
  )
where

import           RIO
import qualified RIO.Text                      as T
import qualified Data.Text.IO                  as T
import           Data.SLE.TMLConfig
import           Data.SLE.TMLProtocol
import           Data.SLE.SLEInput
import           State.SLEEvents
import           State.AppState
import           Network.Socket                 ( PortNumber )

startClient :: ConnectAddr -> SleEventHandler -> TBQueue SLEInput -> IO ()
startClient addr eventHandler queue = do
  defLogOptions <- logOptionsHandle stdout True
  let logOptions = setLogMinLevel LevelDebug defLogOptions
  withLogFunc logOptions $ \logFunc -> do
    state <- initialState defaultConfig logFunc eventHandler queue

    runRIO state $ do
      connectSLE addr


startClientRIO :: ConnectAddr -> RIO AppState ()
startClientRIO = connectSLE



startServer :: PortNumber -> SleEventHandler -> TBQueue SLEInput -> IO ()
startServer port eventHandler queue = do
  defLogOptions <- logOptionsHandle stdout True
  let logOptions = setLogMinLevel LevelDebug defLogOptions
  withLogFunc logOptions $ \logFunc -> do
    state <- initialState defaultConfig logFunc eventHandler queue

    runRIO state $ do
      listenSLE port


startServerRIO :: PortNumber -> RIO AppState ()
startServerRIO = listenSLE
