module Data.SLE.Api
  ( startClient
  , startClientRIO
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

