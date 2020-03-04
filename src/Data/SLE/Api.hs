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
import           State.SLEEvents
import           State.AppState


startClient :: ConnectAddr -> SleEventHandler -> IO ()
startClient addr eventHandler = do
  defLogOptions <- logOptionsHandle stdout True
  let logOptions = setLogMinLevel LevelDebug defLogOptions
  withLogFunc logOptions $ \logFunc -> do
    state <- initialState defaultConfig logFunc eventHandler

    runRIO state $ do
      connectSLE addr


startClientRIO :: ConnectAddr -> RIO AppState ()
startClientRIO = connectSLE

