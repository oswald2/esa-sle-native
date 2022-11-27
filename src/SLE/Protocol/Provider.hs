module SLE.Protocol.Provider
    ( startServer
    ) where

import           RIO

import           SLE.Data.ProviderConfig

import           SLE.State.Events
import           SLE.State.ProviderState

import           SLE.Protocol.RAF


startServer :: ProviderConfig -> SleEventHandler -> IO ()
startServer cfg eventHandler = do
    defLogOptions <- logOptionsHandle stdout True
    let logOptions = setLogMinLevel LevelDebug defLogOptions
    withLogFunc logOptions $ \logFunc -> do
        state <- initialState cfg logFunc eventHandler

        runRIO state $ do
            logDebug "Starting listening on SLE..."
            runRAFs

