module SLE.Protocol.Provider
    ( startServer
    ) where

import           RIO

import           SLE.Data.Handle
import           SLE.Data.ProviderConfig

import           SLE.Protocol.SLEProtocol

import           SLE.State.Events
import           SLE.State.ProviderState



startServer :: ProviderConfig -> SleEventHandler -> SleHandle -> IO ()
startServer cfg eventHandler hdl = do
    defLogOptions <- logOptionsHandle stdout True
    let logOptions = setLogMinLevel LevelDebug defLogOptions
    withLogFunc logOptions $ \logFunc -> do
        state <- initialState cfg logFunc eventHandler hdl

        runRIO state $ do
            logDebug "Starting listening on SLE..."



