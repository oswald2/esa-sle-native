module SLE.Protocol.Provider
    ( startServer
    , startServerRIO
    ) where

import           RIO


import           SLE.Data.Bind
import           SLE.Data.Handle
import           SLE.Data.Input
import           SLE.Data.PDU
import           SLE.Data.ProviderConfig
import           SLE.Data.ServiceInstanceID
import           SLE.Data.TMLConfig
import           SLE.Data.UserConfig

import           SLE.Protocol.SLEProtocol
import           SLE.Protocol.TMLProtocol

import           SLE.State.Events
import           SLE.State.ProviderState

import           Text.Builder



startServer :: ProviderConfig -> SleEventHandler -> SleHandle -> IO ()
startServer cfg eventHandler hdl = do
    defLogOptions <- logOptionsHandle stdout True
    let logOptions = setLogMinLevel LevelDebug defLogOptions
    withLogFunc logOptions $ \logFunc -> do
        state <- initialState cfg logFunc eventHandler hdl

        runRIO state $ do
            logDebug "Starting listening on SLE..."
            listenSLE (hdl ^. slePort)


startServerRIO :: SleHandle -> RIO ProviderState ()
startServerRIO hdl = listenSLE (hdl ^. slePort)

