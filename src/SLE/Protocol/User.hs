module SLE.Protocol.User
    ( startClient
    , startClientRIO
    ) where

import           RIO

import           SLE.Data.Handle
import           SLE.Data.TMLConfig
import           SLE.Data.UserConfig

import           SLE.Protocol.SLEProtocol

import           SLE.State.Events
import           SLE.State.UserState


startClient :: ConnectAddr -> SleEventHandler -> SleHandle -> IO ()
startClient addr eventHandler hdl = do
    defLogOptions <- logOptionsHandle stdout True
    let logOptions = setLogMinLevel LevelDebug defLogOptions
    withLogFunc logOptions $ \logFunc -> do
        state <- initialState defaultUserConfig logFunc eventHandler hdl

        runRIO state $ do
            connectSLE addr


startClientRIO :: ConnectAddr -> RIO UserState ()
startClientRIO = connectSLE

