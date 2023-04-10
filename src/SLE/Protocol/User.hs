module SLE.Protocol.User
    ( startClient
    ) where

import           RIO

import qualified Data.Text.IO                  as T

import           SLE.Data.Bind
import           SLE.Data.Handle
import           SLE.Data.TMLConfig
import           SLE.Data.UserConfig

import           SLE.Protocol.SLEProtocol

import           SLE.State.Events
import           SLE.State.UserState

perfFunc :: Word64 -> IO ()
perfFunc len = T.putStrLn $ "Sent " <> fromString (show len) <> " bytes"

startClient
    :: UserConfig 
    -> ApplicationIdentifier
    -> ConnectAddr
    -> SleEventHandler
    -> SleHandle
    -> IO ()
startClient cfg appID addr eventHandler hdl = do
    defLogOptions <- logOptionsHandle stdout True
    let logOptions = setLogMinLevel LevelDebug defLogOptions
    withLogFunc logOptions $ \logFunc -> do
        state <- initialState cfg logFunc eventHandler hdl

        runRIO state $ do
            connectSLE appID hdl addr perfFunc


