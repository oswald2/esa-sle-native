module SLE.Protocol.Provider
    ( startServer
    ) where

import           RIO

import qualified Data.Text.IO                  as T
import           SLE.Data.ProviderConfig

import           SLE.State.Events
import           SLE.State.ProviderState

import           SLE.Protocol.RAF



perfFunc :: Word64 -> IO ()
perfFunc len = T.putStrLn $ "Sent " <> fromString (show len) <> " bytes"

startServer
    :: ProviderConfig -> SleEventHandler -> ConfigFromApp -> RIO ProviderState () -> IO ()
startServer cfg eventHandler appCfg action = do
    defLogOptions <- logOptionsHandle stdout True
    let logOptions = setLogMinLevel LevelDebug defLogOptions
    withLogFunc logOptions $ \logFunc -> do
        state <- initialState cfg logFunc eventHandler appCfg 

        runRIO state $ do
            logDebug "Starting listening on SLE..."
            concurrently_ (runRAFs perfFunc) action


