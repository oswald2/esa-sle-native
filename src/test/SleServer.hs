{-# LANGUAGE
  OverloadedStrings
  , NoImplicitPrelude
  , StandaloneDeriving
  , DataKinds
  , TypeOperators
  , OverloadedLabels
  , FlexibleInstances
  , DeriveGeneric
#-}
module Main where


import qualified Data.Text.IO                  as T
import           RIO
import qualified RIO.Text                      as T

import           SLE.Data.ProviderConfig
import           SLE.Protocol.RAF
import           SLE.State.Events
import           SLE.State.ProviderState

import           System.Directory               ( doesFileExist )

import           Options.Generic

import           Text.Show.Pretty


data Options w = Options
    { version     :: w ::: Bool <?> "Print version information"
    , config      :: w ::: Maybe String <?> "Specify a config file"
    , writeconfig :: w ::: Bool <?> "Write the default config to a file"
    }
    deriving Generic

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Unwrapped)


main :: IO ()
main = do
    opts <- unwrapRecord "SleServer"

    when (writeconfig opts) $ do
        writeConfigJSON defaultProviderConfig defaultProviderConfigFileName
        T.putStrLn
            $  "Wrote default config to file '"
            <> T.pack defaultProviderConfigFileName
            <> "'"
        exitSuccess

    cfg <- case config opts of
        Nothing -> do
            ex <- doesFileExist defaultProviderConfigFileName
            if ex
                then do
                    T.putStrLn
                        $  "Loading default config from "
                        <> T.pack defaultProviderConfigFileName
                        <> "..."
                    res <- loadConfigJSON defaultProviderConfigFileName
                    case res of
                        Left err -> do
                            T.putStrLn $ "Error loading config: " <> err
                            exitFailure
                        Right c -> pure c
                else do
                    T.putStrLn "Using default config"
                    return defaultProviderConfig
        Just path -> do
            T.putStrLn $ "Loading configuration from file " <> T.pack path
            res <- loadConfigJSON path
            case res of
                Left err -> do
                    T.putStrLn $ "Error loading config: " <> err
                    exitFailure
                Right c -> pure c

    let handler msg = T.putStrLn $ "SLE HANDLER: " <> T.pack (ppShow msg)
        -- port = 55529
    T.putStrLn $ "Running Server using config:\n" <> T.pack (ppShow cfg)
    startServer cfg handler
    return ()

perfFunc :: Word64 -> IO ()
perfFunc len = T.putStrLn $ "Sent " <> fromString (show len) <> " bytes"


startServer :: ProviderConfig -> SleEventHandler -> IO ()
startServer cfg eventHandler = do
    defLogOptions <- logOptionsHandle stdout True
    let logOptions = setLogMinLevel LevelDebug defLogOptions
    withLogFunc logOptions $ \logFunc -> do
        state <- initialState cfg logFunc eventHandler

        runRIO state $ do
            logDebug "Starting listening on SLE..."
            concurrently_ (runRAFs perfFunc) action

action :: RIO ProviderState ()
action = do
    -- perform transfer data test


    return ()
