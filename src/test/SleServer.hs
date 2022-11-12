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

import           Data.SLE.Api
import           Data.SLE.Config

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
        writeConfigJSON defaultConfig "DefaultConfig.json"
        T.putStrLn "Wrote default config to file 'DefaultConfig.json'"
        exitSuccess

    cfg <- case config opts of
        Nothing -> do
            ex <- doesFileExist defaultConfigFileName
            if ex
                then do
                    T.putStrLn
                        $  "Loading default config from "
                        <> T.pack defaultConfigFileName
                        <> "..."
                    res <- loadConfigJSON defaultConfigFileName
                    case res of
                        Left err -> do
                            T.putStrLn $ "Error loading config: " <> err
                            exitFailure
                        Right c -> pure c
                else do
                    T.putStrLn "Using default config"
                    return defaultConfig
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
        port = 5008
    withSleHandle port $ \hdl -> do
        T.putStrLn "Running Server..."
        startServer cfg handler hdl
        return ()


