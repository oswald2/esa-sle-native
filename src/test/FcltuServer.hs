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


import           RIO
import qualified RIO.Text                      as T

import qualified Data.Text.IO                  as T

import           SLE.Data.CCSDSTime
import           SLE.Data.Common
import           SLE.Data.FCLTUOps
import           SLE.Data.ProviderConfig
import           SLE.Protocol.FCLTU
import           SLE.Protocol.ProviderApi
import           SLE.State.Events
import           SLE.State.FCLTUClasses
import           SLE.State.FCLTUState
import           SLE.State.ProviderState

import           System.Directory               ( doesFileExist )

import           Options.Generic

-- import           SLE.State.FCLTUClasses
import           Text.Show.Pretty        hiding ( Time )


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
    opts <- unwrapRecord "FcltuServer"

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

        -- port = 55529
    T.putStrLn $ "Running Server using config:\n" <> T.pack (ppShow cfg)

    let appCfg = ConfigFromApp { appSCID = 1 }

    startServer cfg appCfg
    return ()

perfFunc :: Word64 -> IO ()
perfFunc len = T.putStrLn $ "Sent " <> fromString (show len) <> " bytes"


handler :: TVar (Maybe FCLTUVar) -> SleEvent -> IO ()
handler var (SLEFcltuTransferData _sii _fcltuIdx _tmIdx inv) = do
    now   <- getCurrentTime
    fcltu <- readTVarIO var
    forM_ fcltu $ \v -> do
        fcltuStartRadiation v (inv ^. fcltuDataIdent) (Time now)

        stop <- getCurrentTime
        fcltuRadiationSuccess v
                              (inv ^. fcltuDataIdent)
                              (Time now)
                              (Time stop)
                              UplinkNominal
    return ()
handler _ event = do
    T.putStrLn $ "SLE HANDLER: " <> T.pack (ppShow event)



startServer :: ProviderConfig -> ConfigFromApp -> IO ()
startServer cfg appCfg = do
    defLogOptions <- logOptionsHandle stdout True
    let logOptions = setLogMinLevel LevelDebug defLogOptions

    var <- newTVarIO Nothing

    withLogFunc logOptions $ \logFunc -> do
        state <- initialState cfg logFunc (handler var) appCfg

        runRIO state $ do
            logDebug "Starting listening on SLE..."

            v <- getFCLTUVar (FCLTUIdx 0)
            atomically $ writeTVar var v

            race_ (runFCLTUs perfFunc appCfg) action

action :: RIO ProviderState ()
action = do
    -- perform transfer data test
    liftIO $ T.putStr " > "
    line <- liftIO T.getLine

    case T.toUpper line of
        -- "SEND" -> do
        --     var' <- getRAFVar (RAFIdx 0)
        --     forM_ var' $ \var -> do
        --         -- t <- liftIO $ getCurrentTime
        --         let
        --             frame = TransFrame RafTransferDataInvocation
        --                 { _rafTransCredentials       = Nothing
        --                 , _rafTransERT               = SLE.Data.Common.Time
        --                                                    (CCSDSTime 23749 59327900 352)
        --                 , _rafTransAntennaID         = LocalForm ""
        --                 , _rafTransDataContinuity    = -1
        --                 , _rafTransFrameQuality      = FrameGood
        --                 , _rafTransPrivateAnnotation = Nothing
        --                 , _rafTransData              = frameData
        --                 }
        --         logDebug $ "Sending Transfer Data: " <> fromString
        --             (ppShow frame)
        --         sendFrameOrNotification var frame
        --     action
        "QUIT" -> return ()
        "EXIT" -> return ()
        x      -> do
            liftIO $ T.putStrLn $ "Command " <> x <> " not known, ignoring"
            action


