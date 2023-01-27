{-# LANGUAGE
  OverloadedStrings
  , NumericUnderscores
#-}
module Main where


import qualified Data.Text.IO                  as T
import           RIO
import qualified RIO.Text                      as T

import           SLE.Data.Bind
import           SLE.Data.Common
import           SLE.Data.CommonConfig
import           SLE.Data.FCLTUOps
import           SLE.Data.ServiceInstanceID
import           SLE.Data.TMLConfig
import           SLE.Data.UserConfig
import           SLE.Protocol.User
import           SLE.Protocol.UserApi

import           Text.Show.Pretty



sleConfig :: UserConfig
sleConfig =
    defaultUserConfig
        &  cfgCommon
        .  cfgLocal
        .~ AuthorityIdentifier "SLETT"
        &  cfgCommon
        .  cfgPeers
        .~ [Peer (AuthorityIdentifier "PARAGONTT") (Password T.empty)]


main :: IO ()
main = do
    let addr = ConnectAddr { host = "localhost", port = 5009 }

        handler msg = T.putStrLn $ "HANDLER: " <> T.pack (show msg)
        cfg = sleConfig

    T.putStrLn $ "Config:\n" <> T.pack (ppShow cfg)

    withSleHandle (TCFCLTU (FCLTUIdx 0)) 100 $ \hdl -> do
        void $ concurrently (startClient FwdCltu addr handler hdl)
                            (sleProcedure cfg hdl)


sleProcedure :: UserConfig -> SleHandle -> IO ()
sleProcedure cfg hdl = do
    sendBind cfg hdl
    threadDelay 1_000_000
    sendStart cfg hdl
    threadDelay 200_000_000
    T.putStrLn "Terminating..."
    sendStop cfg hdl
    threadDelay 1_000_000
    sendUnbind cfg hdl


sendBind :: UserConfig -> SleHandle -> IO ()
sendBind cfg hdl = do
    bind
        (cfg ^. cfgCommon)
        hdl
        Nothing
        FwdCltu
        (PortID "TCPORT")
        [ ServiceInstanceAttribute SAGR  "3"
        , ServiceInstanceAttribute SPACK "facility-PASS1"
        , ServiceInstanceAttribute FSLFG "1"
        , ServiceInstanceAttribute FCLTU "cltu1"
        ]


sendStart :: UserConfig -> SleHandle -> IO ()
sendStart cfg hdl = do
    startFCLTU (cfg ^. cfgCommon) hdl Nothing 1


sendStop :: UserConfig -> SleHandle -> IO ()
sendStop cfg hdl = do
    stopFCLTU (cfg ^. cfgCommon) hdl Nothing 2

sendUnbind :: UserConfig -> SleHandle -> IO ()
sendUnbind cfg hdl = do
    unbind (cfg ^. cfgCommon) hdl Nothing UnbindEnd
