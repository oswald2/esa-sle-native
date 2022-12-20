{-# LANGUAGE
  OverloadedStrings
#-}
module Main where


import qualified Data.Text.IO                  as T
import           RIO
import qualified RIO.Text                      as T

import           SLE.Data.Api
import           SLE.Data.Bind
import           SLE.Data.ServiceInstanceID
import           SLE.Data.TMLConfig
import           SLE.Data.UserConfig

import           SLE.Protocol.User


main :: IO ()
main = do
    let addr = ConnectAddr { host = "localhost", port = 5008 }

        handler msg = T.putStrLn $ "HANDLER: " <> T.pack (show msg)
        cfg = defaultUserConfig

    withSleHandle (port addr) $ \hdl -> do
        void $ concurrently (startClient addr handler hdl)
                            (sleProcedure cfg hdl)


sleProcedure :: UserConfig -> SleHandle -> IO ()
sleProcedure cfg hdl = do
    sendBind cfg hdl
    threadDelay 2000000
    sendUnbind cfg hdl


sendBind :: UserConfig -> SleHandle -> IO ()
sendBind cfg hdl = do
    bind
        (cfg ^. cfgCommon)
        hdl
        RtnAllFrames
        (PortID "TMPORT")
        [ ServiceInstanceAttribute SAGR  "3"
        , ServiceInstanceAttribute SPACK "facility-PASS1"
        , ServiceInstanceAttribute RSLFG "1"
        , ServiceInstanceAttribute RAF   "onlc1"
        ]

sendUnbind :: UserConfig -> SleHandle -> IO ()
sendUnbind cfg hdl = do
    unbind (cfg ^. cfgCommon) hdl UnbindEnd
