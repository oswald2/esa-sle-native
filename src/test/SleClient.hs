{-# LANGUAGE
  OverloadedStrings
#-}
module Main where


import qualified Data.Text.IO                  as T
import           RIO
import qualified RIO.Text                      as T

import           SLE.Data.Bind
import           SLE.Data.CommonConfig
import           SLE.Data.RAFOps
import           SLE.Data.ServiceInstanceID
import           SLE.Data.TMLConfig
import           SLE.Data.UserConfig
import           SLE.Protocol.User
import           SLE.Protocol.UserApi



sleConfig :: UserConfig
sleConfig =
    defaultUserConfig & cfgCommon . cfgLocal .~ AuthorityIdentifier "EGSCC"


main :: IO ()
main = do
    let addr = ConnectAddr { host = "localhost", port = 5100 }

        handler msg = T.putStrLn $ "HANDLER: " <> T.pack (show msg)
        cfg = sleConfig

    withSleHandle (port addr) 100 $ \hdl -> do
        void $ concurrently (startClient addr handler hdl)
                            (sleProcedure cfg hdl)


sleProcedure :: UserConfig -> SleHandle -> IO ()
sleProcedure cfg hdl = do
    sendBind cfg hdl
    sendStart cfg hdl
    threadDelay 200000000
    T.putStrLn "Terminating..."
    sendStop cfg hdl
    sendUnbind cfg hdl


sendBind :: UserConfig -> SleHandle -> IO ()
sendBind cfg hdl = do
    bind
        (cfg ^. cfgCommon)
        hdl
        Nothing
        RtnAllFrames
        (PortID "TMPORT")
        [ ServiceInstanceAttribute SAGR  "3"
        , ServiceInstanceAttribute SPACK "facility-PASS1"
        , ServiceInstanceAttribute RSLFG "1"
        , ServiceInstanceAttribute RAF   "onlc1"
        ]


sendStart :: UserConfig -> SleHandle -> IO ()
sendStart cfg hdl = do
    startRAF (cfg ^. cfgCommon) hdl Nothing 1 Nothing Nothing AllFrames


sendStop :: UserConfig -> SleHandle -> IO ()
sendStop cfg hdl = do
    stopRAF (cfg ^. cfgCommon) hdl Nothing 2

sendUnbind :: UserConfig -> SleHandle -> IO ()
sendUnbind cfg hdl = do
    unbind (cfg ^. cfgCommon) hdl Nothing UnbindEnd
