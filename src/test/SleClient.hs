{-# LANGUAGE
  OverloadedStrings
#-}
module Main where


import qualified Data.Text.IO                  as T
import           RIO
import qualified RIO.Text                      as T

import           SLE.Data.Bind
import           SLE.Data.Common
import           SLE.Data.CommonConfig
import           SLE.Data.RAFOps
import           SLE.Data.ServiceInstanceID
import           SLE.Data.TMLConfig
import           SLE.Data.UserConfig
import           SLE.Protocol.User
import           SLE.Protocol.UserApi



sleConfig :: UserConfig
sleConfig =
    defaultUserConfig & cfgCommon . cfgLocal .~ AuthorityIdentifier "SLETT"


main :: IO ()
main = do
    let addr = ConnectAddr { host = "localhost", port = 5100 }

        handler msg = T.putStrLn $ "HANDLER: " <> T.pack (show msg)
        cfg = sleConfig

    withSleHandle (TMRAF (RAFIdx 0)) 100 $ \hdl -> do
        void $ concurrently (startClient RtnAllFrames addr handler hdl)
                            (sleProcedure cfg hdl)


sleProcedure :: UserConfig -> SleHandle -> IO ()
sleProcedure cfg hdl = do
    sendBind cfg hdl
    sendStart cfg hdl
    threadDelay 1000000

    sendSchedule cfg hdl 1    

    threadDelay 200000000
    T.putStrLn "Terminating..."
    sendStop cfg hdl 2 
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


sendStop :: UserConfig -> SleHandle -> Word16 -> IO ()
sendStop cfg hdl invokeID = do
    stopRAF (cfg ^. cfgCommon) hdl Nothing invokeID

sendUnbind :: UserConfig -> SleHandle -> IO ()
sendUnbind cfg hdl = do
    unbind (cfg ^. cfgCommon) hdl Nothing UnbindEnd

sendScheduleImmediately :: UserConfig -> SleHandle -> Word16 -> IO() 
sendScheduleImmediately cfg hdl invokeID = do 
    scheduleReport (cfg ^. cfgCommon) hdl Nothing invokeID ReportImmediately

sendSchedule :: UserConfig -> SleHandle -> Word16 -> IO() 
sendSchedule cfg hdl invokeID = do 
    scheduleReport (cfg ^. cfgCommon) hdl Nothing invokeID (ReportPeriodically 10)

sendScheduleStop :: UserConfig -> SleHandle -> Word16 -> IO() 
sendScheduleStop cfg hdl invokeID = do 
    scheduleReport (cfg ^. cfgCommon) hdl Nothing invokeID ReportStop
