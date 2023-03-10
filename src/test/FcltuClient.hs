{-# LANGUAGE
  OverloadedStrings
  , NumericUnderscores
#-}
module Main where


import qualified Data.Text.IO                  as T
import           RIO
import qualified RIO.ByteString                as B
import qualified RIO.Text                      as T

import           SLE.Data.Bind
import           SLE.Data.Common
import           SLE.Data.CommonConfig
import           SLE.Data.FCLTUOps
import           SLE.Data.HexBytes
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
        .~ [Peer (AuthorityIdentifier "PARAGONTT") (bsToHex "PASSWD")]
        &  cfgCommon
        .  cfgVersion
        .~ SLE5


main :: IO ()
main = do
    let addr = ConnectAddr { host = "localhost", port = 5009 }

        handler msg = T.putStrLn $ "HANDLER: " <> T.pack (show msg)
        cfg = sleConfig

    T.putStrLn $ "Config:\n" <> T.pack (ppShow cfg)

    withSleHandle (TCFCLTU (FCLTUIdx 0)) 100 $ \hdl -> do
        void $ concurrently (startClient cfg FwdCltu addr handler hdl)
                            (sleProcedure cfg hdl)


sleProcedure :: UserConfig -> SleHandle -> IO ()
sleProcedure cfg hdl = do
    sendBind cfg hdl

    threadDelay 1_000_000
    sendStart cfg hdl

    threadDelay 1_000_000

    --sendData cfg hdl cltu
    -- sendScheduleImmediately cfg hdl 1 
    -- sendSchedule cfg hdl 1 

    sendGetParameter cfg hdl 1 ParAcquisitionSequenceLength
    threadDelay 1_000_000

    -- sendGetParameter cfg hdl 1 ParPlop1IdleSequenceLength
    -- threadDelay 1_000_000

    -- sendGetParameter cfg hdl 1 ParBitLockRequired
    -- threadDelay 1_000_000

    -- sendGetParameter cfg hdl 1 ParRfAvailableRequired
    -- threadDelay 1_000_000

    -- sendGetParameter cfg hdl 1 ParClcwGlobalVCID
    -- threadDelay 1_000_000

    -- sendGetParameter cfg hdl 1 ParClcwPhysicalChannel
    -- threadDelay 1_000_000

    -- sendGetParameter cfg hdl 1 ParDeliveryMode
    -- threadDelay 1_000_000

    -- sendGetParameter cfg hdl 1 ParExpectedSlduIdentification
    -- threadDelay 1_000_000

    -- sendGetParameter cfg hdl 1 ParExpectedEventInvocationIdentification
    -- threadDelay 1_000_000

    -- sendGetParameter cfg hdl 1 ParSubcarrierToBitRateRatio
    -- threadDelay 1_000_000

    -- sendGetParameter cfg hdl 1 ParMaximumSlduLength
    -- threadDelay 1_000_000

    -- sendGetParameter cfg hdl 1 ParModulationFrequency
    -- threadDelay 1_000_000

    -- sendGetParameter cfg hdl 1 ParModulationIndex
    -- threadDelay 1_000_000

    -- sendGetParameter cfg hdl 1 ParPlopInEffect
    -- threadDelay 1_000_000

    -- sendGetParameter cfg hdl 1 ParReportingCycle
    -- threadDelay 1_000_000

    -- sendGetParameter cfg hdl 1 ParReturnTimeoutPeriod
    -- threadDelay 1_000_000

    -- sendGetParameter cfg hdl 1 ParMinimumDelayTime
    -- threadDelay 1_000_000

    sendGetParameter cfg hdl 1 ParMinReportingCycle
    threadDelay 1_000_000

    -- sendGetParameter cfg hdl 1 ParNotificationMode
    -- threadDelay 1_000_000

    -- sendGetParameter cfg hdl 1 ParProtocolAbortMode
    -- threadDelay 1_000_000



    threadDelay 200_000_000

    T.putStrLn "Terminating..."
    sendStop cfg hdl
    threadDelay 1_000_000
    sendUnbind cfg hdl

  where
    cltu = B.pack
        [ 0xeb
        , 0x90
        , 0x22
        , 0x15
        , 0x04
        , 0x3b
        , 0x00
        , 0xc0
        , 0x18
        , 0xb4
        , 0x11
        , 0xc0
        , 0x00
        , 0x00
        , 0x2d
        , 0x1b
        , 0x02
        , 0x48
        , 0x0a
        , 0x0a
        , 0x00
        , 0x00
        , 0x00
        , 0x01
        , 0x00
        , 0x9c
        , 0x00
        , 0x00
        , 0x02
        , 0x00
        , 0x00
        , 0x00
        , 0x03
        , 0xb4
        , 0x00
        , 0x00
        , 0x00
        , 0x04
        , 0x00
        , 0x00
        , 0x00
        , 0x5c
        , 0x05
        , 0x00
        , 0x00
        , 0x00
        , 0x06
        , 0x00
        , 0x00
        , 0x3e
        , 0x00
        , 0x07
        , 0x00
        , 0x00
        , 0x00
        , 0x08
        , 0x00
        , 0x80
        , 0x00
        , 0x00
        , 0x09
        , 0x00
        , 0x00
        , 0x00
        , 0x0a
        , 0x9e
        , 0xd9
        , 0x62
        , 0x10
        , 0x78
        , 0xc5
        , 0xc5
        , 0x79
        , 0x92
        , 0xc5
        , 0xc5
        , 0xc5
        , 0xc5
        , 0xc5
        , 0xc5
        , 0xc5
        , 0x79
        ]

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


sendData :: UserConfig -> SleHandle -> ByteString -> IO ()
sendData cfg hdl frame = do
    sendFCLTUData (cfg ^. cfgCommon)
                  hdl
                  Nothing
                  10
                  (CltuIdentification 1)
                  Nothing
                  Nothing
                  (Duration 0)
                  ProduceNotification
                  frame

sendScheduleImmediately :: UserConfig -> SleHandle -> Word16 -> IO ()
sendScheduleImmediately cfg hdl invokeID = do
    scheduleReport (cfg ^. cfgCommon) hdl Nothing invokeID ReportImmediately

sendSchedule :: UserConfig -> SleHandle -> Word16 -> IO ()
sendSchedule cfg hdl invokeID = do
    scheduleReport (cfg ^. cfgCommon)
                   hdl
                   Nothing
                   invokeID
                   (ReportPeriodically 10)

sendScheduleStop :: UserConfig -> SleHandle -> Word16 -> IO ()
sendScheduleStop cfg hdl invokeID = do
    scheduleReport (cfg ^. cfgCommon) hdl Nothing invokeID ReportStop

sendGetParameter :: UserConfig -> SleHandle -> Word16 -> ParameterName -> IO ()
sendGetParameter cfg hdl invokeID paramName = do
    fcltuGetParameter (cfg ^. cfgCommon) hdl Nothing invokeID paramName
