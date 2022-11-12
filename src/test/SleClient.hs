{-# LANGUAGE
  OverloadedStrings
#-}
module Main where


import qualified Data.Text.IO                  as T
import           RIO
import qualified RIO.Text                      as T

import           Data.SLE.Api
import           Data.SLE.Bind
import           Data.SLE.Config
import           Data.SLE.ServiceInstanceID
import           Data.SLE.TMLConfig


main :: IO ()
main = do
    let addr = ConnectAddr { host = "localhost", port = 55529 }

        handler msg = T.putStrLn (T.pack (show msg))
        cfg' = Data.SLE.Config.defaultConfig

        cfg  = cfg' { _cfgInitiator = AuthorityIdentifier "SLE_USER" }

    withSleHandle (port addr) $ \hdl -> do
        void $ concurrently (startClient addr handler hdl) (sendPDU cfg hdl)




sendPDU :: Config -> SleHandle -> IO ()
sendPDU cfg hdl = do
    bind
        cfg
        hdl
        RtnAllFrames
        [ ServiceInstanceAttribute SAGR  "1"
        , ServiceInstanceAttribute SPACK "VST-PASS0001"
        , ServiceInstanceAttribute RSLFG "1"
        , ServiceInstanceAttribute RAF   "onlt1"
        ]
