{-# LANGUAGE
  OverloadedStrings
#-}
module Main where


import           RIO
import qualified RIO.Text                      as T
import qualified Data.Text.IO                  as T

import           Data.SLE.TMLConfig
import           Data.SLE.Api
import           Data.SLE.RAF
import           Data.SLE.DEL
import           Data.SLE.SLEInput
import           Data.SLE.ServiceInstanceID
import           Data.SLE.TMLMessage
import           Data.SLE.SlePdu


main :: IO ()
main = do
  let addr = ConnectAddr { host = "localhost", port = 55529 }

      handler msg = T.putStrLn (T.pack (show msg))

  queue <- newTBQueueIO 5000

  void $ concurrently (startClient addr handler queue) (sendPDU queue)




sendPDU :: TBQueue SLEInput -> IO ()
sendPDU queue = do
  let
    bind = SleBindInvocation
      { _sleBindCredentials     = Nothing
      , _sleBindInitiatorID     = AuthorityIdentifier "SLE_USER"
      , _sleBindResponderPortID = PortID "55529"
      , _sleBindServiceType     = RtnAllFrames
      , _sleVersionNumber       = VersionNumber 2
      , _sleServiceInstanceID   = ServiceInstanceIdentifier
                                    [ ServiceInstanceAttribute SAGR "1"
                                    , ServiceInstanceAttribute SPACK
                                                               "VST-PASS0001"
                                    , ServiceInstanceAttribute RSLFG "1"
                                    , ServiceInstanceAttribute RAF   "onlt1"
                                    ]
      }
    pdu = SlePduBind bind

  bytes <- encodePDUwoCreds pdu

  let tmlMsg = TMLMessage tmlHdr bytes
      tmlHdr = TMLHeader TMLSlePdu 0

  atomically $ writeTBQueue queue (SLEMsg tmlMsg)
  return ()
