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
  return ()




sendPDU :: TBQueue SLEInput -> IO ()
sendPDU queue = do
  let
    bind = SleBindInvocation
      { _sleBindCredentials     = Nothing
      , _sleBindInitiatorID     = AuthorityIdentifier "Test"
      , _sleBindResponderPortID = PortID "TestPort"
      , _sleBindServiceType     = RtnAllFrames
      , _sleVersionNumber       = VersionNumber 1
      , _sleServiceInstanceID   = ServiceInstanceIdentifier
        [ServiceInstanceAttribute { _siAttrID = RAF, _siAttrValue = "onlc1" }]
      }
    pdu = SlePduBind bind

  bytes <- encodePDUwoCreds pdu

  let tmlMsg = TMLMessage tmlHdr bytes
      tmlHdr = TMLHeader TMLSlePdu 0

  atomically $ writeTBQueue queue (SLEMsg tmlMsg)
  return ()
