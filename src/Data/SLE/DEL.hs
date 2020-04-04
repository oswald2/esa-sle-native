module Data.SLE.DEL
  ( ISP1Credentials(..)
  , isp1Credentials
  , newCredentials
  , mkCredentials
  , encodePDU
  , encodePDUwithCreds
  , encodePDUwoCreds
  , isp1CredentialsParser
  )
where


import           RIO
import           Control.Monad.Except

import           Data.ASN1.Types
import           Data.ASN1.Encoding
import           Data.ASN1.BinaryEncoding
import           Data.Bits

import           Data.SLE.Common
import           Data.SLE.CCSDSTime
import           Data.SLE.PDU
import           Data.SLE.AUL
import           Data.SLE.Config

import           System.Random.SplitMix




encodePDU :: Config -> SlePdu -> IO ByteString 
encodePDU cfg pdu = 
  case cfg ^. cfgAuthorize of 
    AuthNone -> encodePDUwoCreds pdu 
    AuthAll -> encodePDUwithCreds cfg pdu 
    AuthBind -> if isBind pdu then encodePDUwithCreds cfg pdu else encodePDUwoCreds pdu



encodePDUwithCreds :: Config -> SlePdu -> IO ByteString
encodePDUwithCreds cfg pdu = do
  t <- getCurrentTime 
  gen <- initSMGen 
  let (x, _) = nextWord32 gen 
      !r = fromIntegral $ 0xef_ff_ff_ff .&. x 

      hi = mkHashInput cfg t r
      prot = theProtected hi 
      isp1 = mkCredentials t r prot 
      
      newPdu = setCredentials pdu (encode isp1)
  
  return (encode newPdu)


encodePDUwoCreds :: SlePdu -> IO ByteString
encodePDUwoCreds pdu = do
  return $ encode pdu


data ISP1Credentials = ISP1Credentials {
  _isp1Time :: CCSDSTime
  , _isp1RandomNumber :: Int32
  , _isp1TheProtected :: ByteString
  }

newCredentials :: ByteString -> IO ISP1Credentials
newCredentials theProt = do
  t   <- getCurrentTime
  gen <- initSMGen
  let (x, _) = nextInt gen
  return $ ISP1Credentials t (fromIntegral x) theProt

mkCredentials :: CCSDSTime -> Int32 -> ByteString -> ISP1Credentials
mkCredentials = ISP1Credentials

isp1Credentials :: ISP1Credentials -> [ASN1]
isp1Credentials ISP1Credentials {..} =
  [ Start Sequence
  , time (Time _isp1Time)
  , IntVal (fromIntegral _isp1RandomNumber)
  , OctetString _isp1TheProtected
  , End Sequence
  ]

instance EncodeASN1 ISP1Credentials where
  encode val = encodeASN1' DER (isp1Credentials val)



isp1CredentialsParser :: Parser ISP1Credentials 
isp1CredentialsParser = do 
  parseSequence isp1Parser
  where 
    isp1Parser = do 
      t <- parseTime
      r <- parseIntVal 
      prot <- parseOctetString
      case t of 
        Time cds -> 
          return ISP1Credentials {
              _isp1Time = cds
              , _isp1RandomNumber = (fromIntegral r)
              , _isp1TheProtected = prot
            }
        TimePico _ -> throwError "isp1CredentialsParser: expected CCSDS Time, got CCSDS Pico Time"





