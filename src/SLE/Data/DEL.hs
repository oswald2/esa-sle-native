module SLE.Data.DEL
    ( ISP1Credentials(..)
    , isp1Credentials
    , mkCredentials
    , encodePDU
    , encodePDUwithCreds
    , encodePDUwoCreds
    , isp1CredentialsParser
    ) where


import           RIO
import qualified RIO.Text                      as T

import           Data.Bits                      ( Bits((.&.)) )

import           SLE.Data.AUL                   ( mkHashInput
                                                , theProtected
                                                )
import           SLE.Data.CCSDSTime             ( getCurrentTime )
import           SLE.Data.Common                ( EncodeASN1(encode)
                                                , ISP1Credentials(..)
                                                , isp1Credentials
                                                , isp1CredentialsParser
                                                , mkCredentials
                                                )
import           SLE.Data.CommonConfig
import           SLE.Data.PDU

import           System.Random.SplitMix         ( initSMGen
                                                , nextWord32
                                                )


encodePDU :: CommonConfig -> SlePdu -> IO ByteString
encodePDU cfg pdu = case cfg ^. cfgAuthorize of
    AuthNone -> encodePDUwoCreds pdu
    AuthAll  -> encodePDUwithCreds cfg pdu
    AuthBind -> if isBindOrReturn pdu
        then encodePDUwithCreds cfg pdu
        else encodePDUwoCreds pdu



encodePDUwithCreds :: CommonConfig -> SlePdu -> IO ByteString
encodePDUwithCreds cfg pdu = do
    t   <- getCurrentTime
    gen <- initSMGen
    let (x, _) = nextWord32 gen
        !r     = fromIntegral $ 0xef_ff_ff_ff .&. x

        hi     = mkHashInput cfg t r
        prot   = theProtected (cfg ^. cfgSHAType) hi
        isp1   = mkCredentials t r prot

        newPdu = setCredentials pdu isp1

    traceM
        $  "encodePDU: new Credentials: "
        <> T.pack (show isp1)
        <> "\nTime: "
        <> textDisplay t
        <> "ASN1: "
        <> T.pack (show (isp1Credentials isp1))
    return (encode newPdu)


encodePDUwoCreds :: SlePdu -> IO ByteString
encodePDUwoCreds pdu = do
    return $ encode pdu






