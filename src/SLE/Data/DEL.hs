module SLE.Data.DEL
    ( ISP1Credentials(..)
    , isp1Credentials
    , mkCredentials
    , encodePDU
    , encodePDUwithCreds
    , encodePDUwoCreds
    , isp1CredentialsParser
    ) where


--import qualified RIO.Text                      as T
import           RIO

import           Data.Bits                      ( Bits((.&.)) )

import           SLE.Data.AUL                   ( mkHashInput
                                                , theProtected
                                                )
import           SLE.Data.CCSDSTime             ( getCurrentTime )
import           SLE.Data.Common                ( ISP1Credentials(..)
                                                , SleVersion(..)
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
    AuthNone -> encodePDUwoCreds (cfg ^. cfgVersion) pdu
    AuthAll  -> encodePDUwithCreds cfg pdu
    AuthBind -> if isBindOrReturn pdu
        then encodePDUwithCreds cfg pdu
        else encodePDUwoCreds (cfg ^. cfgVersion) pdu



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

    return (encodeSlePdu (cfg ^. cfgVersion) newPdu)


encodePDUwoCreds :: SleVersion -> SlePdu -> IO ByteString
encodePDUwoCreds version pdu = do
    return $ encodeSlePdu version pdu






