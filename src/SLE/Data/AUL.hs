module SLE.Data.AUL
    ( HashInput(..)
    , HashToUse(..)
    , mkHashInput
    , hashInput
    , theProtected
    , checkCredentials
    ) where

import           RIO
import qualified RIO.Text                      as T

import           ByteString.StrictBuilder       ( builderBytes )
import           Crypto.Hash.SHA1              as SHA1
import           Crypto.Hash.SHA256            as SHA256
import           Data.ASN1.BinaryEncoding
import           Data.ASN1.Encoding
import           Data.ASN1.Types

import           SLE.Data.Bind
import           SLE.Data.CCSDSTime
import           SLE.Data.Common
import           SLE.Data.CommonConfig
import           SLE.Data.HexBytes


data HashInput = HashInput
    { _hiTime         :: !CCSDSTime
    , _hiRandomNumber :: !Int32
    , _hiUserName     :: !Text
    , _hiPassword     :: !Password
    }
    deriving Show


mkHashInput :: CommonConfig -> CCSDSTime -> Int32 -> HashInput
mkHashInput cfg t r = HashInput { _hiTime = t
                                , _hiRandomNumber = r
                                , _hiUserName = unAuthorityID (cfg ^. cfgLocal)
                                , _hiPassword = Password (cfg ^. cfgPassword)
                                }

hashInput :: HashInput -> [ASN1]
hashInput HashInput {..} =
    [ Start Sequence
    , OctetString (builderBytes (ccsdsTimeBuilder _hiTime))
    , IntVal (fromIntegral _hiRandomNumber)
    , visibleString _hiUserName
    , OctetString (hexToBS (passWordToHex _hiPassword))
    , End Sequence
    ]

instance EncodeASN1 HashInput where
    encode val = encodeASN1' DER (hashInput val)


theProtected :: HashToUse -> HashInput -> ByteString
theProtected SHA1   = SHA1.hash . encode
theProtected SHA256 = SHA256.hash . encode


checkCredentials :: ISP1Credentials -> AuthorityIdentifier -> Password -> Bool
checkCredentials incoming userName pass =
    let hi = HashInput { _hiTime         = _isp1Time incoming
                       , _hiRandomNumber = _isp1RandomNumber incoming
                       , _hiUserName     = unAuthorityID userName
                       , _hiPassword     = pass
                       }
        isp1Prot = _isp1TheProtected incoming
        prot     = if hlength isp1Prot == 20
            then theProtected SHA1 hi
            else theProtected SHA256 hi
    in  trace
                (  "HashInput: "
                <> T.pack (show hi)
                <> "\nTime: "
                <> textDisplay (_isp1Time incoming)
                <> "\nThe Protected: "
                <> T.pack (show (bsToHex prot))
                <> " protected from ISP1: "
                <> T.pack (show isp1Prot)
                )
                hexToBS
                isp1Prot
            == prot

