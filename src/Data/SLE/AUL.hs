module Data.SLE.AUL
  ( HashInput(..)
  , mkHashInput
  , hashInput
  , theProtected
  )
where


import           RIO
import           Data.ASN1.Encoding
import           Data.ASN1.BinaryEncoding
import           Data.ASN1.Types
import           Crypto.Hash.SHA256
import           ByteString.StrictBuilder       ( builderBytes )

import           Data.SLE.Common
import           Data.SLE.CCSDSTime
import           Data.SLE.Config
import           Data.SLE.Bind




data HashInput = HashInput {
  _hiTime :: !CCSDSTime
  , _hiRandomNumber :: !Int32
  , _hiUserName :: !Text
  , _hiPassword :: !Text
  }


mkHashInput :: Config -> CCSDSTime -> Int32 -> HashInput
mkHashInput cfg t r = HashInput
  { _hiTime         = t
  , _hiRandomNumber = r
  , _hiUserName     = unAuthorityID (cfg ^. cfgInitiator)
  , _hiPassword     = cfg ^. cfgPassword
  }

hashInput :: HashInput -> [ASN1]
hashInput HashInput {..} =
  [ Start Sequence
  , OctetString (builderBytes (ccsdsTimeBuilder _hiTime))
  , IntVal (fromIntegral _hiRandomNumber)
  , visibleString _hiUserName
  , OctetString (encodeUtf8 _hiPassword)
  ]

instance EncodeASN1 HashInput where
  encode val = encodeASN1' DER (hashInput val)


theProtected :: HashInput -> ByteString
theProtected = hash . encode

