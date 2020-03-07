{-# LANGUAGE 
  TemplateHaskell
#-}
module Data.SLE.RAF
  ( Credentials
  , SleBindInvocation(..)
  , Time(..)
  , IntPosShort(..)
  , AuthorityIdentifier(..)
  , PortID(..)
  , VersionNumber(..)
  , time
  , sleBindInvocation
  )
where

import           RIO
import qualified RIO.ByteString                as B
--import qualified RIO.ByteString.Lazy as BL
import           Control.Lens 
import           ByteString.StrictBuilder 
import           Language.Asn.Encoding as ASN1
import           Language.Asn.Types
import           Language.Asn.Types.Internal

import           Data.SLE.CCSDSTime


-- newtype IntPosLong = IntPosLong Word32

-- intPosLong :: AsnEncoding Integer 
-- intPosLong = integerRanged 0 4294967295

newtype IntPosShort = IntPosShort { getIntPosShort :: Word16 }


intPosShort :: AsnEncoding Integer 
intPosShort = integerRanged 1 65535


type Credentials = Maybe ByteString

credentials :: AsnEncoding Credentials
credentials = choice [Nothing, Just B.empty] $ 
  \case 
    Nothing -> option 0 "unused" Nothing ASN1.null'  
    Just bs -> option 1 "used" bs octetString


data Time = 
  Time CCSDSTime 
  | TimePico CCSDSTimePico

time :: AsnEncoding Time 
time = choice [Time ccsdsNullTime, TimePico ccsdsPicoNullTime] $
  \case 
    Time t -> option 0 "ccsdsFormat" (builderBytes (ccsdsTimeBuilder t)) octetString 
    TimePico t -> option 1 "ccsdsPicoFormat" (builderBytes (ccsdsTimePicoBuilder t)) octetString

--type ConditionalTime = Maybe 

--conditionalTime :: AsnEndoding ConditionalTime 

visibleString :: AsnEncoding Text 
visibleString = EncUniversalValue (UniversalValueTextualString VisibleString id mempty mempty)


newtype AuthorityIdentifier = AuthorityIdentifier { getAuthorityID :: Text }

newtype PortID = PortID { getPortID :: Text }


data ApplicationIdentifier = 
  RtnAllFrames
  | RtnInsert
  | RtnChFrames
  | RtnChFsh
  | RtnChOcf
  | RtnBitstr
  | RtnSpacePkt
  | FwdAosSpacePkt
  | FwdAosVca
  | FwdBitstr
  | FwdProtoVcdu
  | FwdInsert 
  | FwdCVcdu
  | FwdTcSpacePkt 
  | FwdTcVca 
  | FwdTcFrame 
  | FwdCltu
  deriving (Eq, Ord, Enum, Show, Generic)

class ToASNInteger a where 
  toASNInteger :: a -> Integer 

instance ToASNInteger ApplicationIdentifier where 
  toASNInteger RtnAllFrames = 0
  toASNInteger RtnInsert = 1
  toASNInteger RtnChFrames = 2
  toASNInteger RtnChFsh = 3
  toASNInteger RtnChOcf = 4
  toASNInteger RtnBitstr = 5
  toASNInteger RtnSpacePkt = 6 
  toASNInteger FwdAosSpacePkt = 7
  toASNInteger FwdAosVca = 8
  toASNInteger FwdBitstr = 9
  toASNInteger FwdProtoVcdu = 10
  toASNInteger FwdInsert  = 11
  toASNInteger FwdCVcdu = 12
  toASNInteger FwdTcSpacePkt  = 13
  toASNInteger FwdTcVca  = 14
  toASNInteger FwdTcFrame  = 15
  toASNInteger FwdCltu = 16




newtype VersionNumber = VersionNumber { getVersionNumber :: Word16 }


data SleBindInvocation = SleBindInvocation {
  _sleBindCredentials :: Credentials 
  , _sleBindInitiatorID :: AuthorityIdentifier
  , _sleBindResponderPortID :: PortID 
  , _sleBindServiceType :: ApplicationIdentifier
  , _sleVersionNumber :: VersionNumber
  }
makeLenses ''SleBindInvocation



sleBindInvocation :: AsnEncoding SleBindInvocation 
sleBindInvocation = ASN1.sequence [
  required "invokerCredentials" _sleBindCredentials credentials 
  , required "initiatorIdentifier" (getAuthorityID . _sleBindInitiatorID) visibleString
  , required "responderPortIdentifier" (getPortID . _sleBindResponderPortID) visibleString
  , required "serviceType" (toASNInteger . _sleBindServiceType) integer
  , required "versionNumber" (fromIntegral . getVersionNumber . _sleVersionNumber) intPosShort
  ]