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
  , sleBindCredentials
  , sleBindInitiatorID
  , sleBindResponderPortID
  , sleBindServiceType
  , sleVersionNumber
  , sleServiceInstanceID
  )
where

import           RIO
import           Control.Lens

import           Data.ASN1.Types
import           Data.ASN1.Encoding 
import           Data.ASN1.BinaryEncoding 

import           Data.SLE.Common
import           Data.SLE.ServiceInstanceID


newtype AuthorityIdentifier = AuthorityIdentifier { getAuthorityID :: Text }

authorityIdentifier :: AuthorityIdentifier -> ASN1
authorityIdentifier (AuthorityIdentifier x) = visibleString x


newtype PortID = PortID { getPortID :: Text }

portID :: PortID -> ASN1
portID (PortID x) = visibleString x



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


applicationIdentifier :: ApplicationIdentifier -> ASN1
applicationIdentifier RtnAllFrames   = Enumerated 0
applicationIdentifier RtnInsert      = Enumerated 1
applicationIdentifier RtnChFrames    = Enumerated 2
applicationIdentifier RtnChFsh       = Enumerated 3
applicationIdentifier RtnChOcf       = Enumerated 4
applicationIdentifier RtnBitstr      = Enumerated 5
applicationIdentifier RtnSpacePkt    = Enumerated 6
applicationIdentifier FwdAosSpacePkt = Enumerated 7
applicationIdentifier FwdAosVca      = Enumerated 8
applicationIdentifier FwdBitstr      = Enumerated 9
applicationIdentifier FwdProtoVcdu   = Enumerated 10
applicationIdentifier FwdInsert      = Enumerated 11
applicationIdentifier FwdCVcdu       = Enumerated 12
applicationIdentifier FwdTcSpacePkt  = Enumerated 13
applicationIdentifier FwdTcVca       = Enumerated 14
applicationIdentifier FwdTcFrame     = Enumerated 15
applicationIdentifier FwdCltu        = Enumerated 16


newtype VersionNumber = VersionNumber { getVersionNumber :: Word16 }

versionNumber :: VersionNumber -> ASN1
versionNumber (VersionNumber x) = IntVal (fromIntegral x)



data SleBindInvocation = SleBindInvocation {
  _sleBindCredentials :: Credentials
  , _sleBindInitiatorID :: AuthorityIdentifier
  , _sleBindResponderPortID :: PortID
  , _sleBindServiceType :: ApplicationIdentifier
  , _sleVersionNumber :: VersionNumber
  , _sleServiceInstanceID :: ServiceInstanceIdentifier
  }
makeLenses ''SleBindInvocation


sleBindInvocation :: SleBindInvocation -> [ASN1]
sleBindInvocation SleBindInvocation {..} =
  [ Start Sequence
    , credentials _sleBindCredentials
    , authorityIdentifier _sleBindInitiatorID
    , portID _sleBindResponderPortID
    , applicationIdentifier _sleBindServiceType
    , versionNumber _sleVersionNumber
    ]
    ++ serviceInstanceIdentifier _sleServiceInstanceID
    ++ [End Sequence]


instance EncodeASN1 SleBindInvocation where 
  encode val = encodeASN1' DER (sleBindInvocation val)
