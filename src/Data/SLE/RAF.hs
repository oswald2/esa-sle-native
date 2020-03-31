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
  , ApplicationIdentifier(..)
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
import           Control.Lens            hiding ( Context )

import           Data.ASN1.Types
import           Data.ASN1.Encoding
import           Data.ASN1.BinaryEncoding

import           Data.SLE.Common
import           Data.SLE.ServiceInstanceID


newtype AuthorityIdentifier = AuthorityIdentifier { unAuthorityID :: Text }

authorityIdentifier :: AuthorityIdentifier -> ASN1
authorityIdentifier (AuthorityIdentifier x) = visibleString x

getAuthorityIdentifier :: ASN1 -> Maybe AuthorityIdentifier 
getAuthorityIdentifier x = AuthorityIdentifier <$> getVisibleString x 

newtype PortID = PortID { unPortID :: Text }

portID :: PortID -> ASN1
portID (PortID x) = visibleString x

getPortID :: ASN1 -> Maybe PortID 
getPortID x = PortID <$> getVisibleString x


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
applicationIdentifier RtnAllFrames   = IntVal 0
applicationIdentifier RtnInsert      = IntVal 1
applicationIdentifier RtnChFrames    = IntVal 2
applicationIdentifier RtnChFsh       = IntVal 3
applicationIdentifier RtnChOcf       = IntVal 4
applicationIdentifier RtnBitstr      = IntVal 5
applicationIdentifier RtnSpacePkt    = IntVal 6
applicationIdentifier FwdAosSpacePkt = IntVal 7
applicationIdentifier FwdAosVca      = IntVal 8
applicationIdentifier FwdBitstr      = IntVal 9
applicationIdentifier FwdProtoVcdu   = IntVal 10
applicationIdentifier FwdInsert      = IntVal 11
applicationIdentifier FwdCVcdu       = IntVal 12
applicationIdentifier FwdTcSpacePkt  = IntVal 13
applicationIdentifier FwdTcVca       = IntVal 14
applicationIdentifier FwdTcFrame     = IntVal 15
applicationIdentifier FwdCltu        = IntVal 16


getApplicationIdentifier :: ASN1 -> Maybe ApplicationIdentifier 
getApplicationIdentifier (IntVal 0) = Just RtnAllFrames
getApplicationIdentifier (IntVal 1) = Just RtnInsert      
getApplicationIdentifier (IntVal 2) = Just RtnChFrames    
getApplicationIdentifier (IntVal 3) = Just RtnChFsh       
getApplicationIdentifier (IntVal 4) = Just RtnChOcf       
getApplicationIdentifier (IntVal 5) = Just RtnBitstr      
getApplicationIdentifier (IntVal 6) = Just RtnSpacePkt    
getApplicationIdentifier (IntVal 7) = Just FwdAosSpacePkt 
getApplicationIdentifier (IntVal 8) = Just FwdAosVca      
getApplicationIdentifier (IntVal 9) = Just FwdBitstr      
getApplicationIdentifier (IntVal 10) = Just FwdProtoVcdu   
getApplicationIdentifier (IntVal 11) = Just FwdInsert      
getApplicationIdentifier (IntVal 12) = Just FwdCVcdu       
getApplicationIdentifier (IntVal 13) = Just FwdTcSpacePkt  
getApplicationIdentifier (IntVal 14) = Just FwdTcVca       
getApplicationIdentifier (IntVal 15) = Just FwdTcFrame     
getApplicationIdentifier (IntVal 16) = Just FwdCltu        
getApplicationIdentifier _ = Nothing


newtype VersionNumber = VersionNumber { unVersionNumber :: Word16 }

versionNumber :: VersionNumber -> ASN1
versionNumber (VersionNumber x) = IntVal (fromIntegral x)

getVersionNumber :: ASN1 -> Maybe VersionNumber 
getVersionNumber (IntVal x) = Just (VersionNumber (fromIntegral x))
getVersionNumber _ = Nothing


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
  [ Start (Container Context 100)
    , credentials _sleBindCredentials
    , authorityIdentifier _sleBindInitiatorID
    , portID _sleBindResponderPortID
    , applicationIdentifier _sleBindServiceType
    , versionNumber _sleVersionNumber
    ]
    <> serviceInstanceIdentifier _sleServiceInstanceID
    <> [End (Container Context 100)]


instance EncodeASN1 SleBindInvocation where
  encode val = encodeASN1' DER (sleBindInvocation val)


-- getSleBind :: [ASN1] -> Maybe SleBindInvocation 
-- getSleBind (Start (Container Context 100) : creds : auth : port : apid : vers : rest ) = 
--   let creds = getCredentials rest
--   getAuthorityIdentifier

-- Received ASN1: [Start (Container Context 100)
  -- , Other Context 0 ""
  -- , ASN1String (ASN1CharacterString {characterEncoding = Visible, getCharacterStringRawData = "SLE_USER"})
  -- , ASN1String (ASN1CharacterString {characterEncoding = Visible, getCharacterStringRawData = "55529"})
  -- , IntVal 0
  -- , IntVal 2
  -- , Start Sequence
  -- , Start Set
  -- , Start Sequence
  -- , OID [1,3,112,4,3,1,2,52]
  -- , ASN1String (ASN1CharacterString {characterEncoding = Visible, getCharacterStringRawData = "1"})
  -- , End Sequence
  -- , End Set
  -- , Start Set
  -- , Start Sequence
  -- , OID [1,3,112,4,3,1,2,53]
  -- , ASN1String (ASN1CharacterString {characterEncoding = Visible, getCharacterStringRawData = "VST-PASS0001"})
  -- , End Sequence
  -- , End Set
  -- , Start Set
  -- , Start Sequence
  -- , OID [1,3,112,4,3,1,2,38]
  -- , ASN1String (ASN1CharacterString {characterEncoding = Visible, getCharacterStringRawData = "1"})
  -- , End Sequence
  -- , End Set
  -- , Start Set
  -- , Start Sequence
  -- , OID [1,3,112,4,3,1,2,22]
  -- , ASN1String (ASN1CharacterString {characterEncoding = Visible, getCharacterStringRawData = "onlt1"})
  -- , End Sequence
  -- , End Set
  -- , End Sequence
  -- , End (Container Context 100)]
