{-# LANGUAGE 
  TemplateHaskell
#-}
module SLE.Data.Bind
  ( Credentials
  , SleBindInvocation
  , mkSleBindInvocation
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
  , parseSleBind
  , parseAuthorityIdentifier
  , parsePortID
  , parseApplicationIdentifier
  , SleBindReturn(..)
  , sleBindReturn
  , sleBindRetCredentials
  , sleBindRetResponderID
  , sleBindRetResult
  , parseSleBindReturn
  )
where


import           RIO
import qualified RIO.Text                      as T
import qualified RIO.ByteString.Lazy           as BL
import           Control.Lens            hiding ( Context )
import           Control.Monad.Except
import           Data.Aeson


import           Data.ASN1.Types
import           Data.ASN1.Encoding
import           Data.ASN1.BinaryEncoding

import           SLE.Data.Common
import           SLE.Data.ServiceInstanceID





newtype AuthorityIdentifier = AuthorityIdentifier { unAuthorityID :: Text }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON AuthorityIdentifier
instance ToJSON AuthorityIdentifier where
  toEncoding = genericToEncoding defaultOptions


authorityIdentifier :: AuthorityIdentifier -> ASN1
authorityIdentifier (AuthorityIdentifier x) = visibleString x

-- getAuthorityIdentifier :: ASN1 -> Maybe AuthorityIdentifier 
-- getAuthorityIdentifier x = AuthorityIdentifier <$> getVisibleString x 

parseAuthorityIdentifier :: Parser AuthorityIdentifier
parseAuthorityIdentifier = do
  AuthorityIdentifier <$> parseVisibleString




newtype PortID = PortID { unPortID :: Text }
  deriving (Eq, Ord, Show, Generic)


portID :: PortID -> ASN1
portID (PortID x) = visibleString x

-- getPortID :: ASN1 -> Maybe PortID 
-- getPortID x = PortID <$> getVisibleString x

parsePortID :: Parser PortID
parsePortID = do
  PortID <$> parseVisibleString



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


-- getApplicationIdentifier :: ASN1 -> Maybe ApplicationIdentifier
-- getApplicationIdentifier (IntVal 0 ) = Just RtnAllFrames
-- getApplicationIdentifier (IntVal 1 ) = Just RtnInsert
-- getApplicationIdentifier (IntVal 2 ) = Just RtnChFrames
-- getApplicationIdentifier (IntVal 3 ) = Just RtnChFsh
-- getApplicationIdentifier (IntVal 4 ) = Just RtnChOcf
-- getApplicationIdentifier (IntVal 5 ) = Just RtnBitstr
-- getApplicationIdentifier (IntVal 6 ) = Just RtnSpacePkt
-- getApplicationIdentifier (IntVal 7 ) = Just FwdAosSpacePkt
-- getApplicationIdentifier (IntVal 8 ) = Just FwdAosVca
-- getApplicationIdentifier (IntVal 9 ) = Just FwdBitstr
-- getApplicationIdentifier (IntVal 10) = Just FwdProtoVcdu
-- getApplicationIdentifier (IntVal 11) = Just FwdInsert
-- getApplicationIdentifier (IntVal 12) = Just FwdCVcdu
-- getApplicationIdentifier (IntVal 13) = Just FwdTcSpacePkt
-- getApplicationIdentifier (IntVal 14) = Just FwdTcVca
-- getApplicationIdentifier (IntVal 15) = Just FwdTcFrame
-- getApplicationIdentifier (IntVal 16) = Just FwdCltu
-- getApplicationIdentifier _           = Nothing


parseApplicationIdentifier :: Parser ApplicationIdentifier
parseApplicationIdentifier = do
  v <- parseIntVal
  case v of
    0  -> return RtnAllFrames
    1  -> return RtnInsert
    2  -> return RtnChFrames
    3  -> return RtnChFsh
    4  -> return RtnChOcf
    5  -> return RtnBitstr
    6  -> return RtnSpacePkt
    7  -> return FwdAosSpacePkt
    8  -> return FwdAosVca
    9  -> return FwdBitstr
    10 -> return FwdProtoVcdu
    11 -> return FwdInsert
    12 -> return FwdCVcdu
    13 -> return FwdTcSpacePkt
    14 -> return FwdTcVca
    15 -> return FwdTcFrame
    16 -> return FwdCltu
    _  -> throwError
      "parseApplicationIdentifier: no int value for application identifier"




newtype VersionNumber = VersionNumber { unVersionNumber :: Word16 }
  deriving (Eq, Show, Generic)

versionNumber :: VersionNumber -> ASN1
versionNumber (VersionNumber x) = IntVal (fromIntegral x)

-- getVersionNumber :: ASN1 -> Maybe VersionNumber
-- getVersionNumber (IntVal x) = Just (VersionNumber (fromIntegral x))
-- getVersionNumber _          = Nothing

parseVersionNumber :: Parser VersionNumber
parseVersionNumber = do
  VersionNumber . fromIntegral <$> parseIntVal



data SleBindInvocation = SleBindInvocation {
  _sleBindCredentials :: !Credentials
  , _sleBindInitiatorID :: !AuthorityIdentifier
  , _sleBindResponderPortID :: !PortID
  , _sleBindServiceType :: !ApplicationIdentifier
  , _sleVersionNumber :: !VersionNumber
  , _sleServiceInstanceID :: !ServiceInstanceIdentifier
  } deriving (Eq, Show, Generic)
makeLenses ''SleBindInvocation

mkSleBindInvocation :: 
  AuthorityIdentifier 
  -> PortID 
  -> ApplicationIdentifier
  -> VersionNumber 
  -> ServiceInstanceIdentifier
  -> SleBindInvocation
mkSleBindInvocation authID pID appID vn siID = 
  SleBindInvocation {
  _sleBindCredentials = Nothing 
  , _sleBindInitiatorID = authID
  , _sleBindResponderPortID = pID
  , _sleBindServiceType = appID
  , _sleVersionNumber = vn
  , _sleServiceInstanceID = siID
  }

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


parseSleBind :: Parser SleBindInvocation
parseSleBind = do
  between startContainer endContainer content
 where
  startContainer = parseBasicASN1 (== Start (Container Context 100)) (const ())
  endContainer   = parseBasicASN1 (== End (Container Context 100)) (const ())

  content        = do
    creds     <- parseCredentials
    authority <- parseAuthorityIdentifier
    port      <- parsePortID
    appID     <- parseApplicationIdentifier
    version   <- parseVersionNumber
    attrs     <- parseServiceInstanceIdentifier
    return SleBindInvocation { _sleBindCredentials     = creds
                             , _sleBindInitiatorID     = authority
                             , _sleBindResponderPortID = port
                             , _sleBindServiceType     = appID
                             , _sleVersionNumber       = version
                             , _sleServiceInstanceID   = attrs
                             }


instance EncodeASN1 SleBindInvocation where
  encode val = encodeASN1' DER (sleBindInvocation val)



data BindDiagnostic =
  AccesDenied
  | ServiceTypeNotSupported
  | VersionNotSupported
  | NoSuchServiceInstance
  | AlreadyBound
  | SiNotAccessibleToThisInitiator
  | InconsistentServiceType
  | InvalidTime
  | OutOfService
  | OtherReason
  deriving (Eq, Ord, Enum, Show, Generic)

bindDiagnostic :: BindDiagnostic -> ASN1
bindDiagnostic AccesDenied                    = IntVal 0
bindDiagnostic ServiceTypeNotSupported        = IntVal 1
bindDiagnostic VersionNotSupported            = IntVal 2
bindDiagnostic NoSuchServiceInstance          = IntVal 3
bindDiagnostic AlreadyBound                   = IntVal 4
bindDiagnostic SiNotAccessibleToThisInitiator = IntVal 5
bindDiagnostic InconsistentServiceType        = IntVal 6
bindDiagnostic InvalidTime                    = IntVal 7
bindDiagnostic OutOfService                   = IntVal 8
bindDiagnostic OtherReason                    = IntVal 127

-- toBindDiagnostic :: Int -> BindDiagnostic
-- toBindDiagnostic 0 = AccesDenied
-- toBindDiagnostic 1 = ServiceTypeNotSupported
-- toBindDiagnostic 2 = VersionNotSupported
-- toBindDiagnostic 3 = NoSuchServiceInstance
-- toBindDiagnostic 4 = AlreadyBound
-- toBindDiagnostic 5 = SiNotAccessibleToThisInitiator
-- toBindDiagnostic 6 = InconsistentServiceType
-- toBindDiagnostic 7 = InvalidTime
-- toBindDiagnostic 8 = OutOfService
-- toBindDiagnostic 127 = OtherReason
-- toBindDiagnostic _ = OtherReason


parseBindDiagnostic :: Parser BindDiagnostic
parseBindDiagnostic = do
  x <- parseIntVal
  case x of
    0   -> return AccesDenied
    1   -> return ServiceTypeNotSupported
    2   -> return VersionNotSupported
    3   -> return NoSuchServiceInstance
    4   -> return AlreadyBound
    5   -> return SiNotAccessibleToThisInitiator
    6   -> return InconsistentServiceType
    7   -> return InvalidTime
    8   -> return OutOfService
    127 -> return OtherReason
    _ -> throwError $ "parseBindDiagnostic: illegal value: " <> T.pack (show x)



data SleBindReturn = SleBindReturn {
  _sleBindRetCredentials :: Credentials
  , _sleBindRetResponderID :: AuthorityIdentifier
  , _sleBindRetResult :: Either VersionNumber BindDiagnostic
} deriving (Eq, Show, Generic)
makeLenses ''SleBindReturn

retResult :: Either VersionNumber BindDiagnostic -> ASN1
retResult (Left vn) =
  Other Context 0 (BL.toStrict (encodeASN1 DER [versionNumber vn]))
retResult (Right bd) =
  Other Context 1 (BL.toStrict (encodeASN1 DER [bindDiagnostic bd]))

parseRet :: Parser (Either VersionNumber BindDiagnostic)
parseRet = parseEitherASN1 parseVersionNumber parseBindDiagnostic


sleBindReturn :: SleBindReturn -> [ASN1]
sleBindReturn SleBindReturn {..} =
  [ Start Sequence
  , credentials _sleBindRetCredentials
  , authorityIdentifier _sleBindRetResponderID
  , retResult _sleBindRetResult
  , End Sequence
  ]


instance EncodeASN1 SleBindReturn where
  encode val = encodeASN1' DER (sleBindReturn val)


parseSleBindReturn :: Parser SleBindReturn
parseSleBindReturn = do
  between startContainer endContainer content
 where
  startContainer = parseBasicASN1 (== Start (Container Context 101)) (const ())
  endContainer   = parseBasicASN1 (== End (Container Context 101)) (const ())

  content        = do
    creds     <- parseCredentials
    authority <- parseAuthorityIdentifier
    ret       <- parseRet
    return SleBindReturn { _sleBindRetCredentials = creds
                         , _sleBindRetResponderID = authority
                         , _sleBindRetResult      = ret
                         }
