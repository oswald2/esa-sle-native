{-# LANGUAGE 
  TemplateHaskell
#-}
module SLE.Data.Bind
    ( Credentials
    , SleBindInvocation(..)
    , mkSleBindInvocation
    , mkSleUnbindBindInvocation
    , Time(..)
    , IntPosShort(..)
    , AuthorityIdentifier(..)
    , unAuthorityID
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
    , BindResult(..)
    , BindDiagnostic(..)
    , sleBindReturn
    , sleBindRetCredentials
    , sleBindRetResponderID
    , sleBindRetResult
    , parseSleBindReturn
    , toBindDiagnostic
    , SleUnbind(..)
    , sleUnbindCredentials
    , sleUnbindReason
    , UnbindReason(..)
    , parseSleUnbind
    , parseBindDiagnostic
    , SleUnbindReturn(..)
    , UnbindResult(..)
    , sleUnbindRetCredentials
    , sleUnbindRetResult
    , parseSleUnbindReturn
    ) where


import           RIO
import qualified RIO.ByteString                as B
import           RIO.State
import qualified RIO.Text                      as T

import           Control.Lens            hiding ( Context )
import           Control.Monad.Except
import           Data.Aeson

import           Data.ASN1.BinaryEncoding
import           Data.ASN1.Encoding
import           Data.ASN1.Prim                 ( getInteger )
import           Data.ASN1.Types

import           SLE.Data.Common
import           SLE.Data.ServiceInstanceID





newtype AuthorityIdentifier = AuthorityIdentifier Text
  deriving (Eq, Ord, Show, Read, Generic)

unAuthorityID :: AuthorityIdentifier -> Text
unAuthorityID (AuthorityIdentifier x) = x

instance Display AuthorityIdentifier where
    textDisplay (AuthorityIdentifier x) = x

instance FromJSON AuthorityIdentifier
instance ToJSON AuthorityIdentifier where
    toEncoding = genericToEncoding defaultOptions


authorityIdentifier :: AuthorityIdentifier -> ASN1
authorityIdentifier (AuthorityIdentifier x) = visibleString x

parseAuthorityIdentifier :: Parser AuthorityIdentifier
parseAuthorityIdentifier = do
    AuthorityIdentifier <$> parseVisibleString


newtype PortID = PortID { unPortID :: Text }
  deriving (Eq, Ord, Show, Generic)


portID :: PortID -> ASN1
portID (PortID x) = visibleString x

parsePortID :: Parser PortID
parsePortID = do
    PortID <$> parseVisibleString

instance Display PortID where
    display (PortID val) = display val



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

instance Display ApplicationIdentifier where
    textDisplay x = T.pack (show x)

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
        _ ->
            throwError
                "parseApplicationIdentifier: no int value for application identifier"




newtype VersionNumber = VersionNumber Word16
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (FromJSON, ToJSON)


instance Display VersionNumber where
    display (VersionNumber val) = display val

versionNumber :: VersionNumber -> ASN1
versionNumber (VersionNumber x) = IntVal (fromIntegral x)

parseVersionNumber :: Parser VersionNumber
parseVersionNumber = do
    VersionNumber . fromIntegral <$> parseIntVal



data SleBindInvocation = SleBindInvocation
    { _sleBindCredentials     :: !Credentials
    , _sleBindInitiatorID     :: !AuthorityIdentifier
    , _sleBindResponderPortID :: !PortID
    , _sleBindServiceType     :: !ApplicationIdentifier
    , _sleVersionNumber       :: !VersionNumber
    , _sleServiceInstanceID   :: !ServiceInstanceIdentifier
    }
    deriving (Eq, Show, Generic)
makeLenses ''SleBindInvocation

mkSleBindInvocation
    :: Credentials
    -> AuthorityIdentifier
    -> PortID
    -> ApplicationIdentifier
    -> VersionNumber
    -> ServiceInstanceIdentifier
    -> SleBindInvocation
mkSleBindInvocation creds authID pID appID vn siID = SleBindInvocation
    { _sleBindCredentials     = creds
    , _sleBindInitiatorID     = authID
    , _sleBindResponderPortID = pID
    , _sleBindServiceType     = appID
    , _sleVersionNumber       = vn
    , _sleServiceInstanceID   = siID
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
parseSleBind = content
  where
    -- startContainer =
    --     parseBasicASN1 (== Start (Container Context 100)) (const ())
    endContainer = parseBasicASN1 (== End (Container Context 100)) (const ())

    content      = do
        creds     <- parseCredentials
        authority <- parseAuthorityIdentifier
        port      <- parsePortID
        appID     <- parseApplicationIdentifier
        version   <- parseVersionNumber
        attrs     <- parseServiceInstanceIdentifier
        void endContainer
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
  AccessDenied
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

-- bindDiagnostic :: BindDiagnostic -> ASN1
-- bindDiagnostic AccessDenied                   = IntVal 0
-- bindDiagnostic ServiceTypeNotSupported        = IntVal 1
-- bindDiagnostic VersionNotSupported            = IntVal 2
-- bindDiagnostic NoSuchServiceInstance          = IntVal 3
-- bindDiagnostic AlreadyBound                   = IntVal 4
-- bindDiagnostic SiNotAccessibleToThisInitiator = IntVal 5
-- bindDiagnostic InconsistentServiceType        = IntVal 6
-- bindDiagnostic InvalidTime                    = IntVal 7
-- bindDiagnostic OutOfService                   = IntVal 8
-- bindDiagnostic OtherReason                    = IntVal 127

bindDiagnosticW8 :: BindDiagnostic -> Word8
bindDiagnosticW8 AccessDenied                   = 0
bindDiagnosticW8 ServiceTypeNotSupported        = 1
bindDiagnosticW8 VersionNotSupported            = 2
bindDiagnosticW8 NoSuchServiceInstance          = 3
bindDiagnosticW8 AlreadyBound                   = 4
bindDiagnosticW8 SiNotAccessibleToThisInitiator = 5
bindDiagnosticW8 InconsistentServiceType        = 6
bindDiagnosticW8 InvalidTime                    = 7
bindDiagnosticW8 OutOfService                   = 8
bindDiagnosticW8 OtherReason                    = 127


toBindDiagnostic :: Integer -> BindDiagnostic
toBindDiagnostic 0   = AccessDenied
toBindDiagnostic 1   = ServiceTypeNotSupported
toBindDiagnostic 2   = VersionNotSupported
toBindDiagnostic 3   = NoSuchServiceInstance
toBindDiagnostic 4   = AlreadyBound
toBindDiagnostic 5   = SiNotAccessibleToThisInitiator
toBindDiagnostic 6   = InconsistentServiceType
toBindDiagnostic 7   = InvalidTime
toBindDiagnostic 8   = OutOfService
toBindDiagnostic 127 = OtherReason
toBindDiagnostic _   = OtherReason


parseBindDiagnostic :: Parser BindDiagnostic
parseBindDiagnostic = do
    x <- parseIntVal
    case x of
        0   -> return AccessDenied
        1   -> return ServiceTypeNotSupported
        2   -> return VersionNotSupported
        3   -> return NoSuchServiceInstance
        4   -> return AlreadyBound
        5   -> return SiNotAccessibleToThisInitiator
        6   -> return InconsistentServiceType
        7   -> return InvalidTime
        8   -> return OutOfService
        127 -> return OtherReason
        _   -> throwError $ "parseBindDiagnostic: illegal value: " <> T.pack
            (show x)


data BindResult = BindResVersion VersionNumber | BindResDiag BindDiagnostic
    deriving (Eq, Show, Generic)


data SleBindReturn = SleBindReturn
    { _sleBindRetCredentials :: Credentials
    , _sleBindRetResponderID :: AuthorityIdentifier
    , _sleBindRetResult      :: BindResult
    }
    deriving (Eq, Show, Generic)
makeLenses ''SleBindReturn

retResult :: BindResult -> ASN1
retResult (BindResVersion (VersionNumber v)) =
    Other Context 0 (B.singleton (fromIntegral v))
retResult (BindResDiag bd) =
    Other Context 1 (B.singleton (bindDiagnosticW8 bd))

parseBindResult :: Parser BindResult
parseBindResult = do
    x <- get
    case x of
        (Other Context 0 dat : rest) -> do
            put rest
            case getInteger dat of
                Left err ->
                    throwError
                        $  "parseChoiceBindRet: could not read Version tag: "
                        <> T.pack (show err)
                Right (IntVal vers) ->
                    return (BindResVersion (VersionNumber (fromIntegral vers)))
                Right tp ->
                    throwError
                        $  "parseChoiceBindRet: illegal type received: "
                        <> T.pack (show tp)
        (Other Context 1 dat : rest) -> do
            put rest
            case getInteger dat of
                Left err ->
                    throwError
                        $ "parseChoiceBindRet: could not read Bind Diagnostic: "
                        <> T.pack (show err)
                Right (IntVal diag) ->
                    return (BindResDiag (toBindDiagnostic diag))
                Right tp ->
                    throwError
                        $  "parseChoiceBindRet: illegal type received: "
                        <> T.pack (show tp)

        (Other Context n _ : _) ->
            throwError
                $  "parseChoiceBindRet: illegal value for choice: "
                <> T.pack (show n)
        (o : _) ->
            throwError $ "parseChoiceBindRet: expected CHOICE, got: " <> T.pack
                (show o)
        _ -> throwError "parseChoiceBindRet: expected CHOICE, got nothing."


sleBindReturn :: SleBindReturn -> [ASN1]
sleBindReturn SleBindReturn {..} =
    [ Start (Container Context 101)
    , credentials _sleBindRetCredentials
    , authorityIdentifier _sleBindRetResponderID
    , retResult _sleBindRetResult
    , End (Container Context 101)
    ]


instance EncodeASN1 SleBindReturn where
    encode val = encodeASN1' DER (sleBindReturn val)


parseSleBindReturn :: Parser SleBindReturn
parseSleBindReturn = content
  where
    endContainer = parseBasicASN1 (== End (Container Context 101)) (const ())

    content      = do
        creds     <- parseCredentials
        authority <- parseAuthorityIdentifier
        ret       <- parseBindResult
        void endContainer
        return SleBindReturn { _sleBindRetCredentials = creds
                             , _sleBindRetResponderID = authority
                             , _sleBindRetResult      = ret
                             }


data UnbindReason = UnbindEnd | UnbindSuspend | UnbindVersionNotSupported | UnbindOther
    deriving (Eq, Ord, Enum, Show, Generic)

unbindReason :: UnbindReason -> ASN1
unbindReason UnbindEnd                 = IntVal 0
unbindReason UnbindSuspend             = IntVal 1
unbindReason UnbindVersionNotSupported = IntVal 2
unbindReason UnbindOther               = IntVal 127

instance Display UnbindReason where
    display UnbindEnd                 = "End"
    display UnbindSuspend             = "Suspend"
    display UnbindVersionNotSupported = "Version not supported"
    display UnbindOther               = "Other Reason"

parseUnbindReason :: Parser UnbindReason
parseUnbindReason = do
    x <- parseIntVal
    case x of
        0   -> return UnbindEnd
        1   -> return UnbindSuspend
        2   -> return UnbindVersionNotSupported
        127 -> return UnbindOther
        _ ->
            throwError $ "parseUnbindReason: illegal value: " <> T.pack (show x)


data SleUnbind = SleUnbind
    { _sleUnbindCredentials :: Credentials
    , _sleUnbindReason      :: UnbindReason
    }
    deriving (Show, Generic)
makeLenses ''SleUnbind

mkSleUnbindBindInvocation :: Credentials -> UnbindReason -> SleUnbind
mkSleUnbindBindInvocation creds reason =
    SleUnbind { _sleUnbindCredentials = creds, _sleUnbindReason = reason }


instance EncodeASN1 SleUnbind where
    encode val = encodeASN1' DER (sleUnbind val)

sleUnbind :: SleUnbind -> [ASN1]
sleUnbind SleUnbind {..} =
    [ Start (Container Context 102)
    , credentials _sleUnbindCredentials
    , unbindReason _sleUnbindReason
    , End (Container Context 102)
    ]

parseSleUnbind :: Parser SleUnbind
parseSleUnbind = content
  where
    endContainer = parseBasicASN1 (== End (Container Context 102)) (const ())

    content      = do
        creds  <- parseCredentials
        reason <- parseUnbindReason
        void endContainer
        return SleUnbind { _sleUnbindCredentials = creds
                         , _sleUnbindReason      = reason
                         }

data UnbindResult = Positive | Negative
    deriving (Show, Generic)

data SleUnbindReturn = SleUnbindReturn
    { _sleUnbindRetCredentials :: Credentials
    , _sleUnbindRetResult      :: UnbindResult
    }
    deriving (Show, Generic)
makeLenses ''SleUnbindReturn


sleUnbindReturn :: SleUnbindReturn -> [ASN1]
sleUnbindReturn SleUnbindReturn {..} =
    [ Start (Container Context 103)
    , credentials _sleUnbindRetCredentials
    , unbindResult _sleUnbindRetResult
    , End (Container Context 103)
    ]

unbindResult :: UnbindResult -> ASN1
unbindResult _ = Other Context 0 B.empty

parseUnbindResult :: Parser UnbindResult
parseUnbindResult = do
    x <- get
    case x of
        ((Other Context 0 _) : rest) -> do
            put rest
            return Positive
        rest -> do
            put rest
            return Negative

parseSleUnbindReturn :: Parser SleUnbindReturn
parseSleUnbindReturn = content
  where
    endContainer = parseBasicASN1 (== End (Container Context 103)) (const ())

    content      = do
        creds  <- parseCredentials
        result <- parseUnbindResult
        void endContainer
        return SleUnbindReturn { _sleUnbindRetCredentials = creds
                               , _sleUnbindRetResult      = result
                               }


instance EncodeASN1 SleUnbindReturn where
    encode val = encodeASN1' DER (sleUnbindReturn val)
