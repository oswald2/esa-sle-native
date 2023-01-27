{-# LANGUAGE TemplateHaskell #-}
module SLE.Data.FCLTUOps
    ( CltuIdentification(..)
    , FcltuStartInvocation(..)
    , FcltuStartReturn(..)
    , FcltuStartTimes(..)
    , FcltuStartResult(..)
    , EventInvocationID(..)
    , SlduStatusNotification(..)
    , Duration(..)
    , FcltuThrowEventInvocation(..)
    , FcltuTransDataInvocation(..)
    , parseFcltuStart
    , parseFcltuThrowEvent
    , parseFcltuTransDataInvocation
    , parseFcltuStartReturn
    , fcltuStartCredentials
    , fcltuStartInvokeID
    , fcluStartFirstCltuIdentification
    , fcltuStartRetCredentials
    , fcltuStartRetInvokeID
    , fcltuStartRetResult
    , fcltuThrowCredentials
    , fcltuThrowInvokeID
    , fcltuThrowEventInvocationIdent
    , fcltuThrowEventIdentifier
    , fcltuThrowEventQualifier
    , fcltuDataCredentials
    , fcltuDataInvokeID
    , fcltuDataIdent
    , fcltuDataEarliestTransmission
    , fcltuDataLatestTransmission
    , fcltuDataDelayTime
    , fcltuDataRadiationNotification
    , fcltuData
    ) where

import           RIO
import           RIO.State

import           Control.Lens                   ( makeLenses )
import           Control.Monad.Except

import           Data.ASN1.BinaryEncoding
import           Data.ASN1.Encoding
import           Data.ASN1.Prim
import           Data.ASN1.Types

import           SLE.Data.Common


newtype CltuIdentification = CltuIdentification Word32
    deriving (Show, Generic)

cltuIdentification :: CltuIdentification -> ASN1
cltuIdentification (CltuIdentification x) = IntVal (fromIntegral x)

parseCltuIdentification :: Parser CltuIdentification
parseCltuIdentification = CltuIdentification . fromIntegral <$> parseIntVal

newtype EventInvocationID = EventInvocationID Word32
    deriving (Show, Generic)

eventInvocationID :: EventInvocationID -> ASN1
eventInvocationID (EventInvocationID x) = IntVal (fromIntegral x)

parseEventInvocationID :: Parser EventInvocationID
parseEventInvocationID = EventInvocationID . fromIntegral <$> parseIntVal


newtype Duration = Duration Word32
    deriving (Show, Generic)

duration :: Duration -> ASN1
duration (Duration x) = IntVal (fromIntegral x)

parseDuration :: Parser Duration
parseDuration = Duration . fromIntegral <$> parseIntVal



data FcltuStartInvocation = FcltuStartInvocation
    { _fcltuStartCredentials            :: !Credentials
    , _fcltuStartInvokeID               :: !Word16
    , _fcluStartFirstCltuIdentification :: !CltuIdentification
    }
    deriving (Show, Generic)
makeLenses ''FcltuStartInvocation


fcltuStartInvocation :: FcltuStartInvocation -> [ASN1]
fcltuStartInvocation FcltuStartInvocation {..} =
    [ Start (Container Context 0)
    , credentials _fcltuStartCredentials
    , IntVal (fromIntegral _fcltuStartInvokeID)
    , cltuIdentification _fcluStartFirstCltuIdentification
    , End (Container Context 0)
    ]


parseFcltuStart :: Parser FcltuStartInvocation
parseFcltuStart = content
  where
    endContainer = parseBasicASN1 (== End (Container Context 0)) (const ())

    content      = do
        creds    <- parseCredentials
        invokeID <- parseIntVal
        start    <- parseCltuIdentification
        void endContainer
        return FcltuStartInvocation
            { _fcltuStartCredentials            = creds
            , _fcltuStartInvokeID               = fromIntegral invokeID
            , _fcluStartFirstCltuIdentification = start
            }

instance EncodeASN1 FcltuStartInvocation where
    encode val = encodeASN1' DER (fcltuStartInvocation val)


data FcltuStartTimes = FcltuStartTimes
    { _fcltuStartRadiationTime :: !Time
    , _fcltuStopRadiationTime  :: !ConditionalTime
    }
    deriving (Show, Generic)


data FcltuStartSpecific =
    FcltuStartOutOfService
    | FcltuStartUnableToComply
    | FcltuStartProductionTimeExpired
    | FcltuStartInvalidCltuID
    | FcltuStartInvalid
    deriving(Eq, Ord, Enum, Show, Generic)

fcltuStartSpecific :: FcltuStartSpecific -> ASN1
fcltuStartSpecific FcltuStartOutOfService          = IntVal 0
fcltuStartSpecific FcltuStartUnableToComply        = IntVal 1
fcltuStartSpecific FcltuStartProductionTimeExpired = IntVal 2
fcltuStartSpecific FcltuStartInvalidCltuID         = IntVal 3
fcltuStartSpecific FcltuStartInvalid               = IntVal 1

fcltuStartSpecificFromInt :: ASN1 -> FcltuStartSpecific
fcltuStartSpecificFromInt (IntVal 0) = FcltuStartOutOfService
fcltuStartSpecificFromInt (IntVal 1) = FcltuStartUnableToComply
fcltuStartSpecificFromInt (IntVal 2) = FcltuStartProductionTimeExpired
fcltuStartSpecificFromInt (IntVal 3) = FcltuStartInvalidCltuID
fcltuStartSpecificFromInt _          = FcltuStartInvalid

data DiagnosticFcltuStart = DiagFcltuStartCommon Diagnostics | DiagFcltuStartSpecific FcltuStartSpecific
    deriving(Show, Generic)

diagnosticFcltuStart :: DiagnosticFcltuStart -> ASN1
diagnosticFcltuStart (DiagFcltuStartCommon diag) =
    Other Context 0 (encodeASN1' DER [diagnostics diag])
diagnosticFcltuStart (DiagFcltuStartSpecific diag) =
    Other Context 1 (encodeASN1' DER [fcltuStartSpecific diag])

parseDiagnosticFcltuStart :: Parser DiagnosticFcltuStart
parseDiagnosticFcltuStart = do
    x <- get
    case x of
        ((Other Context 0 bs) : rest) -> do
            put rest
            case decodeASN1' DER bs of
                Left err ->
                    throwError
                        $ "Diagnostic FCLTU Start Parser: could not deocde diagnostics: "
                        <> fromString (show err)
                Right v -> case parseASN1 parseDiagnostics v of
                    Left err ->
                        throwError
                            $ "Diagnostic FCLTU Start Parser: could not deocde diagnostics: "
                            <> fromString (show err)
                    Right diag -> return (DiagFcltuStartCommon diag)
        ((Other Context 1 bs) : rest) -> do
            put rest
            case getInteger bs of
                Left err ->
                    throwError
                        $ "Diagnostic FCLTU Start Parser: could not decode specific diagnostic"
                        <> fromString (show err)
                Right v -> return
                    (DiagFcltuStartSpecific (fcltuStartSpecificFromInt v))
        _ -> throwError
            "FCLTU Start diagnostics parser: could not parse diagnostics"

data FcltuStartResult = FcltuStartNegative DiagnosticFcltuStart | FcltuStartPositive FcltuStartTimes
    deriving (Show, Generic)

fcltuStartResult :: FcltuStartResult -> [ASN1]
fcltuStartResult (FcltuStartPositive startTimes) =
    [ Start (Container Context 0)
    , time (_fcltuStartRadiationTime startTimes)
    , conditionalTime (_fcltuStopRadiationTime startTimes)
    , End (Container Context 0)
    ]
fcltuStartResult (FcltuStartNegative diag) =
    [Other Context 1 (encodeASN1' DER [diagnosticFcltuStart diag])]


parseFcltuStartResult :: Parser FcltuStartResult
parseFcltuStartResult = do
    x <- get
    case x of
        ((Start (Container Context 0)) : rest) -> do
            put rest
            startT <- parseTime
            stopT  <- parseConditionalTime
            void endContainer
            return $ FcltuStartPositive FcltuStartTimes
                { _fcltuStartRadiationTime = startT
                , _fcltuStopRadiationTime  = stopT
                }
        ((Other Context 1 bs) : rest) -> do
            put rest
            case decodeASN1' DER bs of
                Left err ->
                    throwError
                        $ "FCLTU Start Return Parser: could not deocde diagnostics: "
                        <> fromString (show err)
                Right v -> case parseASN1 parseDiagnosticFcltuStart v of
                    Left err ->
                        throwError
                            $ "FCLTU Start Return Parser: could not deocde diagnostics: "
                            <> fromString (show err)
                    Right diag -> return (FcltuStartNegative diag)
        asn1 -> do
            throwError
                $  "FCLTU Start Return: unexpected ASN1 value: "
                <> fromString (show asn1)

  where
    endContainer = parseBasicASN1 (== End (Container Context 0)) (const ())


data FcltuStartReturn = FcltuStartReturn
    { _fcltuStartRetCredentials :: !Credentials
    , _fcltuStartRetInvokeID    :: !Word16
    , _fcltuStartRetResult      :: !FcltuStartResult
    }
    deriving (Show, Generic)
makeLenses ''FcltuStartReturn

fcltuStartReturn :: FcltuStartReturn -> [ASN1]
fcltuStartReturn FcltuStartReturn {..} =
    [ Start (Container Context 1)
        , credentials _fcltuStartRetCredentials
        , IntVal (fromIntegral _fcltuStartRetInvokeID)
        ]
        ++ (fcltuStartResult _fcltuStartRetResult)
        ++ [End (Container Context 1)]

parseFcltuStartReturn :: Parser FcltuStartReturn
parseFcltuStartReturn = content
  where
    endContainer = parseBasicASN1 (== End (Container Context 1)) (const ())

    content      = do
        creds    <- parseCredentials
        invokeID <- parseIntVal
        diag     <- parseFcltuStartResult
        void endContainer
        return FcltuStartReturn { _fcltuStartRetCredentials = creds
                                , _fcltuStartRetInvokeID = fromIntegral invokeID
                                , _fcltuStartRetResult = diag
                                }


instance EncodeASN1 FcltuStartReturn where
    encode val = encodeASN1' DER (fcltuStartReturn val)


data FcltuThrowEventInvocation = FcltuThrowEventInvocation
    { _fcltuThrowCredentials          :: !Credentials
    , _fcltuThrowInvokeID             :: !Word16
    , _fcltuThrowEventInvocationIdent :: !EventInvocationID
    , _fcltuThrowEventIdentifier      :: !Word16
    , _fcltuThrowEventQualifier       :: !ByteString
    }
    deriving (Show, Generic)
makeLenses ''FcltuThrowEventInvocation

fcltuThrowEventInvocation :: FcltuThrowEventInvocation -> [ASN1]
fcltuThrowEventInvocation FcltuThrowEventInvocation {..} =
    [ Start (Container Context 8)
    , credentials _fcltuThrowCredentials
    , IntVal (fromIntegral _fcltuThrowInvokeID)
    , eventInvocationID _fcltuThrowEventInvocationIdent
    , IntVal (fromIntegral _fcltuThrowEventIdentifier)
    , OctetString _fcltuThrowEventQualifier
    , End (Container Context 8)
    ]


parseFcltuThrowEvent :: Parser FcltuThrowEventInvocation
parseFcltuThrowEvent = content
  where
    endContainer = parseBasicASN1 (== End (Container Context 8)) (const ())

    content      = do
        creds    <- parseCredentials
        invokeID <- parseIntVal
        start    <- parseEventInvocationID
        eventID  <- parseIntVal
        dat      <- parseOctetString
        void endContainer
        return FcltuThrowEventInvocation
            { _fcltuThrowCredentials          = creds
            , _fcltuThrowInvokeID             = fromIntegral invokeID
            , _fcltuThrowEventInvocationIdent = start
            , _fcltuThrowEventIdentifier      = fromIntegral eventID
            , _fcltuThrowEventQualifier       = dat
            }

instance EncodeASN1 FcltuThrowEventInvocation where
    encode val = encodeASN1' DER (fcltuThrowEventInvocation val)


data SlduStatusNotification = ProduceNotification | DoNotProduceNotification
    deriving stock (Eq, Ord, Enum, Show, Generic)
    deriving anyclass (NFData)

slduStatusNotification :: SlduStatusNotification -> ASN1
slduStatusNotification ProduceNotification      = IntVal 0
slduStatusNotification DoNotProduceNotification = IntVal 1

parseSlduStatusNotification :: Parser SlduStatusNotification
parseSlduStatusNotification = do
    v <- parseIntVal
    case v of
        0 -> return ProduceNotification
        1 -> return DoNotProduceNotification
        _ -> return DoNotProduceNotification


data FcltuTransDataInvocation = FcltuTransDataInvocation
    { _fcltuDataCredentials           :: !Credentials
    , _fcltuDataInvokeID              :: !Word16
    , _fcltuDataIdent                 :: !CltuIdentification
    , _fcltuDataEarliestTransmission  :: !ConditionalTime
    , _fcltuDataLatestTransmission    :: !ConditionalTime
    , _fcltuDataDelayTime             :: !Duration
    , _fcltuDataRadiationNotification :: !SlduStatusNotification
    , _fcltuData                      :: !ByteString
    }
    deriving (Show, Generic)
makeLenses ''FcltuTransDataInvocation

fcltuTransDataInvocation :: FcltuTransDataInvocation -> [ASN1]
fcltuTransDataInvocation FcltuTransDataInvocation {..} =
    [ Start (Container Context 10)
    , credentials _fcltuDataCredentials
    , IntVal (fromIntegral _fcltuDataInvokeID)
    , cltuIdentification _fcltuDataIdent
    , conditionalTime _fcltuDataEarliestTransmission
    , conditionalTime _fcltuDataLatestTransmission
    , duration _fcltuDataDelayTime
    , slduStatusNotification _fcltuDataRadiationNotification
    , OctetString _fcltuData
    , End (Container Context 10)
    ]

parseFcltuTransDataInvocation :: Parser FcltuTransDataInvocation
parseFcltuTransDataInvocation = content
  where
    endContainer = parseBasicASN1 (== End (Container Context 10)) (const ())

    content      = do
        creds    <- parseCredentials
        invokeID <- parseIntVal
        ident    <- parseCltuIdentification
        earliest <- parseConditionalTime
        latest   <- parseConditionalTime
        dur      <- parseDuration
        notif    <- parseSlduStatusNotification
        dat      <- parseOctetString
        void endContainer
        return FcltuTransDataInvocation
            { _fcltuDataCredentials           = creds
            , _fcltuDataInvokeID              = fromIntegral invokeID
            , _fcltuDataIdent                 = ident
            , _fcltuDataEarliestTransmission  = earliest
            , _fcltuDataLatestTransmission    = latest
            , _fcltuDataDelayTime             = dur
            , _fcltuDataRadiationNotification = notif
            , _fcltuData                      = dat
            }

instance EncodeASN1 FcltuTransDataInvocation where
    encode val = encodeASN1' DER (fcltuTransDataInvocation val)
