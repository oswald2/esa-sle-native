{-# LANGUAGE TemplateHaskell, DerivingVia #-}
module SLE.Data.FCLTUOps
    ( CltuIdentification(..)
    , FcltuStartInvocation(..)
    , FcltuStartReturn(..)
    , FcltuStartTimes(..)
    , FcltuStartResult(..)
    , DiagnosticFcltuStart(..)
    , FcltuStartSpecific(..)
    , EventInvocationID(..)
    , SlduStatusNotification(..)
    , Duration(..)
    , FcltuThrowEventInvocation(..)
    , FcltuTransDataInvocation(..)
    , FcltuTransferDataReturn(..)
    , FcltuTransDiagnostic(..)
    , FcltuTransResult(..)
    , CltuNotification(..)
    , CltuLastProcessed(..)
    , CltuLastOk(..)
    , ProductionStatus(..)
    , UplinkStatus(..)
    , FcltuAsyncNotify(..)
    , CltuStatusReport(..)
    , FcltuGetParameterReturn(..)
    , DiagnosticFcltuGet(..)
    , FcltuGetParameter(..)
    , FcltuDiagFcltuGetSpecific(..)
    , GvcID(..)
    , VChannel(..)
    , ClcwGvcID(..)
    , parseFcltuGetParameterReturn
    , parseFcltuStart
    , parseFcltuThrowEvent
    , parseFcltuTransDataInvocation
    , parseFcltuStartReturn
    , parseFcltuTransferDataReturn
    , parseFcltuAsyncStatus
    , parseCltuStatusReport
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
    , fcltuTransRetCredentials
    , fcltuTransRetInvokeID
    , fcltuTransRetIdentification
    , fcltuTransRetBufferAvailable
    , fcltuTransRetResult
    , fcltuAsyncCredentials
    , fcltuAsyncNotification
    , fcltuAsyncLastProcessed
    , fcltuAsyncLastOK
    , fcltuAsyncProductionStatus
    , fcltuAsyncUplinkStatus
    , parseCltuLastProcessed
    , parseCltuIdentification
    , parseCltuLastOk
    , fcltuStatusCredentials
    , fcltuStatusLastProcessed
    , fcltuStatusLastOk
    , fcltuStatusProductionStatus
    , fcltuStatusUplinkStatus
    , fcltuStatusNumReceived
    , fcltuStatusNumProcessed
    , fcltuStatusNumRadiated
    , fcltuStatusBufferAvailable
    , fgpCredentials
    , fgpInvokeID
    , fgpResult
    ) where

import           RIO
import qualified RIO.ByteString                as B
import           RIO.State

import           Control.Lens                   ( makeLenses )
import           Control.Monad.Except

import           Data.ASN1.BinaryEncoding
import           Data.ASN1.Encoding
import           Data.ASN1.Prim
import           Data.ASN1.Types

import           SLE.Data.Common
import           SLE.Data.HexBytes

--import           Text.Show.Pretty

newtype CltuIdentification = CltuIdentification Word32
    deriving stock (Eq, Ord, Show,Generic)
    deriving Num via Word32

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
    [Start (Container Context 0), time (_fcltuStartRadiationTime startTimes)]
        ++ conditionalTime (_fcltuStopRadiationTime startTimes)
        ++ [End (Container Context 0)]
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
    , _fcltuData                      :: !HexBytes
    }
    deriving (Show, Generic)
makeLenses ''FcltuTransDataInvocation

fcltuTransDataInvocation :: FcltuTransDataInvocation -> [ASN1]
fcltuTransDataInvocation FcltuTransDataInvocation {..} =
    [ Start (Container Context 10)
        , credentials _fcltuDataCredentials
        , IntVal (fromIntegral _fcltuDataInvokeID)
        , cltuIdentification _fcltuDataIdent
        ]
        ++ conditionalTime _fcltuDataEarliestTransmission
        ++ conditionalTime _fcltuDataLatestTransmission
        ++ [ duration _fcltuDataDelayTime
           , slduStatusNotification _fcltuDataRadiationNotification
           , OctetString (hexToBS _fcltuData)
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
            , _fcltuData                      = bsToHex dat
            }

instance EncodeASN1 FcltuTransDataInvocation where
    encode val = encodeASN1' DER (fcltuTransDataInvocation val)



data FcltuTransferDataReturn = FcltuTransferDataReturn
    { _fcltuTransRetCredentials     :: !Credentials
    , _fcltuTransRetInvokeID        :: !Word16
    , _fcltuTransRetIdentification  :: !CltuIdentification
    , _fcltuTransRetBufferAvailable :: !Word32
    , _fcltuTransRetResult          :: !(Maybe FcltuTransResult)
    }
    deriving (Show, Generic)

fcltuTransferDataReturn :: FcltuTransferDataReturn -> [ASN1]
fcltuTransferDataReturn FcltuTransferDataReturn {..} =
    [ Start (Container Context 11)
    , credentials _fcltuTransRetCredentials
    , IntVal (fromIntegral _fcltuTransRetInvokeID)
    , cltuIdentification _fcltuTransRetIdentification
    , IntVal (fromIntegral _fcltuTransRetBufferAvailable)
    , optionalDiagFcltuTrans _fcltuTransRetResult
    , End (Container Context 11)
    ]


parseFcltuTransferDataReturn :: Parser FcltuTransferDataReturn
parseFcltuTransferDataReturn = content
  where
    endContainer = parseBasicASN1 (== End (Container Context 11)) (const ())

    content      = do
        creds    <- parseCredentials
        invokeID <- parseIntVal
        cltuID   <- parseCltuIdentification
        buffer   <- parseIntVal
        diag     <- parseOptionalDiagFcltuTrans
        void endContainer
        return FcltuTransferDataReturn
            { _fcltuTransRetCredentials     = creds
            , _fcltuTransRetInvokeID        = fromIntegral invokeID
            , _fcltuTransRetIdentification  = cltuID
            , _fcltuTransRetBufferAvailable = fromIntegral buffer
            , _fcltuTransRetResult          = diag
            }

instance EncodeASN1 FcltuTransferDataReturn where
    encode val = encodeASN1' DER (fcltuTransferDataReturn val)



data FcltuTransDiagnostic =
    FCLTUUnableToProcess
    | FCLTUUnableToStore
    | FCLTUOutOfSequence
    | FCLTUInconsistentTimeRange
    | FCLTUInvalidTime
    | FCLTULateSldu
    | FCLTUInvalidDelayTime
    | FCLTUCltuError
    deriving (Show, Read, Generic)

fcltuTransDiagnostic :: FcltuTransDiagnostic -> ASN1
fcltuTransDiagnostic FCLTUUnableToProcess       = IntVal 0
fcltuTransDiagnostic FCLTUUnableToStore         = IntVal 1
fcltuTransDiagnostic FCLTUOutOfSequence         = IntVal 2
fcltuTransDiagnostic FCLTUInconsistentTimeRange = IntVal 3
fcltuTransDiagnostic FCLTUInvalidTime           = IntVal 4
fcltuTransDiagnostic FCLTULateSldu              = IntVal 5
fcltuTransDiagnostic FCLTUInvalidDelayTime      = IntVal 6
fcltuTransDiagnostic FCLTUCltuError             = IntVal 7


fcltuTransDiagnosticFromInt :: ASN1 -> FcltuTransDiagnostic
fcltuTransDiagnosticFromInt (IntVal 0) = FCLTUUnableToProcess
fcltuTransDiagnosticFromInt (IntVal 1) = FCLTUUnableToStore
fcltuTransDiagnosticFromInt (IntVal 2) = FCLTUOutOfSequence
fcltuTransDiagnosticFromInt (IntVal 3) = FCLTUInconsistentTimeRange
fcltuTransDiagnosticFromInt (IntVal 4) = FCLTUInvalidTime
fcltuTransDiagnosticFromInt (IntVal 5) = FCLTULateSldu
fcltuTransDiagnosticFromInt (IntVal 6) = FCLTUInvalidDelayTime
fcltuTransDiagnosticFromInt (IntVal 7) = FCLTUCltuError
fcltuTransDiagnosticFromInt _          = FCLTUCltuError



data FcltuTransResult = FcltuTransCommon Diagnostics | FcltuTransSpecific FcltuTransDiagnostic
    deriving (Show, Generic)

diagnosticFcltuTrans :: FcltuTransResult -> ASN1
diagnosticFcltuTrans (FcltuTransCommon diag) =
    Other Context 0 (encodeASN1' DER [diagnostics diag])
diagnosticFcltuTrans (FcltuTransSpecific diag) =
    Other Context 1 (encodeASN1' DER [fcltuTransDiagnostic diag])


optionalDiagFcltuTrans :: Maybe FcltuTransResult -> ASN1
optionalDiagFcltuTrans Nothing = Other Context 0 B.empty
optionalDiagFcltuTrans (Just diag) =
    Other Context 1 (encodeASN1' DER [diagnosticFcltuTrans diag])

parseOptionalDiagFcltuTrans :: Parser (Maybe FcltuTransResult)
parseOptionalDiagFcltuTrans = do
    x <- get
    case x of
        ((Other Context 0 _) : rest) -> do
            put rest
            return Nothing
        ((Other Context 1 bs) : rest) -> do
            put rest
            case decodeASN1' DER bs of
                Left err ->
                    throwError
                        $ "Diagnostic FCLTU TRANSFER RETURN Parser: could not decode diagnostics: "
                        <> fromString (show err)
                Right v -> case parseASN1 parseDiagFcltuTrans v of
                    Left err ->
                        throwError
                            $ "Diagnostic FCLTU TRANSFER RETURN Parser: could not decode diagnostics: "
                            <> fromString (show err)
                    Right diag -> return (Just diag)
        (asn1 : _) -> do
            throwError
                $  "Diagnostic RAF Start Parser: unexpected ASN1 value: "
                <> fromString (show asn1)
        [] -> do
            throwError "Diagnostic RAF Start Parser: no value found."

parseDiagFcltuTrans :: Parser FcltuTransResult
parseDiagFcltuTrans = do
    x <- get
    case x of
        ((Other Context 0 bs) : rest) -> do
            put rest
            case getInteger bs of
                Left err ->
                    throwError
                        $ "Diagnostic FCLTU TRANSFER RETURN Parser: could not decode common diagnostic: "
                        <> fromString (show err)
                Right v -> return (FcltuTransCommon (diagnosticsFromInt v))
        ((Other Context 1 bs) : rest) -> do
            put rest
            case getInteger bs of
                Left err ->
                    throwError
                        $ "Diagnostic FCLTU TRANSFER RETURN Parser: could not decode specific diagnostic"
                        <> fromString (show err)
                Right v ->
                    return (FcltuTransSpecific (fcltuTransDiagnosticFromInt v))
        _ ->
            throwError
                "FCLTU TRANSFER RETURN diagnostics parser: could not parse diagnostics"


makeLenses ''FcltuTransferDataReturn


data CltuNotification =
    CltuRadiated
    | SlduExpired
    | ProductionInterrupted
    | ProductionHalted
    | ProductionOperational
    | BufferEmpty
    | ActionListCompleted !EventInvocationID
    | ActionListNotCompleted !EventInvocationID
    | EventConditionEvFalst !EventInvocationID
    deriving (Show, Generic)


cltuNotification :: CltuNotification -> ASN1
cltuNotification CltuRadiated          = Other Context 0 B.empty
cltuNotification SlduExpired           = Other Context 1 B.empty
cltuNotification ProductionInterrupted = Other Context 2 B.empty
cltuNotification ProductionHalted      = Other Context 3 B.empty
cltuNotification ProductionOperational = Other Context 4 B.empty
cltuNotification BufferEmpty           = Other Context 5 B.empty
cltuNotification (ActionListCompleted evID) =
    Other Context 6 (encodeASN1' DER [eventInvocationID evID])
cltuNotification (ActionListNotCompleted evID) =
    Other Context 7 (encodeASN1' DER [eventInvocationID evID])
cltuNotification (EventConditionEvFalst evID) =
    Other Context 8 (encodeASN1' DER [eventInvocationID evID])


parseCltuNotification :: Parser CltuNotification
parseCltuNotification = do
    x <- get
    case x of
        ((Other Context 0 _) : rest) -> do
            put rest
            return CltuRadiated
        ((Other Context 1 _) : rest) -> do
            put rest
            return SlduExpired
        ((Other Context 2 _) : rest) -> do
            put rest
            return ProductionInterrupted
        ((Other Context 3 _) : rest) -> do
            put rest
            return ProductionHalted
        ((Other Context 4 _) : rest) -> do
            put rest
            return ProductionOperational
        ((Other Context 5 _) : rest) -> do
            put rest
            return BufferEmpty
        ((Other Context 6 bs) : rest) -> do
            put rest
            case decodeASN1' DER bs of
                Left err ->
                    throwError $ "CltuNotification Parser error: " <> fromString
                        (show err)
                Right v -> case parseASN1 parseEventInvocationID v of
                    Left err ->
                        throwError
                            $  "CltuNotification Parser error: "
                            <> fromString (show err)
                    Right evID -> return (ActionListCompleted evID)
        ((Other Context 7 bs) : rest) -> do
            put rest
            case decodeASN1' DER bs of
                Left err ->
                    throwError $ "CltuNotification Parser error: " <> fromString
                        (show err)
                Right v -> case parseASN1 parseEventInvocationID v of
                    Left err ->
                        throwError
                            $  "CltuNotification Parser error: "
                            <> fromString (show err)
                    Right evID -> return (ActionListNotCompleted evID)
        ((Other Context 8 bs) : rest) -> do
            put rest
            case decodeASN1' DER bs of
                Left err ->
                    throwError $ "CltuNotification Parser error: " <> fromString
                        (show err)
                Right v -> case parseASN1 parseEventInvocationID v of
                    Left err ->
                        throwError
                            $  "CltuNotification Parser error: "
                            <> fromString (show err)
                    Right evID -> return (EventConditionEvFalst evID)
        asn1 ->
            throwError $ "CltuNotification: unexpected ASN1: " <> fromString
                (show asn1)

data CltuLastProcessed = NoCltuProcessed
    | CltuProcessed !CltuIdentification !ConditionalTime !ForwardDuStatus
    deriving(Show, Generic)

cltuLastProcessed :: CltuLastProcessed -> [ASN1]
cltuLastProcessed NoCltuProcessed = [Other Context 0 B.empty]
cltuLastProcessed (CltuProcessed cltuID radTime status) =
    [Start (Container Context 1), cltuIdentification cltuID]
        ++ conditionalTime radTime
        ++ [forwardDuStatus status, End (Container Context 1)]

parseCltuLastProcessed :: Parser CltuLastProcessed
parseCltuLastProcessed = do
    x <- get
    case x of
        ((Other Context 0 _) : rest) -> do
            put rest
            return NoCltuProcessed
        ((Start (Container Context 1)) : rest) -> do
            put rest
            cltuID  <- parseCltuIdentification
            radTime <- parseConditionalTime
            status  <- parseForwardDuStatus
            void endContainer
            return (CltuProcessed cltuID radTime status)
        asn1 -> do
            throwError
                $  "Could not parse CltuLastProcessed: unexpeted ASN1 values: "
                <> fromString (show asn1)
  where
    endContainer = parseBasicASN1 (== End (Container Context 1)) (const ())


data CltuLastOk = NoCltuOk | CltuOk !CltuIdentification !Time
    deriving (Show, Generic)

cltuLastOk :: CltuLastOk -> [ASN1]
cltuLastOk NoCltuOk = [Other Context 0 B.empty]
cltuLastOk (CltuOk cltuID stopRadiation) =
    [ Start (Container Context 1)
    , cltuIdentification cltuID
    , time stopRadiation
    , End (Container Context 1)
    ]

parseCltuLastOk :: Parser CltuLastOk
parseCltuLastOk = do
    x <- get
    case x of
        ((Other Context 0 _) : rest) -> do
            put rest
            return NoCltuOk
        ((Start (Container Context 1)) : rest) -> do
            put rest
            cltuID      <- parseCltuIdentification
            stopRadTime <- parseTime
            void endContainer
            return (CltuOk cltuID stopRadTime)
        asn1 -> do
            throwError
                $  "Could not parse CltuLastProcessed: unexpeted ASN1 values: "
                <> fromString (show asn1)
  where
    endContainer = parseBasicASN1 (== End (Container Context 1)) (const ())


data ProductionStatus =
    ProdOperational
    | ProdConfigured
    | ProdInterrupted
    | ProdHalted
    deriving (Show, Generic)

productionStatus :: ProductionStatus -> ASN1
productionStatus ProdOperational = IntVal 0
productionStatus ProdConfigured  = IntVal 1
productionStatus ProdInterrupted = IntVal 2
productionStatus ProdHalted      = IntVal 3


parseProductionStatus :: Parser ProductionStatus
parseProductionStatus = do
    x <- parseIntVal
    case x of
        0 -> return ProdOperational
        1 -> return ProdConfigured
        2 -> return ProdInterrupted
        3 -> return ProdHalted
        _ -> return ProdHalted

data UplinkStatus =
    UplinkStatusNotAvailable
    | UplinkNoRFAvailable
    | UplinkNoBitLock
    | UplinkNominal
    deriving (Show, Generic)

uplinkStatus :: UplinkStatus -> ASN1
uplinkStatus UplinkStatusNotAvailable = IntVal 0
uplinkStatus UplinkNoRFAvailable      = IntVal 1
uplinkStatus UplinkNoBitLock          = IntVal 2
uplinkStatus UplinkNominal            = IntVal 3

parseUplinkStatus :: Parser UplinkStatus
parseUplinkStatus = do
    x <- parseIntVal
    case x of
        0 -> return UplinkStatusNotAvailable
        1 -> return UplinkNoRFAvailable
        2 -> return UplinkNoBitLock
        3 -> return UplinkNominal
        _ -> return UplinkNominal


data FcltuAsyncNotify = FcltuAsyncNotify
    { _fcltuAsyncCredentials      :: !Credentials
    , _fcltuAsyncNotification     :: !CltuNotification
    , _fcltuAsyncLastProcessed    :: !CltuLastProcessed
    , _fcltuAsyncLastOK           :: !CltuLastOk
    , _fcltuAsyncProductionStatus :: !ProductionStatus
    , _fcltuAsyncUplinkStatus     :: !UplinkStatus
    }
    deriving (Show, Generic)

fcltuAsyncStatus :: FcltuAsyncNotify -> [ASN1]
fcltuAsyncStatus FcltuAsyncNotify {..} =
    [ Start (Container Context 12)
        , credentials _fcltuAsyncCredentials
        , cltuNotification _fcltuAsyncNotification
        ]
        ++ cltuLastProcessed _fcltuAsyncLastProcessed
        ++ cltuLastOk _fcltuAsyncLastOK
        ++ [ productionStatus _fcltuAsyncProductionStatus
           , uplinkStatus _fcltuAsyncUplinkStatus
           , End (Container Context 12)
           ]

parseFcltuAsyncStatus :: Parser FcltuAsyncNotify
parseFcltuAsyncStatus = content
  where
    endContainer = parseBasicASN1 (== End (Container Context 12)) (const ())

    content      = do
        creds         <- parseCredentials
        notif         <- parseCltuNotification
        lastProcessed <- parseCltuLastProcessed
        lastOK        <- parseCltuLastOk
        prodStat      <- parseProductionStatus
        uplink        <- parseUplinkStatus
        void endContainer
        return FcltuAsyncNotify { _fcltuAsyncCredentials      = creds
                                , _fcltuAsyncNotification     = notif
                                , _fcltuAsyncLastProcessed    = lastProcessed
                                , _fcltuAsyncLastOK           = lastOK
                                , _fcltuAsyncProductionStatus = prodStat
                                , _fcltuAsyncUplinkStatus     = uplink
                                }


instance EncodeASN1 FcltuAsyncNotify where
    encode val = encodeASN1' DER (fcltuAsyncStatus val)

makeLenses ''FcltuAsyncNotify


data CltuStatusReport = CltuStatusReport
    { _fcltuStatusCredentials      :: !Credentials
    , _fcltuStatusLastProcessed    :: !CltuLastProcessed
    , _fcltuStatusLastOk           :: !CltuLastOk
    , _fcltuStatusProductionStatus :: !ProductionStatus
    , _fcltuStatusUplinkStatus     :: !UplinkStatus
    , _fcltuStatusNumReceived      :: !Word64
    , _fcltuStatusNumProcessed     :: !Word64
    , _fcltuStatusNumRadiated      :: !Word64
    , _fcltuStatusBufferAvailable  :: !Word64
    }
    deriving (Show, Generic)
makeLenses ''CltuStatusReport

cltuStatusReport :: CltuStatusReport -> [ASN1]
cltuStatusReport CltuStatusReport {..} =
    [Start (Container Context 13), credentials _fcltuStatusCredentials]
        ++ cltuLastProcessed _fcltuStatusLastProcessed
        ++ cltuLastOk _fcltuStatusLastOk
        ++ [ productionStatus _fcltuStatusProductionStatus
           , uplinkStatus _fcltuStatusUplinkStatus
           , IntVal (fromIntegral _fcltuStatusNumReceived)
           , IntVal (fromIntegral _fcltuStatusNumProcessed)
           , IntVal (fromIntegral _fcltuStatusNumRadiated)
           , IntVal (fromIntegral _fcltuStatusBufferAvailable)
           , End (Container Context 13)
           ]


instance EncodeASN1 CltuStatusReport where
    encode val = encodeASN1' DER (cltuStatusReport val)

parseCltuStatusReport :: Parser CltuStatusReport
parseCltuStatusReport = content
  where
    endContainer = parseBasicASN1 (== End (Container Context 13)) (const ())

    content      = do
        creds         <- parseCredentials
        lastProcessed <- parseCltuLastProcessed
        lastOk        <- parseCltuLastOk
        prodStatus    <- parseProductionStatus
        uplStatus     <- parseUplinkStatus
        numReceived   <- parseIntVal
        numProcessed  <- parseIntVal
        numRadiated   <- parseIntVal
        bufAvailable  <- parseIntVal
        void endContainer
        return CltuStatusReport
            { _fcltuStatusCredentials      = creds
            , _fcltuStatusLastProcessed    = lastProcessed
            , _fcltuStatusLastOk           = lastOk
            , _fcltuStatusProductionStatus = prodStatus
            , _fcltuStatusUplinkStatus     = uplStatus
            , _fcltuStatusNumReceived      = fromIntegral numReceived
            , _fcltuStatusNumProcessed     = fromIntegral numProcessed
            , _fcltuStatusNumRadiated      = fromIntegral numRadiated
            , _fcltuStatusBufferAvailable  = fromIntegral bufAvailable
            }


data VChannel = MasterChannel | VirtualChannel !Word8
    deriving (Show, Generic)

vChannel :: VChannel -> ASN1
vChannel MasterChannel       = Other Context 0 B.empty
vChannel (VirtualChannel vc) = Other Context 1 (B.singleton vc)


data GvcID = GvcID
    { _gvcidSCID    :: !Word16
    , _gvcidVersion :: !Word8
    , _gvcidVCID    :: !VChannel
    }
    deriving (Show, Generic)

gvcid :: GvcID -> [ASN1]
gvcid GvcID {..} =
    [ IntVal (fromIntegral _gvcidSCID)
    , IntVal (fromIntegral _gvcidVersion)
    , vChannel _gvcidVCID
    ]

parseGvcid :: Parser GvcID
parseGvcid = do
    scid    <- parseIntVal
    version <- parseIntVal
    chan    <- choiceParser (\_ -> return MasterChannel)
                            (\bs -> return (VirtualChannel (bs `B.index` 0)))
                            "Error parsing FCLTU GVCID"
    return GvcID { _gvcidSCID    = fromIntegral scid
                 , _gvcidVersion = fromIntegral version
                 , _gvcidVCID    = chan
                 }



data ClcwGvcID = Configured GvcID | NotConfigured
    deriving(Show, Generic)

clcwGvcID :: ClcwGvcID -> [ASN1]
clcwGvcID (Configured vcid) = Start Sequence : gvcid vcid ++ [End Sequence]
clcwGvcID NotConfigured     = [Other Context 1 ""]

parseClcwGvcID :: Parser ClcwGvcID
parseClcwGvcID = do
    x <- get
    case x of
        Other Context 1 _ : rest -> do
            put rest
            return NotConfigured
        Start Sequence : rest -> do
            put rest
            vcid <- parseGvcid
            parseEndSequence
            return (Configured vcid)
        asn1 -> do
            throwError
                $  "Error parsing CLCW Global GVCID, unexpected ASN1: "
                <> fromString (show asn1)



data FcltuGetParameter =
    FcltuAcquisitionSequenceLength !Word16
    | FcltuPlop1IdleSequenceLength !Word16
    | FcltuBitLockRequired !Bool
    | FcltuRFAvailableRequired !Bool
    | FcltuClcwGlobalVcID !ClcwGvcID
    deriving (Show, Generic)

fcltuGetParameter :: FcltuGetParameter -> [ASN1]
fcltuGetParameter (FcltuAcquisitionSequenceLength size) =
    [ Start (Container Context 12)
    , parameterName ParAcquisitionSequenceLength
    , IntVal (fromIntegral size)
    , End (Container Context 12)
    ]
fcltuGetParameter (FcltuPlop1IdleSequenceLength size) =
    [ Start (Container Context 17)
    , parameterName ParPlop1IdleSequenceLength
    , IntVal (fromIntegral size)
    , End (Container Context 17)
    ]
fcltuGetParameter (FcltuBitLockRequired yes) =
    [ Start (Container Context 0)
    , parameterName ParBitLockRequired
    , if yes then IntVal 0 else IntVal 1
    , End (Container Context 0)
    ]
fcltuGetParameter (FcltuRFAvailableRequired yes) =
    [ Start (Container Context 11)
    , parameterName ParRfAvailableRequired
    , if yes then IntVal 0 else IntVal 1
    , End (Container Context 11)
    ]
fcltuGetParameter (FcltuClcwGlobalVcID vcid) =
    [Start (Container Context 13), parameterName ParClcwGlobalVCID]
        ++ clcwGvcID vcid
        ++ [End (Container Context 13)]


parseFcltuGetParameter :: Parser FcltuGetParameter
parseFcltuGetParameter = do
    x <- get
    case x of
        Start (Container Context 12) : rest -> do
            put rest
            void $ parseParameterName
            val <- parseIntVal
            parseEndContainer 12
            return (FcltuAcquisitionSequenceLength (fromIntegral val))
        Start (Container Context 17) : rest -> do
            put rest
            void $ parseParameterName
            val <- parseIntVal
            parseEndContainer 17
            return (FcltuPlop1IdleSequenceLength (fromIntegral val))
        Start (Container Context 0) : rest -> do
            put rest
            void $ parseParameterName
            val <- parseIntVal
            parseEndContainer 0
            let yes = if val == 0 then True else False
            return (FcltuBitLockRequired yes)
        Start (Container Context 11) : rest -> do
            put rest
            void $ parseParameterName
            val <- parseIntVal
            parseEndContainer 11
            let yes = if val == 0 then True else False
            return (FcltuRFAvailableRequired yes)
        Start (Container Context 13) : rest -> do
            put rest
            void $ parseParameterName
            val <- parseClcwGvcID
            parseEndContainer 13
            return (FcltuClcwGlobalVcID val)
        asn1 -> do
            put asn1
            throwError
                $ "Error parsing returned parameter value: unexpected ASN1 value: "
                <> fromString (show asn1)

data FcltuDiagFcltuGetSpecific = FcltuUnknownParameter | FcltuDiagInvalid
    deriving(Eq, Ord, Enum, Show, Generic)

fcltuDiagFcltuGetSpecific :: FcltuDiagFcltuGetSpecific -> ASN1
fcltuDiagFcltuGetSpecific FcltuUnknownParameter = IntVal 0
fcltuDiagFcltuGetSpecific FcltuDiagInvalid      = IntVal 0

parseFcltuDiagFcltuGetSpecific :: Parser FcltuDiagFcltuGetSpecific
parseFcltuDiagFcltuGetSpecific = do
    x <- parseIntVal
    case x of
        0 -> return FcltuUnknownParameter
        _ -> return FcltuDiagInvalid



data DiagnosticFcltuGet = DiagFcltuGetCommon Diagnostics | DiagFcltuGetSpecific FcltuDiagFcltuGetSpecific
    deriving(Show, Generic)

diagnosticFcltuGet :: DiagnosticFcltuGet -> ASN1
diagnosticFcltuGet (DiagFcltuGetCommon diag) =
    Other Context 0 (encodeASN1' DER [diagnostics diag])
diagnosticFcltuGet (DiagFcltuGetSpecific diag) =
    Other Context 1 (encodeASN1' DER [fcltuDiagFcltuGetSpecific diag])


parseDiagnosticFcltuGet :: Parser DiagnosticFcltuGet
parseDiagnosticFcltuGet = do
    res <- parseEitherASN1 parseDiagnostics parseFcltuDiagFcltuGetSpecific
    case res of
        Left  diag -> return $ DiagFcltuGetCommon diag
        Right diag -> return $ DiagFcltuGetSpecific diag

eitherFcltuGetResult :: Either DiagnosticFcltuGet FcltuGetParameter -> [ASN1]
eitherFcltuGetResult (Left diag) =
    [ Start (Container Context 1)
    , diagnosticFcltuGet diag
    , End (Container Context 1)
    ]
eitherFcltuGetResult (Right param) =
    Start (Container Context 0)
        :  fcltuGetParameter param
        ++ [End (Container Context 0)]


parseEitherFcltuGetResult
    :: Parser (Either DiagnosticFcltuGet FcltuGetParameter)
parseEitherFcltuGetResult = do
    x <- get
    case x of
        Start (Container Context 0) : rest -> do
            put rest
            param <- parseFcltuGetParameter
            parseEndContainer 0
            return (Right param)
        Start (Container Context 1) : rest -> do
            put rest
            diag <- parseDiagnosticFcltuGet
            parseEndContainer 1
            return (Left diag)
        asn1 ->
            throwError
                $ "Error parsing returned parameter value: unexpected ASN1 value: "
                <> fromString (show asn1)


data FcltuGetParameterReturn = FcltuGetParameterReturn
    { _fgpCredentials :: !Credentials
    , _fgpInvokeID    :: !Word16
    , _fgpResult      :: !(Either DiagnosticFcltuGet FcltuGetParameter)
    }
    deriving (Show, Generic)
makeLenses ''FcltuGetParameterReturn

fcltuGetParameterReturn :: FcltuGetParameterReturn -> [ASN1]
fcltuGetParameterReturn FcltuGetParameterReturn {..} =
    [ Start (Container Context 7)
        , credentials _fgpCredentials
        , IntVal (fromIntegral _fgpInvokeID)
        ]
        ++ eitherFcltuGetResult _fgpResult
        ++ [End (Container Context 7)]

instance EncodeASN1 FcltuGetParameterReturn where
    encode val = encodeASN1' DER (fcltuGetParameterReturn val)

parseFcltuGetParameterReturn :: Parser FcltuGetParameterReturn
parseFcltuGetParameterReturn = content
  where
    endContainer = parseBasicASN1 (== End (Container Context 7)) (const ())

    content      = do
        creds    <- parseCredentials
        invokeID <- parseIntVal
        result   <- parseEitherFcltuGetResult
        void endContainer
        return FcltuGetParameterReturn { _fgpCredentials = creds
                                       , _fgpInvokeID    = fromIntegral invokeID
                                       , _fgpResult      = result
                                       }
