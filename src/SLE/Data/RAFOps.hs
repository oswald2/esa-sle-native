{-# LANGUAGE TemplateHaskell #-}
module SLE.Data.RAFOps
    ( FrameQuality(..)
    , RafStartInvocation(..)
    , RafStartReturn(..)
    , rafStartCredentials
    , rafStartInvokeID
    , rafStartTime
    , rafStopTime
    , rafStartRequestedTimeQual
    , parseRafStart
    , parseRafStartReturn
    , rafStartRetCredentials
    , rafStartRetInvokeID
    , rafStartRetResult
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



data FrameQuality = GoodOnly | ErredOnly | AllFrames | FrameQualInvalid
    deriving (Eq, Ord, Enum, Show, Generic)

frameQuality :: FrameQuality -> ASN1
frameQuality GoodOnly         = IntVal 0
frameQuality ErredOnly        = IntVal 1
frameQuality AllFrames        = IntVal 2
frameQuality FrameQualInvalid = IntVal 127


parseFrameQuality :: Parser FrameQuality
parseFrameQuality = do
    v <- parseIntVal
    case v of
        0 -> return GoodOnly
        1 -> return ErredOnly
        2 -> return AllFrames
        _ -> return FrameQualInvalid


data RafStartInvocation = RafStartInvocation
    { _rafStartCredentials       :: !Credentials
    , _rafStartInvokeID          :: !Word16
    , _rafStartTime              :: !ConditionalTime
    , _rafStopTime               :: !ConditionalTime
    , _rafStartRequestedTimeQual :: !FrameQuality
    }
    deriving (Show, Generic)
makeLenses ''RafStartInvocation



rafStartInvocation :: RafStartInvocation -> [ASN1]
rafStartInvocation RafStartInvocation {..} =
    [ Start (Container Context 0)
    , credentials _rafStartCredentials
    , IntVal (fromIntegral _rafStartInvokeID)
    , conditionalTime _rafStartTime
    , conditionalTime _rafStopTime
    , frameQuality _rafStartRequestedTimeQual
    , End (Container Context 0)
    ]

parseRafStart :: Parser RafStartInvocation
parseRafStart = content
  where
    endContainer = parseBasicASN1 (== End (Container Context 0)) (const ())

    content      = do
        creds    <- parseCredentials
        invokeID <- parseIntVal
        start    <- parseConditionalTime
        stop     <- parseConditionalTime
        qual     <- parseFrameQuality
        void endContainer
        return RafStartInvocation { _rafStartCredentials       = creds
                                  , _rafStartInvokeID = fromIntegral invokeID
                                  , _rafStartTime              = start
                                  , _rafStopTime               = stop
                                  , _rafStartRequestedTimeQual = qual
                                  }

instance EncodeASN1 RafStartInvocation where
    encode val = encodeASN1' DER (rafStartInvocation val)



data RafStartSpecific = RafStartOutOfService
        | RafStartUnableToComply
        | RafStartInvalidStartTime
        | RafStartInvalidStopTime
        | RafStartMissingTimeValue
        | RafStartInvalid
    deriving(Eq, Ord, Enum, Show, Generic)

rafStartSpecific :: RafStartSpecific -> ASN1
rafStartSpecific RafStartOutOfService     = IntVal 0
rafStartSpecific RafStartUnableToComply   = IntVal 1
rafStartSpecific RafStartInvalidStartTime = IntVal 2
rafStartSpecific RafStartInvalidStopTime  = IntVal 3
rafStartSpecific RafStartMissingTimeValue = IntVal 4
rafStartSpecific RafStartInvalid          = IntVal 127

-- rafStartSpecificParser :: Parser RafStartSpecific
-- rafStartSpecificParser = do
--     v <- parseIntVal
--     case v of
--         0 -> return RafStartOutOfService
--         1 -> return RafStartUnableToComply
--         2 -> return RafStartInvalidStartTime
--         3 -> return RafStartInvalidStopTime
--         4 -> return RafStartMissingTimeValue
--         _ -> return RafStartInvalid

rafStartSpecificFromInt :: ASN1 -> RafStartSpecific
rafStartSpecificFromInt (IntVal 0) = RafStartOutOfService
rafStartSpecificFromInt (IntVal 1) = RafStartUnableToComply
rafStartSpecificFromInt (IntVal 2) = RafStartInvalidStartTime
rafStartSpecificFromInt (IntVal 3) = RafStartInvalidStopTime
rafStartSpecificFromInt (IntVal 4) = RafStartMissingTimeValue
rafStartSpecificFromInt _          = RafStartInvalid


data DiagnosticRafStart = DiagRafStartCommon Diagnostics | DiagRafStartSpecific RafStartSpecific
    deriving(Show, Generic)

diagnosticRafStart :: DiagnosticRafStart -> ASN1
diagnosticRafStart (DiagRafStartCommon diag) =
    Other Context 0 (encodeASN1' DER [diagnostics diag])
diagnosticRafStart (DiagRafStartSpecific diag) =
    Other Context 1 (encodeASN1' DER [rafStartSpecific diag])


optionalDiagRafStart :: Maybe DiagnosticRafStart -> ASN1
optionalDiagRafStart Nothing = Other Context 0 B.empty
optionalDiagRafStart (Just diag) =
    Other Context 0 (encodeASN1' DER [diagnosticRafStart diag])

parseOptionalRafStartDiag :: Parser (Maybe DiagnosticRafStart)
parseOptionalRafStartDiag = do
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
                        $ "Diagnostic RAF Start Parser: could not decode diagnostics: "
                        <> fromString (show err)
                Right v -> case parseASN1 parseRafStartDiag v of
                    Left err ->
                        throwError
                            $ "Diagnostic RAF Start Parser: could not decode diagnostics: "
                            <> fromString (show err)
                    Right diag -> return (Just diag)
        (asn1 : _) -> do
            throwError
                $  "Diagnostic RAF Start Parser: unexpected ASN1 value: "
                <> fromString (show asn1)
        [] -> do
            throwError "Diagnostic RAF Start Parser: no value found."

parseRafStartDiag :: Parser DiagnosticRafStart
parseRafStartDiag = do
    x <- get
    case x of
        ((Other Context 0 bs) : rest) -> do
            put rest
            case getInteger bs of
                Left err ->
                    throwError
                        $ "Diagnostic RAF Start Parser: could not decode common diagnostic: "
                        <> fromString (show err)
                Right v -> return (DiagRafStartCommon (diagnosticsFromInt v))
        ((Other Context 1 bs) : rest) -> do
            put rest
            case getInteger bs of
                Left err ->
                    throwError
                        $ "Diagnostic RAF Start Parser: could not decode specific diagnostic"
                        <> fromString (show err)
                Right v ->
                    return (DiagRafStartSpecific (rafStartSpecificFromInt v))
        _ -> throwError
            "RAF Start diagnostics parser: could not parse diagnostics"


data RafStartReturn = RafStartReturn
    { _rafStartRetCredentials :: !Credentials
    , _rafStartRetInvokeID    :: !Word16
    , _rafStartRetResult      :: !(Maybe DiagnosticRafStart)
    }
    deriving (Show, Generic)
makeLenses ''RafStartReturn

rafStartReturn :: RafStartReturn -> [ASN1]
rafStartReturn RafStartReturn {..} =
    [ Start (Container Context 1)
    , credentials _rafStartRetCredentials
    , IntVal (fromIntegral _rafStartRetInvokeID)
    , optionalDiagRafStart _rafStartRetResult
    , End (Container Context 1)
    ]


parseRafStartReturn :: Parser RafStartReturn
parseRafStartReturn = content
  where
    endContainer = parseBasicASN1 (== End (Container Context 1)) (const ())

    content      = do
        creds    <- parseCredentials
        invokeID <- parseIntVal
        diag     <- parseOptionalRafStartDiag
        void endContainer
        return RafStartReturn { _rafStartRetCredentials = creds
                              , _rafStartRetInvokeID    = fromIntegral invokeID
                              , _rafStartRetResult      = diag
                              }

instance EncodeASN1 RafStartReturn where
    encode val = encodeASN1' DER (rafStartReturn val)
