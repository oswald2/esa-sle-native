{-# LANGUAGE TemplateHaskell #-}
module SLE.Data.RafOps
    ( FrameQuality(..)
    , RafStartInvocation(..)
    , rafStartCredentials
    , rafStartInvokeID
    , rafStartTime
    , rafStopTime
    , rafStartRequestedTimeQual
    ) where

import           RIO

import           Control.Lens                   ( makeLenses )

import           Data.ASN1.BinaryEncoding
import           Data.ASN1.Encoding
import           Data.ASN1.Prim                 ( getInteger )
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
    [ Start (Container Context 104)
    , credentials _rafStartCredentials
    , IntVal (fromIntegral _rafStartInvokeID)
    , conditionalTime _rafStartTime 
    , conditionalTime _rafStopTime 
    , End (Container Context 104)
    ]
