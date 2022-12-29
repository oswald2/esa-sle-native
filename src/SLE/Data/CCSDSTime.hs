module SLE.Data.CCSDSTime
    ( CCSDSTime(..)
    , CCSDSTimePico(..)
    , ccsdsNullTime
    , ccsdsPicoNullTime
    , ccsdsTimeBuilder
    , ccsdsTimePicoBuilder
    , ccsdsTimeParser
    , ccsdsTimePicoParser
    , SLE.Data.CCSDSTime.getCurrentTime
    ) where


import qualified ByteString.StrictBuilder      as B
--import qualified Data.Attoparsec.ByteString    as A
import qualified Data.Attoparsec.Binary        as A
import           Data.Attoparsec.ByteString     ( Parser )
import           RIO

import           Data.Time.Clock
import           Data.Time.Clock.POSIX



data CCSDSTime = CCSDSTime !Word16 !Word32 !Word16
    deriving stock (Eq, Show, Generic)
    deriving anyclass NFData

ccsdsNullTime :: CCSDSTime
ccsdsNullTime = CCSDSTime 0 0 0

instance Ord CCSDSTime where
    compare (CCSDSTime d1 s1 ss1) (CCSDSTime d2 s2 ss2) = case compare d1 d2 of
        EQ -> case compare s1 s2 of
            EQ -> compare ss1 ss2
            x  -> x
        x -> x

data CCSDSTimePico = CCSDSTimePico !Word16 !Word32 !Word32
    deriving stock (Eq, Show, Generic)
    deriving anyclass NFData

ccsdsPicoNullTime :: CCSDSTimePico
ccsdsPicoNullTime = CCSDSTimePico 0 0 0

instance Ord CCSDSTimePico where
    compare (CCSDSTimePico d1 s1 ss1) (CCSDSTimePico d2 s2 ss2) =
        case compare d1 d2 of
            EQ -> case compare s1 s2 of
                EQ -> compare ss1 ss2
                x  -> x
            x -> x

getCurrentTime :: IO CCSDSTime
getCurrentTime = do
    t <- getPOSIXTime
    let micro :: Int64
        micro       = floor (1e-6 * nominalDiffTimeToSeconds t)
        secs        = micro `div` 1_000_000
        (days, sec) = secs `quotRem` 86400
        msec        = fromIntegral $ (micro `rem` 1_000_000) `div` 1000
    return $ CCSDSTime (fromIntegral days) (fromIntegral sec) msec



ccsdsTimeBuilder :: CCSDSTime -> B.Builder
ccsdsTimeBuilder (CCSDSTime days secs milli) =
    B.word16BE days <> B.word32BE secs <> B.word16BE milli

ccsdsTimeParser :: Parser CCSDSTime
ccsdsTimeParser = do
    CCSDSTime <$> A.anyWord16be <*> A.anyWord32be <*> A.anyWord16be

ccsdsTimePicoBuilder :: CCSDSTimePico -> B.Builder
ccsdsTimePicoBuilder (CCSDSTimePico days secs pico) =
    B.word16BE days <> B.word32BE secs <> B.word32BE pico


ccsdsTimePicoParser :: Parser CCSDSTimePico
ccsdsTimePicoParser = do
    CCSDSTimePico <$> A.anyWord16be <*> A.anyWord32be <*> A.anyWord32be


