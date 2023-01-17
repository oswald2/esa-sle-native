module SLE.Data.CCSDSTime
    ( CCSDSTime(..)
    , CCSDSTimePico(..)
    , ccsdsNullTime
    , ccsdsPicoNullTime
    , ccsdsTimeBuilder
    , ccsdsTimePicoBuilder
    , ccsdsTimeParser
    , ccsdsTimePicoParser
    , toPicoTime
    , SLE.Data.CCSDSTime.getCurrentTime
    , getCurrentTimePico
    , toUTCTime
    , picoToUTCTime
    ) where


import qualified ByteString.StrictBuilder      as B
--import qualified Data.Attoparsec.ByteString    as A
import qualified Data.Attoparsec.Binary        as A
import           Data.Attoparsec.ByteString     ( Parser )
import           RIO

import           Data.Time
import           Data.Time.Calendar.OrdinalDate
import           Data.Time.Clock.POSIX

import           Text.Builder                  as TB



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


toPicoTime :: CCSDSTime -> CCSDSTimePico
toPicoTime (CCSDSTime d s ss) = CCSDSTimePico d s (fromIntegral ss * 1_000_000)

instance Ord CCSDSTimePico where
    compare (CCSDSTimePico d1 s1 ss1) (CCSDSTimePico d2 s2 ss2) =
        case compare d1 d2 of
            EQ -> case compare s1 s2 of
                EQ -> compare ss1 ss2
                x  -> x
            x -> x

getCurrentTime :: (MonadIO m) => m CCSDSTime
getCurrentTime = do
    t <- liftIO getPOSIXTime
    let micro :: Int64
        micro     = floor (1e-6 * epochTime)
        epochTime = nominalDiffTimeToSeconds t + (-378691200)
        secs :: Integer
        secs        = floor epochTime
        (days, sec) = secs `quotRem` 86400
        msec        = fromIntegral $ (micro `rem` 1_000_000) `div` 1000
    return $ CCSDSTime (fromIntegral days) (fromIntegral sec) msec

getCurrentTimePico :: (MonadIO m) => m CCSDSTimePico
getCurrentTimePico = do
    t <- liftIO getPOSIXTime
    let pico :: Integer
        pico      = floor (1e-12 * epochTime)
        epochTime = nominalDiffTimeToSeconds t + (-378691200)
        secs :: Integer
        secs        = floor epochTime
        (days, sec) = secs `quotRem` 86400
        psec        = fromIntegral (pico `rem` 1_000_000_000_000)
    return $ CCSDSTimePico (fromIntegral days) (fromIntegral sec) psec


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

toUTCTime :: CCSDSTime -> UTCTime
toUTCTime (CCSDSTime days milli micro) =
    let epochDay1958 = ModifiedJulianDay 36204
        day          = addDays (fromIntegral days) epochDay1958
        pico = (fromIntegral milli * 1000 + fromIntegral micro) * 1_000_000
        dtime        = picosecondsToDiffTime pico
    in  UTCTime day dtime

picoToUTCTime :: CCSDSTimePico -> UTCTime
picoToUTCTime (CCSDSTimePico days milli pico) =
    let epochDay1958 = ModifiedJulianDay 36204
        day          = addDays (fromIntegral days) epochDay1958
        pico'        = (fromIntegral milli * 1_000_000_000) + fromIntegral pico
        dtime        = picosecondsToDiffTime pico'
    in  UTCTime day dtime


utcTimeToComponents :: UTCTime -> (Integer, Int, Int, Int, Int, Int, Int, Int)
utcTimeToComponents utcTime =
    let
        LocalTime day (TimeOfDay hh mm ss) = utcToLocalTime utc utcTime
        (yr, mn, dom) = toGregorian day
        (_, doy)      = toOrdinalDate day
        sec           = truncate ss
        pico =
            diffTimeToPicoseconds (utctDayTime utcTime) `rem` 1_000_000_000_000
        micro = pico `div` 1_000_000
    in
        (yr, doy, mn, dom, hh, mm, sec, fromIntegral micro)

displayUTCTimeMicro :: UTCTime -> Text
displayUTCTimeMicro utcTime =
    let (yr, doy, _mn, _dom, hh, mm, ss, micro) = utcTimeToComponents utcTime
    in  TB.run
            $  TB.padFromLeft 4 '0' (TB.decimal yr)
            <> TB.char '.'
            <> TB.padFromLeft 3 '0' (TB.decimal doy)
            <> TB.char '.'
            <> TB.padFromLeft 2 '0' (TB.decimal hh)
            <> TB.char '.'
            <> TB.padFromLeft 2 '0' (TB.decimal mm)
            <> TB.char '.'
            <> TB.padFromLeft 2 '0' (TB.decimal ss)
            <> TB.char '.'
            <> TB.padFromLeft 3 '0' (TB.decimal micro)

instance Display CCSDSTime where
    textDisplay t = displayUTCTimeMicro (toUTCTime t)

instance Display CCSDSTimePico where
    textDisplay t = displayUTCTimeMicro (picoToUTCTime t)
