module Data.SLE.CCSDSTime
  ( CCSDSTime(..)
  , CCSDSTimePico(..)
  , ccsdsNullTime
  , ccsdsPicoNullTime
  , ccsdsTimeBuilder
  , ccsdsTimePicoBuilder
  , ccsdsTimeParser
  , ccsdsTimePicoParser
  , Data.SLE.CCSDSTime.getCurrentTime
  )
where


import           RIO
import qualified ByteString.StrictBuilder      as B
import           Data.Attoparsec.ByteString     ( Parser )
--import qualified Data.Attoparsec.ByteString    as A
import qualified Data.Attoparsec.Binary        as A

import           Data.Thyme.Clock
import           Data.Thyme.Clock.POSIX

data CCSDSTime = CCSDSTime !Word16 !Word32 !Word16
  deriving (Eq, Show, Generic)

ccsdsNullTime :: CCSDSTime 
ccsdsNullTime = CCSDSTime 0 0 0


data CCSDSTimePico = CCSDSTimePico !Word16 !Word32 !Word32
  deriving (Eq, Show, Generic)

ccsdsPicoNullTime :: CCSDSTimePico 
ccsdsPicoNullTime = CCSDSTimePico 0 0 0


getCurrentTime :: IO CCSDSTime 
getCurrentTime = do 
  t <- Data.Thyme.Clock.getCurrentTime
  let micro = t ^. posixTime . microseconds
      secs = micro `div` 1_000_000
      (days, sec) = secs `quotRem` 86400
      msec = (micro `rem` 1_000_000) `div` 1000
  return $ CCSDSTime (fromIntegral days) (fromIntegral sec) (fromIntegral msec)



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
