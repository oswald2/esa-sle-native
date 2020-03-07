module Data.SLE.CCSDSTime
  ( CCSDSTime(..)
  , CCSDSTimePico(..)
  , ccsdsNullTime
  , ccsdsPicoNullTime
  , ccsdsTimeBuilder
  , ccsdsTimePicoBuilder
  , ccsdsTimeParser
  , ccsdsTimePicoParser
  )
where


import           RIO
import qualified ByteString.StrictBuilder      as B
import           Data.Attoparsec.ByteString     ( Parser )
--import qualified Data.Attoparsec.ByteString    as A
import qualified Data.Attoparsec.Binary        as A


data CCSDSTime = CCSDSTime Word16 Word32 Word16
  deriving (Eq, Show, Generic)

ccsdsNullTime :: CCSDSTime 
ccsdsNullTime = CCSDSTime 0 0 0


data CCSDSTimePico = CCSDSTimePico Word16 Word32 Word32
  deriving (Eq, Show, Generic)

ccsdsPicoNullTime :: CCSDSTimePico 
ccsdsPicoNullTime = CCSDSTimePico 0 0 0


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
