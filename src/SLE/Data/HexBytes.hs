{-# LANGUAGE TemplateHaskell 
#-}
module SLE.Data.HexBytes
    ( HexBytes
    , bsToHex
    , hexToBS
    , hexBytesBuilder
    , parseHexLineQuoted
    , hlength
    ) where

import           RIO
import qualified RIO.ByteString                as B
import qualified RIO.Text                      as T


import           Data.Aeson
import qualified Data.Aeson.Encoding           as E
import qualified Data.Aeson.Types              as E
import           Data.Bits                      ( Bits((.|.), shiftL) )
import           Data.Char                      ( digitToInt
                                                , isHexDigit
                                                )

import           Text.Builder                  as TB
                                                ( Builder
                                                , hexadecimal
                                                , padFromLeft
                                                , run
                                                )


import qualified Data.Attoparsec.Text          as A
import qualified Text.ParserCombinators.ReadP  as P
import           Text.ParserCombinators.ReadPrec
                                                ( lift )

import           Text.Read
import           Text.Show




newtype HexBytes = HexBytes { unHexBytes :: ByteString }
    deriving (Eq, Ord, Generic)
    deriving newtype (Semigroup, Monoid)
    deriving anyclass (NFData)




-- | same as 'hexdumpLineBS' but does not insert spaces after 4 bytes
{-# INLINABLE hexdumpLineNoSpace #-}
hexdumpLineNoSpace :: ByteString -> T.Text
hexdumpLineNoSpace = TB.run . hexdumpLineNoSpaceBuilder

{-# INLINABLE hexdumpLineNoSpaceBuilder #-}
hexdumpLineNoSpaceBuilder :: ByteString -> TB.Builder
hexdumpLineNoSpaceBuilder =
    mconcat . map (TB.padFromLeft 2 '0' . TB.hexadecimal) . B.unpack


{-# INLINABLE hexBytesBuilder #-}
hexBytesBuilder :: HexBytes -> TB.Builder
hexBytesBuilder (HexBytes b) = hexdumpLineNoSpaceBuilder b


{-# INLINABLE parseHexLine #-}
parseHexLine :: A.Parser HexBytes
parseHexLine = do
    HexBytes . B.pack <$> A.many' parseByte

{-# INLINABLE parseHexLineQuoted #-}
parseHexLineQuoted :: A.Parser HexBytes
parseHexLineQuoted = do
    A.char '"' *> parseHexLine <* A.char '"'

{-# INLINABLE parseByte #-}
parseByte :: A.Parser Word8
parseByte = do
    a <- digitToInt <$> A.satisfy isHexDigit
    b <- digitToInt <$> A.satisfy isHexDigit
    return $ fromIntegral (a `shiftL` 4 .|. b)


instance Show HexBytes where
    {-# INLINABLE showsPrec #-}
    -- show (HexBytes b) = '"' : T.unpack (hexdumpLineNoSpace b) ++ "\""
    showsPrec p (HexBytes x) =
        showParen (p > 10) (showString (T.unpack (hexdumpLineNoSpace x)))

instance ToJSON HexBytes where
    toJSON (HexBytes b) = Data.Aeson.String $ hexdumpLineNoSpace b
    {-# INLINE toJSON #-}

    toEncoding (HexBytes b) = E.text $ hexdumpLineNoSpace b
    {-# INLINE toEncoding #-}

instance FromJSON HexBytes where
    parseJSON (Data.Aeson.String t) = case A.parseOnly parseHexLine t of
        Left  err -> fail err
        Right x   -> return x
    parseJSON invalid = E.prependFailure "parsing HexBytes failed, "
                                         (E.typeMismatch "String" invalid)


hexDigits :: P.ReadP String
hexDigits = P.munch1 isHexDigit


instance Read HexBytes where
    {-# INLINABLE readsPrec #-}
    readsPrec = readPrec_to_S $ parens $ do
        Text.ParserCombinators.ReadPrec.lift P.skipSpaces
        s <- Text.ParserCombinators.ReadPrec.lift hexDigits
        let conv (x : y : _) =
                fromIntegral (digitToInt x `shiftL` 4 .|. digitToInt y)
            conv _ = error
                "HexBytes read: conv failed: no group of 2s for hex string"
            val = map conv . group2 $ s
        return $ HexBytes (B.pack val)

{-# INLINABLE hlength #-}
hlength :: HexBytes -> Int
hlength (HexBytes b) = B.length b


{-# INLINABLE hexToBS #-}
hexToBS :: HexBytes -> B.ByteString
hexToBS (HexBytes b) = b

{-# INLINABLE bsToHex #-}
bsToHex :: B.ByteString -> HexBytes
bsToHex = HexBytes


group2 :: [Char] -> [[Char]]
group2 []           = []
group2 [x         ] = [[x, '0']]
group2 (x : y : xs) = [x, y] : group2 xs

