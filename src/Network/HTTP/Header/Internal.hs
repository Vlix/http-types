{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}

module Network.HTTP.Header.Internal where

import Control.Exception (Exception)
import Data.Array.Byte (ByteArray (..))
import Data.Bits (unsafeShiftR, (.&.), (.|.))
import qualified Data.ByteString as B (ByteString)
import Data.Char (chr)
import Data.List (intercalate)
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Exts (Int (..), sizeofByteArray#)
import Network.HTTP.LowLevel

-- | HTTP Field Name (Header name)
--
-- Technically, this is implemented as a raw 'ByteArray'.
-- The 'ByteArray' is always lower-case and only contains
-- valid bytes for an HTTP Field Name.
--
-- The 'HeaderName' also contains a bitmapping of which
-- bytes were originally upper-case, but is commonly only used
-- in HTTP\/1 when showing\/encoding the header name.
--
-- For efficiency, the 'HeaderName' can also hold on to
-- the original 'B.ByteString' from which it was parsed.
-- (/if it was parsed from a 'B.ByteString', of course/)
data HeaderName
    = HeaderName (Maybe B.ByteString) !ByteArray !Bitmap
    deriving (Eq, Show)

-- | Access the inner 'ByteArray' of the 'HeaderName'
unsafeGetByteArray :: HeaderName -> ByteArray
unsafeGetByteArray (HeaderName _ ba _) = ba

-- | Access the inner 'ByteString' of the 'HeaderName'
unsafeGetByteString :: HeaderName -> Maybe B.ByteString
unsafeGetByteString (HeaderName mbs _ _) = mbs

-- | Bits from "left-to-right" that show which bytes were
-- originally upper-case.
--
-- Equivalent to @NonEmpty Word64@.
data Bitmap
    = OneWord !Word64
    | MoreWords !Word64 !Bitmap
    deriving (Eq)

bitmapIsZero :: Bitmap -> Bool
bitmapIsZero (OneWord 0) = True
bitmapIsZero (MoreWords 0 bm) = bitmapIsZero bm
bitmapIsZero _ = False

-- | Shows the 'Bitmap' as a hexidecimal number.
--
-- show {0xFF00000000000000}
-- > "FF00000000000000"
-- show {0xCBA9876543210FED,0xF000000000000000}
-- > "<CBA9876543210FED-F000000000000000>"
instance Show Bitmap where
    show (OneWord w64) = w64s w64
    show ws =
        "<" <> rest <> ">"
      where
        rest = intercalate "-" $ w64s <$> bitmapToList ws

-- | Turn bitmap into a list of words.
--
-- Will never be '[]'.
bitmapToList :: Bitmap -> [Word64]
bitmapToList = \case
    OneWord w64 -> [w64]
    MoreWords w64 bm -> w64 : bitmapToList bm

-- | Turn 'Word64' into a hexadecimal string representing the bytes.
w64s :: Word64 -> String
w64s =
    loop 16 []
  where
    b2c b =
        chr . fromIntegral $
            if b < 10 then b .|. 0x30 else b + 0x37
    loop :: Int -> String -> Word64 -> String
    loop i acc w64
        | i == 0 = acc
        | otherwise = loop (i - 1) (b2c byte : acc) nextByte
      where
        byte = w64 .&. 0x0000_0000_0000_000F
        nextByte = w64 `unsafeShiftR` 4

-- | Checks for any illegal bytes.
--
--   * 'True': Valid header name
--   * 'False': Bad header name
--
-- [HTTP Field Names](https://www.rfc-editor.org/rfc/rfc9110.html#section-5.6.2)
-- only allow visible characters that are _not_ delimiters. (though the
-- convention is to only use alpha-numeric characters and the minus character)
isValidHeaderName :: HeaderName -> Bool
isValidHeaderName (HeaderName _ arr@(ByteArray ba) _) =
    case baLen of
        0 -> False
        _ -> loop 0
  where
    baLen = I# (sizeofByteArray# ba)
    loop ix
        | ix == baLen = True
        | isBadChar (indexWord8Array arr ix) = False
        | otherwise = loop (ix + 1)

-- | Any failure states of parsing a 'HeaderName'.
data HeaderNameException s
    = -- | The 'Char' is the first encountered invalid character\/byte
      InvalidFieldNameByte s Char
    | -- | The input was empty
      EmptyHeader
    deriving (Eq, Show)

instance (Show s, Typeable s) => Exception (HeaderNameException s)
