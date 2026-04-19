{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
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

data HeaderName
    = -- | Header up to 64 bytes (which is almost every official one)
      HeaderName (Maybe B.ByteString) !ByteArray !Bitmap
    deriving (Eq, Show)

-- Headers longer than 64 bytes (probably only custom headers)
--   LongHeaderName (CI B.ByteString)

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

bitmapToList :: Bitmap -> [Word64]
bitmapToList = \case
    OneWord w64 -> [w64]
    MoreWords w64 bm -> w64 : bitmapToList bm

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
        | otherwise = loop (i - 1) (b2c byte : acc) nextW64
      where
        byte = w64 .&. 0x0000_0000_0000_000F
        nextW64 = w64 `unsafeShiftR` 4

-- | Any failure states of parsing a 'HeaderName'.
data HeaderNameException s
    = InvalidFieldNameByte s Char
    | EmptyHeader
    deriving (Eq, Show)

instance (Show s, Typeable s) => Exception (HeaderNameException s)

newtype LenientHeaderName = Lenient HeaderName
    deriving newtype (Eq, Show)

emptyHeaderName :: LenientHeaderName
emptyHeaderName = Lenient $ HeaderName Nothing mempty (OneWord 0)

arrayFromHeaderName :: HeaderName -> ByteArray
arrayFromHeaderName = undefined
