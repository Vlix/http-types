{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.HTTP.Header.Internal where

import Control.Exception (Exception)
import Control.Monad.ST (runST)
import Data.Array.Byte (ByteArray (..))
import Data.Bits (unsafeShiftR, (.&.), (.|.))
import Data.Char (chr, ord)
import Data.List (find, intercalate)
import Data.STRef (modifySTRef, newSTRef, readSTRef)
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Exts (
    Addr#,
    Int (..),
    Word64#,
    cstringLength#,
    uncheckedShiftL64#,
    unpackCString#,
    (+#),
 )
import GHC.Word (Word64 (..), Word8 (..))
import Network.HTTP.LowLevel (
    adjustBitmap,
    c2w,
    copyAddrToByteArray,
    finalShift,
    indexWord8Array,
    indexWord8OffRawAddr,
    isBadChar,
    isMod64,
    newByteArray,
    sizeOfByteArray,
    strictIndex,
    unsafeFreezeByteArray,
    writeWord8Array,
 )

-- | HTTP Field Name (Header name)
--
-- Technically, this is implemented as a raw 'ByteArray'.
-- The 'ByteArray' is always lower-case and only contains
-- valid bytes for an HTTP Field Name.
--
-- The 'HeaderName' also contains a bitmapping of which
-- bytes were originally upper-case, but is commonly only used
-- in HTTP\/1 when showing\/encoding the header name.
data HeaderName
    = HeaderName !ByteArray !Bitmap
    deriving (Eq, Show)

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
--
-- Only used in testing, since the parse functions should ensure any created
-- 'HeaderName' has no bad bytes.
isValidHeaderName :: HeaderName -> Bool
isValidHeaderName (HeaderName arr _) =
    case baLen of
        0 -> False
        _ -> loop 0
  where
    baLen = sizeOfByteArray arr
    loop ix
        | ix == baLen = True
        | isBadChar (indexWord8Array arr ix) = False
        | otherwise = loop (ix + 1)

-- | Any failure states of parsing a 'HeaderName'.
data HeaderNameException s
    = -- | The 'Char' is the first encountered invalid character\/byte
      InvalidFieldNameByte s Char
    | -- | The input was empty
      EmptyHeaderName
    deriving (Eq, Show)

instance (Show s, Typeable s) => Exception (HeaderNameException s)

-- | Used to make constant 'HeaderName's
--
-- (INLINE pragma helps in making the literal size a strict machine word)
unsafePackLiteral :: Addr# -> Word64# -> HeaderName
unsafePackLiteral addr w64 =
    HeaderName ba (OneWord (W64# w64))
  where
    size = cstringLength# addr
    ba = runST $ do
        mba <- newByteArray (I# size)
        copyAddrToByteArray addr mba size
        unsafeFreezeByteArray mba
{-# INLINE unsafePackLiteral #-}

-- | ONLY to be used as function to create constant 'HeaderName's.
-- Should NEVER be exposed!
--
-- RULES ensure that the constant does not go through 'String',
-- but that the literal 'Addr#' gets used as efficiently as possible.
unsafeMkHeaderName :: String -> Word64 -> HeaderName
unsafeMkHeaderName s w64 =
    case parseHeaderNameFromString s of
        Right (HeaderName hn _) -> HeaderName hn $ OneWord w64
        Left _ -> error $ "http-types: failed to parse literal header name: " <> s
{-# INLINE [0] unsafeMkHeaderName #-}

{-# RULES
"HeaderName unsafeMkHeaderName/packAddress" forall s w64.
    unsafeMkHeaderName (unpackCString# s) (W64# w64) =
        unsafePackLiteral s w64
    #-}

-- We keep 'parseHeaderNameFromString' here to avoid cyclic module dependencies.
-- As it is used in 'unsafeMkHeaderName' when the RULE doesn't get triggered.

-- | Creates a 'HeaderName' from the given 'String', while checking
-- for any invalid characters. A zero-length argument will result in
-- @Left 'EmptyHeaderName'@.
parseHeaderNameFromString :: String -> Either (HeaderNameException String) HeaderName
parseHeaderNameFromString s =
    case find isBadChar' s of
        Just c -> Left (InvalidFieldNameByte s c)
        Nothing -> runST $ do
            mba <- newByteArray len
            mkBitmapRef <- newSTRef (id :: Bitmap -> Bitmap)
            go mkBitmapRef mba
  where
    isBadChar' c = c > '\xFF' || isBadChar (c2w c)
    len = length s
    !(W64# zero#) = 0
    !(I# finalShift#) = finalShift len
    go mkBitmapRef mba = loop zero# 0# s
      where
        loop _ _ [] = pure (Left EmptyHeaderName)
        loop bitmap# ix# (c : cs) = do
            writeWord8Array mba ix# convertedChar#
            if I# nextIx# == len
                then do
                    ba <- unsafeFreezeByteArray mba
                    mkBitmap <- readSTRef mkBitmapRef
                    let finalBitmap = mkBitmap (OneWord (W64# (newBitmap# `uncheckedShiftL64#` finalShift#)))
                    pure $ Right (HeaderName ba finalBitmap)
                else do
                    W64# nextBitmap# <- updateRef newBitmap#
                    loop nextBitmap# nextIx# cs
          where
            charInt = ord c
            !(W8# originalChar#) = fromIntegral charInt
            convertedChar# = indexWord8OffRawAddr strictIndex (fromIntegral (W8# originalChar#))
            newBitmap# = adjustBitmap originalChar# convertedChar# bitmap#
            nextIx# = ix# +# 1#
            updateRef w64
                | isMod64 (I# nextIx#) =
                    0 <$ modifySTRef mkBitmapRef (. MoreWords (W64# w64))
                | otherwise = pure (W64# (w64 `uncheckedShiftL64#` 1#))
