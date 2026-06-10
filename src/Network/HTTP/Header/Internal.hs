{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.HTTP.Header.Internal where

import Control.Exception (Exception)
import Control.Monad.ST (runST)
import Data.Array.Byte (ByteArray (..))
import Data.Bits (unsafeShiftL, unsafeShiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.Char (chr, ord)
import Data.Hashable (Hashable (..))
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
    hashIndex,
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
    = HeaderName
        !ByteArray
        !Bitmap
        -- | The 'HashBitmap' is purposely not strict, so that it is only
        -- evaluated if t'Headers' are used. This means it will be quicker
        -- if anyone needs to make a full HTTP headers 'ByteString' straight
        -- from @[Header]@
        HashBitmap
    deriving (Show)

instance Eq HeaderName where
    HeaderName ba1 _ _ == HeaderName ba2 _ _ = ba1 == ba2

instance Ord HeaderName where
    HeaderName ba1 _ _ `compare` HeaderName ba2 _ _ =
        ba1 `compare` ba2

instance Hashable HeaderName where
    hash (HeaderName a _) = hash a
    hashWithSalt i (HeaderName a _) = hashWithSalt i a

-- | The amount of bytes in a t'HeaderName'.
headerNameLength :: HeaderName -> Int
headerNameLength (HeaderName ba _ _) = sizeOfByteArray ba
{-# INLINE headerNameLength #-}

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
isValidHeaderName (HeaderName arr _ _) =
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
    HeaderName ba (OneWord (W64# w64)) (bitmapFromByteArray ba)
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
        Right (HeaderName hn _ hashBitmap) ->
            HeaderName hn (OneWord w64) hashBitmap
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
        Nothing -> do
            (ba, bitmap) <- runST $ do
                mba <- newByteArray len
                mkBitmapRef <- newSTRef (id :: Bitmap -> Bitmap)
                go mkBitmapRef mba
            pure $ HeaderName ba bitmap (bitmapFromByteArray ba)
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
                    pure $ Right (ba, finalBitmap)
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

-- | Use the 'hashIndex' to map the first six characters to two 'Word64's,
-- and use the remaining 32 bits to add the length of the t'ByteArray'.
bitmapFromByteArray :: ByteArray -> HashBitmap
bitmapFromByteArray ba =
    HashWords firstBitmap secondBitmap
  where
    firstBitmap = getMask 0 3 .|. getMask 1 2 .|. getMask 2 1 .|. getMask 3 0
    -- We also move the 5th and 6th char by 32 and 48 to move
    -- them to the upper half of the second bitmap
    secondBitmap = getMask 4 3 .|. getMask 5 2 .|. lengthBitmap
    getMask i shiftAmount
        | i >= hdrLen = 0
        | otherwise =
            let maskIx = fromIntegral $ indexWord8Array ba i
                maskBit = fromIntegral $ W8# (indexWord8OffRawAddr hashIndex maskIx)
             in 1 `unsafeShiftL` (maskBit + (shiftAmount * 16))
    lengthBitmap = 1 `unsafeShiftL` ((hdrLen `min` 32) - 1)
    hdrLen = sizeOfByteArray ba

-- | Both a header field name and its value.
data Header
    = Header {-# UNPACK #-} !HeaderName ByteString
    deriving (Eq, Show)

-- | Get the field name from the t'Header'
headerName :: Header -> HeaderName
headerName (Header name _) = name
{-# INLINE headerName #-}

-- | Get the field value from the t'Header'
headerValue :: Header -> ByteString
headerValue (Header _ val) = val
{-# INLINE headerValue #-}

-- | Construct a t'Header'
toHeader :: HeaderName -> ByteString -> Header
toHeader = Header
{-# INLINE toHeader #-}

-- | Infix operator synonym to construct a t'Header'
(>:) :: HeaderName -> ByteString -> Header
(>:) = Header
{-# INLINE (>:) #-}

-- | Collection of headers.
--
-- Faster than @[Header]@ in most cases:
--
--   * Adding to the front or back is equally fast
--   * Can determine whether a header is absent or possibly present,
--     which speeds up lookups and overrides. (which happen often)
--   * 'Network.HTTP.Header.setHeader' guarantees you don't get duplicate headers.
data Headers = Headers
    { frontHeaders :: [Header]
    , backHeaders :: [Header]
    , contentBitmap :: {-# UNPACK #-} !HashBitmap
    }

instance Eq Headers where
    Headers f1 b1 _ == Headers f2 b2 _ =
        f1 == f2 && b1 == b2

-- FIXME: make better instance for UX/DX
instance Show Headers where
    show hdrs =
        "Headers {frontHeaders = "
            <> show (frontHeaders hdrs)
            <> ", backHeaders = "
            <> show (backHeaders hdrs)
            <> ", contentBitmap = "
            <> show (MoreWords bitmap1 (OneWord bitmap2))
            <> "}"
      where
        HashWords bitmap1 bitmap2 = contentBitmap hdrs

-- | 128 bit mapping of the first 6 bytes of a t'HeaderName' and
-- the total length of the t'HeaderName'.
--
-- The first 'Word64' contains the first 4 bytes of a t'HeaderName' using the
-- 'hashIndex' to decide which bit to set in a 16 bit range; this results in
-- the following sections:
--
-- > Using the header "Accept-Encoding":
-- >
-- >   a    c    c    e      p    t    length `min` 32
-- >   |    |    |    |      |    |    |
-- > 0x####_####_####_#### 0x####_####_####_####
--
-- Read the 'hashIndex' documentation for more explanation on how the bytes are
-- translated into 16 bit maps.
data HashBitmap
    = HashWords
        {-# UNPACK #-} !Word64
        {-# UNPACK #-} !Word64

instance Show HashBitmap where
    show (HashWords a b) = show $ MoreWords a $ OneWord b
