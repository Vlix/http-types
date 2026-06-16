{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
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
import Data.ByteString.Char8 (unpack)
import Data.Char (chr, ord, toUpper)
#ifdef HASHABLE
import Data.Hashable (Hashable (..))
#endif
import Data.List as L (find, intercalate)
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
    unsafeByteArrayToString,
    unsafeFreezeByteArray,
    writeWord8Array,
 )

-- | HTTP Field Name (Header name)
--
-- Technically, this is implemented as a raw t'ByteArray'.
-- The t'ByteArray' is always lower-case and only contains
-- valid bytes for an HTTP Field Name.
--
-- The t'HeaderName' also contains a bitmapping of which
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

-- | Used in debugging to show the insides of a t'HeaderName'
rawHeaderName :: HeaderName -> String
rawHeaderName (HeaderName ba bitmap hashBitmap) =
    "HeaderName " <> show ba <> " " <> show bitmap <> " " <> show hashBitmap

instance Show HeaderName where
    show = headerNameToString

-- | Turn the t'HeaderName' into a case-sensitive 'String'.
--
-- >>> let Right hdr = parseHeaderNameFromString "Content-Type"
-- >>> headerNameToString hdr
-- "Content-Type"
headerNameToString :: HeaderName -> String
headerNameToString hn@(HeaderName _ bm _)
    | bitmapIsZero bm = lowerCaseList
    | otherwise = go (0 :: Int) (bitmapToList bm) lowerCaseList
  where
    firstBit = 0x8000_0000_0000_0000
    lowerCaseList = headerNameToStringLower hn
    go _ [] rest = rest
    go _ _ [] = []
    go ix (w64 : bmRest) s@(c : cs)
        | ix == 64 = go 0 bmRest s
        | otherwise = c' : go (ix + 1) (newW64 : bmRest) cs
      where
        c' = if w64 .&. firstBit == 0 then c else toUpper c
        newW64 = w64 `unsafeShiftL` 1

-- | Turn the t'HeaderName' into a lower-case 'String'
--
-- >>> let Right hdr = parseHeaderNameFromString "Content-Type"
-- >>> headerNameToStringLower hdr
-- "content-type"
headerNameToStringLower :: HeaderName -> String
headerNameToStringLower (HeaderName arr _ _) = unsafeByteArrayToString arr
{-# INLINE headerNameToStringLower #-}

instance Eq HeaderName where
    HeaderName ba1 _ _ == HeaderName ba2 _ _ = ba1 == ba2

instance Ord HeaderName where
    HeaderName ba1 _ _ `compare` HeaderName ba2 _ _ =
        ba1 `compare` ba2

#ifdef HASHABLE
instance Hashable HeaderName where
    hash (HeaderName a _ _) = hash a
    hashWithSalt i (HeaderName a _ _) = hashWithSalt i a
#endif

-- | The amount of bytes in a t'HeaderName'.
headerNameLength :: HeaderName -> Int
headerNameLength (HeaderName ba _ _) = sizeOfByteArray ba
{-# INLINE headerNameLength #-}

-- | Returns the byte at the given offset (in bytes).
-- Does /NOT/ check bounds, so any @index >= 'headerNameLength'@ will return
-- undefined results.
unsafeHeaderNameIndexAt :: HeaderName -> Int -> Word8
unsafeHeaderNameIndexAt (HeaderName ba _ _) = indexWord8Array ba
{-# INLINE unsafeHeaderNameIndexAt #-}

-- | Returns the byte at the given offset (in bytes), or 'Nothing' if the index
-- is out of bounds.
--
-- If you know you will stay within the bounds, you can use 'unsafeHeaderNameIndexAt'.
headerNameIndexAt :: HeaderName -> Int -> Maybe Word8
headerNameIndexAt hn ix
    | ix < headerNameLength hn =
        Just $ unsafeHeaderNameIndexAt hn ix
    | otherwise = Nothing

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
-- t'HeaderName' has no bad bytes.
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

-- | Any failure states of parsing a t'HeaderName'.
data HeaderNameException s
    = -- | The 'Char' is the first encountered invalid character\/byte
      InvalidFieldNameByte s Char
    | -- | The input was empty
      EmptyHeaderName
    deriving (Eq, Show)

instance (Show s, Typeable s) => Exception (HeaderNameException s)

-- | Used to make constant t'HeaderName's
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

-- | ONLY to be used as function to create constant t'HeaderName's.
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

-- | Creates a t'HeaderName' from the given 'String', while checking
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
--
-- @
-- e.g. both the \"Content-Length\" and \"28\" part of the "Content-Length: 28" header line
-- @
data Header
    = Header {-# UNPACK #-} !HeaderName ByteString
    deriving (Eq)

instance Show Header where
    show (Header name val) =
        show name <> ": " <> unpack val

#ifdef HASHABLE
instance Hashable Header where
    hashWithSalt i (Header name val) = (i `hashWithSalt` name) `hashWithSalt` val
#endif

-- | Get the HTTP field name from the t'Header'
headerName :: Header -> HeaderName
headerName (Header name _) = name
{-# INLINE headerName #-}

-- | Get the HTTP field value from the t'Header'
headerValue :: Header -> ByteString
headerValue (Header _ val) = val
{-# INLINE headerValue #-}

-- | Construct an HTTP t'Header'
toHeader :: HeaderName -> ByteString -> Header
toHeader = Header
{-# INLINE toHeader #-}

-- | Infix operator synonym to construct a t'Header'
(>:) :: HeaderName -> ByteString -> Header
(>:) = Header
{-# INLINE (>:) #-}

-- | Collection of HTTP headers.
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

-- | Shows all headers with newlines.
--
-- Use @show . allHeaders@ to print on one line.
instance Show Headers where
    show = L.intercalate "\n" . fmap show . allHeaders

rawHeaders :: Headers -> String
rawHeaders hdrs =
    "Headers {frontHeaders = "
        <> show (frontHeaders hdrs)
        <> ", backHeaders = "
        <> show (backHeaders hdrs)
        <> ", contentBitmap = "
        <> show (MoreWords bitmap1 (OneWord bitmap2))
        <> "}"
  where
    HashWords bitmap1 bitmap2 = contentBitmap hdrs

-- | Get all t'Header's in order.
allHeaders :: Headers -> [Header]
allHeaders hdrs = frontHeaders hdrs <> reverse (backHeaders hdrs)

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
