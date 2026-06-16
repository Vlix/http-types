{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | A new implementation of "HTTP Headers" (HTTP Fields).
--
-- This module provides a more opaque API, so the internals don't "leak" like
-- they do in the older "Network.HTTP.Types.Header" module.
-- That module forced you to use the "Data.CaseInsensitive" API to create
-- header names, and to use list functions to go through the
-- 'Network.HTTP.Types.Header.Headers'. It also left the adherence to the
-- HTTP RFC to the user, instead of providing guarantees through the API.
--
-- Some recommendations:
--
--   * 'fromList' is the best way to create t'Headers'.
--   * use 'setHeader' to set a header.
--   * only if you /need/ to put a header at the front, use 'setHeaderFront'.
--     (e.g. @"Host"@ or @"Date"@ headers, since "it is good practice to send
--     header fields that contain additional control data first, such as Host
--     on requests and Date on responses, so that implementations can decide
--     when not to handle a message as early as possible." - RFC 9110)
--   * only use 'addHeader' (or 'addHeaderFront') if you're fine with
--     getting duplicates in the t'Headers'. (e.g. the @"Set-Cookie"@ header)
module Network.HTTP.Header (
    -- * HTTP Headers
    Headers,
    allHeaders,
    emptyHeaders,
    -- RequestHeaders,
    -- ResponseHeaders,

    -- ** An HTTP Header
    Header,
    toHeader,
    (>:),
    headerName,
    headerValue,

    -- ** HTTP Header functions
    fromList,
    setHeader,
    setHeaderFront,
    addHeader,
    addHeaderFront,
    lookupHeaders,
    lookupHeader,
    removeHeader,

    -- * HTTP Header Names (HTTP Field Names)

    -- | The part of an HTTP Field before the colon:
    --
    -- @i.e. the \"Content-Type\" part of "Content-Type: application\/json"@
    HeaderName,
    headerNameLength,

    -- ** Parsing \/ Decoding

    -- | Creating t'HeaderName's.
    --
    -- Parsing also checks whether the incoming elements are allowed in
    -- HTTP Field Names
    HeaderNameException (..),
    parseHeaderName,
    parseHeaderNameFromString,
    parseHeaderNameFromText,

    -- *** Unsafe versions

    -- | These functions will throw an exception if they encounter an illegal
    -- byte in the to-be-parsed header name.
    unsafeParseHeaderName,

    -- ** Encoding
    encodeHeaderName,
    encodeHeaderNameLower,
    headerNameToString,
    headerNameToStringLower,

    -- *** Low level encoding

    -- | These functions write straight to a pointer in memory.
    encodeHeaderNameToPtr,

    -- ** Common Header Names

    -- | The following header constants are provided for convenience,
    -- to prevent accidental spelling errors.
    hAccept,
    hAcceptCharset,
    hAcceptEncoding,
    hAcceptLanguage,
    hAcceptRanges,
    hAccessControlAllowCredentials,
    hAccessControlAllowHeaders,
    hAccessControlAllowMethods,
    hAccessControlAllowOrigin,
    hAccessControlExposeHeaders,
    hAccessControlMaxAge,
    hAccessControlRequestMethod,
    hAccessControlRequestHeaders,
    hAge,
    hAllow,
    hAuthorization,
    hCacheControl,
    hConnection,
    hContentDisposition,
    hContentEncoding,
    hContentLanguage,
    hContentLength,
    hContentLocation,
    hContentMD5,
    hContentRange,
    hContentType,
    hCookie,
    hDate,
    hETag,
    hExpect,
    hExpires,
    hFrom,
    hHost,
    hIfMatch,
    hIfModifiedSince,
    hIfNoneMatch,
    hIfRange,
    hIfUnmodifiedSince,
    hLastModified,
    hLink,
    hLocation,
    hMaxForwards,
    hMIMEVersion,
    hOrigin,
    hPragma,
    hPrefer,
    hPreferenceApplied,
    hProxyAuthenticate,
    hProxyAuthorization,
    hRange,
    hReferer,
    hRetryAfter,
    hServer,
    hSetCookie,
    hStrictTransportSecurity,
    hTE,
    hTrailer,
    hTransferEncoding,
    hUpgrade,
    hUserAgent,
    hVary,
    hVia,
    hWarning,
    hWWWAuthenticate,

    -- *** HTTP/2 and HTTP/3
    hPseudoAuthority,
    hPseudoMethod,
    hPseudoPath,
    hPseudoScheme,
    hPseudoStatus,

    -- * Utility functions

    -- | Here are some functions that you might want to
    caseInsensitiveEq,
) where

import Control.Exception (throw, try)
import Control.Monad (when)
import Control.Monad.ST (runST, stToIO)
import Data.Array.Byte (ByteArray (..))
import qualified Data.ByteString as B (intercalate, length)
import Data.ByteString.Internal (ByteString (BS), accursedUnutterablePerformIO, unsafeCreate)
import Data.Char (toUpper)

import Data.STRef (modifySTRef, newSTRef, readSTRef)
import Data.Text (Text)
#if !MIN_VERSION_text(1,2,0)
import Data.Text.Encoding (encodeUtf8)
#elif !MIN_VERSION_text(2,1,0)
import qualified Data.Text.Array as A (Array (..))
#endif
import qualified Data.List as L
import Data.Text.Internal (Text (..))
import Data.Text.Unsafe (lengthWord8)
import Foreign (Bits (..), Storable (..), plusPtr, withForeignPtr)
import GHC.Exts (
    ByteArray#,
    Int (..),
    Ptr (..),
    Word8#,
    clz64#,
    indexWord8Array#,
    indexWord8OffAddr#,
    uncheckedShiftL64#,
    word2Int#,
    (+#),
 )
import GHC.IO.Unsafe (unsafeDupablePerformIO)
import GHC.Word (Word64 (..), Word8 (..))
import Network.HTTP.Header.Constants
import Network.HTTP.Header.Internal (
    Bitmap (..),
    HashBitmap (..),
    Header (..),
    HeaderName (..),
    HeaderNameException (..),
    Headers (..),
    bitmapFromByteArray,
    bitmapIsZero,
    bitmapToList,
    headerName,
    headerNameLength,
    headerValue,
    parseHeaderNameFromString,
    toHeader,
    (>:),
 )
import Network.HTTP.LowLevel (
    adjustBitmap,
    ciIndex,
    copyByteArrayToAddr,
    finalShift,
    indexWord8OffRawAddr,
    isMod64,
    newByteArray,
    sizeOfByteArray,
    strictIndex,
    unsafeByteArrayToString,
    unsafeFreezeByteArray,
    w2c,
    writeWord8Array,
 )

-- $setup
-- >>> :set -XOverloadedStrings

-- | Tries to create a t'HeaderName' from the given 'ByteString', while checking
-- for any invalid characters. A zero-length argument will result in
-- @Left 'EmptyHeaderName'@.
--
-- >>> parseHeaderName "Content-Type"
-- Right (HeaderName [0x63, 0x6f, 0x6e, 0x74, 0x65, 0x6e, 0x74, 0x2d, 0x74, 0x79, 0x70, 0x65] 8080000000000000)
parseHeaderName :: ByteString -> Either (HeaderNameException ByteString) HeaderName
parseHeaderName hdr
    | size <= 0 = Left EmptyHeaderName
    | otherwise =
        unsafeDupablePerformIO $ try (toHeaderNameStrict hdr)
  where
    size = B.length hdr

-- | __Will throw an 'InvalidFieldNameByte' exception if the 'ByteString'__
-- __contains any bytes not defined in__
-- [RFC 9110](https://www.rfc-editor.org/rfc/rfc9110.html#section-5.6.2),
-- __or an 'EmptyHeaderName' if the provided 'ByteString' is empty.__
--
-- Creates a t'HeaderName' from the given 'ByteString'.
--
-- >>> unsafeParseHeaderName "Content-Type"
-- HeaderName [0x63, 0x6f, 0x6e, 0x74, 0x65, 0x6e, 0x74, 0x2d, 0x74, 0x79, 0x70, 0x65] 8080000000000000
unsafeParseHeaderName :: ByteString -> HeaderName
unsafeParseHeaderName hdr
    | size <= 0 = throw (EmptyHeaderName :: HeaderNameException String)
    | otherwise =
        unsafeDupablePerformIO $ toHeaderNameStrict hdr
  where
    size = B.length hdr

toHeaderNameStrict :: ByteString -> IO HeaderName
toHeaderNameStrict bs@(BS fptr size) =
    withForeignPtr fptr $ \ptr ->
        stToIO $ do
            mba <- newByteArray size
            mkBitmapRef <- newSTRef (id :: Bitmap -> Bitmap)
            (ba, bitmap) <- go mkBitmapRef ptr mba
            pure (HeaderName ba bitmap (bitmapFromByteArray ba))
  where
    !(W64# zero#) = 0
    !(I# finalShift#) = finalShift size
    go mkBitmapRef (Ptr addr#) mba =
        loop zero# 0#
      where
        loop bitmap# ix# = do
            when (W8# convertedChar# == 0xFF) $
                throw (InvalidFieldNameByte bs (w2c (W8# originalChar#)))
            writeWord8Array mba ix# convertedChar#
            if I# nextIx# == size
                then do
                    ba <- unsafeFreezeByteArray mba
                    mkBitmap <- readSTRef mkBitmapRef
                    let finalBitmap = newBitmap# `uncheckedShiftL64#` finalShift#
                        !finishedBitmap =
                            mkBitmap $ OneWord (W64# finalBitmap)
                    pure (ba, finishedBitmap)
                else do
                    W64# nextBitmap# <- updateRef newBitmap#
                    loop nextBitmap# nextIx#
          where
            newBitmap# = adjustBitmap originalChar# convertedChar# bitmap#
            nextIx# = ix# +# 1#
            originalChar# = indexWord8OffAddr# addr# ix#
            convertedChar# = indexWord8OffRawAddr strictIndex (fromIntegral (W8# originalChar#))
            updateRef w64
                | isMod64 (I# nextIx#) =
                    0 <$ modifySTRef mkBitmapRef (. MoreWords (W64# w64))
                | otherwise = pure (W64# (w64 `uncheckedShiftL64#` 1#))

-- | Turns the t'HeaderName' into a case-sensitive 'ByteString'.
--
-- Depending on how the t'HeaderName' is constructed, this might only return
-- the original 'ByteString' that was used to create it, or it creates a
-- 'ByteString' from the internal t'ByteArray' + casing bitmap.
--
-- >>> encodeHeaderName (unsafeParseHeaderName "Content-Type")
-- "Content-Type"
encodeHeaderName :: HeaderName -> ByteString
encodeHeaderName hn@(HeaderName arr _ _) =
    unsafeCreate (sizeOfByteArray arr) $ encodeHeaderNameToPtr hn

-- | Like 'encodeHeaderName', but writes to a bare t'Ptr' 'Word8'.
encodeHeaderNameToPtr :: HeaderName -> Ptr Word8 -> IO ()
encodeHeaderNameToPtr (HeaderName arr bitmap _) startPtr = do
    stToIO $ copyByteArrayToAddr arr startPtr
    go bitmap startPtr
  where
    go (OneWord w64) ptr = oneWord w64 ptr
    go (MoreWords w64 more) ptr = do
        oneWord w64 ptr
        go more $ ptr `plusPtr` 64
    unsetFirstBit :: Word64 -> Word64
    unsetFirstBit w64 = w64 .&. 0x7FFF_FFFF_FFFF_FFFF
    -- this unsets the 0x20 bit
    capitalize :: Word8 -> Word8
    capitalize w8 = w8 .&. 0xDF
    oneWord (W64# w64#) ptr = do
        if I# clz# >= 64
            then pure ()
            else do
                let newPtr = ptr `plusPtr` I# clz#
                peek newPtr >>= poke newPtr . capitalize
                let adjustedW64 =
                        unsetFirstBit $ W64# (w64# `uncheckedShiftL64#` clz#)
                oneWord adjustedW64 newPtr
      where
        clz# = word2Int# (clz64# w64#)

-- | Encode the t'HeaderName' to a lower-case 'ByteString'.
--
-- >>> encodeHeaderNameLower (unsafeParseHeaderName "Content-Type")
-- "content-type"
encodeHeaderNameLower :: HeaderName -> ByteString
encodeHeaderNameLower (HeaderName ba _ _) =
    unsafeCreate (sizeOfByteArray ba) $
        stToIO . copyByteArrayToAddr ba

-- | Turn the t'HeaderName' into a case-sensitive 'String'.
--
-- >>> headerNameToString (unsafeParseHeaderName "Content-Type")
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
-- >>> headerNameToStringLower (unsafeParseHeaderName "Content-Type")
-- "content-type"
headerNameToStringLower :: HeaderName -> String
headerNameToStringLower (HeaderName arr _ _) = unsafeByteArrayToString arr
{-# INLINE headerNameToStringLower #-}

-- | Tries to create a t'HeaderName' from the given t'Text', while checking
-- for any invalid characters. A zero-length argument will result in
-- @Left 'EmptyHeaderName'@.
--
-- >>> parseHeaderNameFromText "Content-Type"
-- Right (HeaderName [0x63, 0x6f, 0x6e, 0x74, 0x65, 0x6e, 0x74, 0x2d, 0x74, 0x79, 0x70, 0x65] 8080000000000000)
parseHeaderNameFromText :: Text -> Either (HeaderNameException Text) HeaderName
#if !MIN_VERSION_text(1,2,0)
parseHeaderNameFromText = encodeHeaderName . encodeUtf8
#else
parseHeaderNameFromText txt
    | len <= 0 = Left EmptyHeaderName
    | otherwise = do
        (ba, bitmap) <- runST $ do
            mba <- newByteArray len
            mkBitmapRef <- newSTRef (id :: Bitmap -> Bitmap)
            go mkBitmapRef mba
        pure (HeaderName ba bitmap (bitmapFromByteArray ba))
  where
    arr = arrayFromText txt
    len = lengthWord8 txt
    !(I# finalShift#) = finalShift len
    !(W64# zero#) = 0
    go mkBitmapRef mba =
        loop zero# 0#
      where
        loop bitmap# ix#
            | W8# convertedChar# == 0xFF =
                pure $ Left (InvalidFieldNameByte txt (w2c (W8# originalChar#)))
            | otherwise = do
                writeWord8Array mba ix# convertedChar#
                if I# nextIx# == len
                    then do
                        ba <- unsafeFreezeByteArray mba
                        mkBitmap <- readSTRef mkBitmapRef
                        let finalBitmap = mkBitmap (OneWord (W64# (newBitmap# `uncheckedShiftL64#` finalShift#)))
                        pure $ Right (ba, finalBitmap)
                    else do
                        (W64# nextBitmap#) <- updateRef newBitmap#
                        loop nextBitmap# nextIx#
          where
            newBitmap# = adjustBitmap originalChar# convertedChar# bitmap#
            nextIx# = ix# +# 1#
            updateRef w64
                | isMod64 (I# nextIx#) =
                    0 <$ modifySTRef mkBitmapRef (. MoreWords (W64# w64))
                | otherwise = pure (W64# (w64 `uncheckedShiftL64#` 1#))
            originalChar# = indexWord8Array# arr ix#
            -- This works fine with Text/UTF-8, since anything above the ASCII
            -- range (0-127) is automatically invalid.
            convertedChar# = indexWord8OffRawAddr strictIndex (fromIntegral (W8# originalChar#))
#endif

#if MIN_VERSION_text(2,1,0)
arrayFromText :: Text -> ByteArray#
arrayFromText (Text (ByteArray arr) _ _) = arr
#elif MIN_VERSION_text(1,2,0)
arrayFromText :: Text -> ByteArray#
arrayFromText (Text (A.ByteArray arr) _ _) = arr
#endif

-- | A faster comparison of two 'ByteString's while ignoring case
-- /in the ASCII range ONLY/.
--
-- Useful when comparing header values that aren't actually t'HeaderName's,
-- but where case sensitivity doesn't matter.
--
-- For example, when checking the @Connection@ header value:
--
-- >>> "Close" `caseInsensitiveEq` "close"
-- True
caseInsensitiveEq :: ByteString -> ByteString -> Bool
caseInsensitiveEq (BS _ 0) (BS _ 0) = True
caseInsensitiveEq (BS fptr1 len1) (BS fptr2 len2)
    | len1 /= len2 = False
    | otherwise = accursedUnutterablePerformIO $
        withForeignPtr fptr1 $ \ptr1 ->
            withForeignPtr fptr2 $ \ptr2 ->
                loop ptr1 ptr2 0
  where
    ixW8 :: Word8 -> Word8#
    ixW8 w8 = indexWord8OffRawAddr ciIndex (fromIntegral w8)
    loop p1 p2 ix
        | ix == len1 = pure True
        | otherwise = do
            w1 <- peek p1
            w2 <- peek p2
            if W8# (ixW8 w1) == W8# (ixW8 w2)
                then loop (p1 `plusPtr` 1) (p2 `plusPtr` 1) (ix + 1)
                else pure False

-- | Create a t'Headers' collection from a list of t'Header's.
fromList :: [Header] -> Headers
fromList [] = emptyHeaders
fromList hdrList =
    Headers
        { frontHeaders = hdrList
        , backHeaders = []
        , contentBitmap = cbm
        }
  where
    cbm =
        L.foldl' go (HashWords 0 0) hdrList
      where
        go allContent (Header (HeaderName _ _ hashBitmap) _) =
            allContent `orHashBitmaps` hashBitmap -- bitmapFromHeaderName hdrName

matchBitmap :: HashBitmap -> HashBitmap -> Bool
matchBitmap (HashWords a1 b1) (HashWords a2 b2) =
    a1 `containedInBits` a2
        && b1 `containedInBits` b2

containedInBits :: Word64 -> Word64 -> Bool
containedInBits collection toCheck =
    (collection .&. toCheck) == toCheck

orHashBitmaps :: HashBitmap -> HashBitmap -> HashBitmap
orHashBitmaps (HashWords a1 b1) (HashWords a2 b2) =
    HashWords (a1 .|. a2) (b1 .|. b2)

-- | Set a header at the back of the t'Headers'.
--
-- This will remove any already present t'Header's with the given t'HeaderName'.
setHeader :: Header -> Headers -> Headers
setHeader hdr@(Header name@(HeaderName _ _ hashBitmap) _) Headers{..}
    | isPresent =
        Headers
            { frontHeaders = removeIt frontHeaders
            , backHeaders = hdr : removeIt backHeaders
            , ..
            }
    | otherwise =
        Headers
            { backHeaders = hdr : backHeaders
            , contentBitmap = contentBitmap `orHashBitmaps` hashBitmap
            , ..
            }
  where
    isPresent = contentBitmap `matchBitmap` hashBitmap
    removeIt = filter $ (/= name) . headerName

-- | Set a header at the front of the t'Headers'.
--
-- This will remove any already present t'Header's with the given t'HeaderName'.
setHeaderFront :: Header -> Headers -> Headers
setHeaderFront hdr@(Header name@(HeaderName _ _ hashBitmap) _) Headers{..}
    | isPresent =
        Headers
            { frontHeaders = hdr : removeIt frontHeaders
            , backHeaders = removeIt backHeaders
            , ..
            }
    | otherwise =
        Headers
            { frontHeaders = hdr : frontHeaders
            , contentBitmap = contentBitmap `orHashBitmaps` hashBitmap
            , ..
            }
  where
    isPresent = contentBitmap `matchBitmap` hashBitmap
    removeIt = filter $ (/= name) . headerName

-- | Add a t'Header' to the back of the t'Headers' collection, /possibly resulting/
-- /in a duplicate entry/.
--
-- If you only want one header with the given t'HeaderName', you should use
-- 'setHeader', which will make sure the provided t'Header' will be the only
-- one with that t'HeaderName' in the t'Headers'.
addHeader :: Header -> Headers -> Headers
addHeader hdr@(Header (HeaderName _ _ hashBitmap) _) Headers{..} =
    Headers
        { frontHeaders = frontHeaders
        , backHeaders = hdr : backHeaders
        , contentBitmap = contentBitmap `orHashBitmaps` hashBitmap
        }

-- | Add a t'Header' to the front of the t'Headers' collection, possibly resulting
-- in a duplicate entry.
--
-- If you only want one header with the given t'HeaderName', you should use
-- 'setHeader', which will make sure the provided t'Header' will be the only
-- one with that t'HeaderName' in the t'Headers'.
addHeaderFront :: Header -> Headers -> Headers
addHeaderFront hdr@(Header (HeaderName _ _ hashBitmap) _) Headers{..} =
    Headers
        { frontHeaders = hdr : frontHeaders
        , backHeaders = backHeaders
        , contentBitmap = contentBitmap `orHashBitmaps` hashBitmap
        }

-- | Get the values of all the t'Header's in the t'Headers' that correspond to
-- the given t'HeaderName'.
--
-- >>> lookupHeaders hAccept emptyHeaders
-- []
--
-- >>> lookupHeaders hAccept (fromList [hAccept >: "test"])
-- ["test"]
--
-- >>> let doubleAccept = fromList [hAccept >: "one", hAccept >: "two"]
-- >>> lookupHeaders hAccept doubleAccept
-- ["one","two"]
--
-- /N.B. will return more than one 'ByteString' if the t'Headers' contain/
-- /more than one entry of the searched for t'HeaderName'./
lookupHeaders :: HeaderName -> Headers -> [ByteString]
lookupHeaders name@(HeaderName _ _ hashBitmap) Headers{..}
    | isPresent = headerValue <$> allFoundHeaders
    | otherwise = []
  where
    isPresent = contentBitmap `matchBitmap` hashBitmap
    allFoundHeaders = inFront <> inBack
    inFront = getHeader frontHeaders
    -- we first filter the reversed headers to save us a reverse
    -- on the entire list.
    inBack = reverse $ getHeader backHeaders
    getHeader = filter $ (== name) . headerName

-- | If you would rather handle a 'Maybe', instead of the list from 'lookupHeaders',
-- then use this function.
--
-- /N.B. Please keep in mind this basically calls 'lookupHeaders' and then calls/
-- /\"@'B.intercalate' ", "@\" on the result if there are duplicates of the header/
-- /you are looking for, regardless of whether the header supports multiple values./
--
-- >>> let hdrs = fromList [hContentLength >: "28", hContentLength >: "20"]
-- >>> lookupHeader hContentLength hdrs
-- Just "28, 20"
lookupHeader :: HeaderName -> Headers -> Maybe ByteString
lookupHeader name hdrs =
    case lookupHeaders name hdrs of
        [] -> Nothing
        vs -> Just (B.intercalate ", " vs)

-- | Removes any occurence of the given t'HeaderName' in the t'Headers'.
--
-- This does not recalculate anything, since this action is viewed as uncommon.
--
-- >>> lookupHeaders hAccept (setHeader (hAccept >: "test") emptyHeaders)
-- ["test"]
--
-- >>> removeHeader hAccept (setHeader (hAccept >: "test") emptyHeaders) == emptyHeaders
-- True
removeHeader :: HeaderName -> Headers -> Headers
removeHeader hdrName@(HeaderName _ _ hashBitmap) hdrs@Headers{..}
    | isPresent =
        Headers
            { frontHeaders = removeIt frontHeaders
            , backHeaders = removeIt backHeaders
            , ..
            }
    | otherwise = hdrs
  where
    isPresent = contentBitmap `matchBitmap` hashBitmap
    removeIt = filter $ (/=) hdrName . headerName

-- | Get all t'Header's in order.
allHeaders :: Headers -> [Header]
allHeaders hdrs = frontHeaders hdrs <> reverse (backHeaders hdrs)

-- | An empty collection of headers.
--
-- In general, you'd want to use 'fromList' to create t'Headers'; it is more
-- efficient than starting with 'emptyHeaders' and iteratively adding to it,
-- but sometimes you can't get around it, so it is provided.
emptyHeaders :: Headers
emptyHeaders =
    Headers
        { frontHeaders = []
        , backHeaders = []
        , contentBitmap = HashWords 0 0
        }
