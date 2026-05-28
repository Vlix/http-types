{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

-- | A new implementation of "HTTP Headers" (HTTP Fields).
--
-- This module provides a more opaque API, so the internals don't "leak" like
-- they do in the older "Network.HTTP.Types.Header" module.
-- That module forced you to use the "Data.CaseInsensitive" API to create
-- header names, and to use list functions to go through the
-- 'Network.HTTP.Types.Header.Headers'.
module Network.HTTP.Header (
    -- * HeaderMap

    -- HeaderMap,

    -- * Header Names (HTTP Field Names)

    -- | The part of an HTTP Field before the colon:
    --
    -- @(e.g. the \"Content-Type\" part of "Content-Type: application\/json")@
    HeaderName,

    -- ** Parsing \/ Decoding

    -- | Creating 'HeaderName's.
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

    -- ** Common Header Names
    hAccept,
    hAcceptCharset,
    hAcceptEncoding,
    hAcceptLanguage,
    hAcceptRanges,
    hAge,
) where

import Control.Exception (throw, try)
import Control.Monad (when)
import Control.Monad.ST (runST, stToIO)
import Data.Array.Byte (ByteArray (..))
import qualified Data.ByteString as B (length)
import Data.ByteString.Internal (ByteString (BS), c2w, unsafeCreate, w2c)
import Data.Char (ord, toUpper)
import Data.List (find)
import Data.STRef (modifySTRef, newSTRef, readSTRef)
import Data.Text (Text)
#if !MIN_VERSION_text(1,2,0)
import Data.Text.Encoding (encodeUtf8)
#elif !MIN_VERSION_text(2,1,0)
import qualified Data.Text.Array as A (Array (..))
#endif
import Data.Text.Internal (Text (..))
import Data.Text.Unsafe (lengthWord8)
import Foreign (Bits (..), Storable (..), plusPtr, withForeignPtr)
import GHC.Exts (
    ByteArray#,
    Int (..),
    Ptr (..),
    clz64#,
    indexWord8Array#,
    indexWord8OffAddr#,
    uncheckedShiftL64#,
    word2Int#,
    (+#),
 )
import GHC.IO.Unsafe (unsafeDupablePerformIO)
import GHC.Word (Word64 (..), Word8 (..))
import Network.HTTP.Header.Internal (
    Bitmap (..),
    HeaderName (..),
    HeaderNameException (..),
    bitmapIsZero,
    bitmapToList,
 )
import Network.HTTP.LowLevel (
    adjustBitmap,
    copyByteArrayToAddr,
    finalShift,
    foldByteArrayR,
    indexWord8OffRawAddr,
    isBadChar,
    isMod64,
    newByteArray,
    sizeOfByteArray,
    strictIndex,
    unsafeFreezeByteArray,
    writeWord8Array,
 )

-- | Creates a 'HeaderName' from the given 'ByteString'.
parseHeaderName :: ByteString -> Either (HeaderNameException ByteString) HeaderName
parseHeaderName hdr
    | size <= 0 = Left EmptyHeader
    | otherwise =
        unsafeDupablePerformIO $ try (toHeaderNameStrict hdr)
  where
    size = B.length hdr

-- | __Will throw an 'InvalidFieldNameByte' exception if the 'ByteString'__
-- __contains any bytes not defined in__
-- [RFC 9110](https://www.rfc-editor.org/rfc/rfc9110.html#section-5.6.2)
--
-- Creates a 'HeaderName' from the given 'ByteString'.
unsafeParseHeaderName :: ByteString -> HeaderName
unsafeParseHeaderName hdr
    | size <= 0 = throw (EmptyHeader :: HeaderNameException String)
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
            go mkBitmapRef ptr mba
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
                    pure (HeaderName ba finishedBitmap)
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

-- | Turns the 'HeaderName' into a case-sensitive 'ByteString'.
--
-- Depending on how the 'HeaderName' is constructed, this might only return
-- the original 'ByteString' that was used to create it, or it creates a
-- 'ByteString' from the internal 'ByteArray' + casing bitmap.
encodeHeaderName :: HeaderName -> ByteString
encodeHeaderName (HeaderName arr bitmap) =
    unsafeCreate (sizeOfByteArray arr) $ \ptr -> do
        stToIO $ copyByteArrayToAddr arr ptr
        go bitmap ptr
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

-- | Encode the 'HeaderName' to a lower-case 'ByteString'.
--
-- Will return the held 'ByteString' if it was already lower-case.
--
-- > let hn = unsafeParseHeaderName "Content-Type"
-- > encodeHeaderNameLower hn == "content-type"
encodeHeaderNameLower :: HeaderName -> ByteString
encodeHeaderNameLower (HeaderName ba _) =
    unsafeCreate (sizeOfByteArray ba) $
        stToIO . copyByteArrayToAddr ba

-- | Creates a 'HeaderName' from the given 'String', while checking
-- for any invalid characters.
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
        loop _ _ [] = pure (Left EmptyHeader)
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

-- | Turn the 'HeaderName' into a case-sensitive 'String'.
headerNameToString :: HeaderName -> String
headerNameToString (HeaderName arr bm)
    | bitmapIsZero bm = lowerCastList
    | otherwise =
        zipWith toChar lowerCastList $
            concatMap word64ToBoolList (bitmapToList bm)
  where
    lowerCastList = arrayToStringLower arr
    toChar c b = if b then toUpper c else c
    word64ToBoolList =
        loop 64 []
      where
        loop :: Int -> [Bool] -> Word64 -> [Bool]
        loop 0 acc _ = acc
        loop n acc w64 =
            let b = w64 .&. 1 == 1
             in loop (n - 1) (b : acc) (w64 `unsafeShiftR` 1)

-- | Turn the 'HeaderName' into a lower-case 'String'
--
-- > let hn = unsafeParseHeaderName "Content-Type"
-- > headerNameToStringLower hn == "content-type"
headerNameToStringLower :: HeaderName -> String
headerNameToStringLower (HeaderName arr _) = arrayToStringLower arr
{-# INLINE headerNameToStringLower #-}

arrayToStringLower :: ByteArray -> [Char]
arrayToStringLower = foldByteArrayR ((:) . w2c) []
{-# INLINE arrayToStringLower #-}

parseHeaderNameFromText :: Text -> Either (HeaderNameException Text) HeaderName
#if !MIN_VERSION_text(1,2,0)
parseHeaderNameFromText = encodeHeaderName . encodeUtf8
#else
parseHeaderNameFromText txt
    | len <= 0 = Left EmptyHeader
    | otherwise = runST $ do
        mba <- newByteArray len
        mkBitmapRef <- newSTRef (id :: Bitmap -> Bitmap)
        go mkBitmapRef mba
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
                        pure $ Right (HeaderName ba finalBitmap)
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

----------------------------- HEADERS FROM HERE ON -----------------------------

hAccept :: HeaderName
hAccept = unsafeParseHeaderName "Accept"
{-# NOINLINE hAccept #-}

hAcceptCharset :: HeaderName
hAcceptCharset = unsafeParseHeaderName "Accept-Charset"
{-# NOINLINE hAcceptCharset #-}

hAcceptEncoding :: HeaderName
hAcceptEncoding = unsafeParseHeaderName "Accept-Encoding"
{-# NOINLINE hAcceptEncoding #-}

hAcceptLanguage :: HeaderName
hAcceptLanguage = unsafeParseHeaderName "Accept-Language"
{-# NOINLINE hAcceptLanguage #-}

hAcceptRanges :: HeaderName
hAcceptRanges = unsafeParseHeaderName "Accept-Ranges"
{-# NOINLINE hAcceptRanges #-}

hAge :: HeaderName
hAge = unsafeParseHeaderName "Age"
{-# NOINLINE hAge #-}
