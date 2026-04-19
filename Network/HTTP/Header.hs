{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

-- | A new implementation of "HTTP Headers" (HTTP Fields).
--
-- This module provides a more opaque API, so the internals don't "leak" like
-- they do in the older "Network.HTTP.Types.Header" module.
-- That module forced you to use the "Data.CaseInsensitive" API to create
-- header names, and to use list functions to go through the 'Network.HTTP.Types.Header.Headers'.
module Network.HTTP.Header (
    -- * Header Names

    -- | The part of an HTTP field before the colon:
    --
    -- @(e.g. the \"Content-Type\" part of "Content-Type: application\/json")@
    HeaderName (..),
    HeaderNameException (..),
    parseHeaderName,
    parseNewHeaderName,
    encodeHeaderName,

    -- *** Unsafe versions

    -- | These functions will throw an exception if they encounter an illegal
    -- byte in the to be parsed header name.
    unsafeParseHeaderName,
    unsafeParseNewHeaderName,

    -- ** Common Header Names
) where

-- hAcceptLanguage,

import Control.Exception (throw, try)
import Control.Monad.ST (ST, runST, stToIO)
import Data.Array.Byte (ByteArray (..))
import qualified Data.ByteString as B (ByteString, length)
import Data.ByteString.Internal (c2w, unsafeCreate, w2c)
import Data.Char (ord)
import Data.List (find)
import Foreign (Bits (..), Storable (..), plusPtr)
import GHC.Exts (
    Int (..),
    Ptr (..),
    eqWord8#,
    isTrue#,
    or64#,
    sizeofByteArray#,
    uncheckedShiftL64#,
    (+#),
 )
import GHC.IO.Unsafe (unsafeDupablePerformIO)
import GHC.Word (Word64 (..), Word8 (..))

import Control.Monad (when)
import Network.HTTP.Header.Internal
import Network.HTTP.LowLevel (
    copyByteArrayToAddr,
    indexWord8Array,
    indexWord8OffRawAddr,
    isBadChar,
    longHeader,
    newByteArray,
    strictIndex,
    toHeaderNameHelper,
    unsafeFreezeByteArray,
    withNewByteArray,
    writeWord8Array,
 )

{-# ANN module "HLint: ignore Use uncurry" #-}

-- | Creates a 'HeaderName' from the given 'B.ByteString' while holding on
-- to the original 'B.ByteString', in case of later reuse.
parseHeaderName :: B.ByteString -> Either (HeaderNameException B.ByteString) HeaderName
parseHeaderName hdr =
    parseHeaderName' hdr $
        \(ba, bitmap) ->
            HeaderName (Just hdr) ba bitmap

-- | Creates a 'HeaderName' from the given 'B.ByteString' /without/ holding on
-- to the original 'B.ByteString'.
--
-- Could lead to quicker garbage collection.
parseNewHeaderName :: B.ByteString -> Either (HeaderNameException B.ByteString) HeaderName
parseNewHeaderName hdr =
    parseHeaderName' hdr $
        \(ba, bitmap) ->
            HeaderName Nothing ba bitmap

parseHeaderName' ::
    B.ByteString ->
    ((ByteArray, Bitmap) -> HeaderName) ->
    Either (HeaderNameException B.ByteString) HeaderName
parseHeaderName' hdr mkHeader
    | size <= 0 = Left EmptyHeader
    | otherwise =
        unsafeDupablePerformIO $
            try (mkHeader <$> toHeaderNameStrict hdr)
  where
    size = B.length hdr
{-# INLINE parseHeaderName' #-}

-- | __Will throw an 'InvalidFieldNameByte' exception if the 'B.ByteString'__
-- __contains any bytes not defined in__
-- [RFC 9110](https://www.rfc-editor.org/rfc/rfc9110.html#section-5.6.2)
--
-- Creates a 'HeaderName' from the given 'B.ByteString' while holding on
-- to the original 'B.ByteString', in case of later reuse.
unsafeParseHeaderName :: B.ByteString -> HeaderName
unsafeParseHeaderName hdr =
    unsafeParseHeaderName' hdr $ \(ba, bitmap) ->
        HeaderName (Just hdr) ba bitmap

-- | __Will throw an 'InvalidFieldNameByte' exception if the 'B.ByteString'__
-- __contains any bytes not defined in__
-- [RFC 9110](https://www.rfc-editor.org/rfc/rfc9110.html#section-5.6.2)
--
-- Creates a 'HeaderName' from the given 'B.ByteString' /without/ holding on
-- to the original 'B.ByteString'.
--
-- Could lead to quicker garbage collection.
unsafeParseNewHeaderName :: B.ByteString -> HeaderName
unsafeParseNewHeaderName hdr =
    unsafeParseHeaderName' hdr $ \(ba, bitmap) ->
        HeaderName Nothing ba bitmap

unsafeParseHeaderName' ::
    B.ByteString ->
    ((ByteArray, Bitmap) -> HeaderName) ->
    HeaderName
unsafeParseHeaderName' hdr mkHeader
    | size <= 0 = throw (EmptyHeader :: HeaderNameException String)
    | otherwise =
        unsafeDupablePerformIO $
            mkHeader <$> toHeaderNameStrict hdr
  where
    size = B.length hdr
{-# INLINE unsafeParseHeaderName' #-}

toHeaderNameStrict :: B.ByteString -> IO (ByteArray, Bitmap)
toHeaderNameStrict bs
    | size > 64 = longHeader bs
    | otherwise = withNewByteArray bs go
  where
    !(W64# zero#) = 0
    !(W64# one#) = 1
    size = B.length bs
    !(I# finalShift#) = 64 - size
    go (Ptr addr#) mba = do
        loop zero# 0#
      where
        -- loop :: Word64# -> Int# -> ST s (ByteArray, Bitmap)
        loop bitmap# ix# = do
            when (W8# convertedChar# == 0xFF) $
                throw (InvalidFieldNameByte bs (w2c (W8# originalChar#)))
            writeWord8Array mba ix# convertedChar#
            let newBitmap =
                    if isTrue# (originalChar# `eqWord8#` convertedChar#)
                        then bitmap#
                        else bitmap# `or64#` one#
            if I# nextLen# == size
                then do
                    ba <- unsafeFreezeByteArray mba
                    let !finishedBitmap = newBitmap `uncheckedShiftL64#` finalShift#
                    pure (ba, OneWord (W64# finishedBitmap))
                else do
                    let nextBitmap# = newBitmap `uncheckedShiftL64#` 1#
                    loop nextBitmap# nextLen#
          where
            !(# originalChar#, convertedChar#, nextLen# #) =
                toHeaderNameHelper strictIndex addr# ix#
{-# INLINE toHeaderNameStrict #-}

headerNameToString :: HeaderName -> String
headerNameToString (HeaderName _ arr bm)
    | bm == 0 = undefined arr

-- | Turns the 'HeaderName' into a case-sensitive 'B.ByteString'.
--
-- Depending on how the 'HeaderName' is constructed, this might only return
-- the original 'B.ByteString' that was used to create it, or it creates a
-- 'B.ByteString' from the internal 'ByteArray' + casing bitmap.
encodeHeaderName :: HeaderName -> B.ByteString
encodeHeaderName (HeaderName (Just bs) _ _) = bs
encodeHeaderName (HeaderName _ arr@(ByteArray ba) bm) =
    unsafeCreate baLen $
        if bitmapIsZero bm
            then stToIO . copyByteArrayToAddr arr
            else loop 0 bm
  where
    baLen = I# (sizeofByteArray# ba)
    firstBit = 0x8000_0000_0000_0000
    loop ix bitmap ptr =
        let w8 = indexWord8Array arr ix
            char =
                if bitmap .&. firstBit == 0
                    then w8
                    else w8 - 0x20
            newIx = ix + 1
         in do
                poke ptr char
                if newIx == baLen
                    then pure ()
                    else loop newIx (bitmap `unsafeShiftL` 1) (ptr `plusPtr` 1)

-- hAcceptLanguage :: HeaderName
-- hAcceptLanguage = parseHeaderName "Accept-Language"

parseHeaderNameFromString :: String -> Either (HeaderNameException String) HeaderName
parseHeaderNameFromString [] =
    Left (EmptyHeader :: HeaderNameException String)
parseHeaderNameFromString s =
    case find isBadChar' s of
        Just c -> throwIllegal c
        Nothing ->
            runST $ newByteArray len >>= go
  where
    isBadChar' c
        | c > '\xFF' = True
        | otherwise = isBadChar $ c2w c
    throwIllegal c = throw (InvalidFieldNameByte s c)
    len = length s
    !(W64# zero#) = 0
    !(W64# one#) = 1
    go mba = loop zero# 0# s
      where
        loop bitmap# _ [] = do
            ba <- unsafeFreezeByteArray mba
            pure (HeaderName Nothing ba (W64# bitmap#))
        loop bitmap# ix# (c : cs)
            | charInt > 0xFF || W8# convertedChar# == 0xFF =
                throwIllegal c
            | otherwise = do
                writeWord8Array mba ix# convertedChar#
                let newBitmap# =
                        if isTrue# (originalChar# `eqWord8#` convertedChar#)
                            then bitmap#
                            else bitmap# `or64#` one#
                loop newBitmap# (ix# +# 1#) cs
          where
            charInt = ord c
            !(W8# originalChar#) = fromIntegral charInt
            convertedChar# = indexWord8OffRawAddr strictIndex (fromIntegral (W8# originalChar#))
