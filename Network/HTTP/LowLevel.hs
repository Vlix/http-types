{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Network.HTTP.LowLevel where

import Control.Exception (assert, throw)
import Control.Monad (when)
import Control.Monad.ST (stToIO)
import Data.Array.Byte (ByteArray (..), MutableByteArray (..))
import Data.Bits ((.&.))
import qualified Data.ByteString as B
import Data.ByteString.Internal (ByteString (BS), w2c)
import Data.STRef (modifySTRef, newSTRef, readSTRef)
import Foreign (withForeignPtr)
import GHC.Exts (
    Addr#,
    Int (..),
    Int#,
    Ptr (..),
    RealWorld,
    Word8#,
    copyByteArrayToAddr#,
    eqWord8#,
    indexWord8Array#,
    indexWord8OffAddr#,
    isTrue#,
    newByteArray#,
    or64#,
    sizeofByteArray#,
    uncheckedShiftL64#,
    unsafeFreezeByteArray#,
    writeWord8Array#,
 )
import GHC.ST (ST (..))
import GHC.Word (Word64 (..), Word8 (..))
import Network.HTTP.Header.Internal

data RawAddr = RawAddr Addr#

strictIndex :: RawAddr
strictIndex =
    RawAddr
        "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\
        \\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\
        \\xFF\x21\xFF\x23\x24\x25\x26\x27\xFF\xFF\x2A\x2B\xFF\x2D\x2E\xFF\
        \\x30\x31\x32\x33\x34\x35\x36\x37\x38\x39\xFF\xFF\xFF\xFF\xFF\xFF\
        \\xFF\x61\x62\x63\x64\x65\x66\x67\x68\x69\x6A\x6B\x6C\x6D\x6E\x6F\
        \\x70\x71\x72\x73\x74\x75\x76\x77\x78\x79\x7A\xFF\xFF\xFF\x5E\x5F\
        \\x60\x61\x62\x63\x64\x65\x66\x67\x68\x69\x6A\x6B\x6C\x6D\x6E\x6F\
        \\x70\x71\x72\x73\x74\x75\x76\x77\x78\x79\x7A\xFF\x7C\xFF\x7E\xFF\
        \\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\
        \\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\
        \\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\
        \\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\
        \\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\
        \\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\
        \\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\
        \\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"#
{-# INLINE strictIndex #-}

indexWord8Array :: ByteArray -> Int -> Word8
indexWord8Array (ByteArray ba) (I# ix) =
    W8# (indexWord8Array# ba ix)
{-# INLINE indexWord8Array #-}

indexWord8OffRawAddr :: RawAddr -> Int -> Word8#
indexWord8OffRawAddr (RawAddr addr#) (I# i#) =
    indexWord8OffAddr# addr# i#
{-# INLINE indexWord8OffRawAddr #-}

newByteArray :: Int -> ST s (MutableByteArray s)
newByteArray (I# len) = ST $ \s ->
    case newByteArray# len s of
        (# s2, mba #) -> (# s2, MutableByteArray mba #)
{-# INLINE newByteArray #-}

writeWord8Array :: MutableByteArray s -> Int# -> Word8# -> ST s ()
writeWord8Array (MutableByteArray mba) ix byte = ST $ \s ->
    case writeWord8Array# mba ix byte s of
        s2 -> (# s2, () #)
{-# INLINE writeWord8Array #-}

unsafeFreezeByteArray :: MutableByteArray s -> ST s ByteArray
unsafeFreezeByteArray (MutableByteArray mba) = ST $ \s ->
    case unsafeFreezeByteArray# mba s of
        (# s2, ba #) -> (# s2, ByteArray ba #)
{-# INLINE unsafeFreezeByteArray #-}

withNewByteArray :: ByteString -> (Ptr Word8 -> MutableByteArray RealWorld -> ST RealWorld a) -> IO a
withNewByteArray (BS fptr size) f =
    withForeignPtr fptr $ \ptr ->
        stToIO $ do
            mba <- newByteArray size
            f ptr mba
{-# INLINE withNewByteArray #-}

-- | @src offset dst length@
copyByteArrayToAddr :: ByteArray -> Ptr Word8 -> ST s ()
copyByteArrayToAddr (ByteArray ba) (Ptr ptr) =
    ST $ \s ->
        case copyByteArrayToAddr# ba 0# ptr len s of
            s2 -> (# s2, () #)
  where
    len = sizeofByteArray# ba
{-# INLINE copyByteArrayToAddr #-}

isBadChar :: Word8 -> Bool
isBadChar char =
    W8# (indexWord8OffRawAddr strictIndex (fromIntegral char)) == 0xFF
{-# INLINE isBadChar #-}

toHeaderNameHelper :: RawAddr -> Addr# -> Int# -> (# Word8#, Word8#, Int# #)
toHeaderNameHelper index addr charIx =
    (# originalChar, convertedChar, nextLen #)
  where
    !(I# nextLen) = I# charIx + 1
    originalChar = indexWord8OffAddr# addr charIx
    convertedChar = indexWord8OffRawAddr index (fromIntegral (W8# originalChar))
{-# INLINE toHeaderNameHelper #-}

-- | UNSAFE HELPER FUNCTION
--
-- Should ONLY be used with bytestring longer than 64 bytes.
--
-- NEVER INLINE! As this is the exception to the norm
longHeader :: ByteString -> IO (ByteArray, Bitmap)
longHeader bs =
    assert (size > 64) $
        withNewByteArray bs $ \ptr mba -> do
            mkBitmapRef <- newSTRef (id :: Bitmap -> Bitmap)
            go mkBitmapRef ptr mba
  where
    !(W64# zero#) = 0
    !(W64# one#) = 1
    size = B.length bs
    !(I# finalShift#) = 64 - (size .&. 0xBF) -- bitmask of (0011 1111)
    go mkBitmapRef (Ptr addr#) mba =
        loop zero# 0#
      where
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
                    mkBitmap <- readSTRef mkBitmapRef
                    let finalBitmap = newBitmap `uncheckedShiftL64#` finalShift#
                        !finishedBitmap =
                            mkBitmap $ OneWord (W64# finalBitmap)
                    pure (ba, finishedBitmap)
                else do
                    let nextBitmap# = newBitmap `uncheckedShiftL64#` 1#
                    W64# newBitmap# <- updateRef (W64# nextBitmap#)
                    loop newBitmap# nextLen#
          where
            !(# originalChar#, convertedChar#, nextLen# #) =
                toHeaderNameHelper strictIndex addr# ix#
            updateRef nextBitmap
                | I# nextLen# .&. 0xBF == 0 =
                    0 <$ modifySTRef mkBitmapRef (. MoreWords nextBitmap)
                | otherwise = pure nextBitmap
{-# NOINLINE longHeader #-}

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
