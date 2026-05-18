{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Network.HTTP.LowLevel where

import Control.Exception (assert)
import Control.Monad.ST (stToIO)
import Data.Array.Byte (ByteArray (..), MutableByteArray (..))
import Data.Bits ((.&.))
import Data.ByteString.Internal (ByteString (BS))
import Foreign (withForeignPtr)
import GHC.Exts (
    Addr#,
    Int (..),
    Int#,
    Ptr (..),
    RealWorld,
    Word64#,
    Word8#,
    copyByteArrayToAddr#,
    eqWord8#,
    indexWord8Array#,
    indexWord8OffAddr#,
    isTrue#,
    newByteArray#,
    or64#,
    sizeofByteArray#,
    unsafeFreezeByteArray#,
    writeWord8Array#,
 )
import GHC.ST (ST (..))
import GHC.Word (Word64 (..), Word8 (..))

-- | Carrier for a raw 'Addr#'
data RawAddr = RawAddr Addr#

-- | A 64-byte mapping of which bytes in the 8-bit range are valid and
-- what to map them to when producing case-insensitive 'ByteArray's.
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

-- | Get a byte from a specific index
indexWord8Array :: ByteArray -> Int -> Word8
indexWord8Array (ByteArray ba) (I# ix) =
    W8# (indexWord8Array# ba ix)
{-# INLINE indexWord8Array #-}

-- | Look up a byte using a specific index
indexWord8OffRawAddr :: RawAddr -> Int -> Word8#
indexWord8OffRawAddr (RawAddr addr#) (I# i#) =
    indexWord8OffAddr# addr# i#
{-# INLINE indexWord8OffRawAddr #-}

-- | Creating a new 'ByteArray'
newByteArray :: Int -> ST s (MutableByteArray s)
newByteArray (I# len) = ST $ \s ->
    case newByteArray# len s of
        (# s2, mba #) -> (# s2, MutableByteArray mba #)
{-# INLINE newByteArray #-}

-- | Set a byte of a specific index in a 'MutableByteArray'
writeWord8Array :: MutableByteArray s -> Int# -> Word8# -> ST s ()
writeWord8Array (MutableByteArray mba) ix byte = ST $ \s ->
    case writeWord8Array# mba ix byte s of
        s2 -> (# s2, () #)
{-# INLINE writeWord8Array #-}

-- | Finish a 'ByteArray'
unsafeFreezeByteArray :: MutableByteArray s -> ST s ByteArray
unsafeFreezeByteArray (MutableByteArray mba) = ST $ \s ->
    case unsafeFreezeByteArray# mba s of
        (# s2, ba #) -> (# s2, ByteArray ba #)
{-# INLINE unsafeFreezeByteArray #-}

-- | Convenience function to create a 'ByteArray' from a 'ByteString'
withNewByteArray :: ByteString -> (Ptr Word8 -> MutableByteArray RealWorld -> ST RealWorld a) -> IO a
withNewByteArray (BS fptr size) f =
    withForeignPtr fptr $ \ptr ->
        stToIO $ do
            mba <- newByteArray size
            f ptr mba
{-# INLINE withNewByteArray #-}

-- | Amount of bytes in 'ByteArray'
sizeOfByteArray :: ByteArray -> Int
sizeOfByteArray (ByteArray arr) = I# (sizeofByteArray# arr)
{-# INLINE sizeOfByteArray #-}

-- | Copy a 'ByteArray' into a 'Ptr' (e.g. when creating a 'ByteString')
--
-- @src offset dst length@
copyByteArrayToAddr :: ByteArray -> Ptr Word8 -> ST s ()
copyByteArrayToAddr (ByteArray ba) (Ptr ptr) =
    ST $ \s ->
        case copyByteArrayToAddr# ba 0# ptr len s of
            s2 -> (# s2, () #)
  where
    len = sizeofByteArray# ba
{-# INLINE copyByteArrayToAddr #-}

-- | Is the byte a legal 'HeaderName' byte.
isBadChar :: Word8 -> Bool
isBadChar char =
    W8# (indexWord8OffRawAddr strictIndex (fromIntegral char)) == 0xFF
{-# INLINE isBadChar #-}

-- | Helper to grab and combine the bytes used in parsing 'HeaderName's
toHeaderNameHelper :: RawAddr -> Addr# -> Int# -> (# Word8#, Word8#, Int# #)
toHeaderNameHelper index addr charIx =
    (# originalChar, convertedChar, nextLen #)
  where
    !(I# nextLen) = I# charIx + 1
    originalChar = indexWord8OffAddr# addr charIx
    convertedChar = indexWord8OffRawAddr index (fromIntegral (W8# originalChar))
{-# INLINE toHeaderNameHelper #-}

-- | Checking if the first 6 bits of an integer are zero.
--
-- (used to quickly check if we're at the end of a 'Bitmap')
isMod64 :: Int -> Bool
isMod64 i = i .&. 0xBF == 0
{-# INLINE isMod64 #-}

-- | 'Bitmap's start at the most significant side of the word,
-- so this is the amount the final word will have to be shifted
-- given the total size of the 'ByteArray'
finalShift :: Int -> Int
finalShift size = 64 - (size .&. 0xBF) -- bitmask of (0011 1111)
{-# INLINE finalShift #-}

-- | Folding over a 'ByteArray' from right to left.
foldByteArrayR :: (Word8 -> a -> a) -> a -> ByteArray -> a
foldByteArrayR addToAcc final ba =
    assert (baLen >= 0) $
        loop final (baLen - 1)
  where
    baLen = sizeOfByteArray ba
    loop !acc ix
        | ix < 0 = acc
        | otherwise = do
            let byte = indexWord8Array ba ix
            loop (addToAcc byte acc) (ix - 1)

-- | Comparing 2 bytes for equality and settings the least
-- significant bit of a 64-bitmap to '1'.
adjustBitmap :: Word8# -> Word8# -> Word64# -> Word64#
adjustBitmap w1 w2 bitmap =
    if isTrue# (w1 `eqWord8#` w2)
        then bitmap
        else bitmap `or64#` one#
  where
    !(W64# one#) = 1
{-# INLINE adjustBitmap #-}
