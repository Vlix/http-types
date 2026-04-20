{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Network.HTTP.LowLevel where

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
    Word8#,
    copyByteArrayToAddr#,
    indexWord8Array#,
    indexWord8OffAddr#,
    newByteArray#,
    sizeofByteArray#,
    unsafeFreezeByteArray#,
    writeWord8Array#,
 )
import GHC.ST (ST (..))
import GHC.Word (Word8 (..))

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

isMod64 :: Int -> Bool
isMod64 i = i .&. 0xBF == 0
{-# INLINE isMod64 #-}

finalShift :: Int -> Int
finalShift size = 64 - (size .&. 0xBF) -- bitmask of (0011 1111)
{-# INLINE finalShift #-}
