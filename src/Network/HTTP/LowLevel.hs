{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Network.HTTP.LowLevel where

import Control.Exception (assert)
import Data.Array.Byte (ByteArray (..), MutableByteArray (..))
import Data.Bits ((.&.))
import GHC.Exts (
    Addr#,
    Char (..),
    Int (..),
    Int#,
    Ptr (..),
    Word64#,
    Word8#,
    chr#,
    copyAddrToByteArray#,
    copyByteArrayToAddr#,
    eqWord8#,
    indexWord8Array#,
    indexWord8OffAddr#,
    int2Word#,
    isTrue#,
    newByteArray#,
    or64#,
    ord#,
    sizeofByteArray#,
    unsafeFreezeByteArray#,
    word2Int#,
    word8ToWord#,
    wordToWord8#,
    writeWord8Array#,
 )
import GHC.ST (ST (..))
import GHC.Word (Word64 (..), Word8 (..))

-- | Carrier for a raw 'Addr#'
data RawAddr = RawAddr Addr#

-- | A 256-byte mapping of which bytes in the 8-bit range are valid and
-- what to map them to when producing case-insensitive t'ByteArray's.
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

-- | A 256-byte mapping of all bytes in the 7-bit ASCII range to lower case
-- when possible. _IGNORES EVERYTHING IN THE RANGE OUTSIDE OF ASCII_
ciIndex :: RawAddr
ciIndex =
    RawAddr
        "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0A\x0B\x0C\x0D\x0E\x0F\
        \\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1A\x1B\x1C\x1D\x1E\x1F\
        \\x20\x21\x22\x23\x24\x25\x26\x27\x28\x29\x2A\x2B\x2C\x2D\x2E\x2F\
        \\x30\x31\x32\x33\x34\x35\x36\x37\x38\x39\x3A\x3B\x3C\x3D\x3E\x3F\
        \\x40\x61\x62\x63\x64\x65\x66\x67\x68\x69\x6A\x6B\x6C\x6D\x6E\x6F\
        \\x70\x71\x72\x73\x74\x75\x76\x77\x78\x79\x7A\x5B\x5C\x5D\x5E\x5F\
        \\x60\x61\x62\x63\x64\x65\x66\x67\x68\x69\x6A\x6B\x6C\x6D\x6E\x6F\
        \\x70\x71\x72\x73\x74\x75\x76\x77\x78\x79\x7A\x7B\x7C\x7D\x7E\x7F\
        \\x80\x81\x82\x83\x84\x85\x86\x87\x88\x89\x8A\x8B\x8C\x8D\x8E\x8F\
        \\x90\x91\x92\x93\x94\x95\x96\x97\x98\x99\x9A\x9B\x9C\x9D\x9E\x9F\
        \\xA0\xA1\xA2\xA3\xA4\xA5\xA6\xA7\xA8\xA9\xAA\xAB\xAC\xAD\xAE\xAF\
        \\xB0\xB1\xB2\xB3\xB4\xB5\xB6\xB7\xB8\xB9\xBA\xBB\xBC\xBD\xBE\xBF\
        \\xC0\xC1\xC2\xC3\xC4\xC5\xC6\xC7\xC8\xC9\xCA\xCB\xCC\xCD\xCE\xCF\
        \\xD0\xD1\xD2\xD3\xD4\xD5\xD6\xD7\xD8\xD9\xDA\xDB\xDC\xDD\xDE\xDF\
        \\xE0\xE1\xE2\xE3\xE4\xE5\xE6\xE7\xE8\xE9\xEA\xEB\xEC\xED\xEE\xEF\
        \\xF0\xF1\xF2\xF3\xF4\xF5\xF6\xF7\xF8\xF9\xFA\xFB\xFC\xFD\xFE\xFF"#

-- |
--
-- This mapping has been optimized to get as little as possible overlap
-- between characters while fitting all allowed characters into 16 bits.
-- (i.e. from 0x00 to 0x0F, which is used to set a bit in a bitmap later)
--
-- The polled headers were taken from [here](https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Headers>)
-- and the first four characters of each referenced header was taken and
-- sampled for occurence.
--
-- The results were as follows:
-- @
--  01 \'e\': 98
--  02 \'c\': 94
--  03 \'-\': 60
--  04 \'s\': 59
--  05 \'r\': 51
--  06 \'t\': 45
--  07 \'a\': 44
--  08 \'o\': 40
--  09 \'n\': 27
--  10 \'i\': 25
--  11 \'p\': 23
--  12 \'x\': 17
--  13 \'f\': 16
--  14 \'v\': 12
--  15 \'w\': 10
--  16 \'l\': 10
--     \'u\': 9
--     \'d\': 8
--     \'m\': 8
--     \'g\': 8
--     \'k\': 4
--     \'h\': 3
--     \'y\': 1
--     \'b\': 1
--     \'z\': 0
--     \'q\': 0
--     \'j\': 0
--  (all other special chars, none)
--  (all numbers, none)
--  -------------------------------
-- @
--
-- And they have been sectioned into
-- the following groups of 16
--
-- @
--  01 (\'e\',98)
--  02 (\'c\',94)
--  03 (\'-\',60)
--      (and all other special chars)
--  04 (\'s\',59)
--  05 (\'r\',51)
--      - (\'z\',0)
--  06 (\'t\',45)
--      - (\'q\',0)
--  07 (\'a\',44)
--      - (\'j\',0)
--  08 (\'o\',40)
--      - (\'y\',1)
--  09 (\'n\',27)
--      - (\'b\',1)
--  10 (\'i\',25)
--      - (\'h\',3)
--  11 (\'p\',23)
--      - (\'k\',4)
--  12 (\'x\',17)
--      (and all numbers)
--  13 (\'f\',16)
--      - (\'g\',8)
--  14 (\'v\',12)
--      - (\'u\',8)
--  15 (\'l\',10)
--      - (\'m\',8)
--  16 (\'w\',10)
--      - (\'d\',9)
-- @
hashIndex :: RawAddr
hashIndex =
    RawAddr
        "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\
        \\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\
        \\xFF\x02\xFF\x02\x02\x02\x02\x02\xFF\xFF\x02\x02\xFF\x02\x02\xFF\
        \\x0B\x0B\x0B\x0B\x0B\x0B\x0B\x0B\x0B\x0B\xFF\xFF\xFF\xFF\xFF\xFF\
        \\xFF\x06\x08\x01\x0F\x00\x0C\x0C\x0A\x09\x09\x0A\x0E\x0E\x08\x07\
        \\x0A\x05\x04\x03\x05\x0D\x0D\x0F\x0B\x07\x04\xFF\xFF\xFF\x02\x02\
        \\x02\x06\x08\x01\x0F\x00\x0C\x0C\x0A\x09\x09\x0A\x0E\x0E\x08\x07\
        \\x0A\x05\x04\x03\x05\x0D\x0D\x0F\x0B\x07\x04\xFF\x02\xFF\x02\xFF\
        \\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\
        \\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\
        \\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\
        \\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\
        \\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\
        \\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\
        \\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\
        \\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"#

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

-- | Creating a new t'ByteArray'
newByteArray :: Int -> ST s (MutableByteArray s)
newByteArray (I# len) = ST $ \s ->
    case newByteArray# len s of
        (# s2, mba #) -> (# s2, MutableByteArray mba #)

-- | Set a byte of a specific index in a t'MutableByteArray'.
writeWord8Array :: MutableByteArray s -> Int# -> Word8# -> ST s ()
writeWord8Array (MutableByteArray mba) ix byte = ST $ \s ->
    case writeWord8Array# mba ix byte s of
        s2 -> (# s2, () #)

-- | Finish a t'ByteArray'.
unsafeFreezeByteArray :: MutableByteArray s -> ST s ByteArray
unsafeFreezeByteArray (MutableByteArray mba) = ST $ \s ->
    case unsafeFreezeByteArray# mba s of
        (# s2, ba #) -> (# s2, ByteArray ba #)

-- | Amount of bytes in t'ByteArray'.
sizeOfByteArray :: ByteArray -> Int
sizeOfByteArray (ByteArray arr) = I# (sizeofByteArray# arr)
{-# INLINE sizeOfByteArray #-}

-- | Copy a t'ByteArray' into a t'Ptr' (e.g. when creating a ByteString)
--
-- @src offset dst length@
copyByteArrayToAddr :: ByteArray -> Ptr Word8 -> ST s ()
copyByteArrayToAddr (ByteArray ba) (Ptr ptr) =
    ST $ \s ->
        case copyByteArrayToAddr# ba 0# ptr len s of
            s2 -> (# s2, () #)
  where
    len = sizeofByteArray# ba

-- | Copy from an 'Addr#' into a t'MutableByteArray'.
--
-- @src dst offset length@
copyAddrToByteArray :: Addr# -> MutableByteArray s -> Int# -> ST s ()
copyAddrToByteArray addr (MutableByteArray mba) len = ST $ \s ->
    case copyAddrToByteArray# addr mba 0# len s of
        s2 -> (# s2, () #)

-- | Is the byte a legal 'Network.HTTP.Header.HeaderName' byte.
isBadChar :: Word8 -> Bool
isBadChar char =
    W8# (indexWord8OffRawAddr strictIndex (fromIntegral char)) == 0xFF

-- | Checking if the first 6 bits of an integer are zero.
--
-- (used to quickly check if we're at the end of a 'Network.HTTP.Header.Internal.Bitmap')
isMod64 :: Int -> Bool
isMod64 i = i .&. 0xBF == 0
{-# INLINE isMod64 #-}

-- | 'Network.HTTP.Header.Internal.Bitmap's start at the most significant side of the word,
-- so this is the amount the final word will have to be shifted
-- given the total size of the t'ByteArray'
finalShift :: Int -> Int
-- 0xBF == bitmask of (0011 1111), basically "modulo 64"
finalShift size = 64 - (size .&. 0xBF)
{-# INLINE finalShift #-}

-- | Create a 'String' from the t'ByteArray' in a streaming fashion.
unsafeByteArrayToString :: ByteArray -> String
unsafeByteArrayToString ba =
    assert (baLen >= 0) $
        loop 0
  where
    baLen = sizeOfByteArray ba
    loop ix
        | ix >= baLen = []
        | otherwise = do
            let c = w2c $ indexWord8Array ba ix
             in c : loop (ix + 1)

-- | Comparing 2 bytes for equality and settings the least
-- significant bit of a 64-bitmap to '1'.
adjustBitmap :: Word8# -> Word8# -> Word64# -> Word64#
adjustBitmap w1 w2 bitmap
    | isTrue# (w1 `eqWord8#` w2) = bitmap
    | otherwise = bitmap `or64#` one#
  where
    !(W64# one#) = 1

w2c :: Word8 -> Char
w2c (W8# w8) = C# (chr# (word2Int# (word8ToWord# w8)))
{-# INLINE w2c #-}

c2w :: Char -> Word8
c2w (C# c) = W8# (wordToWord8# (int2Word# (ord# c)))
{-# INLINE c2w #-}
