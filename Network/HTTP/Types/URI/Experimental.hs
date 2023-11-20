{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Types.URI.Experimental
(
  -- * Query string
  QueryItem
, Query
, SimpleQueryItem
, SimpleQuery
, simpleQueryToQuery
, renderQuery
, renderQueryBuilder
, renderSimpleQuery
, parseQuery
, parseQueryReplacePlus
, parseSimpleQuery
  -- **Escape only parts
, renderQueryPartialEscape
, renderQueryBuilderPartialEscape
, EscapeItem(..)
, PartialEscapeQueryItem
, PartialEscapeQuery
  -- ** Text query string (UTF8 encoded)
, QueryText
, queryTextToQuery
, queryToQueryText
, renderQueryText
, parseQueryText
  -- * Path segments
, encodePathSegments
, decodePathSegments
, encodePathSegmentsRelative
  -- * Path (segments + query string)
, extractPath
, encodePath
, decodePath
  -- * URL encoding / decoding
, urlEncodeBuilder
, urlEncode
, urlDecode
)
where

import           Control.Arrow
import           Data.Bits
import           Data.Char (ord)
-- import           Data.Functor ((<&>))
import           Data.Maybe
#if __GLASGOW_HASKELL__ < 710
import           Data.Monoid
#endif
import           Data.Text                      (Text)
import           Data.Text.Encoding             (encodeUtf8, decodeUtf8With)
import           Data.Text.Encoding.Error       (lenientDecode)
import           Data.Word
import qualified Data.ByteString                as B
import qualified Data.ByteString.Unsafe         as B (unsafeDrop, unsafeTake)
import qualified Data.ByteString.Builder        as B
import qualified Data.ByteString.Lazy           as BL
import           Data.ByteString.Char8          () {-IsString-}
import           GHC.Exts
import           GHC.Word                       (Word8 (W8#))

-- | Query item
type QueryItem = (B.ByteString, Maybe B.ByteString)

-- | Query.
--
-- General form: @a=b&c=d@, but if the value is Nothing, it becomes
-- @a&c=d@.
type Query = [QueryItem]

-- | Like Query, but with 'Text' instead of 'B.ByteString' (UTF8-encoded).
type QueryText = [(Text, Maybe Text)]

-- | Convert 'QueryText' to 'Query'.
queryTextToQuery :: QueryText -> Query
queryTextToQuery = map $ encodeUtf8 *** fmap encodeUtf8

-- | Convert 'QueryText' to a 'B.Builder'.
renderQueryText :: Bool -- ^ prepend a question mark?
                -> QueryText
                -> B.Builder
renderQueryText b = renderQueryBuilder b . queryTextToQuery

-- | Convert 'Query' to 'QueryText' (leniently decoding the UTF-8).
queryToQueryText :: Query -> QueryText
queryToQueryText =
    map $ go *** fmap go
  where
    go = decodeUtf8With lenientDecode

-- | Parse 'QueryText' from a 'B.ByteString'. See 'parseQuery' for details.
parseQueryText :: B.ByteString -> QueryText
parseQueryText = queryToQueryText . parseQuery

-- | Simplified Query item type without support for parameter-less items.
type SimpleQueryItem = (B.ByteString, B.ByteString)

-- | Simplified Query type without support for parameter-less items.
type SimpleQuery = [SimpleQueryItem]

-- | Convert 'SimpleQuery' to 'Query'.
simpleQueryToQuery :: SimpleQuery -> Query
simpleQueryToQuery = map (second Just)

-- | Convert 'Query' to a 'Builder'.
renderQueryBuilder :: Bool -- ^ prepend a question mark?
                   -> Query
                   -> B.Builder
renderQueryBuilder _ [] = mempty
renderQueryBuilder qmark' query =
    a <> foldr go mempty query
  where
    a = if qmark' then qmark else mempty
    qmark = B.word8 63 -- ?
    amp = B.word8 38 -- &
    equal = B.word8 61 -- =
    encode = urlEncodeBuilder True
    go (k, mv) acc =
        amp <> encode k <> maybe mempty (\v -> equal <> encode v) mv <> acc

-- | Convert 'Query' to 'ByteString'.
renderQuery :: Bool -- ^ prepend question mark?
            -> Query -> B.ByteString
renderQuery qm = BL.toStrict . B.toLazyByteString . renderQueryBuilder qm

-- | Convert 'SimpleQuery' to 'ByteString'.
renderSimpleQuery :: Bool -- ^ prepend question mark?
                  -> SimpleQuery -> B.ByteString
renderSimpleQuery qm = renderQuery qm . simpleQueryToQuery

-- | Split out the query string into a list of keys and values. A few
-- importants points:
--
-- * The result returned is still bytestrings, since we perform no character
-- decoding here. Most likely, you will want to use UTF-8 decoding, but this is
-- left to the user of the library.
--
-- * Percent decoding errors are ignored. In particular, @"%Q"@ will be output as
-- @"%Q"@.
--
-- * It decodes @\'+\'@ characters to @\' \'@
parseQuery :: B.ByteString -> Query
parseQuery = parseQueryReplacePlus True
{-
-- | Same functionality as 'parseQuery' with the option to decode @\'+\'@ characters to @\' \'@
-- or preserve @\'+\'@
parseQueryReplacePlus :: Bool -> B.ByteString -> Query
parseQueryReplacePlus replacePlus bs =
    parseQueryString' $ dropFirst 63 bs -- 63 == '?'
  where
    parseQueryString' q | B.null q = []
    parseQueryString' q =
      where
        parsePair x =
 -}

-- | Same functionality as 'parseQuery' with the option to decode @\'+\'@ characters to @\' \'@
-- or preserve @\'+\'@
parseQueryReplacePlus :: Bool -> B.ByteString -> Query
parseQueryReplacePlus replacePlus bs =
    parseQueryString' $ dropFirst 63 bs
  where
    parseQueryString' q | B.null q = []
    parseQueryString' q =
        let (x, xs) = breakDiscard (\w8 -> w8 == 38 || w8 == 59) mempty id q
         in parsePair x : parseQueryString' xs
      where
        parsePair x =
            let res = breakDiscard (== 61) Nothing Just x
             in urlDecode replacePlus *** fmap (urlDecode replacePlus) $ res

-- | Break the second bytestring at the first occurrence of any bytes from
-- the first bytestring, discarding that byte.
breakDiscard ::
    (Word8 -> Bool) ->
    a ->
    (B.ByteString -> a) ->
    B.ByteString ->
    (B.ByteString, a)
breakDiscard p def f s =
    case B.findIndex p s of
        Nothing -> (s, def)
        Just i ->
            let !a = B.unsafeTake i s
                !b = B.unsafeDrop (i + 1) s
             in (a, f b)
{-# INLINE breakDiscard #-}

-- | Parse 'SimpleQuery' from a 'ByteString'.
parseSimpleQuery :: B.ByteString -> SimpleQuery
parseSimpleQuery = map (second $ fromMaybe B.empty) . parseQuery

ord8 :: Char -> Word8
ord8 = fromIntegral . ord

unreservedQS, unreservedPI :: [Word8]
unreservedQS = map ord8 "-_.~"
unreservedPI = map ord8 "-_.~:@&=+$,"

-- | Percent-encoding for URLs.
urlEncodeBuilder' :: [Word8] -> B.ByteString -> B.Builder
urlEncodeBuilder' extraUnreserved =
    B.foldl' (\acc w8 -> acc <> encodeChar w8) mempty
  where
    encodeChar ch | unreserved ch = B.word8 ch
                  | otherwise     = h2 ch

    unreserved ch =
      isAlphaNum ch || ch `elem` extraUnreserved

    -- must be upper-case
    h2 v = B.word8 37 `mappend` B.word8 (h a) `mappend` B.word8 (h b) -- 37 = %
        where (a, b) = v `divMod` 16
    h i | i < 10    = 48 + i -- zero (0)
        | otherwise = 65 + i - 10 -- 65: A

{-
-- | Percent-encoding for URLs.
urlEncodeBuilder' :: [Word8] -> B.ByteString -> B.Builder
urlEncodeBuilder' extraUnreserved = mconcat . map encodeChar . B.unpack
    where
      encodeChar ch | unreserved ch = B.word8 ch
                    | otherwise     = h2 ch

      unreserved ch =
        isAlphaNum ch || ch `elem` extraUnreserved

      -- must be upper-case
      h2 v = B.word8 37 `mappend` B.word8 (h a) `mappend` B.word8 (h b) -- 37 = %
          where (a, b) = v `divMod` 16
      h i | i < 10    = 48 + i -- zero (0)
          | otherwise = 65 + i - 10 -- 65: A
 -}
-- | Percent-encoding for URLs (using 'B.Builder').
urlEncodeBuilder
    :: Bool -- ^ Whether input is in query string. True: Query string, False: Path element
    -> B.ByteString
    -> B.Builder
urlEncodeBuilder True  = urlEncodeBuilder' unreservedQS
urlEncodeBuilder False = urlEncodeBuilder' unreservedPI

-- | Percent-encoding for URLs.
urlEncode :: Bool -- ^ Whether to decode @\'+\'@ to @\' \'@
          -> B.ByteString -- ^ The ByteString to encode as URL
          -> B.ByteString -- ^ The encoded URL
urlEncode q = BL.toStrict . B.toLazyByteString . urlEncodeBuilder q
{-
-- | Percent-decoding.
urlDecode :: Bool -- ^ Whether to decode @\'+\'@ to @\' \'@
          -> B.ByteString -> B.ByteString
urlDecode replacePlus =
    BL.toStrict . B.toLazyByteString . mconcat . go
  where
    breakF = B.break (\w8 -> w8 == 37 || isPlus w8)
    isPlus w8
        | replacePlus = w8 == 43
        | otherwise = True
    go bs =
        case breakF bs of
            (chunk, rest) -> B.byteString chunk :
                case B.uncons rest of
                    Nothing -> mempty
                    Just (43, ws) -> B.word8 32 : go ws -- plus to space
                    Just (37, ws) ->
                        let (w8, w8s) =
                                fromMaybe (37, ws) $ do -- percent
                                    (x, xs) <- B.uncons ws
                                    x' <- hexVal x
                                    (y, ys) <- B.uncons xs
                                    y' <- hexVal y
                                    pure (combine x' y', ys)
                         in B.word8 w8 : go w8s
                    Just _ -> go rest
    hexVal w
        | 48 <= w && w <= 57  = Just $ w - 48 -- 0 - 9
        | 65 <= w && w <= 70  = Just $ w - 55 -- A - F
        | 97 <= w && w <= 102 = Just $ w - 87 -- a - f
        | otherwise = Nothing
    combine :: Word8 -> Word8 -> Word8
    combine a b = shiftL a 4 .|. b
 -}

-- | Percent-decoding.
urlDecode :: Bool -- ^ Whether to decode @\'+\'@ to @\' \'@
          -> B.ByteString -> B.ByteString
urlDecode replacePlus z = fst $ B.unfoldrN (B.length z) go z
  where
    go bs =
        case B.uncons bs of
            Nothing -> Nothing
            Just (43, ws) | replacePlus -> Just (32, ws) -- plus to space
            Just (37, ws) | Just res <- decodeHex ws -> Just res -- percent
            Just (w, ws) -> Just (w, ws)
    decodeHex bs = do
        (x, xs) <- B.uncons bs
        (y, ys) <- B.uncons xs
        new <- hexToW8 x y
        pure (new, ys)

-- > Copied this from the 'base16' package
hexToW8 :: Word8 -> Word8 -> Maybe Word8
hexToW8 x y
    | x == 0xff || y == 0xff = Nothing
    | otherwise = Just $ a .|. b
  where
    !a = aix x hi
    !b = aix y lo

    --     \NUL\SOH\STX\ETX\EOT\ENQ\ACK\a  \b  \t  \n  \v  \f  \r  \SO \SI \DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\EM \SUB\ESC\FS \GS \RS \US     !   \"  #   $   %   &   '   (   )   *   +   ,   -   .   /   0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ?   @   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O   P   Q   R   S   T   U   V   W   X   Y   Z   [   \\  ]   ^   _   `   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o   p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~   \DEL
    !lo = "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\xff\xff\xff\xff\xff\xff\xff\x0a\x0b\x0c\x0d\x0e\x0f\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x0b\x0c\x0d\x0e\x0f\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#
    !hi = "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x10\x20\x30\x40\x50\x60\x70\x80\x90\xff\xff\xff\xff\xff\xff\xff\xa0\xb0\xc0\xd0\xe0\xf0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xa0\xb0\xc0\xd0\xe0\xf0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

isAlphaNum :: Word8 -> Bool
isAlphaNum w8 =
    aix w8 lo /= 0xff
  where
    --     \NUL\SOH\STX\ETX\EOT\ENQ\ACK\a  \b  \t  \n  \v  \f  \r  \SO \SI \DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\EM \SUB\ESC\FS \GS \RS \US     !   \"  #   $   %   &   '   (   )   *   +   ,   -   .   /   0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ?   @   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O   P   Q   R   S   T   U   V   W   X   Y   Z   [   \\  ]   ^   _   `   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o   p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~   \DEL
    !lo = "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\xff\xff\xff\xff\xff\xff\xff\x0a\x0b\x0c\x0d\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x20\x21\x22\x23\x24\x25\x26\x27\x28\x29\xff\xff\xff\xff\xff\xff\x0a\x0b\x0c\x0d\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x20\x21\x22\x23\x24\x25\x26\x27\x28\x29\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

-- | Read 'Word8' index off alphabet addr
--
-- > Copied this from the 'base16' package
aix :: Word8 -> Addr# -> Word8
aix w alpha = W8# (indexWord8OffAddr# alpha i)
  where
    !(I# i) = fromIntegral w
{-# INLINE aix #-}

-- | Encodes a list of path segments into a valid URL fragment.
--
-- This function takes the following three steps:
--
-- * UTF-8 encodes the characters.
--
-- * Performs percent encoding on all unreserved characters, as well as @\:\@\=\+\$@,
--
-- * Prepends each segment with a slash.
--
-- For example:
--
-- > encodePathSegments [\"foo\", \"bar\", \"baz\"]
-- \"\/foo\/bar\/baz\"
--
-- > encodePathSegments [\"foo bar\", \"baz\/bin\"]
-- \"\/foo\%20bar\/baz\%2Fbin\"
--
-- > encodePathSegments [\"שלום\"]
-- \"\/%D7%A9%D7%9C%D7%95%D7%9D\"
--
-- Huge thanks to Jeremy Shaw who created the original implementation of this
-- function in web-routes and did such thorough research to determine all
-- correct escaping procedures.
encodePathSegments :: [Text] -> B.Builder
encodePathSegments = encodePathSegmentsRaw . map encodeUtf8

encodePathSegmentsRaw :: [B.ByteString] -> B.Builder
encodePathSegmentsRaw = encodePathSegments' True

-- | Like 'encodePathSegments', but without the initial slash.
encodePathSegmentsRelative :: [Text] -> B.Builder
encodePathSegmentsRelative =
    encodePathSegmentsRelativeRaw . map encodeUtf8

encodePathSegmentsRelativeRaw :: [B.ByteString] -> B.Builder
encodePathSegmentsRelativeRaw = encodePathSegments' False

encodePathSegments' :: Bool -> [B.ByteString] -> B.Builder
encodePathSegments' _ [] = mempty
encodePathSegments' firstSlash paths@(b:bs)
    | firstSlash = addSlashes paths
    | otherwise = encode b `mappend` addSlashes bs
  where
    encode = urlEncodeBuilder False
    addSlashes = foldr (\x -> mappend (B.byteString "/" `mappend` encode x)) mempty

-- | Parse a list of path segments from a valid URL fragment.
decodePathSegments :: B.ByteString -> [Text]
decodePathSegments =
    fmap (decodeUtf8With lenientDecode) . decodePathSegmentsRaw

-- | Parse a list of path segments from a valid URL fragment
-- without decoding the 'ByteString's to 'Text'
decodePathSegmentsRaw :: B.ByteString -> [B.ByteString]
decodePathSegmentsRaw "" = []
decodePathSegmentsRaw "/" = []
decodePathSegmentsRaw a =
    go $ dropFirst 47 a -- 47 == '/'
  where
    decode = urlDecode False
    go bs =
        let (k, v) = breakDiscard (== 47) [] go bs
         in decode k : v

dropFirst :: Word8 -> B.ByteString -> B.ByteString
dropFirst w8 bs =
    case B.uncons bs of
        Just (w, bs') | w == w8 -> bs'
        _ -> bs
{-# INLINE dropFirst #-}

-- | Extract whole path (path segments + query) from a
-- <http://tools.ietf.org/html/rfc2616#section-5.1.2 RFC 2616 Request-URI>.
--
-- >>> extractPath "/path"
-- "/path"
--
-- >>> extractPath "http://example.com:8080/path"
-- "/path"
--
-- >>> extractPath "http://example.com"
-- "/"
--
-- >>> extractPath ""
-- "/"
extractPath :: B.ByteString -> B.ByteString
extractPath = ensureNonEmpty . extract
  where
    extract path
      | "http://"  `B.isPrefixOf` path = breakOnSlash $ B.drop 7 path
      | "https://" `B.isPrefixOf` path = breakOnSlash $ B.drop 8 path
      | otherwise                      = path
    breakOnSlash = B.dropWhile (/= 47)
    ensureNonEmpty "" = "/"
    ensureNonEmpty p  = p

-- | Encode a whole path (path segments + query).
encodePath :: [Text] -> Query -> B.Builder
encodePath x [] = encodePathSegments x
encodePath x y = encodePathSegments x `mappend` renderQueryBuilder True y

-- | Decode a whole path (path segments + query).
decodePath :: B.ByteString -> ([Text], Query)
decodePath b =
    let (x, y) = B.break (== 63) b -- question mark
    in (decodePathSegments x, parseQuery y)

-----------------------------------------------------------------------------------------

-- | For some URIs characters must not be URI encoded,
-- e.g. @\'+\'@ or @\':\'@ in @q=a+language:haskell+created:2009-01-01..2009-02-01&sort=stars@
-- The character list unreservedPI instead of unreservedQS would solve this.
-- But we explicitly decide what part to encode.
-- This is mandatory when searching for @\'+\'@: @q=%2B+language:haskell@.
data EscapeItem = QE B.ByteString -- will be URL encoded
                | QN B.ByteString -- will not be url encoded, e.g. @\'+\'@ or @\':\'@
    deriving (Show, Eq, Ord)

-- | Query item
type PartialEscapeQueryItem = (B.ByteString, [EscapeItem])

-- | Query with some chars that should not be escaped.
--
-- General form: @a=b&c=d:e+f&g=h@
type PartialEscapeQuery = [PartialEscapeQueryItem]

-- | Convert 'PartialEscapeQuery' to 'ByteString'.
renderQueryPartialEscape :: Bool -- ^ prepend question mark?
            -> PartialEscapeQuery -> B.ByteString
renderQueryPartialEscape qm = BL.toStrict . B.toLazyByteString . renderQueryBuilderPartialEscape qm

-- | Convert 'PartialEscapeQuery' to a 'Builder'.
renderQueryBuilderPartialEscape :: Bool -- ^ prepend a question mark?
                   -> PartialEscapeQuery
                   -> B.Builder
renderQueryBuilderPartialEscape _ [] = mempty
-- FIXME: replace mconcat + map with foldr
renderQueryBuilderPartialEscape qmark' (p:ps) = mconcat
    $ go (if qmark' then qmark else mempty) p
    : map (go amp) ps
  where
    qmark = B.byteString "?"
    amp = B.byteString "&"
    equal = B.byteString "="
    go sep (k, mv) = mconcat [
                      sep
                     , urlEncodeBuilder True k
                     , case mv of
                         [] -> mempty
                         vs -> equal `mappend` mconcat (map encode vs)
                     ]
    encode (QE v) = urlEncodeBuilder True v
    encode (QN v) = B.byteString v
