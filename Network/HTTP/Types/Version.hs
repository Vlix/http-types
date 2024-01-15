{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Types and constants to describe the HTTP version.
module Network.HTTP.Types.Version (
    HttpVersion (..),
    http09,
    http10,
    http11,
    http20,
    http30,
    pattern Http09,
    pattern Http10,
    pattern Http11,
    pattern Http20,
    pattern Http30,
    parseHttpVersion,
    renderHttpVersion,
) where

import Data.ByteString as B (ByteString, foldl', null, span, stripPrefix, uncons)
import Data.ByteString.Char8 as B8 (cons, pack)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Word8 (isDigit, _0, _period)
import GHC.Generics (Generic)

-- | HTTP Version.
--
-- Note that the 'Show' instance is intended merely for debugging.
data HttpVersion = HttpVersion
    { httpMajor :: !Int
    , httpMinor :: !Int
    }
    deriving
        ( Eq
        , Ord
        , Typeable
        , -- | @since 0.12.4
          Data
        , -- | @since 0.12.4
          Generic
        )

-- | >>> show http11
-- "HTTP/1.1"
instance Show HttpVersion where
    show (HttpVersion major minor) = "HTTP/" ++ show major ++ "." ++ show minor

-- | HTTP 0.9
http09 :: HttpVersion
http09 = HttpVersion 0 9

pattern Http09 :: HttpVersion
pattern Http09 <- HttpVersion 0 9

-- | HTTP 1.0
http10 :: HttpVersion
http10 = HttpVersion 1 0

pattern Http10 :: HttpVersion
pattern Http10 <- HttpVersion 1 0

-- | HTTP 1.1
http11 :: HttpVersion
http11 = HttpVersion 1 1

pattern Http11 :: HttpVersion
pattern Http11 <- HttpVersion 1 1

-- | HTTP 2.0
--
-- @since 0.10
http20 :: HttpVersion
http20 = HttpVersion 2 0

pattern Http20 :: HttpVersion
pattern Http20 <- HttpVersion 2 0

-- | HTTP 3.0
--
-- @since 0.12.5
http30 :: HttpVersion
http30 = HttpVersion 3 0

pattern Http30 :: HttpVersion
pattern Http30 <- HttpVersion 3 0

-- | Attempt to parse a 'ByteString' as an 'HttpVersion'.
--
-- If there is no dot and minor version, then a minor version of
-- zero is implied.
--
-- === __Examples__
--
-- > parseHttpVersion "HTTP/1.1"  == Right (HttpVersion 1 1)
-- > parseHttpVersion "HTTP/2"    == Right (HttpVersion 2 0)
-- > parseHttpVersion "Hello"     == Left "Not an HTTP protocol version"
-- > parseHttpVersion "HTTP:2"    == Left "Not an HTTP protocol version"
-- > parseHttpVersion "HTTP/TWO"  == Left "No HTTP protocol major version provided"
-- > parseHttpVersion "HTTP/2DOT" == Left "Expected '.' after first digit(s)"
-- > parseHttpVersion "HTTP/2."   == Left "No HTTP protocol minor version provided"
-- > parseHttpVersion "HTTP/2.@"  == Left "Unexpected bytes after HTTP minor version"
--
-- @since 0.12.5
parseHttpVersion :: ByteString -> Either String HttpVersion
parseHttpVersion bs =
    case B.stripPrefix "HTTP/" bs of
        Nothing -> Left "Not an HTTP protocol version"
        Just rest ->
            withDigits "major" rest $ \majV more ->
                HttpVersion majV <$> getMinorVersion more
  where
    withDigits s rest f =
        case B.span isDigit rest of
            ("", _) -> Left $ "No HTTP protocol " <> s <> " version provided"
            (ds, more) -> f (unsafeDigitsToInt ds) more
    getMinorVersion =
        maybe (pure 0) go . B.uncons
      where
        go (w8, final)
            | w8 /= _period = Left "Expected '.' after first digit(s)"
            | otherwise =
                withDigits "minor" final $ \minV extra ->
                    if B.null extra
                        then pure minV
                        else Left "Unexpected bytes after HTTP minor version"

unsafeDigitsToInt :: ByteString -> Int
unsafeDigitsToInt = B.foldl' go 0
  where
    go i w8 = 10 * i + fromIntegral (w8 - _0)

-- | Convert an 'HttpVersion' to a 'ByteString'.
--
-- If the minor version is zero, it is not rendered.
--
-- > renderHttpVersion http11 == "HTTP/1.1"
-- > renderHttpVersion http20 == "HTTP/2"
--
-- @since 0.12.5
renderHttpVersion :: HttpVersion -> ByteString
renderHttpVersion httpV =
    "HTTP/"
        <> toBS majV
        <> if minV == 0
            then ""
            else B8.cons '/' $ toBS minV
  where
    majV = httpMajor httpV
    minV = httpMinor httpV
    toBS = B8.pack . show
