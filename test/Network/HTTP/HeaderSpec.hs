{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Network.HTTP.HeaderSpec where

import Control.Exception (throw)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B8 (any, length, pack, replicate, unpack)
import Data.Char (isUpper)
import Data.Foldable (for_)
import Data.String (IsString)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Typeable (Typeable)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..), Property, elements, listOf1, (===))

import Network.HTTP.Header

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "HeaderName" $ do
        roundTripIt "ByteString" roundTripByteString
        roundTripIt "unsafe ByteString" roundTripByteStringUnsafe
        roundTripIt "String" roundTripString
        roundTripIt "Text" roundTripText
        prop "encodes to lowercase ByteString" $
            not . B8.any isUpper . encodeHeaderNameLower
        prop "encodes to lowercase String" $
            not . any isUpper . headerNameToStringLower
        equalEncoding "ByteString <==> String" encodeHeaderName headerNameToString
        equalEncoding "ByteString <==> String (lower)" encodeHeaderNameLower headerNameToStringLower
        describe "Constants" $
            for_ allConstants staticHeaderCheck
  where
    equalEncoding s f g = prop s $ \hdr -> B8.unpack (f hdr) === g hdr
    roundTripIt ::
        (Arbitrary (AllowedHeaderName a), Show a, IsString a) =>
        String ->
        (a -> (Expectation, Property)) ->
        SpecWith (Arg (IO ()))
    roundTripIt s f = do
        it name $ do
            fst $ f "TE"
            fst $ f "Accept-Language"
            fst $ f "X-Permitted-Cross-Domain-Policies"
            -- (sic) the upper case characters at the end are for testing bitmaps > 1x Word64
            fst $ f "Some-Weird-Header-That-For-Some-Reason-Is-Longer-Than-64-ChaRaCtErS"
            fst $ f "test-with-lowercase"
        prop name $ snd . f . getHeaderName
      where
        name = "roundtrips " <> s <> "s correctly"

roundTripByteString :: ByteString -> (Expectation, Property)
roundTripByteString = headerRoundtrip parseHeaderName id

roundTripString :: String -> (Expectation, Property)
roundTripString = headerRoundtrip parseHeaderNameFromString B8.unpack

roundTripText :: Text -> (Expectation, Property)
roundTripText = headerRoundtrip parseHeaderNameFromText decodeUtf8

roundTripByteStringUnsafe :: ByteString -> (Expectation, Property)
roundTripByteStringUnsafe = headerRoundtrip (Right . unsafeParseHeaderName) id

newtype AllowedHeaderName a = AllowedHeaderName {getHeaderName :: a}
    deriving (Show)

instance Arbitrary (AllowedHeaderName ByteString) where
    arbitrary = AllowedHeaderName . B8.pack <$> listOf1 (elements allValidHeaderNameChars)

instance Arbitrary (AllowedHeaderName String) where
    arbitrary = AllowedHeaderName <$> listOf1 (elements allValidHeaderNameChars)

instance Arbitrary (AllowedHeaderName Text) where
    arbitrary = AllowedHeaderName . pack <$> listOf1 (elements allValidHeaderNameChars)

instance Arbitrary HeaderName where
    arbitrary = unsafeParseHeaderName . getHeaderName <$> arbitrary

allValidHeaderNameChars :: String
allValidHeaderNameChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!#$%&'*+-.^_`|~0123456789"

headerRoundtrip ::
    (Eq a, Show a, Typeable a) =>
    (a -> Either (HeaderNameException a) HeaderName) ->
    (ByteString -> a) ->
    a ->
    (Expectation, Property)
headerRoundtrip toHeaderName toOriginal a =
    case toHeaderName a of
        Left e -> throw e
        Right hdr ->
            let result = toOriginal (encodeHeaderName hdr)
             in (result `shouldBe` a, result === a)

type HeaderTuple = (HeaderName, ByteString)

allConstants :: [HeaderTuple]
allConstants =
    [ (hAccept, "Accept")
    , (hAcceptCharset, "Accept-Charset")
    , (hAcceptEncoding, "Accept-Encoding")
    , (hAcceptLanguage, "Accept-Language")
    , (hAcceptRanges, "Accept-Ranges")
    , (hAccessControlAllowCredentials, "Access-Control-Allow-Credentials")
    , (hAccessControlAllowHeaders, "Access-Control-Allow-Headers")
    , (hAccessControlAllowMethods, "Access-Control-Allow-Methods")
    , (hAccessControlAllowOrigin, "Access-Control-Allow-Origin")
    , (hAccessControlExposeHeaders, "Access-Control-Expose-Headers")
    , (hAccessControlMaxAge, "Access-Control-Max-Age")
    , (hAccessControlRequestMethod, "Access-Control-Request-Method")
    , (hAccessControlRequestHeaders, "Access-Control-Request-Headers")
    , (hAge, "Age")
    , (hAllow, "Allow")
    , (hAuthorization, "Authorization")
    , (hCacheControl, "Cache-Control")
    , (hConnection, "Connection")
    , (hContentDisposition, "Content-Disposition")
    , (hContentEncoding, "Content-Encoding")
    , (hContentLanguage, "Content-Language")
    , (hContentLength, "Content-Length")
    , (hContentLocation, "Content-Location")
    , (hContentMD5, "Content-MD5")
    , (hContentRange, "Content-Range")
    , (hContentType, "Content-Type")
    , (hCookie, "Cookie")
    , (hDate, "Date")
    , (hETag, "ETag")
    , (hExpect, "Expect")
    , (hExpires, "Expires")
    , (hFrom, "From")
    , (hHost, "Host")
    , (hIfMatch, "If-Match")
    , (hIfModifiedSince, "If-Modified-Since")
    , (hIfNoneMatch, "If-None-Match")
    , (hIfRange, "If-Range")
    , (hIfUnmodifiedSince, "If-Unmodified-Since")
    , (hLastModified, "Last-Modified")
    , (hLink, "Link")
    , (hLocation, "Location")
    , (hMaxForwards, "Max-Forwards")
    , (hMIMEVersion, "MIME-Version")
    , (hOrigin, "Origin")
    , (hPragma, "Pragma")
    , (hPrefer, "Prefer")
    , (hPreferenceApplied, "Preference-Applied")
    , (hProxyAuthenticate, "Proxy-Authenticate")
    , (hProxyAuthorization, "Proxy-Authorization")
    , (hRange, "Range")
    , (hReferer, "Referer")
    , (hRetryAfter, "Retry-After")
    , (hServer, "Server")
    , (hSetCookie, "Set-Cookie")
    , (hStrictTransportSecurity, "Strict-Transport-Security")
    , (hTE, "TE")
    , (hTrailer, "Trailer")
    , (hTransferEncoding, "Transfer-Encoding")
    , (hUpgrade, "Upgrade")
    , (hUserAgent, "User-Agent")
    , (hVary, "Vary")
    , (hVia, "Via")
    , (hWWWAuthenticate, "WWW-Authenticate")
    , (hWarning, "Warning")
    , (hPseudoAuthority, ":authority")
    , (hPseudoMethod, ":method")
    , (hPseudoPath, ":path")
    , (hPseudoScheme, ":scheme")
    , (hPseudoStatus, ":status")
    ]

staticHeaderCheck :: HeaderTuple -> Spec
staticHeaderCheck (hdr, raw) = do
    it (B8.unpack $ pad raw) $ do
        -- No capital ASCII in lowercase
        B8.any (flip elem ['A' .. 'Z']) (encodeHeaderNameLower hdr) `shouldBe` False
        -- Case sensitive like example
        encodeHeaderName hdr `shouldBe` raw
  where
    pad bs =
        let padding = B8.replicate (maxMsg - B8.length bs) ' '
         in bs <> padding
    maxMsg = maximum $ fmap (B8.length . snd) allConstants

{-
allMozillaHeaders :: [HeaderName]
allMozillaHeaders =
    unsafeParseHeaderName
        <$> [ "Accept"
            , "Accept-CH"
            , "Accept-Encoding"
            , "Accept-Language"
            , "Accept-Patch"
            , "Accept-Post"
            , "Accept-Ranges"
            , "Access-Control-Allow-Credentials"
            , "Access-Control-Allow-Headers"
            , "Access-Control-Allow-Methods"
            , "Access-Control-Allow-Origin"
            , "Access-Control-Expose-Headers"
            , "Access-Control-Max-Age"
            , "Access-Control-Request-Headers"
            , "Access-Control-Request-Method"
            , "Activate-Storage-Access"
            , "Age"
            , "Allow"
            , "Alt-Svc"
            , "Alt-Used"
            , "Attribution-Reporting-Eligible"
            , "Attribution-Reporting-Register-Source"
            , "Attribution-Reporting-Register-Trigger"
            , "Authorization"
            , "Available-Dictionary"
            , "Cache-Control"
            , "Clear-Site-Data"
            , "Connection"
            , "Content-Digest"
            , "Content-Disposition"
            , "Content-DPR"
            , "Content-Encoding"
            , "Content-Language"
            , "Content-Length"
            , "Content-Location"
            , "Content-Range"
            , "Content-Security-Policy"
            , "Content-Security-Policy-Report-Only"
            , "Content-Type"
            , "Cookie"
            , "Critical-CH"
            , "Cross-Origin-Embedder-Policy"
            , "Cross-Origin-Embedder-Policy-Report-Only"
            , "Cross-Origin-Opener-Policy"
            , "Cross-Origin-Resource-Policy"
            , "Date"
            , "Device-Memory"
            , "Dictionary-ID"
            , "DNT"
            , "Downlink"
            , "DPR"
            , "Early-Data"
            , "ECT"
            , "ETag"
            , "Expect"
            , "Expect-CT"
            , "Expires"
            , "Forwarded"
            , "From"
            , "Host"
            , "Idempotency-Key"
            , "If-Match"
            , "If-Modified-Since"
            , "If-None-Match"
            , "If-Range"
            , "If-Unmodified-Since"
            , "Integrity-Policy"
            , "Integrity-Policy-Report-Only"
            , "Keep-Alive"
            , "Last-Modified"
            , "Link"
            , "Location"
            , "Max-Forwards"
            , "NEL"
            , "No-Vary-Search"
            , "Observe-Browsing-Topics"
            , "Origin"
            , "Origin-Agent-Cluster"
            , "Permissions-Policy"
            , "Permissions-Policy-Report-Only"
            , "Pragma"
            , "Prefer"
            , "Preference-Applied"
            , "Priority"
            , "Proxy-Authenticate"
            , "Proxy-Authorization"
            , "Range"
            , "Referer"
            , "Referrer-Policy"
            , "Refresh"
            , "Report-To"
            , "Reporting-Endpoints"
            , "Repr-Digest"
            , "Retry-After"
            , "RTT"
            , "Save-Data"
            , "Sec-Browsing-Topics"
            , "Sec-CH-Device-Memory"
            , "Sec-CH-DPR"
            , "Sec-CH-Prefers-Color-Scheme"
            , "Sec-CH-Prefers-Reduced-Motion"
            , "Sec-CH-Prefers-Reduced-Transparency"
            , "Sec-CH-UA"
            , "Sec-CH-UA-Arch"
            , "Sec-CH-UA-Bitness"
            , "Sec-CH-UA-Form-Factors"
            , "Sec-CH-UA-Full-Version"
            , "Sec-CH-UA-Full-Version-List"
            , "Sec-CH-UA-Mobile"
            , "Sec-CH-UA-Model"
            , "Sec-CH-UA-Platform"
            , "Sec-CH-UA-Platform-Version"
            , "Sec-CH-UA-WoW64"
            , "Sec-CH-Viewport-Height"
            , "Sec-CH-Viewport-Width"
            , "Sec-CH-Width"
            , "Sec-Fetch-Dest"
            , "Sec-Fetch-Mode"
            , "Sec-Fetch-Site"
            , "Sec-Fetch-Storage-Access"
            , "Sec-Fetch-User"
            , "Sec-GPC"
            , "Sec-Private-State-Token"
            , "Sec-Private-State-Token-Crypto-Version"
            , "Sec-Private-State-Token-Lifetime"
            , "Sec-Purpose"
            , "Sec-Redemption-Record"
            , "Sec-Speculation-Tags"
            , "Sec-WebSocket-Accept"
            , "Sec-WebSocket-Extensions"
            , "Sec-WebSocket-Key"
            , "Sec-WebSocket-Protocol"
            , "Sec-WebSocket-Version"
            , "Server"
            , "Server-Timing"
            , "Service-Worker"
            , "Service-Worker-Allowed"
            , "Service-Worker-Navigation-Preload"
            , "Set-Cookie"
            , "Set-Login"
            , "SourceMap"
            , "Speculation-Rules"
            , "Strict-Transport-Security"
            , "Supports-Loading-Mode"
            , "TE"
            , "Timing-Allow-Origin"
            , "Tk"
            , "Trailer"
            , "Transfer-Encoding"
            , "Upgrade"
            , "Upgrade-Insecure-Requests"
            , "Use-As-Dictionary"
            , "User-Agent"
            , "Vary"
            , "Via"
            , "Viewport-Width"
            , "Want-Content-Digest"
            , "Want-Repr-Digest"
            , "Warning"
            , "Width"
            , "WWW-Authenticate"
            , "X-Content-Type-Options"
            , "X-DNS-Prefetch-Control"
            , "X-Forwarded-For"
            , "X-Forwarded-Host"
            , "X-Forwarded-Proto"
            , "X-Frame-Options"
            , "X-Permitted-Cross-Domain-Policies"
            , "X-Powered-By"
            , "X-Robots-Tag"
            , "X-XSS-Protection"
            ]
 -}
