{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Network.HTTP.Types.HeaderSpec (main, spec) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.CaseInsensitive (original)
import Data.Word (Word8)
import Test.Hspec
import Test.QuickCheck (Arbitrary (..), Gen, NonEmptyList (..), oneof, property)
import Test.QuickCheck.Instances ()

import Network.HTTP.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Regression tests" $ do
        mapM_ headerCheck allHeaders

    describe "byte ranges" $ do
        it "is identity to render and parse ByteRanges" $
            property $ \(NonEmpty brs) ->
                Just brs == parseByteRanges (renderByteRanges brs)
        it "is satisfiable with from-to of zero" $
            parseByteRanges "bytes=0-0" `shouldBe` Just [ByteRangeFromTo 0 0]
        it "is not satisfiable with suffix of zero" $
            parseByteRanges "bytes=-0" `shouldBe` Nothing
        it "is not satisfiable with 'from' lower than 'to'" $
            property $ \w81 w82 ->
                let w8toInt = fromIntegral :: Word8 -> Integer
                    -- if both are 0 it's @not (start < end)@ so we add 1
                    start = w8toInt w81 + end + 1
                    end = w8toInt w82
                    range = show start <> "-" <> show end
                 in parseByteRanges ("bytes=" <> B8.pack range) `shouldBe` Nothing

type HeaderTuple = (HeaderName, HeaderName, B.ByteString)

allHeaders :: [HeaderTuple]
allHeaders =
    [ (hAccept, "Accept", "Accept")
    , (hAcceptCharset, "Accept-Charset", "Accept-Charset")
    , (hAcceptEncoding, "Accept-Encoding", "Accept-Encoding")
    , (hAcceptLanguage, "Accept-Language", "Accept-Language")
    , (hAcceptRanges, "Accept-Ranges", "Accept-Ranges")
    , (hAge, "Age", "Age")
    , (hAllow, "Allow", "Allow")
    , (hAuthorization, "Authorization", "Authorization")
    , (hCacheControl, "Cache-Control", "Cache-Control")
    , (hConnection, "Connection", "Connection")
    , (hContentDisposition, "Content-Disposition", "Content-Disposition")
    , (hContentEncoding, "Content-Encoding", "Content-Encoding")
    , (hContentLanguage, "Content-Language", "Content-Language")
    , (hContentLength, "Content-Length", "Content-Length")
    , (hContentLocation, "Content-Location", "Content-Location")
    , (hContentMD5, "Content-MD5", "Content-MD5")
    , (hContentRange, "Content-Range", "Content-Range")
    , (hContentType, "Content-Type", "Content-Type")
    , (hCookie, "Cookie", "Cookie")
    , (hDate, "Date", "Date")
    , (hETag, "ETag", "ETag")
    , (hExpect, "Expect", "Expect")
    , (hExpires, "Expires", "Expires")
    , (hFrom, "From", "From")
    , (hHost, "Host", "Host")
    , (hIfMatch, "If-Match", "If-Match")
    , (hIfModifiedSince, "If-Modified-Since", "If-Modified-Since")
    , (hIfNoneMatch, "If-None-Match", "If-None-Match")
    , (hIfRange, "If-Range", "If-Range")
    , (hIfUnmodifiedSince, "If-Unmodified-Since", "If-Unmodified-Since")
    , (hLastModified, "Last-Modified", "Last-Modified")
    , (hLocation, "Location", "Location")
    , (hMaxForwards, "Max-Forwards", "Max-Forwards")
    , (hMIMEVersion, "MIME-Version", "MIME-Version")
    , (hOrigin, "Origin", "Origin")
    , (hPragma, "Pragma", "Pragma")
    , (hPrefer, "Prefer", "Prefer")
    , (hPreferenceApplied, "Preference-Applied", "Preference-Applied")
    , (hProxyAuthenticate, "Proxy-Authenticate", "Proxy-Authenticate")
    , (hProxyAuthorization, "Proxy-Authorization", "Proxy-Authorization")
    , (hRange, "Range", "Range")
    , (hReferer, "Referer", "Referer")
    , (hRetryAfter, "Retry-After", "Retry-After")
    , (hServer, "Server", "Server")
    , (hSetCookie, "Set-Cookie", "Set-Cookie")
    , (hTE, "TE", "TE")
    , (hTrailer, "Trailer", "Trailer")
    , (hTransferEncoding, "Transfer-Encoding", "Transfer-Encoding")
    , (hUpgrade, "Upgrade", "Upgrade")
    , (hUserAgent, "User-Agent", "User-Agent")
    , (hVary, "Vary", "Vary")
    , (hVia, "Via", "Via")
    , (hWWWAuthenticate, "WWW-Authenticate", "WWW-Authenticate")
    , (hWarning, "Warning", "Warning")
    ]

headerCheck :: HeaderTuple -> Spec
headerCheck (hdr, msg, raw) = do
    it (B8.unpack $ pad raw) $ do
        hdr `shouldBe` msg
        original hdr `shouldBe` raw
  where
    pad bs =
        let padding = B8.replicate (maxMsg - B.length bs) ' '
         in bs <> padding

maxMsg :: Int
maxMsg = maximum $ fmap (\(_, _, raw) -> B.length raw) allHeaders

-- | Generate valid ranges.
--
-- All values are positive and non-zero for easier testing.
instance Arbitrary ByteRange where
    arbitrary =
        oneof
            [ ByteRangeFrom <$> num
            , num >>= \from ->
                ByteRangeFromTo from . (from +) <$> num
            , ByteRangeSuffix <$> num
            ]
      where
        num =
            (+ 1) -- making sure it's non-zero
                . fromIntegral
                <$> (arbitrary :: Gen Word) -- making sure it's positive
