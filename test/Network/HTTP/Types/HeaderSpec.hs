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

type HeaderTuple = (HeaderName, HeaderName)

allHeaders :: [HeaderTuple]
allHeaders =
    [ (hAccept, "Accept")
    , (hAcceptCharset, "Accept-Charset")
    , (hAcceptEncoding, "Accept-Encoding")
    , (hAcceptLanguage, "Accept-Language")
    , (hAcceptRanges, "Accept-Ranges")
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
    , (hTE, "TE")
    , (hTrailer, "Trailer")
    , (hTransferEncoding, "Transfer-Encoding")
    , (hUpgrade, "Upgrade")
    , (hUserAgent, "User-Agent")
    , (hVary, "Vary")
    , (hVia, "Via")
    , (hWWWAuthenticate, "WWW-Authenticate")
    , (hWarning, "Warning")
    ]

headerCheck :: HeaderTuple -> Spec
headerCheck (hdr, msg) = do
    it (B8.unpack . pad $ original msg) $ hdr `shouldBe` msg
  where
    pad bs =
        let padding = B8.pack $ replicate (maxMsg - B.length bs) ' '
         in bs <> padding

maxMsg :: Int
maxMsg = maximum $ fmap (B.length . original . snd) allHeaders

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
