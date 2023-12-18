{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Network.HTTP.Types.StatusSpec (main, spec) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Function (on)
import qualified Data.List as L
import Test.Hspec
import Test.QuickCheck (Arbitrary (..), choose, property, resize)
import Test.QuickCheck.Instances ()

import Network.HTTP.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Regression tests" $ do
        mapM_ statusCheck allStatusses
        context "Category checks" $ do
            categoryCheck "statusIsInformational" statusIsInformational _100Statusses
            categoryCheck "statusIsSuccessful" statusIsSuccessful _200Statusses
            categoryCheck "statusIsRedirection" statusIsRedirection _300Statusses
            categoryCheck "statusIsClientError" statusIsClientError _400Statusses
            categoryCheck "statusIsServerError" statusIsServerError _500Statusses
    describe "Eq instance" $ do
        it "only matches on 'statusCode'" $
            property $
                \st1 st2 -> (st1 == st2) == ((==) `on` statusCode) st1 st2
    describe "Ord instance" $ do
        it "only orders on 'statusCode'" $
            property $
                \st1 st2 -> (st1 < st2) == ((<) `on` statusCode) st1 st2

categoryCheck :: String -> (Status -> Bool) -> [StatusTuple] -> Spec
categoryCheck name p shoulds = do
    it msg $ do
        mapM_ (\(st, _, _, _) -> st `shouldSatisfy` p) shoulds
        mapM_ (\(st, _, _, _) -> st `shouldNotSatisfy` p) $
            allStatusses L.\\ shoulds
  where
    msg = "'" <> name <> "' " <> "identifies correct statusses" <> extra
    extra = replicate (length ("statusIsInformational" :: String) - length name) ' '

type StatusTuple = (Status, Status, Int, B.ByteString)

_100Statusses :: [StatusTuple]
_100Statusses =
    [ (status100, continue100, 100, "Continue")
    , (status101, switchingProtocols101, 101, "Switching Protocols")
    ]

_200Statusses :: [StatusTuple]
_200Statusses =
    [ (status200, ok200, 200, "OK")
    , (status201, created201, 201, "Created")
    , (status202, accepted202, 202, "Accepted")
    , (status203, nonAuthoritative203, 203, "Non-Authoritative Information")
    , (status204, noContent204, 204, "No Content")
    , (status205, resetContent205, 205, "Reset Content")
    , (status206, partialContent206, 206, "Partial Content")
    ]

_300Statusses :: [StatusTuple]
_300Statusses =
    [ (status300, multipleChoices300, 300, "Multiple Choices")
    , (status301, movedPermanently301, 301, "Moved Permanently")
    , (status302, found302, 302, "Found")
    , (status303, seeOther303, 303, "See Other")
    , (status304, notModified304, 304, "Not Modified")
    , (status305, useProxy305, 305, "Use Proxy")
    , (status307, temporaryRedirect307, 307, "Temporary Redirect")
    , (status308, permanentRedirect308, 308, "Permanent Redirect")
    ]

_400Statusses :: [StatusTuple]
_400Statusses =
    [ (status400, badRequest400, 400, "Bad Request")
    , (status401, unauthorized401, 401, "Unauthorized")
    , (status402, paymentRequired402, 402, "Payment Required")
    , (status403, forbidden403, 403, "Forbidden")
    , (status404, notFound404, 404, "Not Found")
    , (status405, methodNotAllowed405, 405, "Method Not Allowed")
    , (status406, notAcceptable406, 406, "Not Acceptable")
    , (status407, proxyAuthenticationRequired407, 407, "Proxy Authentication Required")
    , (status408, requestTimeout408, 408, "Request Timeout")
    , (status409, conflict409, 409, "Conflict")
    , (status410, gone410, 410, "Gone")
    , (status411, lengthRequired411, 411, "Length Required")
    , (status412, preconditionFailed412, 412, "Precondition Failed")
    , (status413, requestEntityTooLarge413, 413, "Request Entity Too Large")
    , (status414, requestURITooLong414, 414, "Request-URI Too Long")
    , (status415, unsupportedMediaType415, 415, "Unsupported Media Type")
    , (status416, requestedRangeNotSatisfiable416, 416, "Requested Range Not Satisfiable")
    , (status417, expectationFailed417, 417, "Expectation Failed")
    , (status418, imATeapot418, 418, "I'm a teapot")
    , (status422, unprocessableEntity422, 422, "Unprocessable Entity")
    , (status426, upgradeRequired426, 426, "Upgrade Required")
    , (status428, preconditionRequired428, 428, "Precondition Required")
    , (status429, tooManyRequests429, 429, "Too Many Requests")
    , (status431, requestHeaderFieldsTooLarge431, 431, "Request Header Fields Too Large")
    ]

_500Statusses :: [StatusTuple]
_500Statusses =
    [ (status500, internalServerError500, 500, "Internal Server Error")
    , (status501, notImplemented501, 501, "Not Implemented")
    , (status502, badGateway502, 502, "Bad Gateway")
    , (status503, serviceUnavailable503, 503, "Service Unavailable")
    , (status504, gatewayTimeout504, 504, "Gateway Timeout")
    , (status505, httpVersionNotSupported505, 505, "HTTP Version Not Supported")
    , (status511, networkAuthenticationRequired511, 511, "Network Authentication Required")
    ]

allStatusses :: [StatusTuple]
allStatusses =
    concat
        [ _100Statusses
        , _200Statusses
        , _300Statusses
        , _400Statusses
        , _500Statusses
        ]

statusCheck :: (Status, Status, Int, B.ByteString) -> Spec
statusCheck (st, st', code, msg) = do
    it (show code <> " " <> B8.unpack (pad msg)) $ do
        st `shouldBe` st'
  where
    pad bs =
        let padding = B8.pack $ replicate (maxMsg - B.length bs) ' '
         in bs <> padding

maxMsg :: Int
maxMsg = maximum $ fmap (\(_, _, _, bs) -> B.length bs) allStatusses

instance Arbitrary Status where
    arbitrary =
        Status
            <$> choose (statusCode minBound, statusCode maxBound)
            <*> resize 20 arbitrary
