{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.HeaderSpec where

import Data.ByteString (ByteString)
import Network.HTTP.Header
import Test.Hspec

main :: IO ()
main = hspec spec

-- | Round trip test that doesn't hold on to the original 'ByteString'
headerRoundtrip :: ByteString -> Expectation
headerRoundtrip bs =
    let hdr = unsafeNewHeaderName bs
     in encodeHeaderName hdr `shouldBe` bs

spec :: Spec
spec =
    describe "HeaderName" $ do
        it "roundtrips correctly" $ do
            headerRoundtrip "TE"
            headerRoundtrip "Accept-Language"
            headerRoundtrip "X-Permitted-Cross-Domain-Policies"
            headerRoundtrip "Some-Weird-Header-That-For-Some-Reason-Is-Longer-Than-64-Characters"
            headerRoundtrip "test-with-lowercase"
