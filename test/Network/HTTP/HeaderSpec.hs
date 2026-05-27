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
import Data.ByteString.Char8 qualified as B8 (any, pack, unpack)
import Data.Char (isUpper, ord)
import Data.Foldable (traverse_)
import Data.String (IsString)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Typeable (Typeable)
import GHC.Word (Word8 (..))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..), Property, elements, listOf1, (===))

import Network.HTTP.Header
import Network.HTTP.LowLevel (indexWord8OffRawAddr, strictIndex)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "HeaderName" $ do
        it "uses valid chars" $ do
            traverse_ isValidChar allValidHeaderNameChars
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
  where
    isValidChar c = W8# (indexWord8OffRawAddr strictIndex $ ord c) `shouldNotBe` 0xFF
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
