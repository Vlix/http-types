{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Types.URISpec (main, spec) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Text as T (Text, null)
import Debug.Trace (traceShow)
import Test.Hspec
import Test.QuickCheck (Arbitrary (..), listOf, property, suchThat, (==>), Gen)
import Test.QuickCheck.Instances ()

import Network.HTTP.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "encode/decode path" $ do
        it "is identity to encode and then decode" $
            property propEncodeDecodePath
        it "does not escape period and dash" $
            BL.toStrict (B.toLazyByteString (encodePath ["foo-bar.baz"] [])) `shouldBe` "/foo-bar.baz"

    describe "encode/decode query" $ do
        it "is identity to encode and then decode" $
            property propEncodeDecodeQuery
        it "is identity to convert to and from Text" $
            property propConvertQueryText
        it "add ? in front of Query if and only if necessary" $
            property propQueryQuestionMark

    describe "decodePathSegments" $ do
        it "is inverse to encodePathSegments" $
            property $ \p ->
                (p /= [""]) ==> do
                    (decodePathSegments . BL.toStrict . B.toLazyByteString . encodePathSegments) p `shouldBe` p

    describe "extractPath" $ do
        context "when used with a relative URL" $ do
            it "returns URL unmodified" $ do
                property $ \p ->
                    (not . B.null) p
                        ==> extractPath p
                        `shouldBe` p

            context "when path is empty" $ do
                it "returns /" $ do
                    extractPath "" `shouldBe` "/"

        context "when used with an absolute URL" $ do
            context "when used with a HTTP URL" $ do
                it "it extracts path" $ do
                    extractPath "http://example.com/foo" `shouldBe` "/foo"

                context "when path is empty" $ do
                    it "returns /" $ do
                        extractPath "http://example.com" `shouldBe` "/"

            context "when used with a HTTPS URL" $ do
                it "it extracts path" $ do
                    extractPath "https://example.com/foo" `shouldBe` "/foo"

                context "when path is empty" $ do
                    it "returns /" $ do
                        extractPath "https://example.com" `shouldBe` "/"

    describe "parseQuery" $ do
        it "returns value with '+' replaced to ' '" $ do
            parseQuery "?a=b+c+d" `shouldBe` [("a", Just "b c d")]

    describe "parseQueryReplacePlus" $ do
        it "returns value with '+' replaced to ' '" $ do
            parseQueryReplacePlus True "?a=b+c+d" `shouldBe` [("a", Just "b c d")]

        it "returns value with '+' preserved" $ do
            parseQueryReplacePlus False "?a=b+c+d" `shouldBe` [("a", Just "b+c+d")]

propEncodeDecodePath :: ([Text], QueryGen B.ByteString) -> Bool
propEncodeDecodePath (p', QueryGen b) =
    if z then z else traceShow (a, b, x, y) z
  where
    a = if p' == [""] then [] else p'
    x = BL.toStrict . B.toLazyByteString $ encodePath a b
    y = decodePath x
    z = y == (a, b)

propEncodeDecodeQuery :: QueryGen B.ByteString -> Bool
propEncodeDecodeQuery (QueryGen q) =
    q == parseQuery (renderQuery True q)

propQueryQuestionMark :: (Bool, Query) -> Bool
propQueryQuestionMark (useQuestionMark, query) =
    actual == expected
  where
    actual = case B8.uncons $ renderQuery useQuestionMark query of
        Just ('?', _) -> True
        _ -> False
    expected = useQuestionMark && not (Prelude.null query)

propConvertQueryText :: QueryGen Text -> Bool
propConvertQueryText (QueryGen q) =
    q == (queryToQueryText . queryTextToQuery) q

newtype QueryGenItem b = QueryGenItem (b, Maybe b)
    deriving newtype Show
newtype QueryGen b = QueryGen [(b, Maybe b)]
    deriving newtype Show

instance Arbitrary (QueryGenItem B.ByteString) where arbitrary = arbQueryGenItem B.null
instance Arbitrary (QueryGenItem Text) where arbitrary = arbQueryGenItem T.null
instance Arbitrary (QueryGen B.ByteString) where arbitrary = arbQueryGen
instance Arbitrary (QueryGen Text) where arbitrary = arbQueryGen

arbQueryGenItem :: Arbitrary a => (a -> Bool) -> Gen (QueryGenItem a)
arbQueryGenItem p = do
    k <- arbitrary `suchThat` (not . p)
    v <- arbitrary
    pure $ QueryGenItem (k, v)

arbQueryGen :: Arbitrary (QueryGenItem a) => Gen (QueryGen a)
arbQueryGen = do
    items <- listOf arbitrary
    pure . QueryGen $ go <$> items
  where
    go (QueryGenItem i) = i
