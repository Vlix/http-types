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
import Data.Maybe (fromMaybe)
import Data.Text as T (Text, null)
import Debug.Trace (traceShow)
import Test.Hspec
import Test.QuickCheck (
    Arbitrary (..),
    Gen,
    Property,
    listOf,
    property,
    suchThat,
    (.&&.),
    (==>),
 )
import Test.QuickCheck.Instances ()

import Network.HTTP.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "encode/decode path" $ do
        it "is identity to encode and then decode" $
            property propEncodeDecodePath
        it "renders as expected" $
            renderQuery True [("a", Just "x"), ("b", Nothing), ("c", Just "")]
                `shouldBe` "?a=x&b&c="
        it "renders as expected without '?'" $
            renderQuery False [("a", Just "x"), ("b", Nothing), ("c", Just "")]
                `shouldBe` "a=x&b&c="
        it "does not escape period and dash" $
            toStrictBS (encodePath ["foo-bar.baz"] [])
                `shouldBe` "/foo-bar.baz"

    describe "encode/decode query" $ do
        it "is identity to encode and then decode" $
            property propEncodeDecodeQuery
        it "is identity to convert to and from Text" $
            property propConvertQueryText
        it "is identity to convert to and from Simple" $
            property propEncodeDecodeQuerySimple
        it "renderQuery is same as renderQueryText" $
            property propEncodeQueryText
        it "parseQuery is same as parseQueryText" $
            property propDecodeQueryText
        it "add ? in front of Query if and only if necessary" $
            property propQueryQuestionMark

    describe "URL encode/decode" $ do
        it "is identity to encode and then decode" $
            property propEncodeDecodeURL

    describe "decodePathSegments" $ do
        it "is inverse to encodePathSegments" $
            property $ \p ->
                (p /= [""])
                    ==> p
                    == (decodePathSegments . toStrictBS . encodePathSegments) p

    describe "extractPath" $ do
        context "when used with a relative URL" $ do
            it "returns URL unmodified" $ do
                property $ \p ->
                    (not . B.null) p ==> extractPath p == p

            context "when path is empty" $ do
                it "returns /" $
                    extractPath "" `shouldBe` "/"

        context "when used with an absolute URL" $ do
            context "when used with a HTTP URL" $ do
                it "it extracts path" $
                    extractPath "http://example.com/foo" `shouldBe` "/foo"

                context "when path is empty" $
                    it "returns /" $
                        extractPath "http://example.com" `shouldBe` "/"

            context "when used with a HTTPS URL" $ do
                it "it extracts path" $
                    extractPath "https://example.com/foo" `shouldBe` "/foo"

                context "when path is empty" $ do
                    it "returns /" $
                        extractPath "https://example.com" `shouldBe` "/"

    describe "parseQuery" $ do
        it "returns value with '+' replaced to ' '" $
            parseQuery "?a=b+c+d&x=&y" `shouldBe` [("a", Just "b c d"), ("x", Just ""), ("y", Nothing)]
        it "also does so without the question mark" $
            parseQuery "a=b+c+d&x=&y" `shouldBe` [("a", Just "b c d"), ("x", Just ""), ("y", Nothing)]

    describe "parseQueryReplacePlus" $ do
        it "returns value with '+' replaced to ' '" $
            parseQueryReplacePlus True "?a=b+c+d&x=&y" `shouldBe` [("a", Just "b c d"), ("x", Just ""), ("y", Nothing)]
        it "returns value with '+' preserved" $
            parseQueryReplacePlus False "?a=b+c+d&x=&y" `shouldBe` [("a", Just "b+c+d"), ("x", Just ""), ("y", Nothing)]

propEncodeDecodePath :: ([Text], QueryGen B.ByteString) -> Bool
propEncodeDecodePath (p', QueryGen b) =
    if z then z else traceShow (a, b, x, y) z
  where
    a = if p' == [""] then [] else p'
    x = toStrictBS $ encodePath a b
    y = decodePath x
    z = y == (a, b)

propEncodeDecodeQuery :: QueryGen B.ByteString -> Bool -> Bool
propEncodeDecodeQuery (QueryGen q) b =
    q == parseQuery (renderQuery b q)

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

propEncodeQueryText :: QueryGen Text -> Bool -> Bool
propEncodeQueryText (QueryGen q) b =
    toStrictBS (renderQueryText b q) == renderQuery b (queryTextToQuery q)

propDecodeQueryText :: QueryGen Text -> Property
propDecodeQueryText (QueryGen q) =
    (parseQueryText rq == queryToQueryText (parseQuery rq))
        .&&. (queryTextToQuery (parseQueryText rq) == parseQuery rq)
  where
    rq = toStrictBS (renderQueryText True q)

propEncodeDecodeQuerySimple :: QueryGen B.ByteString -> Bool -> Bool
propEncodeDecodeQuerySimple (QueryGen q') b =
    q == (parseSimpleQuery . renderSimpleQuery b) q
  where
    q = fmap (fmap $ fromMaybe "") q'

propEncodeDecodeURL :: B.ByteString -> Bool -> Bool
propEncodeDecodeURL bs b =
    bs == urlDecode b (urlEncode b bs)

newtype QueryGenItem b = QueryGenItem (b, Maybe b)
    deriving newtype (Show)
newtype QueryGen b = QueryGen [(b, Maybe b)]
    deriving newtype (Show)

instance Arbitrary (QueryGenItem B.ByteString) where arbitrary = arbQueryGenItem B.null
instance Arbitrary (QueryGenItem Text) where arbitrary = arbQueryGenItem T.null
instance Arbitrary (QueryGen B.ByteString) where arbitrary = arbQueryGen
instance Arbitrary (QueryGen Text) where arbitrary = arbQueryGen

arbQueryGenItem :: (Arbitrary a) => (a -> Bool) -> Gen (QueryGenItem a)
arbQueryGenItem p = do
    k <- arbitrary `suchThat` (not . p)
    v <- arbitrary
    pure $ QueryGenItem (k, v)

arbQueryGen :: (Arbitrary (QueryGenItem a)) => Gen (QueryGen a)
arbQueryGen = do
    items <- listOf arbitrary
    pure . QueryGen $ go <$> items
  where
    go (QueryGenItem i) = i

toStrictBS :: B.Builder -> B.ByteString
toStrictBS = BL.toStrict . B.toLazyByteString
