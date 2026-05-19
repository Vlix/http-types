{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Network.HTTP.Types.VersionSpec (main, spec) where

import Data.String (fromString)
import Data.Word (Word8)
import Test.Hspec
import Test.QuickCheck (Arbitrary (..), property)

import Network.HTTP.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Regression tests" $
        mapM_ checkVersion allVersions
    describe "parseHttpVersion" $ do
        it "also works with unorthodox versions" $
            parseHttpVersion "HTTP/15.32" `shouldBe` Right (HttpVersion 15 32)
        it "works with any number" $
            property $ \a b ->
                let fromW8 = fromIntegral @Word8 @Int
                    majV = fromW8 a
                    minV = fromW8 b
                    v = fromString $ "HTTP/" <> show majV <> "." <> show minV
                 in parseHttpVersion v == Right (HttpVersion majV minV)
    describe "renderHttpVersion" $ do
        it "also works with unorthodox versions" $
            renderHttpVersion (HttpVersion 61 98) `shouldBe` "HTTP/61.98"
        it "works with any number" $
            property $ \v@(HttpVersion majV minV) ->
                renderHttpVersion v == fromString ("HTTP/" <> show majV <> "." <> show minV)

-- | [({string}, {constant}, {literal})]
allVersions :: [(String, HttpVersion, HttpVersion)]
allVersions =
    [ ("HTTP/0.9", http09, HttpVersion 0 9)
    , ("HTTP/1.0", http10, HttpVersion 1 0)
    , ("HTTP/1.1", http11, HttpVersion 1 1)
    , ("HTTP/2.0", http20, HttpVersion 2 0)
    , ("HTTP/3.0", http30, HttpVersion 3 0)
    ]

checkVersion :: (String, HttpVersion, HttpVersion) -> Spec
checkVersion (str, v1, v2) =
    it str $ do
        v1 `shouldBe` v2
        show v1 `shouldBe` str
        renderHttpVersion v1 `shouldBe` bsStr
        parseHttpVersion bsStr `shouldBe` Right v1
        patternCheck str v1
  where
    bsStr = fromString str

instance Arbitrary HttpVersion where
    arbitrary = HttpVersion <$> arbitrary <*> arbitrary

patternCheck :: String -> HttpVersion -> Expectation
patternCheck s v =
    case v of
        Http09 -> s `shouldBe` "HTTP/0.9"
        Http10 -> s `shouldBe` "HTTP/1.0"
        Http11 -> s `shouldBe` "HTTP/1.1"
        Http20 -> s `shouldBe` "HTTP/2.0"
        Http30 -> s `shouldBe` "HTTP/3.0"
        _ -> expectationFailure $ s ++ " does not have a Pattern Synonym"
