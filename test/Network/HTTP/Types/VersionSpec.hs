{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Types.VersionSpec (main, spec) where

import Test.Hspec

import Network.HTTP.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "Regression tests" $ do
        it "HTTP/0.9" $ do
            http09 `shouldBe` HttpVersion 0 9
            show http09 `shouldBe` "HTTP/0.9"
        it "HTTP/1.0" $ do
            http10 `shouldBe` HttpVersion 1 0
            show http10 `shouldBe` "HTTP/1.0"
        it "HTTP/1.1" $ do
            http11 `shouldBe` HttpVersion 1 1
            show http11 `shouldBe` "HTTP/1.1"
        it "HTTP/2  " $ do
            http20 `shouldBe` HttpVersion 2 0
            show http20 `shouldBe` "HTTP/2.0"

-- it "parses to HTTP/3" $ http30 `shouldBe` HttpVersion 3 0
