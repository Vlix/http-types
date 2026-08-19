{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Network.HTTP.Types.MethodSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck (property)
import Test.QuickCheck.Instances ()

import Network.HTTP.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Regression tests" $ do
        it "GET    " $ methodGet `shouldBe` "GET"
        it "POST   " $ methodPost `shouldBe` "POST"
        it "HEAD   " $ methodHead `shouldBe` "HEAD"
        it "PUT    " $ methodPut `shouldBe` "PUT"
        it "DELETE " $ methodDelete `shouldBe` "DELETE"
        it "TRACE  " $ methodTrace `shouldBe` "TRACE"
        it "CONNECT" $ methodConnect `shouldBe` "CONNECT"
        it "OPTIONS" $ methodOptions `shouldBe` "OPTIONS"
        it "PATCH  " $ methodPatch `shouldBe` "PATCH"
        it "StdMethod has all constants" $
            let methodList =
                    [ methodGet
                    , methodPost
                    , methodHead
                    , methodPut
                    , methodDelete
                    , methodTrace
                    , methodConnect
                    , methodOptions
                    , methodPatch
                    ]
             in allMethods `shouldBe` methodList

    describe "parse/render method" $ do
        it "round trips" $ do
            renderMethod . parseMethod <$> allMethods `shouldBe` allMethods
        it "also round trips for any ByteString" $
            property $ \bs ->
                renderMethod (parseMethod bs) `shouldBe` bs

allMethods :: [Method]
allMethods =
    renderStdMethod <$> [minBound @StdMethod .. maxBound]
