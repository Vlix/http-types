-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MagicHash #-}

-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TypeApplications #-}
-- {-# OPTIONS_GHC -Wno-orphans #-}

module Network.HTTP.LowLevelSpec where

import Control.Monad (when)
import Data.Char (chr)
import Data.Foldable (for_)
import GHC.Word (Word8 (..))
import Test.Hspec

import Network.HTTP.LowLevel (RawAddr, ciIndex, hashIndex, indexWord8OffRawAddr, strictIndex)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "LowLevel" $ do
        describe "Strict Index" $ do
            it "allows valid chars (and uses lower case)" strictIndexTest
        describe "Case Insensitive Index" $ do
            it "only changes ['A'..'Z']" ciIndexTest
        describe "Hash Index" $ do
            it "mirrors strict index" hashIndexTest

strictIndexTest :: Expectation
strictIndexTest =
    forAllBytes $ \ix -> do
        let c = chr ix
            expected
                -- If upper case, then to lower
                | c `elem` ['A' .. 'Z'] = ix + 0x20
                -- If valid, then 'id'
                | c `elem` allExpectedChars = ix
                -- Others are invalid
                | otherwise = 0xFF
        indexOffStrictIx ix `shouldBe` expected

ciIndexTest :: Expectation
ciIndexTest =
    forAllBytes $ \ix -> do
        let expected =
                if chr ix `elem` ['A' .. 'Z']
                    then ix + 0x20
                    else ix
        indexOffInsensitiveIx ix `shouldBe` expected

hashIndexTest :: Expectation
hashIndexTest =
    forAllBytes $ \ix -> do
        let strictIx = indexOffStrictIx ix
        when (strictIx == 0xFF) $
            indexOff hashIndex ix `shouldBe` 0xFF

allExpectedChars :: String
allExpectedChars =
    ['a' .. 'z'] ++ ['0' .. '9'] ++ "!#$%&'*+-.^_`|~"

forAllBytes :: (Int -> Expectation) -> Expectation
forAllBytes = for_ [0x00 .. 0xFF]

indexOffInsensitiveIx :: Int -> Int
indexOffInsensitiveIx = indexOff ciIndex

indexOffStrictIx :: Int -> Int
indexOffStrictIx = indexOff strictIndex

indexOff :: RawAddr -> Int -> Int
indexOff addr ix =
    fromIntegral $ W8# (indexWord8OffRawAddr addr ix)
