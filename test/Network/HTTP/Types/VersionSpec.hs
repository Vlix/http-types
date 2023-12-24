{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Types.VersionSpec (main, spec) where

import Test.Hspec

import Network.HTTP.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "Regression tests" $
        mapM_ checkVersion allVersions

-- | [("Rendered", {constant}, {literal}, "Shown")]
allVersions :: [(String, HttpVersion, HttpVersion, String)]
allVersions =
    [ ("HTTP/0.9", http09, HttpVersion 0 9, "HTTP/0.9")
    , ("HTTP/1.0", http10, HttpVersion 1 0, "HTTP/1.0")
    , ("HTTP/1.1", http11, HttpVersion 1 1, "HTTP/1.1")
    , ("HTTP/2", http20, HttpVersion 2 0, "HTTP/2.0")
    ]

checkVersion :: (String, HttpVersion, HttpVersion, String) -> Spec
checkVersion (msg, v1, v2, str) =
    it (msg ++ extra) $ do
        v1 `shouldBe` v2
        show v1 `shouldBe` str
  where
    extra = replicate (len - length msg) ' '
    len = maximum $ (\(a, _, _, _) -> length a) <$> allVersions

-- it "parses to HTTP/3" $ http30 `shouldBe` HttpVersion 3 0
