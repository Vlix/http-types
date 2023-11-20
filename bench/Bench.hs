{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Main where

import Control.DeepSeq (NFData)
import Control.Monad (replicateM)
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Char (chr, ord)
import qualified Data.List as L (intercalate)
import Data.Word (Word8)
import Test.Tasty.Bench
import Test.QuickCheck hiding ((.&.))

import Network.HTTP.Types.URI as URI
import Network.HTTP.Types.URI.Experimental as EURI

newtype RandomURI = RandomURI { fromRandomURI :: B.ByteString }
    deriving NFData
newtype URIChars = URIChars { fromURIChars :: String }
    deriving NFData

instance Arbitrary URIChars where
    arbitrary = do
        b <- arbitrary
        let percents = if b then ['\128'..'\137'] else []
        str <- sized $ \i ->
            replicateM i $
                elements $ "-_~+" ++ percents ++ ['a'..'z'] ++ ['0'..'9'] ++ ['A'..'Z']
        pure $ URIChars $ concatMap toPercent str
      where
        toPercent c
            | c > '\127' =
                let w8 = fromIntegral (ord c)
                    a :: Word8
                    a = shiftR w8 4
                    b = 15 .&. w8
                    toC x =
                        let offset = if x < 10 then 48 else 55
                         in chr $ fromIntegral (x + offset)
                 in ['%', toC a, toC b]
            | otherwise = [c]

instance Arbitrary RandomURI where
    arbitrary = do
        hasS <- arbitrary
        let mS = if hasS then "s" else ""
            proto = "http" <> mS <> "://"
        strs <- resize 8 $ replicateM 2 arbitrary
        let domain = L.intercalate "." $ (fromURIChars <$> strs) <> ["com"]
        strs2 <- resize 7 $ replicateM 4 arbitrary
        let path = L.intercalate "/" $ fromURIChars <$> strs2
        strs3 <- resize 6 $ replicateM 4 arbitrary
        let query =
                L.intercalate "&" $
                    map (\(URIChars a, URIChars b) -> a <> "=" <> b) strs3
        pure . RandomURI . B8.pack $
            proto <> domain <> "/" <> path <> "?" <> query

main :: IO ()
main = do
    RandomURI bs <- generate arbitrary
    putStrLn $ B8.unpack bs
    defaultMain
        [ env paths $ \ ~(path1, path5, path25, path125, path625) ->
            bgroup "parseQuery"
                [ bgroup "URI"
                    [ bench "path1"   $ nf fromPaths path1
                    , bench "path5"   $ nf fromPaths path5
                    , bench "path25"  $ nf fromPaths path25
                    , bench "path125" $ nf fromPaths path125
                    , bench "path625" $ nf fromPaths path625
                    ]
                , bgroup "URI Experimental"
                    [ bench "path1"   $ nf fromPathsEXP path1
                    , bench "path5"   $ nf fromPathsEXP path5
                    , bench "path25"  $ nf fromPathsEXP path25
                    , bench "path125" $ nf fromPathsEXP path125
                    , bench "path625" $ nf fromPathsEXP path625
                    ]
                ]
        , env queries $ \ ~(query1, query5, query25, query125, query625) ->
            bgroup "renderQuery"
                [ bgroup "URI"
                    [ bench "query1"   $ nf fromQueries query1
                    , bench "query5"   $ nf fromQueries query5
                    , bench "query25"  $ nf fromQueries query25
                    , bench "query125" $ nf fromQueries query125
                    , bench "query625" $ nf fromQueries query625
                    ]
                , bgroup "URI Experimental"
                    [ bench "query1"   $ nf fromQueriesEXP query1
                    , bench "query5"   $ nf fromQueriesEXP query5
                    , bench "query25"  $ nf fromQueriesEXP query25
                    , bench "query125" $ nf fromQueriesEXP query125
                    , bench "query625" $ nf fromQueriesEXP query625
                    ]
                ]
        ]
  where
    fromPaths :: [RandomURI] -> [URI.Query]
    fromPaths = fmap (URI.parseQuery . fromRandomURI)
    fromPathsEXP :: [RandomURI] -> [EURI.Query]
    fromPathsEXP = fmap (EURI.parseQuery . fromRandomURI)
    fromQueries :: [URI.Query] -> [B.ByteString]
    fromQueries = fmap $ URI.renderQuery True
    fromQueriesEXP :: [EURI.Query] -> [B.ByteString]
    fromQueriesEXP = fmap $ EURI.renderQuery True
    mkPath :: Int -> IO [RandomURI]
    mkPath i = replicateM i $ generate arbitrary
    paths = do
        path1 <- mkPath 1
        path5 <- mkPath 5
        path25 <- mkPath 25
        path125 <- mkPath 125
        path625 <- mkPath 625
        pure (path1, path5, path25, path125, path625)
    queries = do
        query1 <- fromPaths <$> mkPath 1
        query5 <- fromPaths <$> mkPath 5
        query25 <- fromPaths <$> mkPath 25
        query125 <- fromPaths <$> mkPath 125
        query625 <- fromPaths <$> mkPath 625
        pure (query1, query5, query25, query125, query625)
