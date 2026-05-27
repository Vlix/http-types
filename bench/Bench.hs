{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.DeepSeq (NFData (..))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Char8 as B8
import Data.CaseInsensitive (CI (..), mk)
import Data.Functor ((<&>))
import qualified Data.List as L
import GHC.Generics (Generic)
import Network.HTTP.Header as H (
    encodeHeaderName,
    encodeHeaderNameLower,
    unsafeParseHeaderName,
    unsafeParseNewHeaderName,
 )
import Network.HTTP.Header.Internal
import Test.Tasty.Bench

main :: IO ()
main =
    defaultMain
        [bgroup "HeaderName" headerNameBenches]

headerNameBenches :: [Benchmark]
headerNameBenches =
    [ createBench1
    , createBench
    , eqBench
    , lookupBench
    , encodeBench
    , encodeLowerBench
    , sequenceOfOperationsNoEncoding $ head averageDDG
    , sequenceOfOperationsNoEncoding $ last averageDDG
    ]

benchBoth ::
    String ->
    [Benchmark] ->
    ((String -> Benchmark -> Benchmark) -> [Benchmark]) ->
    Benchmark
benchBoth topName classicBench newBench =
    bgroup
        topName
        [ bgroup "Classic" classicBench
        , bgroup "New" . newBench $ \name b ->
            bcompare (topName <> ".Classic." <> name) b
        ]

createBench1 :: Benchmark
createBench1 =
    benchBoth "Create1" classicBench $ \f ->
        newBench ".Full" f unsafeParseHeaderName
            ++ newBench ".New" f unsafeParseNewHeaderName
  where
    classicBench =
        allMarks <&> \(name, bss) -> bench name $ nf mk bss
    newBench s f toHN =
        allMarks <&> \(name, bss) ->
            f name . bench (name <> s) $
                nf toHN bss
    altSvc, sts :: ByteString
    altSvc = "Alt-Svc"
    sts = "Strict-Transport-Security"
    allMarks =
        [ ("Alt-Svc", altSvc)
        , ("Strict-Transport-Security", sts)
        ]

createBench :: Benchmark
createBench =
    benchBoth "Create" classicBench $ \f ->
        newBench ".Full" f unsafeParseHeaderName
            ++ newBench ".New" f unsafeParseNewHeaderName
  where
    classicBench =
        allMarks <&> \(name, bss) ->
            bench name $ nf (fmap mk) bss
    newBench s f toHN =
        allMarks <&> \(name, bss) ->
            f name . bench (name <> s) $
                nf (fmap toHN) bss
    allMarks =
        [ ("short-small", shortSmall)
        , ("long-small", longSmall)
        , ("short-big", shortBig)
        , ("long-big", longBig)
        , ("Average", averageDDG)
        ]

encodeBench :: Benchmark
encodeBench =
    benchBoth "Encode" classicBench $ \f ->
        newBench ".Full" f unsafeParseHeaderName
            ++ newBench ".New" f unsafeParseNewHeaderName
  where
    hdrs :: [ByteString]
    hdrs =
        [ "TE"
        , "Accept-Language"
        , "Cross-Origin-Embedder-Policy-Report-Only"
        ]
    classicBench =
        hdrs <&> \hdr ->
            env (pure $ mk hdr) $
                bench (B8.unpack hdr) . nf original
    newBench s f toHN =
        hdrs <&> \hdr ->
            env (pure $ toHN hdr) $
                let benchName = B8.unpack hdr
                 in f benchName . bench (benchName <> s) . nf encodeHeaderName

encodeLowerBench :: Benchmark
encodeLowerBench =
    benchBoth "Encode lower case" classicBench newBench
  where
    hdrs :: [ByteString]
    hdrs =
        [ "TE"
        , "Accept-Language"
        , "Cross-Origin-Embedder-Policy-Report-Only"
        ]
    classicBench =
        hdrs <&> \hdr ->
            env (pure $ mk hdr) $
                bench (B8.unpack hdr) . nf foldedCase
    newBench f =
        hdrs <&> \hdr ->
            env (pure $ unsafeParseNewHeaderName hdr) $
                let benchName = B8.unpack hdr
                 in f benchName . bench benchName . nf encodeHeaderNameLower

eqBench :: Benchmark
eqBench =
    benchBoth "Eq" classicBench newBench
  where
    classicBench =
        allMarks <&> \(name, bss) ->
            env (pure (mk <$> bss)) $
                bench name . nf (\x -> x == x)
    newBench f =
        allMarks <&> \(name, bss) ->
            env (pure (unsafeParseNewHeaderName <$> bss, unsafeParseNewHeaderName <$> bss)) $
                f name
                    . bench name
                    . nf (uncurry (==))
    allMarks =
        [ ("short-small", shortSmall)
        , ("long-small", longSmall)
        , ("short-big", shortBig)
        , ("long-big", longBig)
        , ("average", averageDDG)
        ]

lookupBench :: Benchmark
lookupBench =
    benchBoth "Lookup" classicBench newBench
  where
    classicBench =
        allMarks <&> \(hdr, name, bss) ->
            mkEnv mk hdr bss $
                bench (unpack hdr <> "." <> name) . nf (uncurry lookup)
    newBench f =
        let go s toHN =
                allMarks <&> \(hdr, name, bss) ->
                    let benchName = unpack hdr <> "." <> name
                     in mkEnv toHN hdr bss $
                            f benchName
                                . bench (benchName <> s)
                                . nf (uncurry lookup)
         in go ".Full" unsafeParseHeaderName ++ go ".New" unsafeParseNewHeaderName
    mkEnv f hdr bss = env $ pure (f hdr, toKeyValueList f bss)
    allMarks =
        [ ("Age", "short-small", shortSmall)
        , ("Age", "short-big", shortBig)
        , ("Via", "short-small", shortSmall)
        , ("Via", "short-big", shortBig)
        ]
    toKeyValueList f = fmap $ \x -> (f x, ())

sequenceOfOperationsNoEncoding :: ByteString -> Benchmark
sequenceOfOperationsNoEncoding lookupHdrBS =
    benchBoth ("Sequence of operations." <> B8.unpack lookupHdrBS) classicBench newBench
  where
    runAll :: (Eq a) => (ByteString -> a) -> [ByteString] -> Bool
    runAll f bss =
        let allHdrs = fmap f bss
            extraHdr = f "Connection"
            replaceHdr =
                let h = f "Content-Type"
                 in (h :) . L.delete h
            deleteHdr = L.delete $ f "Cache-Control"
            lookupHdr = f lookupHdrBS
            finalHdrs = deleteHdr . replaceHdr $ extraHdr : allHdrs
         in L.elem lookupHdr finalHdrs
    benchName = "Average"
    classicBench =
        [bench benchName $ nf (runAll (mk :: ByteString -> CI ByteString)) averageDDG]
    newBench f =
        [ f benchName . bench (benchName <> ".Full") $ nf (runAll unsafeParseHeaderName) averageDDG
        , f benchName . bench (benchName <> ".New") $ nf (runAll unsafeParseNewHeaderName) averageDDG
        ]

-- parseHeaders :: String -> Benchmark
-- parseHeaders name =
--     env (readFile "bench/headers.txt") $ \bs ->
--         benchBoth name (classicBench bs) (newBench bs)
--   where
--     getHdrs f bs = do

--     classicBench =
--         bench "headers" . getHdrs mk
--     newBench bs f =
--         let n = "headers"
--          in f n . bench n $ getHdrs unsafeParseHeaderName bs

shortSmall, longSmall, shortBig, longBig, averageDDG :: [ByteString]
shortSmall = ["Age", "TE", "Tk", "Via"]
longSmall =
    [ "Cross-Origin-Embedder-Policy-Report-Only"
    , "Sec-Private-State-Token-Crypto-Version"
    , "Service-Worker-Navigation-Preload"
    , "X-Permitted-Cross-Domain-Policies"
    ]
shortBig =
    [ "Accept"
    , "Age"
    , "Allow"
    , "Cookie"
    , "Date"
    , "DNT"
    , "DPR"
    , "ECT"
    , "Expect"
    , "From"
    , "Host"
    , "Link"
    , "NEL"
    , "Origin"
    , "Pragma"
    , "Prefer"
    , "Range"
    , "RTT"
    , "Server"
    , "TE"
    , "Tk"
    , "Vary"
    , "Via"
    , "Width"
    ]
longBig =
    [ "Access-Control-Allow-Credentials"
    , "Access-Control-Allow-Headers"
    , "Access-Control-Allow-Methods"
    , "Access-Control-Allow-Origin"
    , "Access-Control-Expose-Headers"
    , "Access-Control-Request-Headers"
    , "Access-Control-Request-Method"
    , "Attribution-Reporting-Eligible"
    , "Attribution-Reporting-Register-Source"
    , "Attribution-Reporting-Register-Trigger"
    , "Content-Security-Policy-Report-Only"
    , "Cross-Origin-Embedder-Policy"
    , "Cross-Origin-Embedder-Policy-Report-Only"
    , "Cross-Origin-Opener-Policy"
    , "Cross-Origin-Resource-Policy"
    , "Integrity-Policy-Report-Only"
    , "Sec-CH-Prefers-Color-Scheme"
    , "Sec-CH-Prefers-Reduced-Motion"
    , "Sec-CH-Prefers-Reduced-Transparency"
    , "Sec-CH-UA-Full-Version-List"
    , "Sec-CH-UA-Platform-Version"
    , "Sec-Private-State-Token-Crypto-Version"
    , "Sec-Private-State-Token-Lifetime"
    , "Sec-WebSocket-Extensions"
    , "Service-Worker-Navigation-Preload"
    , "Strict-Transport-Security"
    , "Upgrade-Insecure-Requests"
    , "X-Permitted-Cross-Domain-Policies"
    ]
averageDDG =
    [ "Server"
    , "Date"
    , "Content-Type"
    , "Vary"
    , "Server-Timing"
    , "X-Detected-Query-Lang"
    , "X-Duckduckgo-Results"
    , "Strict-Transport-Security"
    , "Permissions-Policy"
    , "Content-Security-Policy"
    , "X-Frame-Options"
    , "X-Xss-Protection"
    , "X-Content-Type-Options"
    , "Referrer-Policy"
    , "Expect-Ct"
    , "Nel"
    , "Report-To"
    , "Expires"
    , "Cache-Control"
    , "X-Duckduckgo-Locale"
    , "Content-Encoding"
    , "X-Firefox-Spdy"
    ]

deriving instance Generic HeaderName
instance NFData HeaderName

deriving instance Generic Bitmap
instance NFData Bitmap
deriving instance (Generic s) => Generic (HeaderNameException s)
instance (Generic s, NFData s) => NFData (HeaderNameException s)
