{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.DeepSeq (NFData (..))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.CaseInsensitive (mk)
import Data.Functor ((<&>))
import GHC.Generics (Generic)
import Network.HTTP.Header as H (unsafeParseNewHeaderName)
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
    ]

createBench1 :: Benchmark
createBench1 =
    bgroup
        "Create1"
        [ bgroup "Classic" $
            allMarks <&> \(name, bss) ->
                bench name $ nf mk bss
        , bgroup "New" $
            allMarks <&> \(name, bss) ->
                bcompare ("Create1.Classic." <> name) $
                    bench name $
                        nf unsafeParseNewHeaderName bss
        ]
  where
    altSvc, sts :: ByteString
    altSvc = "Alt-Svc"
    sts = "Strict-Transport-Security"
    allMarks =
        [ ("Alt-Svc", altSvc)
        , ("Strict-Transport-Security", sts)
        ]

createBench :: Benchmark
createBench =
    bgroup
        "Create"
        [ bgroup "Classic" $
            allMarks <&> \(name, bss) ->
                bench name $ nf (fmap mk) bss
        , bgroup "New" $
            allMarks <&> \(name, bss) ->
                bcompare ("Create.Classic." <> name) $
                    bench name $
                        nf (fmap unsafeParseNewHeaderName) bss
        ]
  where
    allMarks =
        [ ("short-small", shortSmall)
        , ("long-small", longSmall)
        , ("short-big", shortBig)
        , ("long-big", longBig)
        , ("Average", averageDDG)
        ]

eqBench :: Benchmark
eqBench =
    bgroup
        "Eq"
        [ bgroup "Classic" $
            allMarks <&> \(name, bss) ->
                env (pure (mk <$> bss)) $
                    bench name . nf (\x -> x == x)
        , bgroup "New" $
            allMarks <&> \(name, bss) ->
                env (pure (unsafeParseNewHeaderName <$> bss, unsafeParseNewHeaderName <$> bss)) $
                    bcompare ("Eq.Classic." <> name)
                        . bench name
                        . nf (uncurry (==))
        ]
  where
    allMarks =
        [ ("short-small", shortSmall)
        , ("long-small", longSmall)
        , ("short-big", shortBig)
        , ("long-big", longBig)
        , ("average", averageDDG)
        ]

lookupBench :: Benchmark
lookupBench =
    bgroup
        "Lookup"
        [ bgroup "Classic" $
            allMarks <&> \(hdr, name, bss) ->
                mkEnv mk hdr bss $
                    bench (unpack hdr <> "." <> name) . nf (uncurry lookup)
        , bgroup "New" $
            allMarks <&> \(hdr, name, bss) ->
                let benchName = unpack hdr <> "." <> name
                 in mkEnv unsafeParseNewHeaderName hdr bss $
                        bcompare ("Lookup.Classic." <> benchName)
                            . bench benchName
                            . nf (uncurry lookup)
        ]
  where
    mkEnv f hdr bss = env $ pure (f hdr, toKeyValueList f bss)
    allMarks =
        [ ("Age", "short-small", shortSmall)
        , ("Age", "short-big", shortBig)
        , ("Via", "short-small", shortSmall)
        , ("Via", "short-big", shortBig)
        ]
    toKeyValueList f = fmap $ \x -> (f x, ())

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
