{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.DeepSeq (NFData (..))
import Data.ByteString (ByteString)

-- newHeaderName,

import Data.ByteString.Char8 (unpack)
import Data.CaseInsensitive (mk)
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
        [ both "Alt-Svc" altSvc
        , both "Strict-Transport-Security" sts
        ]
  where
    altSvc, sts :: ByteString
    altSvc = "Alt-Svc"
    sts = "Strict-Transport-Security"
    both name bss =
        bgroup
            name
            [ bench "Classic" $ nf mk bss
            , bcompare ("Create1." <> name <> ".Classic") $
                bench "New" $
                    nf unsafeParseNewHeaderName bss
            ]

createBench :: Benchmark
createBench =
    bgroup
        "Create"
        [ both "short-small" shortSmall
        , both "long-small" longSmall
        , both "short-big" shortBig
        , both "long-big" longBig
        , both "Average" averageDDG
        ]
  where
    both name bss =
        bgroup
            name
            [ bench "Classic" $ nf (fmap mk) bss
            , bcompare ("Create." <> name <> ".Classic") $
                bench "New" $
                    nf (fmap unsafeParseNewHeaderName) bss
            ]

eqBench :: Benchmark
eqBench =
    bgroup
        "Eq"
        [ bothEqual "short-small" shortSmall
        , bothEqual "long-small" longSmall
        , bothEqual "short-big" shortBig
        , bothEqual "long-big" longBig
        , bothEqual "average" averageDDG
        ]
  where
    bothEqual name bss =
        bgroup
            name
            [ env (pure (mk <$> bss)) $
                bench "Classic" . nf (\x -> x == x)
            , env (pure (unsafeParseNewHeaderName <$> bss, unsafeParseNewHeaderName <$> bss)) $
                bcompare ("Eq." <> name <> ".Classic")
                    . bench "New"
                    . nf (uncurry (==))
            ]

lookupBench :: Benchmark
lookupBench =
    bgroup
        "Lookup"
        [ bothLookup "Age" "short-small" shortSmall
        , bothLookup "Age" "short-big" shortBig
        , bothLookup "Via" "short-small" shortSmall
        , bothLookup "Via" "short-big" shortBig
        ]
  where
    toKeyValueList f = fmap $ \x -> (f x, ())
    bothLookup hdr name bss =
        bgroup
            (unpack hdr <> "." <> name)
            [ env (pure (mk hdr, toKeyValueList mk bss)) $
                bench "Classic" . nf (uncurry lookup)
            , env (pure (unsafeParseNewHeaderName hdr, toKeyValueList unsafeParseNewHeaderName bss)) $
                bcompare ("Lookup." <> unpack hdr <> "." <> name <> ".Classic")
                    . bench "New"
                    . nf (uncurry lookup)
            ]

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

-- where
--     rnf EmptyHeader = ()
--     rnf (InvalidFieldNameByte bs w8) =
--         bs `seq` w8 `seq` ()
