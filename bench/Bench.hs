{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-x-partial #-}

module Main where

import Control.DeepSeq (NFData (..), force)
import Control.Exception (evaluate)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Char8 as B8
import Data.CaseInsensitive (CI (..), foldCase, mk)
import Data.Char (toLower)
import Data.Functor ((<&>))
import qualified Data.HashMap.Lazy as LHM
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Map.Lazy as LM
import qualified Data.Map.Strict as M
import GHC.Generics (Generic)
import Network.HTTP.Header as H (
    caseInsensitiveEq,
    encodeHeaderName,
    encodeHeaderNameLower,
    unsafeParseHeaderName,
 )
import Network.HTTP.Header.Constants
import Network.HTTP.Header.Internal
import Test.Tasty.Bench

main :: IO ()
main =
    defaultMain
        [ caseInsensitiveEqBench
        , -- , bgroup "HeaderName" headerNameBenches
          bgroup "HeaderMap" headerMapBenches
        ]

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
    benchBoth "Create1" classicBench newBench
  where
    classicBench =
        allMarks <&> \(name, bss) -> bench name $ nf mk bss
    newBench f =
        allMarks <&> \(name, bss) ->
            f name . bench name $
                nf unsafeParseHeaderName bss
    altSvc, sts :: ByteString
    altSvc = "Alt-Svc"
    sts = "Strict-Transport-Security"
    allMarks =
        [ ("Alt-Svc", altSvc)
        , ("Strict-Transport-Security", sts)
        ]

createBench :: Benchmark
createBench =
    benchBoth "Create" classicBench newBench
  where
    classicBench =
        allMarks <&> \(name, bss) ->
            bench name $ nf (fmap mk) bss
    newBench f =
        allMarks <&> \(name, bss) ->
            f name . bench name $
                nf (fmap unsafeParseHeaderName) bss
    allMarks =
        [ ("short-small", shortSmall)
        , ("long-small", longSmall)
        , ("short-big", shortBig)
        , ("long-big", longBig)
        , ("Average", averageDDG)
        ]

encodeBench :: Benchmark
encodeBench =
    benchBoth "Encode" classicBench newBench
  where
    hdrs :: [ByteString]
    hdrs =
        [ "TE"
        , "Accept-Language"
        , "Cross-Origin-Embedder-Policy-Report-Only"
        ]
    classicBench =
        hdrs <&> \hdr ->
            pureEnv (mk hdr) $
                bench (B8.unpack hdr) . nf original
    newBench f =
        hdrs <&> \hdr ->
            pureEnv (unsafeParseHeaderName hdr) $
                let benchName = B8.unpack hdr
                 in f benchName . bench benchName . nf encodeHeaderName

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
            pureEnv (mk hdr) $
                bench (B8.unpack hdr) . nf foldedCase
    newBench f =
        hdrs <&> \hdr ->
            pureEnv (unsafeParseHeaderName hdr) $
                let benchName = B8.unpack hdr
                 in f benchName . bench benchName . nf encodeHeaderNameLower

eqBench :: Benchmark
eqBench =
    benchBoth "Eq" classicBench newBench
  where
    classicBench =
        allMarks <&> \(name, bss) ->
            pureEnv (mk <$> bss) $
                bench name . nf (\x -> x == x)
    newBench f =
        allMarks <&> \(name, bss) ->
            pureEnv (unsafeParseHeaderName <$> bss, unsafeParseHeaderName <$> bss) $
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
        allMarks <&> \(hdr, name, bss) ->
            let benchName = unpack hdr <> "." <> name
             in mkEnv unsafeParseHeaderName hdr bss $
                    f benchName
                        . bench benchName
                        . nf (uncurry lookup)
    mkEnv f hdr bss = pureEnv (f hdr, toKeyValueList f bss)
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
        [f benchName . bench benchName $ nf (runAll unsafeParseHeaderName) averageDDG]

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

headerMapBenches :: [Benchmark]
headerMapBenches =
    [datatypeBenches]

datatypeBenches :: Benchmark
datatypeBenches =
    bgroup
        "Datatype"
        [ bgroup "List" $
            headerLists <&> runDataBench' False listActions
        , bgroup "Map.Strict" $
            headerLists <&> runDataBench mapActions
        , -- , bgroup "Map.Lazy" $
          --     headerLists <&> runDataBench lazyMapActions
          bgroup "HashMap.Strict" $
            headerLists <&> runDataBench hashMapActions
            -- , bgroup "HashMap.Lazy" $
            --     headerLists <&> runDataBench lazyHashMapActions
        ]

-- | Create the structure
type Create a = [(HeaderName, ByteString)] -> a

-- | Add a header
type Add a = HeaderName -> ByteString -> a -> a

-- | Add a header and remove all other duplicates of the header
type Replace a = HeaderName -> ByteString -> a -> a

-- | Find the header and show first value
type Lookup a b = HeaderName -> a -> Maybe b

-- | Find the header and if present change the value
type Update a b = (b -> b) -> HeaderName -> a -> a

-- | Remove all instances of the header
type Delete a = HeaderName -> a -> a

data Actions a b = Actions
    { aCreate :: Create a
    , aAdd :: Add a
    , aAddEnd :: Add a
    , aReplace :: Replace a
    , aLookup :: Lookup a b
    , aUpdate :: Update a b
    , aDelete :: Delete a
    , aUpdateF :: b -> b
    }

listActions :: Actions [(HeaderName, ByteString)] ByteString
listActions =
    Actions
        { aCreate = id
        , aAdd = listAdd
        , aAddEnd = \k v xs -> xs <> [(k, v)]
        , aReplace = listReplace
        , aLookup = L.lookup
        , aUpdate = listUpdate
        , aDelete = listDelete
        , aUpdateF = updateHeader
        }
  where
    listAdd k v xs = (k, v) : xs
    listReplace k v xs = (k, v) : listDelete k xs
    listUpdate _ _ [] = []
    listUpdate f k ((a, b) : xs)
        | a == k = (a, f b) : listDelete k xs
        | otherwise = (a, b) : listUpdate f k xs
    listDelete k = filter $ (/= k) . fst

mapActions :: Actions (M.Map HeaderName [ByteString]) [ByteString]
mapActions =
    Actions
        { aCreate = M.fromListWith (<>) . fmap (\(a, b) -> (a, [b]))
        , aAdd = \k -> M.insertWith (flip (<>)) k . pure
        , aAddEnd = \k -> M.insertWith (<>) k . pure
        , aReplace = \k -> M.insert k . pure
        , aLookup = M.lookup
        , aUpdate = M.adjust
        , aDelete = M.delete
        , aUpdateF = fmap updateHeader . take 1
        }

_lazyMapActions :: Actions (LM.Map HeaderName [ByteString]) [ByteString]
_lazyMapActions =
    Actions
        { aCreate = LM.fromListWith (<>) . fmap (\(a, b) -> (a, [b]))
        , aAdd = \k -> LM.insertWith (flip (<>)) k . pure
        , aAddEnd = \k -> LM.insertWith (<>) k . pure
        , aReplace = \k -> LM.insert k . pure
        , aLookup = LM.lookup
        , aUpdate = LM.adjust
        , aDelete = LM.delete
        , aUpdateF = fmap updateHeader . take 1
        }

hashMapActions :: Actions (HM.HashMap HeaderName [ByteString]) [ByteString]
hashMapActions =
    Actions
        { aCreate = HM.fromListWith (<>) . fmap (\(a, b) -> (a, [b]))
        , aAdd = \k -> HM.insertWith (flip (<>)) k . pure
        , aAddEnd = \k -> HM.insertWith (<>) k . pure
        , aReplace = \k -> HM.insert k . pure
        , aLookup = HM.lookup
        , aUpdate = HM.adjust
        , aDelete = HM.delete
        , aUpdateF = fmap updateHeader . take 1
        }

_lazyHashMapActions :: Actions (LHM.HashMap HeaderName [ByteString]) [ByteString]
_lazyHashMapActions =
    Actions
        { aCreate = LHM.fromListWith (<>) . fmap (\(a, b) -> (a, [b]))
        , aAdd = \k -> LHM.insertWith (flip (<>)) k . pure
        , aAddEnd = \k -> LHM.insertWith (<>) k . pure
        , aReplace = \k -> LHM.insert k . pure
        , aLookup = LHM.lookup
        , aUpdate = LHM.adjust
        , aDelete = LHM.delete
        , aUpdateF = fmap updateHeader . take 1
        }

updateHeader :: ByteString -> ByteString
updateHeader = (<> ", text/html;q=0.3")

headerLists :: [(String, [HeaderName])]
headerLists =
    [ ("One", [hAccept])
    , -- , ("Two", [hAccept, hServer])
      -- , ("Four", [hAccept, hContentDisposition, hServer, hVia])
      ("Average", unsafeParseHeaderName <$> averageDDG)
    ]

runDataBench :: (NFData a, NFData b) => Actions a b -> (String, [HeaderName]) -> Benchmark
runDataBench = runDataBench' True

runDataBench' :: (NFData a, NFData b) => Bool -> Actions a b -> (String, [HeaderName]) -> Benchmark
runDataBench' shouldCompare actions (testName, hdrs) =
    bgroup
        testName
        [ pureEnv hdrsList $ \hs ->
            comp "Create" $ bench "Create" $ nf (aCreate actions) hs
        , prepHdrs "Add Front" $ \ ~(hs, _) ->
            whnf (aAdd actions hName "99") hs
        , prepHdrs "Add End" $ \ ~(hs, _) ->
            nf (aAddEnd actions hName "99") hs
        , prepHdrs "Replace" $ \ ~(hs, _) ->
            nf (aReplace actions hName "99") hs
        , prepHdrs "Lookup First" $ \ ~(hs, firstHdr) ->
            whnf (aLookup actions firstHdr) hs
        , prepHdrs "Lookup Missing" $ \ ~(hs, _) ->
            whnf (aLookup actions hName) hs
        , prepHdrs "Update First" $ \ ~(hs, firstHdr) ->
            nf (aUpdate actions (aUpdateF actions) firstHdr) hs
        , prepHdrs "Update Missing" $ \ ~(hs, _) ->
            nf (aUpdate actions (aUpdateF actions) hName) hs
        , prepHdrs "Delete First" $ \ ~(hs, firstHdr) ->
            nf (aDelete actions firstHdr) hs
        , prepHdrs "Delete Missing" $ \ ~(hs, _) ->
            nf (aDelete actions hName) hs
        ]
  where
    comp name =
        if shouldCompare
            then bcompare ("List." <> testName <> "." <> name)
            else id
    hName = unsafeParseHeaderName "Access-Control-Allow-Headers"
    hdrsList =
        zip hdrs $ B8.pack . show <$> [0 :: Int ..]
    prepHdrs name f =
        pureEnv (aCreate actions hdrsList, head hdrs) $
            comp name . bench name . f

pureEnv :: (NFData env) => env -> (env -> Benchmark) -> Benchmark
pureEnv x = env (evaluate $ force x)

caseInsensitiveEqBench :: Benchmark
caseInsensitiveEqBench =
    bgroup
        topName
        [ pureEnv ("Close" :: ByteString) $
            bench "BS" . nf (\bs -> foldCase bs == "close")
        , pureEnv ("Close" :: ByteString) $
            bcompare (topName <> ".BS")
                . bench "Map"
                . nf (\bs -> B8.map (toLower) bs == "close")
        , pureEnv ("Close" :: ByteString) $
            bcompare (topName <> ".BS")
                . bench "Custom"
                . nf (caseInsensitiveEq "close")
        ]
  where
    topName = "Case insensitive EQ"
