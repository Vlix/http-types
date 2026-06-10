{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-x-partial #-}

module Main where

import Control.DeepSeq (NFData (..), force)
import Control.Exception (evaluate)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Char8 as B8
import Data.CaseInsensitive (CI (..), foldCase, mk)
import Data.Char (toLower)
import Data.Functor ((<&>))

-- import qualified Data.HashMap.Lazy as LHM
-- import qualified Data.HashMap.Strict as HM
import qualified Data.List as L

-- import qualified Data.Map.Lazy as LM
-- import qualified Data.Map.Strict as M
-- import Data.Maybe (fromMaybe)
-- import qualified Data.Vector as V
import GHC.Generics (Generic)
import Network.HTTP.Header as H (
    addHeader,
    addHeaderFront,
    caseInsensitiveEq,
    encodeHeaderName,
    encodeHeaderNameLower,
    fromList,
    lookupHeader,
    removeHeader,
    setHeader,
    setHeaderFront,
    unsafeParseHeaderName,
 )
import Network.HTTP.Header.Constants
import Network.HTTP.Header.Internal
import Test.Tasty.Bench

main :: IO ()
main =
    defaultMain
        [ caseInsensitiveEqBench
        , bgroup "HeaderName" headerNameBenches
        , bgroup "HeaderMap" headerMapBenches
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

-- We make a copy in the 'classicBench'mark since that more accurately
-- describes what happens when actually writing it to an outgoing socket.
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
                bench (B8.unpack hdr) . nf (B8.copy . original)
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
                bench (B8.unpack hdr) . nf (B8.copy . foldedCase)
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
deriving instance Generic Header
instance NFData Header
deriving instance Generic Headers
instance NFData Headers

deriving instance Generic Bitmap
instance NFData Bitmap
deriving instance Generic HashBitmap
instance NFData HashBitmap
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
        , bgroup "List.Header" $
            headerLists <&> runDataBench listActionsNewHeader
        , -- , bgroup "Vector" $
          --     headerLists <&> runDataBench vectorActions
          -- , bgroup "Map.Strict" $
          --     headerLists <&> runDataBench mapActions
          -- , -- , bgroup "Map.Lazy" $
          --     headerLists <&> runDataBench _lazyMapActions
          --   bgroup "HashMap.Strict" $
          --     headerLists <&> runDataBench hashMapActions
          -- , -- , bgroup "HashMap.Lazy" $
          --     headerLists <&> runDataBench _lazyHashMapActions
          bgroup "New Headers" $
            headerLists <&> runDataBench newHeadersActions
        ]

-- | Create the structure
type Create a hdr = [hdr] -> a

-- | Add a header
type Add a hdr = hdr -> a -> a

-- | Add a header and remove all other duplicates of the header
type Replace a hdr = hdr -> a -> a

-- | Find the header and show first value
type Lookup a b = HeaderName -> a -> [b]

-- | Remove all instances of the header
type Delete a = HeaderName -> a -> a

data Actions hdr a b = Actions
    { aCreate :: Create a hdr
    , aAdd :: Add a hdr
    , aAddEnd :: Add a hdr
    , aReplace :: Replace a hdr
    , aReplaceEnd :: Replace a hdr
    , aLookup :: Lookup a b
    , aDelete :: Delete a
    , aFromByteString :: ByteString -> a
    , aFromTuple :: HeaderName -> ByteString -> hdr
    }

listActions :: Actions (HeaderName, ByteString) [(HeaderName, ByteString)] ByteString
listActions =
    Actions
        { aCreate = id
        , aAdd = (:)
        , aAddEnd = \tup xs -> xs <> [tup]
        , aReplace = listReplace
        , aReplaceEnd = \tup@(k, _) xs -> listDelete k xs <> [tup]
        , aLookup = listLookup
        , aDelete = listDelete
        , aFromByteString = reverse . withFile (,) (:) []
        , aFromTuple = (,)
        }
  where
    listReplace tup@(k, _) xs = tup : listDelete k xs
    listDelete k = filter $ (/= k) . fst
    listLookup nm xs = snd <$> filter ((==) nm . fst) xs

listActionsNewHeader :: Actions Header [Header] ByteString
listActionsNewHeader =
    Actions
        { aCreate = id
        , aAdd = (:)
        , aAddEnd = \hdr xs -> xs <> [hdr]
        , aReplace = listReplace
        , aReplaceEnd = \hdr xs -> listDelete (headerName hdr) xs <> [hdr]
        , aLookup = listLookup
        , aDelete = listDelete
        , aFromByteString = reverse . withFile Header (:) []
        , aFromTuple = Header
        }
  where
    listReplace hdr xs = hdr : listDelete (headerName hdr) xs
    listDelete k = filter $ (/= k) . headerName
    listLookup nm xs = headerValue <$> filter ((==) nm . headerName) xs

{-
-- Other types I've tried but found lacking in one way or another.
--  * Maps lose sequentiality (which would mean you'd need a list or something
--    extra to keep the order, which would defeat the performance gains)
--  * Vector would be kind of ok, but adding would be slower/the same, but most
--    most importantly, the lookup would not be any quicker.

_vectorActions :: Actions (V.Vector Header) ByteString
_vectorActions =
    Actions
        { aCreate = V.fromList . fmap (uncurry Header)
        , aAdd = V.cons
        , aAddEnd = flip V.snoc
        , aReplace = \hdr ->
            V.cons hdr . V.filter ((== (headerName hdr)) . headerName)
        , aReplaceEnd = \hdr xs ->
            V.filter ((== (headerName hdr)) . headerName) xs `V.snoc` hdr
        , aLookup = \k ->
            V.toList . fmap headerValue . V.filter ((== k) . headerName)
        , aDelete = \k -> V.filter ((/= k) . headerName)
        , aFromByteString = V.fromList . reverse . withFile (:) []
        }

_mapActions :: Actions (M.Map HeaderName [ByteString]) ByteString
_mapActions =
    Actions
        { aCreate = M.fromListWith (<>) . fmap (\(a, b) -> (a, [b]))
        , aAdd = \(Header k v) -> M.insertWith (flip (<>)) k [v]
        , aAddEnd = addEnd
        , aReplace = \(Header k v) -> M.insert k [v]
        , aReplaceEnd = \(Header k v) -> M.insert k [v]
        , aLookup = \k -> fromMaybe [] . M.lookup k
        , aDelete = M.delete
        , aFromByteString = withFile addEnd mempty
        }
  where
    addEnd (Header k v) = M.insertWith (<>) k [v]

_lazyMapActions :: Actions (LM.Map HeaderName [ByteString]) ByteString
_lazyMapActions =
    Actions
        { aCreate = LM.fromListWith (<>) . fmap (\(a, b) -> (a, [b]))
        , aAdd = \(Header k v) -> LM.insertWith (flip (<>)) k [v]
        , aAddEnd = addEnd
        , aReplace = \(Header k v) -> LM.insert k [v]
        , aReplaceEnd = \(Header k v) -> LM.insert k [v]
        , aLookup = \k -> fromMaybe [] . LM.lookup k
        , aDelete = LM.delete
        , aFromByteString = withFile addEnd mempty
        }
  where
    addEnd (Header k v) = LM.insertWith (<>) k [v]

_hashMapActions :: Actions (HM.HashMap HeaderName [ByteString]) ByteString
_hashMapActions =
    Actions
        { aCreate = HM.fromListWith (<>) . fmap (\(a, b) -> (a, [b]))
        , aAdd = \(Header k v) -> HM.insertWith (flip (<>)) k [v]
        , aAddEnd = addEnd
        , aReplace = \(Header k v) -> HM.insert k [v]
        , aReplaceEnd = \(Header k v) -> HM.insert k [v]
        , aLookup = \k -> fromMaybe [] . HM.lookup k
        , aDelete = HM.delete
        , aFromByteString = withFile addEnd mempty
        }
  where
    addEnd (Header k v) = HM.insertWith (<>) k [v]

_lazyHashMapActions :: Actions (LHM.HashMap HeaderName [ByteString]) ByteString
_lazyHashMapActions =
    Actions
        { aCreate = LHM.fromListWith (<>) . fmap (\(a, b) -> (a, [b]))
        , aAdd = \(Header k v) -> LHM.insertWith (flip (<>)) k [v]
        , aAddEnd = addEnd
        , aReplace = \(Header k v) -> LHM.insert k [v]
        , aReplaceEnd = \(Header k v) -> LHM.insert k [v]
        , aLookup = \k -> fromMaybe [] . LHM.lookup k
        , aDelete = LHM.delete
        , aFromByteString = withFile addEnd mempty
        }
  where
    addEnd (Header k v) = LHM.insertWith (<>) k [v]
 -}

newHeadersActions :: Actions Header Headers ByteString
newHeadersActions =
    Actions
        { aCreate = fromList
        , aAdd = addHeaderFront
        , aAddEnd = addHeader
        , aReplace = setHeaderFront
        , aReplaceEnd = setHeader
        , aLookup = lookupHeader
        , aDelete = removeHeader
        , aFromByteString = fromList . reverse . withFile Header (:) []
        , aFromTuple = Header
        }

updateHeader :: ByteString -> ByteString
updateHeader = (<> ", text/html;q=0.3")

headerLists :: [(String, [HeaderName])]
headerLists =
    [ ("Four", [hAccept, hContentDisposition, hServer, hVia])
    , ("Average", unsafeParseHeaderName <$> averageDDG)
    ]

runDataBench :: (NFData a, NFData b, NFData hdr) => Actions hdr a b -> (String, [HeaderName]) -> Benchmark
runDataBench = runDataBench' True

runDataBench' :: (NFData a, NFData b, NFData hdr) => Bool -> Actions hdr a b -> (String, [HeaderName]) -> Benchmark
runDataBench' shouldCompare actions (testName, hdrs) =
    bgroup
        testName
        [ pureEnv (uncurry toHeader <$> hdrsList) $ \hs ->
            comp "Create" $ bench "Create" $ nf (aCreate actions) hs
        , -- , prepHdrs "Add Front Missing" $ \ ~(hs, _, _) ->
          --     nf (aAdd actions $ hName >: "99") hs
          prepHdrs "Add End Missing" $ \ ~(hs, _, _) ->
            nf (aAddEnd actions $ toHeader hName "99") hs
        , -- , prepHdrs "Add Front Middle" $ \ ~(hs, _, middleHdr) ->
          --     nf (aAdd actions $ toHeader (middleHdr, "99")) hs
          prepHdrs "Add End Middle" $ \ ~(hs, _, middleHdr) ->
            nf (aAddEnd actions $ toHeader middleHdr "99") hs
        , prepHdrs "Replace Missing" $ \ ~(hs, _, _) ->
            nf (aReplace actions $ toHeader hName "99") hs
        , prepHdrs "Replace Exists" $ \ ~(hs, _, xFrameHdr) ->
            nf (aReplace actions $ toHeader xFrameHdr "99") hs
        , -- , prepHdrs "Lookup First" $ \ ~(hs, firstHdr, _) ->
          --     nf (aLookup actions firstHdr) hs
          -- , prepHdrs "Lookup Middle" $ \ ~(hs, _, middleHdr) ->
          --     nf (aLookup actions middleHdr) hs
          prepHdrs "Lookup Missing" $ \ ~(hs, _, _) ->
            nf (aLookup actions hName) hs
        , -- , prepHdrs "Delete First" $ \ ~(hs, firstHdr, _) ->
          --     nf (aDelete actions firstHdr) hs
          -- , prepHdrs "Delete Missing" $ \ ~(hs, _, _) ->
          --     nf (aDelete actions hName) hs
          prepHdrs "Everything" $ \ ~(hs, firstHdr, middleHdr) ->
            nf
                ( \headers ->
                    let newHdrs =
                            aAddEnd actions (toHeader firstHdr "everything")
                                -- . aDelete actions firstHdr
                                . aReplace actions (toHeader hDate "other date")
                                . aReplaceEnd actions (toHeader firstHdr "new val")
                                . aReplaceEnd actions (toHeader hRetryAfter "hehe")
                                $ headers
                     in aLookup actions hRange newHdrs <> aLookup actions middleHdr newHdrs
                )
                hs
        , env (B.readFile "bench/headers.txt") $
            comp "File" . bench "File" . nf (aFromByteString actions)
        ]
  where
    toHeader = aFromTuple actions
    comp name =
        if shouldCompare
            then bcompare ("List." <> testName <> "." <> name)
            else id
    hName = unsafeParseHeaderName "Access-Control-Allow-Headers"
    hdrsList =
        zip hdrs $ B8.pack . show <$> [0 :: Int ..]
    prepHdrs name f =
        pureEnv
            ( aCreate actions $ uncurry toHeader <$> hdrsList
            , head hdrs
            , unsafeParseHeaderName "X-Frame-Options"
            )
            $ comp name . bench name . f

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
                . nf (\bs -> B8.map toLower bs == "close")
        , pureEnv ("Close" :: ByteString) $
            bcompare (topName <> ".BS")
                . bench "Custom"
                . nf (caseInsensitiveEq "close")
        ]
  where
    topName = "Case insensitive EQ"

withFile :: forall a b. (HeaderName -> ByteString -> b) -> (b -> a -> a) -> a -> ByteString -> a
withFile f add =
    loop
  where
    loop :: a -> ByteString -> a
    loop !acc rest =
        case B.break (== 0x0A) rest of
            ("", "") -> acc
            (final, "") ->
                add (fromLine final) acc
            (line, more) ->
                loop (add (fromLine line) acc) $ B.drop 1 more
      where
        fromLine bs =
            case B.break (== 0x3A) bs of
                (_, "") -> error $ "BAD LINE: " <> B8.unpack bs
                (k, v) -> unsafeParseHeaderName k `f` B.drop 1 v
