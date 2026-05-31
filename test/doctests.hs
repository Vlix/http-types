module Main where

import Test.DocTest

main :: IO ()
main =
    doctest
        [ "-XOverloadedStrings"
        , "-isrc"
        , "src/Network/HTTP/Types.hs"
        ]
