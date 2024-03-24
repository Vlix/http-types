![Build Status](https://github.com/Vlix/http-types/actions/workflows/ci.yml/badge.svg?branch=master)
[![Hackage](https://img.shields.io/hackage/v/http-types.svg)](https://hackage.haskell.org/package/http-types)
[![Stackage LTS](http://stackage.org/package/http-types/badge/lts)](http://stackage.org/lts/package/http-types)
[![Stackage Nightly](http://stackage.org/package/http-types/badge/nightly)](http://stackage.org/nightly/package/http-types)
[![BSD 3-Clause License](https://img.shields.io/badge/License-BSD_3--Clause-blue.svg)](./LICENSE)

# Generic HTTP types for Haskell (for both client and server code).

The goal of this library is to have one location for any library, package or project
to base their general HTTP types on for better interoperability.

This library also provides some utility functions for parsing and rendering HTTP types.

### This library provides basic types for the following:

* HTTP versions (e.g. `HTTP/1.1`) in [`Network.HTTP.Types.Version`](https://hackage.haskell.org/package/http-types/docs/Network-HTTP-Types-Version.html)
* HTTP methods (e.g. `GET`) in [`Network.HTTP.Types.Method`](https://hackage.haskell.org/package/http-types/docs/Network-HTTP-Types-Method.html)
* HTTP headers (e.g. `Content-Type`) in [`Network.HTTP.Types.Header`](https://hackage.haskell.org/package/http-types/docs/Network-HTTP-Types-Header.html)
* HTTP statusses (e.g. `404`) in [`Network.HTTP.Types.Status`](https://hackage.haskell.org/package/http-types/docs/Network-HTTP-Types-Status.html)
* HTTP URIs (e.g. paths, query parameters, etc.) in [`Network.HTTP.Types.URI`](https://hackage.haskell.org/package/http-types/docs/Network-HTTP-Types-URI.html)

The main module [`Network.HTTP.Types`](https://hackage.haskell.org/package/http-types/docs/Network-HTTP-Types.html)
exports everything as well, so you don't have to import the modules separately if you don't want to.
