![Build Status](https://github.com/Vlix/http-types/actions/workflows/ci.yml/badge.svg?branch=master)
[![Hackage](https://img.shields.io/hackage/v/http-types.svg)](https://hackage.haskell.org/package/http-types)
[![Stackage LTS](http://stackage.org/package/http-types/badge/lts)](http://stackage.org/lts/package/http-types)
[![Stackage Nightly](http://stackage.org/package/http-types/badge/nightly)](http://stackage.org/nightly/package/http-types)
[![BSD 3-Clause License](https://img.shields.io/badge/License-BSD_3--Clause-blue.svg)](./LICENSE)

# Generic HTTP types for Haskell (for both client and server code).

The goal of this library is to have one location for any library, package or project
to base their general HTTP types on for better interoperability.

### This library provides basic types for the following:

* HTTP versions (e.g. `HTTP/1.1`)
* HTTP methods (e.g. `GET`)
* HTTP headers (e.g. `Content-Type`)
* HTTP statusses (e.g. `404`)

This library also contains some utility functions, e.g. related to URI handling,
that are not necessarily restricted in use to HTTP, but the scope is restricted
to things that are useful inside HTTP, i.e. no FTP URI parsing.
