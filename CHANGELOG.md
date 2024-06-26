# Changelog for `http-types`

## 0.12.5 [XXXX-XX-XX]

* Export everything from `Network.HTTP.Types`
* Added a bunch of regression, unit and property tests for stability.
* Updated the `README.md`

## 0.12.4 [2023-11-29]

* Add `Data` and `Generic` instances to `ByteRange`, `StdMethod`, `Status` and `HttpVersion`.
* Rework of all the documentation, with the addition of `@since` notations.

## 0.12.3 [2019-02-24]

* Remove now-invalid doctest options from `doctests.hs`.

## 0.12.2 [2018-09-26]

* Add new `parseQueryReplacePlus` function, which allows specifying whether to replace `'+'` with `' '`.
* Add header name constants for "Prefer" and "Preference-Applied" (RFC 7240).

## 0.12.1 [2018-01-31]

* Add new functions for constructing a query URI where not all parts are escaped.

## 0.12 [2018-01-28]

* URI encoding is now back to upper-case hexadecimal, as that is the preferred canonicalization, and the previous change caused issues with URI signing in at least `amazonka`.

## 0.11 [2017-11-29]

* Remove dependency on `blaze-builder`. (Note that as a side effect of this, URI encoding is now using lower-case rather than upper-case hexadecimal.)
* Add `Bounded` instance to `Status`.
* Re-export more status codes and `http20` from `Network.HTTP.Types`.

## 0.10 [2017-10-22]

* New status codes, new headers.
* Fixed typo in `imATeapot`, added missing `toEnum`.
* Oh, and `http20`.

## 0.9.1 [2016-06-04]

* New function: `parseByteRanges`.
* Support for HTTP status 422 "Unprocessable Entity" (RFC 4918).

## 0.9 [2015-10-09]

* No changelog was maintained up to version `0.9`.
