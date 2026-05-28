{-# LANGUAGE NumericUnderscores #-}

module Network.HTTP.Header.Constants where

import Network.HTTP.Header.Internal (HeaderName, unsafeMkHeaderName)

-- | [Accept](https://www.rfc-editor.org/rfc/rfc9110.html#name-accept)
--
-- @since 0.13.0
hAccept :: HeaderName
hAccept = unsafeMkHeaderName "accept" 0x8000_0000_0000_0000

-- | [Accept-Charset](https://www.rfc-editor.org/rfc/rfc9110.html#name-accept-charset)
--
-- @since 0.13.0
hAcceptCharset :: HeaderName
hAcceptCharset = unsafeMkHeaderName "accept-charset" 0x8100_0000_0000_0000

-- | [Accept-Encoding](https://www.rfc-editor.org/rfc/rfc9110.html#name-accept-encoding)
--
-- @since 0.13.0
hAcceptEncoding :: HeaderName
hAcceptEncoding = unsafeMkHeaderName "accept-encoding" 0x8100_0000_0000_0000

-- | [Accept-Language](https://www.rfc-editor.org/rfc/rfc9110.html#name-accept-language)
--
-- @since 0.13.0
hAcceptLanguage :: HeaderName
hAcceptLanguage = unsafeMkHeaderName "accept-language" 0x8100_0000_0000_0000

-- | [Accept-Ranges](https://www.rfc-editor.org/rfc/rfc9110.html#name-accept-ranges)
--
-- @since 0.13.0
hAcceptRanges :: HeaderName
hAcceptRanges = unsafeMkHeaderName "accept-ranges" 0x8100_0000_0000_0000

-- | [Age](https://www.rfc-editor.org/rfc/rfc9111.html#name-age)
--
-- @since 0.13.0
hAge :: HeaderName
hAge = unsafeMkHeaderName "age" 0x8000_0000_0000_0000

-- | [Allow](https://www.rfc-editor.org/rfc/rfc9110.html#name-allow)
--
-- @since 0.13.0
hAllow :: HeaderName
hAllow = unsafeMkHeaderName "allow" 0x8000_0000_0000_0000

-- | [Authorization](https://www.rfc-editor.org/rfc/rfc9110.html#name-authorization)
--
-- @since 0.13.0
hAuthorization :: HeaderName
hAuthorization = unsafeMkHeaderName "authorization" 0x8000_0000_0000_0000

-- | [Cache-Control](https://www.rfc-editor.org/rfc/rfc9111.html#name-cache-control)
--
-- @since 0.13.0
hCacheControl :: HeaderName
hCacheControl = unsafeMkHeaderName "cache-control" 0x8200_0000_0000_0000

-- | [Connection](https://www.rfc-editor.org/rfc/rfc9110.html#name-connection)
--
-- @since 0.13.0
hConnection :: HeaderName
hConnection = unsafeMkHeaderName "connection" 0x8000_0000_0000_0000

-- | [Content-Disposition](https://www.rfc-editor.org/rfc/rfc6266.html)
--
-- @since 0.13.0
hContentDisposition :: HeaderName
hContentDisposition = unsafeMkHeaderName "content-disposition" 0x8080_0000_0000_0000

-- | [Content-Encoding](https://www.rfc-editor.org/rfc/rfc9110.html#name-content-encoding)
--
-- @since 0.13.0
hContentEncoding :: HeaderName
hContentEncoding = unsafeMkHeaderName "content-encoding" 0x8080_0000_0000_0000

-- | [Content-Language](https://www.rfc-editor.org/rfc/rfc9110.html#name-content-language)
--
-- @since 0.13.0
hContentLanguage :: HeaderName
hContentLanguage = unsafeMkHeaderName "content-language" 0x8080_0000_0000_0000

-- | [Content-Length](https://www.rfc-editor.org/rfc/rfc9110.html#name-content-length)
--
-- @since 0.13.0
hContentLength :: HeaderName
hContentLength = unsafeMkHeaderName "content-length" 0x8080_0000_0000_0000

-- | [Content-Location](https://www.rfc-editor.org/rfc/rfc9110.html#name-content-location)
--
-- @since 0.13.0
hContentLocation :: HeaderName
hContentLocation = unsafeMkHeaderName "content-location" 0x8080_0000_0000_0000

-- | [Content-MD5](https://www.rfc-editor.org/rfc/rfc2616.html#section-14.15)
--
-- /This header has been obsoleted in RFC 9110./
--
-- @since 0.13.0
hContentMD5 :: HeaderName
hContentMD5 = unsafeMkHeaderName "content-md5" 0x80C0_0000_0000_0000

-- | [Content-Range](https://www.rfc-editor.org/rfc/rfc9110.html#name-content-range)
--
-- @since 0.13.0
hContentRange :: HeaderName
hContentRange = unsafeMkHeaderName "content-range" 0x8080_0000_0000_0000

-- | [Content-Type](https://www.rfc-editor.org/rfc/rfc9110.html#name-content-type)
--
-- @since 0.13.0
hContentType :: HeaderName
hContentType = unsafeMkHeaderName "content-type" 0x8080_0000_0000_0000

-- | [Cookie](https://www.rfc-editor.org/rfc/rfc6265.html#section-4.2)
--
-- @since 0.13.0
hCookie :: HeaderName
hCookie = unsafeMkHeaderName "cookie" 0x8000_0000_0000_0000

-- | [Date](https://www.rfc-editor.org/rfc/rfc9110.html#name-date)
--
-- @since 0.13.0
hDate :: HeaderName
hDate = unsafeMkHeaderName "date" 0x8000_0000_0000_0000

-- | [ETag](https://www.rfc-editor.org/rfc/rfc9110.html#name-etag)
--
-- @since 0.13.0
hETag :: HeaderName
hETag = unsafeMkHeaderName "etag" 0xC000_0000_0000_0000

-- | [Expect](https://www.rfc-editor.org/rfc/rfc9110.html#name-expect)
--
-- @since 0.13.0
hExpect :: HeaderName
hExpect = unsafeMkHeaderName "expect" 0x8000_0000_0000_0000

-- | [Expires](https://www.rfc-editor.org/rfc/rfc9111.html#name-expires)
--
-- @since 0.13.0
hExpires :: HeaderName
hExpires = unsafeMkHeaderName "expires" 0x8000_0000_0000_0000

-- | [From](https://www.rfc-editor.org/rfc/rfc9110.html#name-from)
--
-- @since 0.13.0
hFrom :: HeaderName
hFrom = unsafeMkHeaderName "from" 0x8000_0000_0000_0000

-- | [Host](https://www.rfc-editor.org/rfc/rfc9110.html#name-host-and-authority)
--
-- @since 0.13.0
hHost :: HeaderName
hHost = unsafeMkHeaderName "host" 0x8000_0000_0000_0000

-- | [If-Match](https://www.rfc-editor.org/rfc/rfc9110.html#name-if-match)
--
-- @since 0.13.0
hIfMatch :: HeaderName
hIfMatch = unsafeMkHeaderName "if-match" 0x9000_0000_0000_0000

-- | [If-Modified-Since](https://www.rfc-editor.org/rfc/rfc9110.html#name-if-modified-since)
--
-- @since 0.13.0
hIfModifiedSince :: HeaderName
hIfModifiedSince = unsafeMkHeaderName "if-modified-since" 0x9008_0000_0000_0000

-- | [If-None-Match](https://www.rfc-editor.org/rfc/rfc9110.html#name-if-none-match)
--
-- @since 0.13.0
hIfNoneMatch :: HeaderName
hIfNoneMatch = unsafeMkHeaderName "if-none-match" 0x9080_0000_0000_0000

-- | [If-Range](https://www.rfc-editor.org/rfc/rfc9110.html#name-if-range)
--
-- @since 0.13.0
hIfRange :: HeaderName
hIfRange = unsafeMkHeaderName "if-range" 0x9000_0000_0000_0000

-- | [If-Unmodified-Since](https://www.rfc-editor.org/rfc/rfc9110.html#name-if-unmodified-since)
--
-- @since 0.13.0
hIfUnmodifiedSince :: HeaderName
hIfUnmodifiedSince = unsafeMkHeaderName "if-unmodified-since" 0x9002_0000_0000_0000

-- | [Last-Modified](https://www.rfc-editor.org/rfc/rfc9110.html#name-last-modified)
--
-- @since 0.13.0
hLastModified :: HeaderName
hLastModified = unsafeMkHeaderName "last-modified" 0x8400_0000_0000_0000

-- | [Location](https://www.rfc-editor.org/rfc/rfc9110.html#name-location)
--
-- @since 0.13.0
hLocation :: HeaderName
hLocation = unsafeMkHeaderName "location" 0x8000_0000_0000_0000

-- | [Max-Forwards](https://www.rfc-editor.org/rfc/rfc9110.html#name-max-forwards)
--
-- @since 0.13.0
hMaxForwards :: HeaderName
hMaxForwards = unsafeMkHeaderName "max-forwards" 0x8800_0000_0000_0000

-- | [MIME-Version](https://www.rfc-editor.org/rfc/rfc2616.html#section-19.4.1)
--
-- @since 0.13.0
hMIMEVersion :: HeaderName
hMIMEVersion = unsafeMkHeaderName "mime-version" 0xF400_0000_0000_0000

-- | [Origin](https://www.rfc-editor.org/rfc/rfc6454.html#section-7)
--
-- @since 0.13.0
hOrigin :: HeaderName
hOrigin = unsafeMkHeaderName "origin" 0x8000_0000_0000_0000

-- | [Pragma](https://www.rfc-editor.org/rfc/rfc9111.html#name-pragma)
--
-- /This header has been deprecated in RFC 9111 in favor of "Cache-Control"./
--
-- @since 0.13.0
hPragma :: HeaderName
hPragma = unsafeMkHeaderName "pragma" 0x8000_0000_0000_0000

-- | [Prefer](https://www.rfc-editor.org/rfc/rfc7240.html#section-2)
--
-- @since 0.13.0
hPrefer :: HeaderName
hPrefer = unsafeMkHeaderName "prefer" 0x8000_0000_0000_0000

-- | [Preference-Applied](https://www.rfc-editor.org/rfc/rfc7240.html#section-3)
--
-- @since 0.13.0
hPreferenceApplied :: HeaderName
hPreferenceApplied = unsafeMkHeaderName "preference-applied" 0x8010_0000_0000_0000

-- | [Proxy-Authenticate](https://www.rfc-editor.org/rfc/rfc9110.html#name-proxy-authenticate)
--
-- @since 0.13.0
hProxyAuthenticate :: HeaderName
hProxyAuthenticate = unsafeMkHeaderName "proxy-authenticate" 0x8200_0000_0000_0000

-- | [Proxy-Authorization](https://www.rfc-editor.org/rfc/rfc9110.html#name-proxy-authorization)
--
-- @since 0.13.0
hProxyAuthorization :: HeaderName
hProxyAuthorization = unsafeMkHeaderName "proxy-authorization" 0x8200_0000_0000_0000

-- | [Range](https://www.rfc-editor.org/rfc/rfc9110.html#name-range)
--
-- @since 0.13.0
hRange :: HeaderName
hRange = unsafeMkHeaderName "range" 0x8000_0000_0000_0000

-- | [Referer](https://www.rfc-editor.org/rfc/rfc9110.html#name-referer)
--
-- @since 0.13.0
hReferer :: HeaderName
hReferer = unsafeMkHeaderName "referer" 0x8000_0000_0000_0000

-- | [Retry-After](https://www.rfc-editor.org/rfc/rfc9110.html#name-retry-after)
--
-- @since 0.13.0
hRetryAfter :: HeaderName
hRetryAfter = unsafeMkHeaderName "retry-after" 0x8200_0000_0000_0000

-- | [Server](https://www.rfc-editor.org/rfc/rfc9110.html#name-server)
--
-- @since 0.13.0
hServer :: HeaderName
hServer = unsafeMkHeaderName "server" 0x8000_0000_0000_0000

-- | [Set-Cookie](https://www.rfc-editor.org/rfc/rfc6265.html#section-4.1)
--
-- @since 0.13.0
hSetCookie :: HeaderName
hSetCookie = unsafeMkHeaderName "set-cookie" 0x8800_0000_0000_0000

-- | [TE](https://www.rfc-editor.org/rfc/rfc9110.html#name-te)
--
-- @since 0.13.0
hTE :: HeaderName
hTE = unsafeMkHeaderName "te" 0xC000_0000_0000_0000

-- | [Trailer](https://www.rfc-editor.org/rfc/rfc9110.html#name-trailer)
--
-- @since 0.13.0
hTrailer :: HeaderName
hTrailer = unsafeMkHeaderName "trailer" 0x8000_0000_0000_0000

-- | [Transfer-Encoding](https://www.rfc-editor.org/rfc/rfc9112#name-transfer-encoding)
--
-- @since 0.13.0
hTransferEncoding :: HeaderName
hTransferEncoding = unsafeMkHeaderName "transfer-encoding" 0x8040_0000_0000_0000

-- | [Upgrade](https://www.rfc-editor.org/rfc/rfc9110.html#name-upgrade)
--
-- @since 0.13.0
hUpgrade :: HeaderName
hUpgrade = unsafeMkHeaderName "upgrade" 0x8000_0000_0000_0000

-- | [User-Agent](https://www.rfc-editor.org/rfc/rfc9110.html#name-user-agent)
--
-- @since 0.13.0
hUserAgent :: HeaderName
hUserAgent = unsafeMkHeaderName "user-agent" 0x8400_0000_0000_0000

-- | [Vary](https://www.rfc-editor.org/rfc/rfc9110.html#name-vary)
--
-- @since 0.13.0
hVary :: HeaderName
hVary = unsafeMkHeaderName "vary" 0x8000_0000_0000_0000

-- | [Via](https://www.rfc-editor.org/rfc/rfc9110.html#name-via)
--
-- @since 0.13.0
hVia :: HeaderName
hVia = unsafeMkHeaderName "via" 0x8000_0000_0000_0000

-- | [Warning](https://www.rfc-editor.org/rfc/rfc9111.html#name-warning)
--
-- /This header has been obsoleted in RFC 9111./
--
-- @since 0.13.0
hWarning :: HeaderName
hWarning = unsafeMkHeaderName "warning" 0x8000_0000_0000_0000

-- | [WWW-Authenticate](https://www.rfc-editor.org/rfc/rfc9110.html#name-www-authenticate)
--
-- @since 0.13.0
hWWWAuthenticate :: HeaderName
hWWWAuthenticate = unsafeMkHeaderName "www-authenticate" 0xE800_0000_0000_0000
