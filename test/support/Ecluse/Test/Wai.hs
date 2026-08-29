-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Readers for an in-process WAI stub and the responses a proxy serves from it.

The integration pipeline fixture, the publish spec, and the load bench all address
loopback stubs the same way and read the same fields off a served response, so the
readers live here rather than inside one suite's fixture module.
-}
module Ecluse.Test.Wai (
    -- * Addressing an in-process stub
    localhost,
    localhostUrl,
    selfBaseUrl,

    -- * Reading a request
    lookupAuth,
    lookupIfNoneMatch,

    -- * Reading a response
    status,
    reason,
    header,
    decodedBody,
    servedVersions,
) where

import Data.Aeson (Value (Null, Object), eitherDecodeStrict)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.List (lookup)
import Network.HTTP.Types (Header, hAuthorization, statusCode, statusMessage)
import Network.HTTP.Types.Header (hHost, hIfNoneMatch)
import Network.Wai (Request (requestHeaders))
import Network.Wai.Test (SResponse (simpleBody, simpleHeaders, simpleStatus))

import Ecluse.Core.Security.Egress (RegistryUrl)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)

-- | The base URL of a loopback stub on the given port, by the @localhost@ DNS name.
localhost :: Int -> Text
localhost port = "http://localhost:" <> show port

{- | 'localhost' as the egress witness a mount's upstream is bound with, through the
test-only plain-HTTP former a release build does not carry.
-}
localhostUrl :: Int -> RegistryUrl
localhostUrl = loopbackRegistryUrl . localhost

{- | The base URL a request reached a stub at, from its @Host@ header: the only place the
harness's ephemeral port appears. An absent header falls back to @http:\/\/localhost@.
-}
selfBaseUrl :: Request -> Text
selfBaseUrl req = "http://" <> maybe "localhost" decodeUtf8 (lookup hHost (requestHeaders req))

-- | The @Authorization@ header value a request carried, if any.
lookupAuth :: [Header] -> Maybe ByteString
lookupAuth = lookup hAuthorization

-- | The @If-None-Match@ header value a request carried, if any.
lookupIfNoneMatch :: [Header] -> Maybe ByteString
lookupIfNoneMatch = lookup hIfNoneMatch

-- | The numeric status of a response.
status :: SResponse -> Int
status = statusCode . simpleStatus

{- | The HTTP reason phrase of a response (e.g. @"Forbidden"@). Reading it forces the status'
lazy message, so an assertion covers the per-status reason mapping, not just the code.
-}
reason :: SResponse -> ByteString
reason = statusMessage . simpleStatus

-- | A response header by name, case-insensitively.
header :: ByteString -> SResponse -> Maybe ByteString
header name resp = lookup (CI.mk name) (simpleHeaders resp)

{- | The decoded JSON body of a response, or 'Null' if it did not decode. A non-JSON body
then surfaces as a plain assertion mismatch, not a crash.
-}
decodedBody :: SResponse -> Value
decodedBody resp = fromRight Null (eitherDecodeStrict (LBS.toStrict (simpleBody resp)))

-- | The version keys present in a served packument body, sorted.
servedVersions :: SResponse -> [Text]
servedVersions resp = case decodedBody resp of
    Object o -> case KeyMap.lookup "versions" o of
        Just (Object vs) -> sort (map Key.toText (KeyMap.keys vs))
        _ -> []
    _ -> []
