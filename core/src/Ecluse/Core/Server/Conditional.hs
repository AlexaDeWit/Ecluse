-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Conditional-GET and @ETag@ handling, split by how the served body relates to upstream's.

A pass-through body (an artifact, unfiltered private metadata) is byte-identical to
upstream's, so the client's validators are relayed upstream ('forwardValidators') and an
upstream @304@ is passed back ('isNotModified'). A transformed body (every packument is
merged and filtered) takes our own strong 'ETag' instead, derived from the serve's inputs
rather than hashed over its output. It can therefore be stale only in the safe direction, a
spurious @200@ and never a wrong @304@, and a @304@ costs no assembly at all.
-}
module Ecluse.Core.Server.Conditional (
    -- * Our own ETag (transformed bodies)
    ETag,
    mkStrongETag,
    renderETag,
    etagHeader,
    Conditional (..),
    evaluateETag,

    -- * Relaying validators (pass-through bodies)
    forwardValidators,
    isNotModified,
) where

import Crypto.Hash (Digest, SHA256)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Data.Text qualified as T
import Network.HTTP.Types (Header, RequestHeaders, Status, statusCode)
import Network.HTTP.Types.Header (hETag, hIfModifiedSince, hIfNoneMatch)

{- | A strong entity tag in the quoted wire form (@"…"@) the @ETag@ header carries. The
'newtype' keeps that quoted form from being confused with the bare digest.
-}
newtype ETag = ETag Text
    deriving stock (Eq, Ord, Show)

{- | Quote a SHA-256 digest as a strong 'ETag', hex-encoded. The digest is whatever
fingerprint the serving layer stands behind.
-}
mkStrongETag :: Digest SHA256 -> ETag
mkStrongETag digest = ETag ("\"" <> hex <> "\"")
  where
    hex :: Text
    hex = decodeUtf8 (convertToBase Base16 digest :: ByteString)

-- | The 'ETag's wire form, the quoted opaque tag as it goes into the header.
renderETag :: ETag -> Text
renderETag (ETag t) = t

-- | The @ETag@ response header carrying this validator.
etagHeader :: ETag -> Header
etagHeader etag = (hETag, encodeUtf8 (renderETag etag))

{- | The conditional outcome for a transformed body: whether the client's
validator already matches what we would serve.
-}
data Conditional
    = {- | The served body is unchanged from the client's validator. Answer @304@
      with this 'ETag' and no body.
      -}
      NotModified ETag
    | {- | The served body differs, or no validator was sent. Serve @200@ with
      this 'ETag' header.
      -}
      Modified ETag
    deriving stock (Eq, Show)

{- | Evaluate a conditional request against our own 'ETag'. The comparison is __weak__ (RFC
7232), and @If-Modified-Since@ is not consulted: a merge has no single upstream timestamp.
-}
evaluateETag :: RequestHeaders -> ETag -> Conditional
evaluateETag headers etag
    | matches = NotModified etag
    | otherwise = Modified etag
  where
    matches :: Bool
    matches = any clientMatches (lookupAll hIfNoneMatch headers)

    -- One If-None-Match header value matches if it is a wildcard or lists a tag
    -- whose opaque value equals ours (weak comparison).
    clientMatches :: ByteString -> Bool
    clientMatches raw =
        let value = T.strip (decodeUtf8 raw)
         in value == "*"
                || ours `elem` map normaliseTag (splitTags value)

    ours :: Text
    ours = normaliseTag (renderETag etag)

-- Split a comma-separated If-None-Match value into its individual tags, trimmed.
splitTags :: Text -> [Text]
splitTags = filter (not . T.null) . map T.strip . T.splitOn ","

-- Normalise an entity tag for weak comparison: drop a leading @W/@ weakness
-- marker, leaving the quoted opaque tag the two sides are compared on.
normaliseTag :: Text -> Text
normaliseTag t = fromMaybe t (T.stripPrefix "W/" t)

{- | The client's conditional validators to relay upstream for a __pass-through__ body. Only
these two are forwarded, so upstream answers @304@ without receiving any other client header.
-}
forwardValidators :: RequestHeaders -> RequestHeaders
forwardValidators = filter (isValidator . fst)
  where
    isValidator name = name == hIfNoneMatch || name == hIfModifiedSince

{- | Whether an upstream response is a @304 Not Modified@ to pass straight back to the
client. Used on the pass-through path, where upstream's own validator decided.
-}
isNotModified :: Status -> Bool
isNotModified s = statusCode s == 304

-- All values for a header name (a header may legally repeat).
lookupAll :: (Eq a) => a -> [(a, b)] -> [b]
lookupAll name = map snd . filter ((== name) . fst)
