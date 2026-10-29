-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Conditional-GET and @ETag@ handling, split by how the served body relates to upstream's.
A pass-through body (an artifact) is upstream's bytes, so the client's validators are relayed
upstream and an upstream @304@ is passed back. A merged listing on the packument path takes our own
'ETag', derived from the serve's inputs rather than hashed over its output. It can be stale only in
the safe direction (a spurious @200@, never a wrong @304@), and a @304@ costs no assembly. RubyGems
Compact Index replies and compact specifications hash their rendered identity bytes instead, so a
@304@ skips compression but still pays the fetch, merge or serialisation.
-}
module Ecluse.Core.Server.Conditional (
    -- * Our own ETag (transformed bodies)
    ETag,
    mkStrongETag,
    mkWeakETag,
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

import Ecluse.Core.Server.FieldValue (fieldValues)

-- | An entity tag in the wire form the @ETag@ header carries, kept apart from the bare digest.
newtype ETag = ETag Text
    deriving stock (Eq, Ord, Show)

-- | Quote a SHA-256 fingerprint, hex-encoded, as a strong 'ETag': one exact byte representation.
mkStrongETag :: Digest SHA256 -> ETag
mkStrongETag digest = ETag (quotedHex digest)

-- | A weak 'ETag' over a fingerprint, for a representation whose exact bytes the codec may change.
mkWeakETag :: Digest SHA256 -> ETag
mkWeakETag digest = ETag ("W/" <> quotedHex digest)

quotedHex :: Digest SHA256 -> Text
quotedHex digest = "\"" <> decodeUtf8 (convertToBase Base16 digest :: ByteString) <> "\""

-- | The 'ETag's wire form, the quoted opaque tag as it goes into the header.
renderETag :: ETag -> Text
renderETag (ETag t) = t

-- | The @ETag@ response header carrying this validator.
etagHeader :: ETag -> Header
etagHeader etag = (hETag, encodeUtf8 (renderETag etag))

-- | Whether the client's validator already matches the transformed body we would serve.
data Conditional
    = -- | Unchanged from the client's validator: answer @304@ with this 'ETag' and no body.
      NotModified ETag
    | -- | Changed, or no validator was sent: serve @200@ with this 'ETag'.
      Modified ETag
    deriving stock (Eq, Show)

-- | Compare weakly (RFC 9110) against our own 'ETag'. A merge has no single timestamp, so @If-Modified-Since@ is ignored.
evaluateETag :: RequestHeaders -> ETag -> Conditional
evaluateETag headers etag
    | matches = NotModified etag
    | otherwise = Modified etag
  where
    matches :: Bool
    matches = any clientMatches (fieldValues hIfNoneMatch headers)

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

-- | The only two client headers relayed upstream for a __pass-through__ body, so upstream can answer @304@.
forwardValidators :: RequestHeaders -> RequestHeaders
forwardValidators = filter (isValidator . fst)
  where
    isValidator name = name == hIfNoneMatch || name == hIfModifiedSince

-- | Whether a pass-through upstream response is a @304@ to pass back, upstream's own validator having decided.
isNotModified :: Status -> Bool
isNotModified s = statusCode s == 304
