{- | Conditional-GET / ETag handling, split by how the served body relates to
upstream's.

The proxy serves two kinds of body, and they validate differently (see
@docs\/architecture\/web-layer.md@ → "Middleware and helper libraries"):

* __Pass-through bodies__ — artifacts, and unfiltered private-upstream metadata —
  are byte-identical to upstream's, so upstream's own validator is authoritative.
  The client's validators are __relayed upstream__ ('forwardValidators') and an
  upstream @304@ is passed straight back ('isNotModified'). Relaying is correct
  precisely because we do not change the bytes.

* __Transformed bodies__ — every packument, which is merged across upstreams and
  filtered by the rules — differ from any single upstream's body, so an upstream
  validator would validate the wrong bytes. We instead compute our __own__ strong
  'ETag' over __what we serve__ ('ownETag') and answer the client's conditional
  request against that ('evaluateOwnETag').

The own-ETag is a SHA-256 over the exact served bytes, so it changes iff the
served document changes — a filtered version dropping in or out, a @latest@
repoint, an integrity divergence — and never collides a stale body onto a fresh
one. The functions here are pure; turning a 'Conditional' or relayed status into a
WAI response is the serving layer's job.
-}
module Ecluse.Server.Conditional (
    -- * Our own ETag (transformed bodies)
    ETag,
    ownETag,
    renderETag,
    etagHeader,
    Conditional (..),
    evaluateOwnETag,

    -- * Relaying validators (pass-through bodies)
    forwardValidators,
    isNotModified,
) where

import Crypto.Hash (Digest, SHA256, hashlazy)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Data.Text qualified as T
import Network.HTTP.Types (Header, RequestHeaders, Status, hETag, hIfModifiedSince, hIfNoneMatch, statusCode)

-- ── our own ETag (transformed bodies) ────────────────────────────────────────

{- | A strong entity tag for a body we serve: the quoted opaque-tag form
(@"…"@), as it appears in the @ETag@ header. A 'newtype' so the quoted wire form
is not confused with the bare digest or any other 'Text'.
-}
newtype ETag = ETag Text
    deriving stock (Eq, Ord, Show)

{- | Compute our own strong 'ETag' over the __served bytes__ — a SHA-256 digest,
hex-encoded and quoted. It tracks exactly what we serve, so a transformed
(filtered, merged) packument gets a validator that changes when, and only when,
the served document does. Computing it over upstream's body instead would
validate the wrong bytes.
-}
ownETag :: LByteString -> ETag
ownETag body = ETag ("\"" <> digest <> "\"")
  where
    digest :: Text
    digest = decodeUtf8 (convertToBase Base16 (hashlazy body :: Digest SHA256) :: ByteString)

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
    = {- | The served body is unchanged from the client's validator — answer @304@
      with this 'ETag', no body.
      -}
      NotModified ETag
    | {- | The served body differs (or no validator was sent) — serve @200@ with
      this 'ETag' header.
      -}
      Modified ETag
    deriving stock (Eq, Show)

{- | Evaluate a conditional request against our own ETag for a transformed body.

The body's 'ownETag' is computed, then matched against the request's
@If-None-Match@: a @*@ wildcard, or any tag in the (comma-separated) list whose
opaque value equals ours, is a match → 'NotModified'. The match is __weak__ (RFC
7232): a @W/@ prefix on either side is ignored, so a client echoing our tag with a
weakness marker still matches. Anything else — a stale tag, or no validator —
is 'Modified'.

@If-Modified-Since@ is deliberately not consulted for transformed bodies: a merged
packument has no single upstream @Last-Modified@ to compare to, and the strong
content ETag is the precise validator.
-}
evaluateOwnETag :: RequestHeaders -> LByteString -> Conditional
evaluateOwnETag headers body
    | matches = NotModified etag
    | otherwise = Modified etag
  where
    etag :: ETag
    etag = ownETag body

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

-- ── relaying validators (pass-through bodies) ────────────────────────────────

{- | The client's conditional validators to relay upstream for a __pass-through__
body. Only the request-side conditional headers (@If-None-Match@,
@If-Modified-Since@) are forwarded; everything else is dropped, since this is the
exact set that lets upstream answer @304@ for a body we serve unchanged.
-}
forwardValidators :: RequestHeaders -> RequestHeaders
forwardValidators = filter (isValidator . fst)
  where
    isValidator name = name == hIfNoneMatch || name == hIfModifiedSince

{- | Whether an upstream response is a @304 Not Modified@ to pass straight back to
the client unchanged. Used on the pass-through path, where upstream's own
validator decided the conditional request.
-}
isNotModified :: Status -> Bool
isNotModified s = statusCode s == 304

-- ── header helpers ───────────────────────────────────────────────────────────

-- All values for a header name (a header may legally repeat).
lookupAll :: (Eq a) => a -> [(a, b)] -> [b]
lookupAll name = map snd . filter ((== name) . fst)
