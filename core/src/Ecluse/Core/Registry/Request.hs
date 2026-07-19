-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Ecosystem-agnostic request mechanics shared by every registry adapter's request
layer: the redirect-pin finaliser every credential-bearing request must pass through,
conditional-GET validators, URL parsing into a typed 'UrlFormationError', the
empty-base-guarded path join, and the opaque-artifact request core that streams a body
byte-for-byte.

An adapter supplies only its ecosystem's protocol facts (its media types, its path
encoding, its credential scheme). The request formation itself, pinning the redirect
count, marking an artifact non-decompressing, relaying validators, parsing a URL, is
uniform across npm, PyPI, and RubyGems, so it lives here rather than in any one
ecosystem's namespace. This module reads no credential: the credential attach is an
injected 'Request' transformation, so no ecosystem's scheme leaks into the shared home.
-}
module Ecluse.Core.Registry.Request (
    -- * Request finalisation
    finaliseRequest,

    -- * Conditional-GET validators
    Validators (..),
    noValidators,
    addValidators,

    -- * Request building
    artifactRequestByUrl,
    joinPath,
    parseRequestEither,
) where

import Data.Text qualified as T
import Network.HTTP.Client (Request (decompress, redirectCount, requestHeaders), parseRequest)
import Network.HTTP.Types.Header (hIfModifiedSince, hIfNoneMatch)

import Ecluse.Core.Registry (UrlFormationError (EmptyBaseUrl, UnparseableUrl))
import Ecluse.Core.Text (joinUrlPath)

{- | Finalise a data-plane request: __disable redirect following__ ('redirectCount' = 0)
on __every__ request, then apply the ecosystem's injected credential attach.

This is the single request-finalisation point for the whole data plane: every adapter's
request builder funnels through it, so pinning @redirectCount = 0@ here makes one
invariant universal: __Écluse never follows an upstream redirect__, on the credentialed
and the anonymous plane alike. The credential attach is injected (a 'Request' -> 'Request'
function, an ecosystem's own scheme: npm's @Bearer@, another's @Basic@) rather than fixed
here, so no attach can reach the wire around the pin.

Two dangers it forecloses, one per plane:

\* __Credential leakage__ (credentialed plane). http-client's default ('redirectCount' =
  10) re-sends the @Authorization@ header to the redirect's @Location@, and its
  @shouldStripHeaderOnRedirect@ does not strip it cross-host, so a hostile or
  misconfigured upstream could @302@ a forwarded/minted credential to an attacker-chosen
  host. That is especially dangerous on the __trusted private manager__, where a redirect
  could exfiltrate the credential to an attacker-chosen target; pinning @redirectCount = 0@
  removes the hop entirely rather than relying on the per-hop egress controls.

\* __SSRF via redirect__ (anonymous plane). The host allowlist is enforced when the URL is
  built, not per redirect hop, so following a @302@ would let an allowlisted upstream
  steer an anonymous fetch to __any__ host: an internal/cloud-metadata address or any
  off-allowlist host: re-gated by nothing. Not following the redirect removes the hop
  there is to gate.

The accepted consequence, symmetric across both planes: a read no longer follows an
upstream's CDN @302@: it returns the @3xx@ to the serve path rather than chasing it. That
is the safer posture, and the proxy already honours the __packument's__ @dist.tarball@
location explicitly, gated by the egress policy, rather than relying on redirects.
Redirect-following for a nonstandard upstream (a presigned/redirecting object store) is an
explicit, per-upstream opt-in, never the default.
-}
finaliseRequest :: (Request -> Request) -> Request -> Request
finaliseRequest attachCredential request = attachCredential request{redirectCount = 0}

{- | The conditional-GET validators to relay on a metadata fetch. Replaying an
upstream's @ETag@ as @If-None-Match@ (or its @Last-Modified@ as @If-Modified-Since@) lets
the upstream answer @304 Not Modified@ with no body: the cheap freshness check the proxy
uses on a cache revalidation. Both are forwarded only when present.
-}
data Validators = Validators
    { validatorIfNoneMatch :: Maybe ByteString
    -- ^ An entity tag to send as @If-None-Match@ (an upstream @ETag@).
    , validatorIfModifiedSince :: Maybe ByteString
    {- ^ An RFC-1123 date to send as @If-Modified-Since@ (an upstream
    @Last-Modified@).
    -}
    }
    deriving stock (Eq, Show)

-- | No conditional-GET validators: an unconditional fetch.
noValidators :: Validators
noValidators = Validators{validatorIfNoneMatch = Nothing, validatorIfModifiedSince = Nothing}

-- Add the present conditional-GET validators as request headers.
addValidators :: Validators -> Request -> Request
addValidators validators request =
    request{requestHeaders = newHeaders <> requestHeaders request}
  where
    newHeaders =
        catMaybes
            [ (,) hIfNoneMatch <$> validatorIfNoneMatch validators
            , (,) hIfModifiedSince <$> validatorIfModifiedSince validators
            ]

{- | Build the artifact @GET@ request addressing a tarball at its __authoritative
upstream location__: the absolute @url@ a projection preserved from the upstream's
@dist.tarball@, rather than reconstructing it from a @(base, package, file)@ coordinate.
The ecosystem's credential attach is injected and the request is finalised through
'finaliseRequest', so the redirect pin always applies.

The artifact location is server-chosen data, not a derivable fact: a registry may serve a
version's tarball from a different host or a path a naming convention cannot rebuild.
Honouring the preserved location is what lets Écluse front those registries; the URL it
fetches is the same one the served metadata's integrity digest is paired with, so the
bytes still verify.

The request is marked __non-decompressing__ ('decompress' returns 'False'): a tarball is
opaque binary that must reach the client byte-for-byte, so it is never gunzipped in flight
and its integrity digest stays valid. Fails with a 'UrlFormationError' only when the @url@
cannot be parsed into a request.
-}
artifactRequestByUrl :: (Request -> Request) -> Text -> Either UrlFormationError Request
artifactRequestByUrl attachCredential url = do
    base <- parseRequestEither url
    pure . finaliseRequest attachCredential $ base{decompress = const False}

{- Join a base URL and an already-encoded path, tolerating one trailing slash on the base
so the join never doubles it. An empty base URL is refused with a 'UrlFormationError': the
read- and write-path builders share this report, so an unformable URL is never mislabelled
as a publish failure.
-}
joinPath :: Text -> Text -> Either UrlFormationError Text
joinPath baseUrl path
    | T.null baseUrl = Left EmptyBaseUrl
    | otherwise = Right (joinUrlPath baseUrl path)

{- Parse a built URL into a 'Request', mapping a parse failure into a 'UrlFormationError'.
The URL is derived from configuration and an already-safe name, so a failure here is a
configuration fault, reported uniformly with the other URL-formation errors.
-}
parseRequestEither :: Text -> Either UrlFormationError Request
parseRequestEither url =
    case parseRequest (toString url) of
        Just request -> Right request
        Nothing -> Left (UnparseableUrl url)
