-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Ecosystem-agnostic request mechanics shared by every registry adapter's request
layer: the redirect-pin finaliser every credential-bearing request must pass through,
conditional-GET validators, URL parsing into a typed 'UrlFormationError', the
empty-base-guarded path join, and the opaque-artifact request core that streams a body
byte-for-byte.

An adapter supplies only its ecosystem's protocol facts (its media types, its path
encoding, its credential presentation). The request formation itself, pinning the
redirect count, marking an artifact non-decompressing, relaying validators, parsing a
URL, is uniform across npm, PyPI, and RubyGems, so it lives here rather than in any one
ecosystem's namespace. The presentation an adapter declares ('CredentialMapping') is
data this module carries but never spells: its encoding half is opaque, and
'attachCredential' is the only way to run one, so every attach composes through the pin.
-}
module Ecluse.Core.Registry.Request (
    -- * Request finalisation
    finaliseRequest,

    -- * Credential presentation
    CredentialMapping,
    credentialMapping,
    credentialRecover,
    attachCredential,

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
import Network.HTTP.Types.Header (HeaderName, RequestHeaders, hIfModifiedSince, hIfNoneMatch)

import Ecluse.Core.Credential (Secret)
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
finaliseRequest attach request = (attach request){redirectCount = 0}

{- | One ecosystem's credential presentation: how a client's credential is recovered from
the headers it presented, and how a credential is carried on an outbound request. The two
are one type because they are one contract: what is recovered is the token __text__, so
re-attaching it always goes back through the same ecosystem's encoding rather than
replaying a header verbatim. An adapter declares exactly one
('Ecluse.Core.Registry.Adapter.Types.serveCredential'), and the neutral serve pipeline
consumes the recovery, so no scheme is spelled outside the ecosystem that presents it.

The encoding half (the carrying header and how a token renders into it) is unreachable
outside this module: the constructor is hidden and 'attachCredential' is the only way to
run one. That makes the redirect pin un-bypassable by construction rather than by
convention, since every attach is composed through 'finaliseRequest' here.
-}
data CredentialMapping = CredentialMapping
    { credentialRecover :: RequestHeaders -> Maybe Secret
    {- ^ Recover the token text a client presented, or 'Nothing' when the request carries
    no credential in this ecosystem's form. The edge gate compares the result against the
    configured inbound token and denies a 'Nothing' whenever one is configured, so a
    foreign presentation is refused rather than half-read.
    -}
    , -- The header an outbound credential is carried on: named per ecosystem, never assumed.
      credentialHeader :: HeaderName
    , -- How a token renders into that header's value (the ecosystem's own scheme).
      credentialRender :: Secret -> ByteString
    }

{- | Declare an ecosystem's credential presentation: the recovery over the headers a
client presented, the header an outbound credential is carried on, and how the token
renders into that header's value.
-}
credentialMapping ::
    (RequestHeaders -> Maybe Secret) ->
    HeaderName ->
    (Secret -> ByteString) ->
    CredentialMapping
credentialMapping recover header render =
    CredentialMapping
        { credentialRecover = recover
        , credentialHeader = header
        , credentialRender = render
        }

{- | Attach a credential to an outbound request under the mapping's own header, and
finalise it through 'finaliseRequest'. This is the only way to run an ecosystem's
encoding, so the redirect pin holds on the credentialed and the anonymous request alike:
'Nothing' attaches no header and is still pinned.
-}
attachCredential :: CredentialMapping -> Maybe Secret -> Request -> Request
attachCredential mapping token = finaliseRequest $ case token of
    Nothing -> id
    Just secret -> \request ->
        request
            { requestHeaders =
                (credentialHeader mapping, credentialRender mapping secret) : requestHeaders request
            }

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
The credential is attached under the ecosystem's own presentation and the request is
finalised through 'attachCredential', so the redirect pin always applies.

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
artifactRequestByUrl :: CredentialMapping -> Maybe Secret -> Text -> Either UrlFormationError Request
artifactRequestByUrl mapping token url = do
    base <- parseRequestEither url
    pure . attachCredential mapping token $ base{decompress = const False}

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
