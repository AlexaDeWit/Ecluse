{- | Request shaping and URL building for the npm data plane.

Three details of the wire protocol are load-bearing and handled here:

* __Content negotiation.__ Metadata comes in two forms selected by @Accept@: the
  __abbreviated__ install view (@application/vnd.npm.install-v1+json@), which the
  proxy treats as primary, and the __full__ packument (@application/json@),
  needed when a rule reasons over publish age (the abbreviated form drops the
  @time@ map). 'MetadataForm' selects between them; both request
  @Accept-Encoding: gzip@, since popular packuments are megabytes.
* __Scoped-name path encoding.__ A scoped name @\@scope/name@ is encoded on the
  wire as @\@scope%2Fname@: the scope separator is percent-encoded but the
  leading @\@@ is not. 'metadataRequest' builds this from an
  __already-parsed__ 'PackageName', never from raw client path segments.
* __Streaming and buffering.__ 'artifactRequest' marks its request
  __non-decompressing__ ('decompress' returns 'False'): a tarball is opaque
  binary that must reach the client byte-for-byte, so the @.tgz@ is never
  gunzipped in flight (and its @dist.integrity@ stays valid).
-}
module Ecluse.Core.Registry.Npm.Request (
    -- * Content negotiation
    MetadataForm (..),
    metadataAccept,

    -- * Conditional-GET validators
    Validators (..),
    noValidators,

    -- * Request building
    metadataRequest,
    artifactRequest,
    artifactRequestByFile,
    artifactRequestByUrl,
    artifactFileUrl,
    packageUrl,
    joinPath,

    -- * Shared internals
    encodePackagePath,
    withToken,
    addValidators,
    parseRequestEither,
) where

import Data.Text qualified as T
import Network.HTTP.Client (Request (decompress, redirectCount, requestHeaders), applyBearerAuth, parseRequest)
import Network.HTTP.Types.Header (hAccept, hAcceptEncoding, hIfModifiedSince, hIfNoneMatch)

import Ecluse.Core.Credential (Secret, unSecret)
import Ecluse.Core.Package (PackageName, pkgNamespace, renderPackageName, unScope, unscopedName)
import Ecluse.Core.Registry (UrlFormationError (EmptyBaseUrl, UnparseableUrl))
import Ecluse.Core.Server.Route (encodeComponent)
import Ecluse.Core.Text (joinUrlPath)
import Ecluse.Core.Version (Version, renderVersion)

{- | Which of npm's two metadata documents to request, selected by the @Accept@
header (see 'metadataAccept').
-}
data MetadataForm
    = {- | The install-optimised __abbreviated__ packument
      (@application/vnd.npm.install-v1+json@). Smaller and the proxy's primary
      view, but it drops the @time@ map.
      -}
      Abbreviated
    | {- | The __full__ packument (@application/json@). Larger, but the only form
      carrying the @time@ map a publish-age rule needs.
      -}
      Full
    deriving stock (Eq, Show)

{- | The @Accept@ header value selecting a 'MetadataForm'.

>>> metadataAccept Abbreviated
"application/vnd.npm.install-v1+json"

>>> metadataAccept Full
"application/json"
-}
metadataAccept :: MetadataForm -> ByteString
metadataAccept = \case
    Abbreviated -> "application/vnd.npm.install-v1+json"
    Full -> "application/json"

{- | The conditional-GET validators to relay on a metadata fetch. Replaying an
upstream's @ETag@ as @If-None-Match@ (or its @Last-Modified@ as
@If-Modified-Since@) lets the upstream answer @304 Not Modified@ with no body:
the cheap freshness check the proxy uses on a cache revalidation. Both are
forwarded only when present.
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

{- | Build the metadata @GET@ request for a package: the URL is
@{baseUrl}/{encoded-name}@ with the @Accept@ header for the chosen
'MetadataForm', @Accept-Encoding: gzip@, an optional bearer token, and any
relayed conditional-GET 'Validators'.

The package path is derived from an __already-parsed__ 'PackageName', then the
scope separator is percent-encoded (@\@scope/name@ -> @\@scope%2Fname@). Fails
with a 'UrlFormationError' only when the URL cannot be formed (an empty base URL).
-}
metadataRequest ::
    Text ->
    Maybe Secret ->
    MetadataForm ->
    Validators ->
    PackageName ->
    Either UrlFormationError Request
metadataRequest baseUrl token form validators name = do
    url <- packageUrl baseUrl name
    base <- parseRequestEither url
    pure
        . withToken token
        . addValidators validators
        $ base
            { requestHeaders =
                (hAccept, metadataAccept form)
                    : (hAcceptEncoding, "gzip")
                    : requestHeaders base
            }

{- | Build the artifact @GET@ request for one version's tarball.

The request is marked __non-decompressing__ ('decompress' returns 'False') so the
@.tgz@ bytes are streamed through verbatim: a tarball is opaque binary and must
reach the client byte-for-byte for its @dist.integrity@ to verify. The artifact
URL is the registry-served tarball location, derived like 'metadataRequest' but
addressing the version's artifact path. Exposed so the web layer can bracket it
for bounded-memory streaming (see the module header).

Fails with a 'UrlFormationError' only when the URL cannot be formed.
-}
artifactRequest ::
    Text ->
    Maybe Secret ->
    PackageName ->
    Version ->
    Either UrlFormationError Request
artifactRequest baseUrl token name version = do
    url <- artifactUrl baseUrl name version
    base <- parseRequestEither url
    pure
        . withToken token
        $ base
            { -- A tarball must never be gunzipped in flight: it is opaque binary
              -- whose integrity the client verifies, so stream the raw bytes. We
              -- deliberately advertise no @Accept-Encoding@ here: a @.tgz@ is
              -- already-compressed application data, and requesting a transport
              -- encoding we then refuse to decode ('decompress' is 'False') would
              -- risk a doubly-gzipped body that fails its @dist.integrity@.
              decompress = const False
            }

{- | Build the artifact @GET@ request addressing a tarball by its __preserved
on-the-wire filename__, at @{baseUrl}/{encoded-pkg}/-/{filename}@.

The serve path fetches an artifact by the exact filename the client requested:
the authoritative name for the bytes: rather than reconstructing it from
@(package, version)@ as 'artifactRequest' does, so a registry whose tarball naming
differs from the proxy's own convention still resolves. The @filename@ is taken
verbatim (the classifier has already passed it through the component-safety gate),
and the package segment is the same scope-percent-encoded path 'artifactRequest'
uses. The request is marked __non-decompressing__ for the same reason: a @.tgz@ is
opaque binary streamed byte-for-byte so its @dist.integrity@ verifies. Exposed so
the web layer can bracket it for bounded-memory streaming.

Fails with a 'UrlFormationError' only when the URL cannot be formed.
-}
artifactRequestByFile ::
    Text ->
    Maybe Secret ->
    PackageName ->
    Text ->
    Either UrlFormationError Request
artifactRequestByFile baseUrl token name filename = do
    url <- artifactFileUrl baseUrl name filename
    base <- parseRequestEither url
    pure
        . withToken token
        $ base
            { -- A tarball must never be gunzipped in flight (see 'artifactRequest').
              decompress = const False
            }

{- | Build the artifact @GET@ request addressing a tarball at its __authoritative
upstream location__: the absolute @url@ the projection preserved from the
upstream's @dist.tarball@: rather than reconstructing it from @(base, package,
file)@.

The artifact location is server-chosen data, not a derivable fact: a registry may
serve a version's tarball from a different host or a path the npm @/-/@ convention
cannot rebuild. Honouring the preserved location is what lets Écluse front those
registries; the URL it fetches is the same one the served packument's
@dist.integrity@ is paired with, so the bytes still verify.

The request is marked __non-decompressing__ for the same reason as 'artifactRequest':
a @.tgz@ is opaque binary streamed byte-for-byte. Fails with a 'UrlFormationError'
only when the @url@ cannot be parsed into a request.
-}
artifactRequestByUrl ::
    Text ->
    Maybe Secret ->
    Text ->
    Either UrlFormationError Request
artifactRequestByUrl _baseUrl token url = do
    base <- parseRequestEither url
    pure
        . withToken token
        $ base
            { -- A tarball must never be gunzipped in flight (see 'artifactRequest').
              decompress = const False
            }

{- The metadata/publish URL for a package: @{baseUrl}/{encoded-name}@, with
the scoped-name separator percent-encoded (@\@scope/name@ -> @\@scope%2Fname@).
-}
packageUrl :: Text -> PackageName -> Either UrlFormationError Text
packageUrl baseUrl name =
    joinPath baseUrl (encodePackagePath name)

{- The artifact (tarball) URL for one version:
@{baseUrl}/{encoded-name}/-/{tarball-file}@. npm serves a version's tarball
under the package's @/-/@ path; the filename is @{base}-{version}.tgz@ (scope
dropped from the file segment, as npm names it).
-}
artifactUrl :: Text -> PackageName -> Version -> Either UrlFormationError Text
artifactUrl baseUrl name version =
    joinPath baseUrl (encodePackagePath name <> "/-/" <> tarballFile name version)

{- | The artifact (tarball) URL addressing a __preserved filename__:
@{baseUrl}/{encoded-name}/-/{encoded-filename}@. The filename is the exact
on-the-wire name (not @{base}-{version}.tgz@ rebuilt from the coordinate), so the
bytes are fetched by the name the client requested; it is percent-encoded as a
single component ('Ecluse.Core.Server.Route.encodeComponent') so a once-decoded escape
in it cannot reach the upstream raw. Exposed so the serve path can record the
public artifact location on a mirror job (the same URL its public fetch targets).

Fails with a 'UrlFormationError' only when the URL cannot be formed.
-}
artifactFileUrl :: Text -> PackageName -> Text -> Either UrlFormationError Text
artifactFileUrl baseUrl name filename =
    joinPath baseUrl (encodePackagePath name <> "/-/" <> encodeComponent filename)

{- Join a base URL and an already-encoded path, tolerating one trailing slash
on the base so the join never doubles it. An empty base URL is refused with a
'UrlFormationError': the read- and write-path builders share this report, so an
unformable URL is never mislabelled as a publish failure.
-}
joinPath :: Text -> Text -> Either UrlFormationError Text
joinPath baseUrl path
    | T.null baseUrl = Left EmptyBaseUrl
    | otherwise = Right (joinUrlPath baseUrl path)

{- Encode a package name as its on-the-wire path segment. Each name component
(scope, base name) is percent-encoded ('Ecluse.Core.Server.Route.encodeComponent')
around the structural delimiters this builder writes: a scoped @\@scope/name@
becomes @\@{enc-scope}%2F{enc-base}@: the leading @\@@ and the @%2F@ separator
are written here, never derived from a component, so a legitimate scoped name
yields exactly one @%2F@: and an unscoped name is its single encoded component.
Encoding each component is the defence in depth that keeps a @'%'@, @'/'@, or
other reserved byte inside a decoded name from reaching the upstream URL raw (a
once-decoded @%2e%2e%2f@ is re-encoded to @%252e%252e%252f@), without
double-encoding the structural separator.
-}
encodePackagePath :: PackageName -> Text
encodePackagePath name = case pkgNamespace name of
    Just scope -> "@" <> encodeComponent (unScope scope) <> "%2F" <> encodeComponent (unscopedName name)
    Nothing -> encodeComponent (renderPackageName name)

{- The conventional npm tarball filename for a version: @{base}-{version}.tgz@.
The base name and version are percent-encoded as components around the structural
@'-'@ and @.tgz@ this builder writes, so a reserved byte in either cannot reach
the upstream URL raw. -}
tarballFile :: PackageName -> Version -> Text
tarballFile name version =
    encodeComponent (unscopedName name) <> "-" <> encodeComponent (renderVersion version) <> ".tgz"

{- Finalize an npm data-plane request: __disable redirect following__ ('redirectCount'
= 0) on __every__ request, and attach a bearer token when one is injected.

This is the single request-finalization point for the whole npm data plane: every
builder and call site funnels through it (it is also the only 'applyBearerAuth'): so
pinning @redirectCount = 0@ here makes one invariant universal: __Écluse never follows an
upstream redirect__, on the credentialed and the anonymous plane alike.

Two dangers it forecloses, one per plane:

\* __Credential leakage__ (credentialed plane). http-client's default ('redirectCount' =
  10) re-sends the @Authorization@ header to the redirect's @Location@, and its
  @shouldStripHeaderOnRedirect@ does not strip it cross-host: so a hostile or
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
withToken :: Maybe Secret -> Request -> Request
withToken Nothing request = request{redirectCount = 0}
withToken (Just secret) request =
    applyBearerAuth (encodeUtf8 (unSecret secret)) request{redirectCount = 0}

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

{- Parse a built URL into a 'Request', mapping a parse failure into a
'UrlFormationError'. The URL is derived from configuration and an already-safe
name, so a failure here is a configuration fault, reported uniformly with the
other URL-formation errors.
-}
parseRequestEither :: Text -> Either UrlFormationError Request
parseRequestEither url =
    case parseRequest (toString url) of
        Just request -> Right request
        Nothing -> Left (UnparseableUrl url)
