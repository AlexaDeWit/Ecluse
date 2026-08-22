-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Request shaping and URL building for the npm data plane. The ecosystem-agnostic
mechanics (the redirect-pin finaliser, conditional-GET validators, URL parsing, the path
join, the opaque-artifact request core) live in "Ecluse.Core.Registry.Request". This
module holds only npm's own protocol facts and composes them through that shared home.

Three details of the wire protocol are load-bearing and handled here:

* __Content negotiation__. Metadata comes in two forms selected by @Accept@. The
  __abbreviated__ install view (@application/vnd.npm.install-v1+json@) is the proxy's
  primary view. A rule that reasons over publish age needs the __full__ packument
  (@application/json@), because the abbreviated form drops the @time@ map.
  'MetadataForm' selects between them. Both request @Accept-Encoding: gzip@, because
  popular packuments are megabytes.
* __Scoped-name path encoding__. The wire form of a scoped name @\@scope/name@ is
  @\@scope%2Fname@: the scope separator is percent-encoded, the leading @\@@ is
  not. 'metadataRequest' builds this from an __already-parsed__ 'PackageName',
  never from raw client path segments.
* __Streaming and buffering__. The artifact builders ('artifactRequestByFile',
  'artifactRequestByUrl') mark their request __non-decompressing__. A @.tgz@ is
  opaque binary that reaches the client byte-for-byte, so its @dist.integrity@ stays
  valid. 'artifactRequestByUrl' forms its request through the shared
  'Ecluse.Core.Registry.Request.artifactRequestByUrl', under npm's own credential
  presentation ("Ecluse.Core.Registry.Npm.Credential").
-}
module Ecluse.Core.Registry.Npm.Request (
    -- * Content negotiation
    MetadataForm (..),
    metadataAccept,

    -- * Conditional-GET validators (re-exported from the shared request home)
    Validators (..),
    noValidators,

    -- * Request building
    metadataRequest,
    artifactRequestByFile,
    artifactRequestByUrl,
    artifactFileUrl,
    packageUrl,

    -- * Shared internals
    encodePackagePath,
    withToken,
    parseRequestEither,
) where

import Network.HTTP.Client (Request (decompress, requestHeaders))
import Network.HTTP.Types.Header (hAccept, hAcceptEncoding)

import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Package (PackageName, pkgNamespace, renderPackageName, unScope, unscopedName)
import Ecluse.Core.Registry (UrlFormationError)
import Ecluse.Core.Registry.Npm.Credential (npmCredential)
import Ecluse.Core.Registry.Request (Validators (..), addValidators, attachCredential, joinPath, noValidators, parseRequestEither)
import Ecluse.Core.Registry.Request qualified as Request
import Ecluse.Core.Server.Path (encodeComponent)

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

{- | Build the metadata @GET@ request for a package. The URL is
@{baseUrl}/{encoded-name}@. The request carries the @Accept@ header for the chosen
'MetadataForm', @Accept-Encoding: gzip@, an optional bearer token, and any relayed
conditional-GET 'Validators'.

The package path comes from an __already-parsed__ 'PackageName', with the scope
separator percent-encoded (@\@scope/name@ -> @\@scope%2Fname@). Fails with a
'UrlFormationError' only when the URL cannot be formed (an empty base URL).
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

{- | Build the artifact @GET@ request addressing a tarball by its __preserved
on-the-wire filename__, at @{baseUrl}/{encoded-pkg}/-/{filename}@.

The serve path fetches an artifact by the exact filename the client requested, never a
name rebuilt from @(package, version)@. That filename is the authoritative name for the
bytes. A registry whose tarball naming differs from the proxy's own convention
therefore still resolves. The @filename@ is taken verbatim, because the
classifier already passed it through the component-safety gate. The package segment
is the same scope-percent-encoded path 'metadataRequest' builds. The request is
__non-decompressing__: a @.tgz@ is opaque binary, streamed byte-for-byte so its
@dist.integrity@ verifies. Exported so the web layer can bracket it for
bounded-memory streaming.

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
            { -- Never gunzip a tarball in flight: it is opaque binary whose
              -- integrity the client verifies, so stream the raw bytes. This builder
              -- deliberately advertises no @Accept-Encoding@: a @.tgz@ is
              -- already-compressed application data. Requesting a transport encoding
              -- it then refuses to decode ('decompress' is 'False') would risk a
              -- doubly-gzipped body that fails its @dist.integrity@.
              decompress = const False
            }

{- | Build npm's artifact @GET@ request addressing a tarball at its __authoritative
upstream location__: the absolute @url@ the projection preserved from the upstream's
@dist.tarball@. This builder ignores the @baseUrl@, because the location is absolute.
It attaches the credential under npm's presentation through the shared
'Ecluse.Core.Registry.Request.artifactRequestByUrl', which marks the request
non-decompressing and pins the redirect count. See that symbol for the opaque-bytes
rationale.

Fails with a 'UrlFormationError' only when the @url@ cannot be parsed into a request.
-}
artifactRequestByUrl ::
    Text ->
    Maybe Secret ->
    Text ->
    Either UrlFormationError Request
artifactRequestByUrl _baseUrl = Request.artifactRequestByUrl npmCredential

{- The metadata/publish URL for a package: @{baseUrl}/{encoded-name}@, with
the scoped-name separator percent-encoded (@\@scope/name@ -> @\@scope%2Fname@).
-}
packageUrl :: Text -> PackageName -> Either UrlFormationError Text
packageUrl baseUrl name =
    joinPath baseUrl (encodePackagePath name)

{- | The artifact (tarball) URL addressing a __preserved filename__:
@{baseUrl}/{encoded-name}/-/{encoded-filename}@. The filename is the exact
on-the-wire name, not @{base}-{version}.tgz@ rebuilt from the coordinate, so the
proxy fetches the bytes by the name the client requested. It is percent-encoded as a
single component ('Ecluse.Core.Server.Route.encodeComponent'), so a once-decoded
escape in it cannot reach the upstream raw. Exported so the serve path can record the
public artifact location on a mirror job (the same URL its public fetch targets).

Fails with a 'UrlFormationError' only when the URL cannot be formed.
-}
artifactFileUrl :: Text -> PackageName -> Text -> Either UrlFormationError Text
artifactFileUrl baseUrl name filename =
    joinPath baseUrl (encodePackagePath name <> "/-/" <> encodeComponent filename)

{- Encode a package name as its on-the-wire path segment. Each name component
(scope, base name) is percent-encoded ('Ecluse.Core.Server.Route.encodeComponent')
around the structural delimiters this builder writes. A scoped @\@scope/name@ becomes
@\@{enc-scope}%2F{enc-base}@. This builder writes the leading @\@@ and the @%2F@
separator, never derives them from a component. A legitimate scoped name therefore
yields exactly one @%2F@, and an unscoped name is its single encoded component. Encoding
each component is the defence in depth: a @'%'@, @'/'@, or other reserved byte inside a
decoded name never reaches the upstream URL raw. A once-decoded @%2e%2e%2f@ is
re-encoded to @%252e%252e%252f@, without double-encoding the structural separator.
-}
encodePackagePath :: PackageName -> Text
encodePackagePath name = case pkgNamespace name of
    Just scope -> "@" <> encodeComponent (unScope scope) <> "%2F" <> encodeComponent (unscopedName name)
    Nothing -> encodeComponent (renderPackageName name)

{- npm's data-plane request finalisation: attach the injected credential under npm's own
presentation ('Ecluse.Core.Registry.Npm.Credential.npmCredential') through
'Ecluse.Core.Registry.Request.attachCredential', which pins @redirectCount = 0@. The
redirect invariant and its full rationale live with that attach. Every npm builder
reaches the wire through it, anonymous requests included.
-}
withToken :: Maybe Secret -> Request -> Request
withToken = attachCredential npmCredential
