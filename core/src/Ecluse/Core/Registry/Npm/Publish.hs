-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The npm publish-document schema: the mirror-write side (document assembly,
request shaping, and the codec that carries them into the shared publish transport)
and the read side ('declaredNames', the identity names the ecosystem-neutral publish
pipeline's anti-shadowing guard reads from a first-party publish body).

Everything here is pure. 'npmPublishCodec' is npm's
'Ecluse.Core.Registry.Publish.PublishCodec': the composition root marries it to
the shared transport ('Ecluse.Core.Registry.Publish.newMirrorPublish'), which
executes what this module forms. The first-party publish relay (a different
concern: a client's own document forwarded verbatim) lives in
"Ecluse.Core.Registry.Npm".
-}
module Ecluse.Core.Registry.Npm.Publish (
    npmPublishCodec,
    publishRequest,
    npmPublishDocument,
    declaredNames,
) where

import Data.Aeson (Value (String), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteArray.Encoding (Base (Base64), convertToBase)
import Data.ByteString qualified as BS

import Lens.Micro ((^?))
import Lens.Micro.Aeson (key, _Object)
import Network.HTTP.Client (Request (method, requestBody, requestHeaders), RequestBody (RequestBodyBS))
import Network.HTTP.Types.Header (hAccept, hContentType)

import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Package (HashAlg (SHA1, SRI), PackageName, renderPackageName)
import Ecluse.Core.Registry (
    MirrorArtifact (maFilename),
    PublishError (PublishError),
    PublishFault (PublishRejected),
    UrlFormationError,
    firstHashValue,
 )
import Ecluse.Core.Registry.Npm.Project qualified as Project
import Ecluse.Core.Registry.Npm.Request (MetadataForm (Abbreviated), metadataRequest, noValidators, packageUrl, parseRequestEither, withToken)
import Ecluse.Core.Registry.Publish (PublishCodec (..))
import Ecluse.Core.Version (Version, renderVersion)

{- | npm's mirror-write protocol codec: the presence probe reads the abbreviated
packument and projects its version list; the publish assembles the
packument-fragment @PUT@ ('npmPublishDocument' under 'publishRequest'), with the
@dist@ digests picked from the re-admitted artifact's verified set; and a @409@
answer is idempotent success (versions are immutable, so an already-present
version is the write's goal already met).
-}
npmPublishCodec :: PublishCodec
npmPublishCodec =
    PublishCodec
        { pcProbeRequest = \targetUrl token -> metadataRequest targetUrl token Abbreviated noValidators
        , pcParseVersionList = Project.parseVersionList
        , pcPublishRequest = \targetUrl token name version artifact bytes ->
            publishRequest
                targetUrl
                token
                name
                (npmPublishDocument name version (maFilename artifact) (sriOf artifact) (sha1Of artifact) bytes)
        , pcPublishOutcome = classifyPublish
        }

{- Map a publish response status onto success or a 'PublishFault'. A 2xx or a
@409@ (already present, immutable) is success; anything else is a retryable
'PublishRejected' naming the status the job saw.
-}
classifyPublish :: Int -> Either PublishFault ()
classifyPublish code
    | code >= 200 && code < 300 = Right ()
    | code == 409 = Right () -- version already present; immutable, so success-equivalent
    | otherwise =
        Left (PublishRejected (PublishError ("publish failed with HTTP status " <> show code)))

-- Pick the SRI (@dist.integrity@) string from the admitted digests, if present.
sriOf :: MirrorArtifact -> Maybe Text
sriOf = firstHashValue SRI

-- Pick the SHA-1 shasum from the admitted digests, if present.
sha1Of :: MirrorArtifact -> Maybe Text
sha1Of = firstHashValue SHA1

{- | Build the publish @PUT /{pkg}@ request: the body is the npm publish
document (a packument carrying the version manifest and the base64 tarball under
@_attachments@), already serialised by the caller. Carries the bearer token and a
@Content-Type: application/json@ header.

Fails with a 'UrlFormationError' only when the URL cannot be formed; a genuine
write fault (a non-2xx, non-409 status) is the 'PublishError' that
'Ecluse.Core.Registry.publishArtifact' reports.
-}
publishRequest ::
    Text ->
    Maybe Secret ->
    PackageName ->
    ByteString ->
    Either UrlFormationError Request
publishRequest baseUrl token name document = do
    url <- packageUrl baseUrl name
    base <- parseRequestEither url
    pure
        . withToken token
        $ base
            { method = "PUT"
            , requestBody = RequestBodyBS document
            , -- A spec-compliant registry (e.g. Verdaccio) rejects a publish whose
              -- body is not declared @application/json@ with a 415; the npm publish
              -- protocol requires it. Accept is set too, for the registry's response.
              requestHeaders =
                (hContentType, "application/json")
                    : (hAccept, "application/json")
                    : requestHeaders base
            }

{- | Assemble the npm publish document for one version from its verified tarball
bytes: the serialised body 'publishRequest' (hence
'Ecluse.Core.Registry.publishArtifact') @PUT@s to @/{pkg}@.

The document is the npm @PUT /{pkg}@ shape: the package name and a single-version
@versions@ map carrying the version manifest (@name@, @version@, and a @dist@ with
the integrity digests), @dist-tags.latest@ pointed at that version, and the tarball
itself base64-encoded under @_attachments@ with its byte @length@. A managed npm
registry (CodeArtifact, Artifact Registry, Verdaccio) recomputes the served
@dist.tarball@ location from the attachment, so the location is not carried.

The integrity digests written into @dist@ are the __caller's__: the worker passes
the serve-time-admitted digests it has already verified the bytes against: so the
published manifest's integrity matches exactly the bytes attached. The tarball
@length@ is taken from the actual byte count, never a caller-declared size, so the
attachment can never disagree with its own bytes.

This is the inverse of the read-side decode in "Ecluse.Core.Registry.Npm.Wire", which
deliberately does not model @_attachments@: it is constructed only here, for the
write.
-}
npmPublishDocument ::
    -- | The package being published.
    PackageName ->
    -- | The version being published.
    Version ->
    -- | The tarball's filename: the @_attachments@ key and tarball file segment.
    Text ->
    -- | The @dist.integrity@ SRI string, if known (e.g. @"sha512-…"@).
    Maybe Text ->
    -- | The @dist.shasum@ (SHA-1, hex), if known.
    Maybe Text ->
    -- | The verified tarball bytes.
    ByteString ->
    ByteString
npmPublishDocument name version filename integrity shasum tarball =
    toStrict . Aeson.encode $
        object
            [ "_id" .= rendered
            , "name" .= rendered
            , "dist-tags" .= object ["latest" .= versionText]
            , "versions" .= object [Key.fromText versionText .= manifest]
            , "_attachments" .= object [Key.fromText filename .= attachmentObject tarball]
            ]
  where
    versionText = renderVersion version
    rendered = renderPackageName name
    manifest = versionManifestObject rendered versionText (distObject filename integrity shasum)

-- The one-version manifest under @versions.{version}@: the package name, the
-- version, and its @dist@ descriptor.
versionManifestObject :: Text -> Text -> Aeson.Value -> Aeson.Value
versionManifestObject rendered versionText dist =
    object
        [ "name" .= rendered
        , "version" .= versionText
        , "dist" .= dist
        ]

-- The manifest's @dist@ descriptor: the tarball filename plus whichever of the
-- caller's verified digests are known (an absent digest is omitted, never
-- fabricated).
distObject :: Text -> Maybe Text -> Maybe Text -> Aeson.Value
distObject filename integrity shasum =
    object
        ( ["tarball" .= filename]
            <> maybe [] (\i -> ["integrity" .= i]) integrity
            <> maybe [] (\s -> ["shasum" .= s]) shasum
        )

-- The @_attachments@ entry for the tarball, with the @length@ taken from the
-- actual byte count.
attachmentObject :: ByteString -> Aeson.Value
attachmentObject tarball =
    object
        [ "content_type" .= ("application/octet-stream" :: Text)
        , "data" .= encodedTarball
        , "length" .= BS.length tarball
        ]
  where
    -- The npm attachment carries the raw tarball bytes, standard-base64-encoded.
    encodedTarball :: Text
    encodedTarball = decodeUtf8 (convertToBase Base64 tarball :: ByteString)

{- | Every package name a first-party npm publish body declares as its own identity:
the top-level @_id@ and @name@, and each @versions.\<v\>.name@. Only string-valued
name slots are read (a non-string slot is no name claim); a body that does not decode
to a JSON object declares no readable name (the empty list). The base64 @_attachments@
are never decoded.

This is the read-side inverse of 'npmPublishDocument', which assembles the same
@_id@\/@name@\/@versions@ shape for the mirror write. The ecosystem-neutral publish
pipeline injects it as its adapter's declared-name extractor, so the anti-shadowing
body-name guard (issue #391) can refuse a crafted body that names a package the scope
guard never authorised without the neutral pipeline knowing npm's document schema.
-}
declaredNames :: LByteString -> [Text]
declaredNames body =
    [ declared
    | document <- maybeToList (Aeson.decode body :: Maybe Value)
    , slot <-
        [document ^? key "_id", document ^? key "name"]
            <> [ versionDoc ^? key "name"
               | versions <- maybeToList (document ^? key "versions" . _Object)
               , versionDoc <- KeyMap.elems versions
               ]
    , Just (String declared) <- [slot]
    ]
