-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The npm registry publish-document assembly and request shaping.

This module provides the pure data assembly for an npm publish request: forming
the JSON document from verified bytes and shaping the @PUT@ request. The actual
side-effecting relay and publish operations live in the top-level
"Ecluse.Core.Registry.Npm" client.
-}
module Ecluse.Core.Registry.Npm.Publish (
    publishRequest,
    npmPublishDocument,
) where

import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.ByteArray.Encoding (Base (Base64), convertToBase)
import Data.ByteString qualified as BS

import Network.HTTP.Client (Request (method, requestBody, requestHeaders), RequestBody (RequestBodyBS))
import Network.HTTP.Types.Header (hAccept, hContentType)

import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Package (PackageName, renderPackageName)
import Ecluse.Core.Registry (UrlFormationError)
import Ecluse.Core.Registry.Npm.Request (packageUrl, parseRequestEither, withToken)
import Ecluse.Core.Version (Version, renderVersion)

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
