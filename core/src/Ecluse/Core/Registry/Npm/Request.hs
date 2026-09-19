-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Request shaping and URL building for the npm data plane, composed over the
ecosystem-agnostic mechanics in "Ecluse.Core.Registry.Request" (the outbound seal,
conditional-GET validators, URL parsing, the path join, the opaque-artifact request core).

Three of npm's protocol facts are load-bearing here. Metadata comes in two forms chosen by
@Accept@, a scoped name travels as the single segment @\@scope%2Fname@, and an artifact
request must not decompress in flight, because the client verifies the relayed bytes
against the packument's @dist.integrity@.
-}
module Ecluse.Core.Registry.Npm.Request (
    -- * Content negotiation
    MetadataForm (..),

    -- * The ecosystem's artifact hosts
    npmArtifactHosts,

    -- * Request building
    metadataRequest,
    artifactRequestByFile,
    artifactRequestByUrl,
    artifactFileUrl,
    packageUrl,

    -- * Shared internals
    jsonPutRequest,
    withToken,
) where

import Network.HTTP.Client (
    Request (decompress, method, requestBody, requestHeaders),
    RequestBody (RequestBodyBS),
 )
import Network.HTTP.Types.Header (hAccept, hAcceptEncoding, hContentType)

import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Package (PackageName, pkgNamespace, renderPackageName, unScope, unscopedName)
import Ecluse.Core.Registry (UrlFormationError)
import Ecluse.Core.Registry.Npm.Credential (npmCredential)
import Ecluse.Core.Registry.Request (Validators, addValidators, attachCredential, joinPath, parseRequestEither)
import Ecluse.Core.Registry.Request qualified as Request
import Ecluse.Core.Server.Path (encodeComponent)

-- | Which of npm's two metadata documents to request, selected by the @Accept@ header.
data MetadataForm
    = -- | The install view (@application/vnd.npm.install-v1+json@), which drops the @time@ map.
      Abbreviated
    | -- | The full packument (@application/json@), the only form carrying the @time@ map.
      Full
    deriving stock (Eq, Show)

metadataAccept :: MetadataForm -> ByteString
metadataAccept = \case
    Abbreviated -> "application/vnd.npm.install-v1+json"
    Full -> "application/json"

{- | npm's canonical artifact hosts: none, because a registry serves its own tarball bytes.
The gate and the projection read this one list, so an artifact authority means one thing on both.
-}
npmArtifactHosts :: [Text]
npmArtifactHosts = []

{- | Build the metadata @GET@ request for a package at @{baseUrl}/{encoded-name}@. It asks for
@gzip@, because a popular packument is megabytes.
-}
metadataRequest ::
    Text ->
    Maybe ClientCredential ->
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

{- | Build the artifact @GET@ at @{baseUrl}/{encoded-pkg}/-/{filename}@, addressing the tarball by
the filename the client requested and never one rebuilt from @(package, version)@, so a registry
with its own tarball naming still resolves.
-}
artifactRequestByFile ::
    Text ->
    Maybe ClientCredential ->
    PackageName ->
    Text ->
    Either UrlFormationError Request
artifactRequestByFile baseUrl token name filename = do
    url <- artifactFileUrl baseUrl name filename
    base <- parseRequestEither url
    pure
        . withToken token
        $ base
            { -- A @.tgz@ is opaque, already-compressed binary. It advertises no
              -- @Accept-Encoding@ either, because a doubly-gzipped body fails @dist.integrity@.
              decompress = const False
            }

{- | Build npm's artifact @GET@ for the absolute @url@ the projection preserved from the upstream's
@dist.tarball@, under npm's credential presentation.
-}
artifactRequestByUrl ::
    Maybe ClientCredential ->
    Text ->
    Either UrlFormationError Request
artifactRequestByUrl = Request.artifactRequestByUrl npmCredential

-- The metadata and publish URL for a package: @{baseUrl}/{encoded-name}@.
packageUrl :: Text -> PackageName -> Either UrlFormationError Text
packageUrl baseUrl name =
    joinPath baseUrl (encodePackagePath name)

{- | The artifact URL @{baseUrl}/{encoded-name}/-/{encoded-filename}@, where @filename@ is the
exact on-the-wire name, percent-encoded as one component so a once-decoded escape in it cannot
reach the upstream raw.
-}
artifactFileUrl :: Text -> PackageName -> Text -> Either UrlFormationError Text
artifactFileUrl baseUrl name filename =
    joinPath baseUrl (encodePackagePath name <> "/-/" <> encodeComponent filename)

{- Encode a package name as its on-the-wire path segment. This builder writes the @\@@ and the
@%2F@ itself and percent-encodes every component, so a reserved byte in a decoded name never
reaches the upstream URL raw (@%2e%2e%2f@ becomes @%252e%252e%252f@). -}
encodePackagePath :: PackageName -> Text
encodePackagePath name = case pkgNamespace name of
    Just scope -> "@" <> encodeComponent (unScope scope) <> "%2F" <> encodeComponent (unscopedName name)
    Nothing -> encodeComponent (renderPackageName name)

{- | Build the JSON @PUT@ at @url@ carrying @document@, under the injected credential. An npm
registry answers 415 unless the body declares @application\/json@.
-}
jsonPutRequest :: Maybe ClientCredential -> Text -> ByteString -> Either UrlFormationError Request
jsonPutRequest credential url document = do
    base <- parseRequestEither url
    pure
        . withToken credential
        $ base
            { method = "PUT"
            , requestBody = RequestBodyBS document
            , requestHeaders =
                (hContentType, "application/json")
                    : (hAccept, "application/json")
                    : requestHeaders base
            }

-- Attach the injected credential under npm's presentation. The redirect pin and the proxy
-- identity belong to Ecluse.Core.Registry.Request, which seals every request it parses.
withToken :: Maybe ClientCredential -> Request -> Request
withToken = attachCredential npmCredential
