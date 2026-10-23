-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | npm request paths, metadata negotiation and credentials over shared HTTP mechanics.
module Ecluse.Core.Registry.Npm.Request (
    -- * Content negotiation
    MetadataForm (..),

    -- * The ecosystem's artifact hosts
    npmArtifactHosts,

    -- * Request building
    metadataRequest,
    metadataRequestUnder,
    artifactRequestByFile,
    artifactRequestUnder,
    artifactRequestByUrl,
    artifactFileUrl,
    packageUrl,

    -- * Shared internals
    jsonPutRequest,
    withToken,
) where

import Network.HTTP.Client (
    Request (method, requestBody, requestHeaders),
    RequestBody (RequestBodyBS),
 )
import Network.HTTP.Types.Header (hAccept, hAcceptEncoding, hContentType)

import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Package (PackageName, pkgNamespace, renderPackageName, unScope, unscopedName)
import Ecluse.Core.Registry (UrlFormationError)
import Ecluse.Core.Registry.Npm.Credential (npmCredential)
import Ecluse.Core.Registry.Request (BaseRequest, attachCredential, baseRequestUrl, joinPath, parseRequestEither, requestUnder)
import Ecluse.Core.Registry.Request qualified as Request
import Ecluse.Core.Security.Egress (registryUrlText)
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
    PackageName ->
    Either UrlFormationError Request
metadataRequest baseUrl token form name = metadataGet token form <$> (packageUrl baseUrl name >>= parseRequestEither)

-- | 'metadataRequest' under a base parsed once. A path the base does not cover takes 'metadataRequest'.
metadataRequestUnder ::
    BaseRequest ->
    Maybe ClientCredential ->
    MetadataForm ->
    PackageName ->
    Either UrlFormationError Request
metadataRequestUnder base token form name = case requestUnder base (encodePackagePath name) of
    Just request -> Right (metadataGet token form request)
    Nothing -> metadataRequest (registryUrlText (baseRequestUrl base)) token form name

metadataGet :: Maybe ClientCredential -> MetadataForm -> Request -> Request
metadataGet token form base =
    withToken token $
        base
            { requestHeaders =
                (hAccept, metadataAccept form)
                    : (hAcceptEncoding, "gzip")
                    : requestHeaders base
            }

{- | Build the artifact @GET@ at @{baseUrl}/{encoded-pkg}/-/{filename}@. It addresses the tarball by
the filename the client requested, so a registry with its own tarball naming still resolves.
-}
artifactRequestByFile ::
    Text ->
    Maybe ClientCredential ->
    PackageName ->
    Text ->
    Either UrlFormationError Request
artifactRequestByFile baseUrl token name filename = do
    url <- artifactFileUrl baseUrl name filename
    artifactRequestByUrl token url

-- | 'artifactRequestByFile' under a base parsed once. A path the base does not cover takes 'artifactRequestByFile'.
artifactRequestUnder ::
    BaseRequest ->
    Maybe ClientCredential ->
    PackageName ->
    Text ->
    Either UrlFormationError Request
artifactRequestUnder base token name filename = case requestUnder base (artifactPath name filename) of
    Just request -> Right (Request.artifactGet npmCredential token request)
    Nothing -> artifactRequestByFile (registryUrlText (baseRequestUrl base)) token name filename

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
packageUrl baseUrl name = joinPath baseUrl (encodePackagePath name)

{- | The artifact URL @{baseUrl}\/{encoded-name}\/-\/{encoded-filename}@, with @filename@
percent-encoded as one component so a once-decoded escape cannot reach the upstream raw.
-}
artifactFileUrl :: Text -> PackageName -> Text -> Either UrlFormationError Text
artifactFileUrl baseUrl name filename = joinPath baseUrl (artifactPath name filename)

artifactPath :: PackageName -> Text -> Text
artifactPath name filename = encodePackagePath name <> "/-/" <> encodeComponent filename

{- This builder writes the @\@@ and the @%2F@ itself and percent-encodes every component, so a
reserved byte in a decoded name never reaches the upstream URL raw. -}
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
