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
import Network.HTTP.Types.Header (hAccept, hContentType)

import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Package (PackageName, pkgNamespace, renderPackageName, unScope, unscopedName)
import Ecluse.Core.Registry (UrlFormationError)
import Ecluse.Core.Registry.Npm.Credential (npmCredential)
import Ecluse.Core.Registry.Request (BaseRequest, attachCredential, joinPath, parseRequestEither, requestAt)
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

-- | npm registries serve their own artifacts, so no extra hosts enter the artifact gate.
npmArtifactHosts :: [Text]
npmArtifactHosts = []

-- | Fetch @{baseUrl}/{encoded-name}@ with the requested metadata form and gzip negotiation.
metadataRequest ::
    Text ->
    Maybe ClientCredential ->
    MetadataForm ->
    PackageName ->
    Either UrlFormationError Request
metadataRequest baseUrl token form name = metadataGet token form <$> (packageUrl baseUrl name >>= parseRequestEither)

-- | 'metadataRequest' under a base parsed once, through 'requestAt'.
metadataRequestUnder ::
    BaseRequest ->
    Maybe ClientCredential ->
    MetadataForm ->
    PackageName ->
    Either UrlFormationError Request
metadataRequestUnder base token form name = metadataGet token form <$> requestAt base (encodePackagePath name)

metadataGet :: Maybe ClientCredential -> MetadataForm -> Request -> Request
metadataGet token form = Request.metadataGet npmCredential token (metadataAccept form)

-- | Fetch @{base}/{encoded-pkg}/-/{filename}@ under the client's filename through 'requestAt'.
artifactRequestByFile ::
    BaseRequest ->
    Maybe ClientCredential ->
    PackageName ->
    Text ->
    Either UrlFormationError Request
artifactRequestByFile base token name filename = Request.artifactGet npmCredential token <$> requestAt base (artifactPath name filename)

-- | Fetch the preserved @dist.tarball@ URL under npm's credential presentation.
artifactRequestByUrl ::
    Maybe ClientCredential ->
    Text ->
    Either UrlFormationError Request
artifactRequestByUrl = Request.artifactRequestByUrl npmCredential

-- | The metadata and publish URL for a package: @{baseUrl}/{encoded-name}@.
packageUrl :: Text -> PackageName -> Either UrlFormationError Text
packageUrl baseUrl name = joinPath baseUrl (encodePackagePath name)

-- | Encode the filename as one component in @{baseUrl}/{encoded-name}/-/{encoded-filename}@.
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

-- | Publish JSON under the injected credential, declaring the media type npm registries require.
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

-- | Attach npm's credential presentation and preserve the shared redirect and identity controls.
withToken :: Maybe ClientCredential -> Request -> Request
withToken = attachCredential npmCredential
