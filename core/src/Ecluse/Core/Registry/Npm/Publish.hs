-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | npm mirror publication through "Ecluse.Core.Registry.Publish", plus identity
extraction for the first-party publish guard. Published SRI retains all alternatives
at its strongest algorithm, matching the worker's verification contract.
-}
module Ecluse.Core.Registry.Npm.Publish (
    npmPublishCodec,
    publishRequest,
    npmPublishDocument,
    declaredNames,
    npmPublishAllowed,
) where

import Data.Aeson (Value (String), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteArray.Encoding (Base (Base64), convertToBase)
import Data.ByteString qualified as BS
import Data.Text qualified as T

import Lens.Micro ((^?))
import Lens.Micro.Aeson (key, _Object)
import Network.HTTP.Client (Request (method, requestBody, requestHeaders), RequestBody (RequestBodyBS))
import Network.HTTP.Types.Header (hAccept, hContentType)

import Ecluse.Core.Credential (ClientCredential, bareCredential)
import Ecluse.Core.Package (HashAlg (SHA1, SRI), PackageName, Scope, hashAlg, hashValue, pkgNamespace, renderPackageName)
import Ecluse.Core.Package.Integrity (assertedAlg, authoritativeDigest)
import Ecluse.Core.Registry (
    MirrorArtifact (maFilename, maHashes),
    PublishError (PublishError),
    PublishFault (PublishRejected),
    UrlFormationError,
    firstHashValue,
 )
import Ecluse.Core.Registry.Npm.Project qualified as Project
import Ecluse.Core.Registry.Npm.Request (MetadataForm (Abbreviated), metadataRequest, packageUrl, parseRequestEither, withToken)
import Ecluse.Core.Registry.Publish (PublishCodec (..))
import Ecluse.Core.Registry.Request (noValidators)
import Ecluse.Core.Server.Path (unFilename)
import Ecluse.Core.Version (Version, renderVersion)

-- | Probe an abbreviated packument and publish verified bytes with their strongest SRI alternatives.
npmPublishCodec :: PublishCodec
npmPublishCodec =
    PublishCodec
        { pcProbeRequest = \targetUrl token -> metadataRequest targetUrl (bareCredential <$> token) Abbreviated noValidators
        , pcParseVersionList = Project.parseVersionList
        , pcPublishRequest = \targetUrl token name version artifact bytes ->
            publishRequest
                targetUrl
                (bareCredential <$> token)
                name
                (npmPublishDocument name version (unFilename (maFilename artifact)) (strongestSriValue artifact) (firstHashValue SHA1 artifact) bytes)
        , pcPublishOutcome = classifyPublish
        }

strongestSriValue :: MirrorArtifact -> Maybe Text
strongestSriValue artifact = do
    hashes <- nonEmpty (filter ((== SRI) . hashAlg) (toList (maHashes artifact)))
    let strongestAlg = assertedAlg (authoritativeDigest hashes)
    pure (T.unwords [hashValue h | h <- toList hashes, assertedAlg h == strongestAlg])

classifyPublish :: Int -> Either PublishFault ()
classifyPublish code
    | code >= 200 && code < 300 = Right ()
    | code == 409 = Right () -- version already present, immutable, so success-equivalent
    | otherwise =
        Left (PublishRejected (PublishError ("publish failed with HTTP status " <> show code)))

-- | Build the publish request with its credential, failing when the URL cannot be formed.
publishRequest ::
    Text ->
    Maybe ClientCredential ->
    PackageName ->
    ByteString ->
    Either UrlFormationError Request
publishRequest baseUrl credential name document = do
    url <- packageUrl baseUrl name
    base <- parseRequestEither url
    pure
        . withToken credential
        $ base
            { method = "PUT"
            , requestBody = RequestBodyBS document
            , -- npm registries reject a publish without the JSON content type with HTTP 415.
              requestHeaders =
                (hContentType, "application/json")
                    : (hAccept, "application/json")
                    : requestHeaders base
            }

-- | Assemble one version with caller-verified digests and bytes. The registry expands the tarball filename into its served URL.
npmPublishDocument ::
    PackageName ->
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

versionManifestObject :: Text -> Text -> Aeson.Value -> Aeson.Value
versionManifestObject rendered versionText dist =
    object
        [ "name" .= rendered
        , "version" .= versionText
        , "dist" .= dist
        ]

distObject :: Text -> Maybe Text -> Maybe Text -> Aeson.Value
distObject filename integrity shasum =
    object
        ( ["tarball" .= filename]
            <> maybe [] (\i -> ["integrity" .= i]) integrity
            <> maybe [] (\s -> ["shasum" .= s]) shasum
        )

attachmentObject :: ByteString -> Aeson.Value
attachmentObject tarball =
    object
        [ "content_type" .= ("application/octet-stream" :: Text)
        , "data" .= encodedTarball
        , "length" .= BS.length tarball
        ]
  where
    encodedTarball :: Text
    encodedTarball = decodeUtf8 (convertToBase Base64 tarball :: ByteString)

-- | Read @_id@, @name@ and each version's name for the anti-shadowing guard. An undecodable body declares nothing.
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

-- | Require an exact configured scope, refusing unscoped names and scope prefixes.
npmPublishAllowed :: [Scope] -> PackageName -> Bool
npmPublishAllowed scopes name = case pkgNamespace name of
    Just scope -> scope `elem` scopes
    Nothing -> False
