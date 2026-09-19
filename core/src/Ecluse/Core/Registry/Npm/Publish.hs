-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | npm mirror publication through "Ecluse.Core.Registry.Publish", plus identity
extraction for the first-party publish guard. Published SRI retains all alternatives
at its strongest algorithm, matching the worker's verification contract. The published version
object keeps what the author wrote and strips what the public registry issued about itself, and
a plan whose version object is not an npm object is refused rather than reduced.
-}
module Ecluse.Core.Registry.Npm.Publish (
    npmPublishCodec,
    publishRequest,
    npmPublishDocument,
    declaredNames,
    npmPublishAllowed,
) where

import Data.Aeson (Value (Object, String), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteArray.Encoding (Base (Base64), convertToBase)
import Data.ByteString qualified as BS
import Data.Text qualified as T

import Lens.Micro ((^?))
import Lens.Micro.Aeson (key, _Object)
import Network.HTTP.Client (Request)

import Ecluse.Core.Credential (ClientCredential, bareCredential)
import Ecluse.Core.Package (HashAlg (SHA1, SRI), PackageName, Scope, hashAlg, hashValue, pkgNamespace, renderPackageName)
import Ecluse.Core.Package.Integrity (assertedAlg, authoritativeDigest)
import Ecluse.Core.Registry (
    FetchFault (FetchUrlUnformable),
    MirrorArtifact (maFilename, maHashes),
    PublishError (PublishError),
    PublishFault (PublishFetch, PublishRejected, PublishSourceUnavailable),
    UrlFormationError,
    firstHashValue,
    isSuccessStatus,
 )
import Ecluse.Core.Registry.CachedDocument (CachedDoc, npmCached)
import Ecluse.Core.Registry.Npm.Project qualified as Project
import Ecluse.Core.Registry.Npm.Request (MetadataForm (Abbreviated), jsonPutRequest, metadataRequest, packageUrl)
import Ecluse.Core.Registry.Publish (PublishCodec (..), PublishPlan (ppLatest, ppMetadata, ppVersion))
import Ecluse.Core.Registry.Request (noValidators)
import Ecluse.Core.Server.Path (unFilename)
import Ecluse.Core.Version (renderVersion)

-- | Probe an abbreviated packument and publish verified bytes with their strongest SRI alternatives.
npmPublishCodec :: PublishCodec
npmPublishCodec =
    PublishCodec
        { pcProbeRequest = \targetUrl token -> metadataRequest targetUrl (bareCredential <$> token) Abbreviated noValidators
        , pcParseVersionList = Project.parseVersionList
        , pcPublishRequest = \targetUrl token name plan artifact bytes -> do
            document <- npmPublishDocument name plan (unFilename (maFilename artifact)) (strongestSriValue artifact) (firstHashValue SHA1 artifact) bytes
            first (PublishFetch . FetchUrlUnformable) (publishRequest targetUrl (bareCredential <$> token) name document)
        , pcPublishOutcome = classifyPublish
        }

strongestSriValue :: MirrorArtifact -> Maybe Text
strongestSriValue artifact = do
    hashes <- nonEmpty (filter ((== SRI) . hashAlg) (toList (maHashes artifact)))
    let strongestAlg = assertedAlg (authoritativeDigest hashes)
    pure (T.unwords [hashValue h | h <- toList hashes, assertedAlg h == strongestAlg])

classifyPublish :: Int -> Either PublishFault ()
classifyPublish code
    | isSuccessStatus code = Right ()
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
    jsonPutRequest credential url document

{- | Assemble one version from the plan's metadata, under local authority for the name, version,
and verified @dist@ fields. The declared @latest@ is the plan's: a registry left to choose can retag.
-}
npmPublishDocument ::
    PackageName ->
    PublishPlan ->
    -- | The tarball's filename: the @_attachments@ key and tarball file segment.
    Text ->
    -- | The @dist.integrity@ SRI string, if known (e.g. @"sha512-…"@).
    Maybe Text ->
    -- | The @dist.shasum@ (SHA-1, hex), if known.
    Maybe Text ->
    -- | The verified tarball bytes.
    ByteString ->
    Either PublishFault ByteString
npmPublishDocument name plan filename integrity shasum tarball = do
    authored <- authoredFields (ppMetadata plan)
    let manifest = versionManifestObject rendered versionText (distObject filename integrity shasum (objectAt "dist" authored)) authored
    pure . toStrict . Aeson.encode $
        object
            [ "_id" .= rendered
            , "name" .= rendered
            , "dist-tags" .= object ["latest" .= renderVersion (ppLatest plan)]
            , "versions" .= object [Key.fromText versionText .= manifest]
            , "_attachments" .= object [Key.fromText filename .= attachmentObject tarball]
            ]
  where
    versionText = renderVersion (ppVersion plan)
    rendered = renderPackageName name

{- The fields the author wrote on the source version object. An underscore-prefixed key is the
public registry's bookkeeping about itself, so none reaches the mirror. -}
authoredFields :: CachedDoc -> Either PublishFault (KeyMap Value)
authoredFields doc = case snd npmCached doc of
    Just (Object o) -> Right (KeyMap.filterWithKey (\k _ -> not (T.isPrefixOf "_" (Key.toText k))) o)
    Just _ -> Left (PublishSourceUnavailable "the carried version object is not a JSON object")
    Nothing -> Left (PublishSourceUnavailable "the carried version object is not an npm document")

objectAt :: Key.Key -> KeyMap Value -> KeyMap Value
objectAt slot o = case KeyMap.lookup slot o of
    Just (Object inner) -> inner
    _ -> mempty

-- 'KeyMap.union' is left-biased, so the local authority fields win over the authored ones.
versionManifestObject :: Text -> Text -> Aeson.Value -> KeyMap Value -> Aeson.Value
versionManifestObject rendered versionText dist authored =
    Object (KeyMap.fromList [("name", String rendered), ("version", String versionText), ("dist", dist)] `KeyMap.union` authored)

{- The verified location and digests replace the source's, and an unverified source digest never
survives their absence. Signatures and attestations reference the public registry's own keys. -}
distObject :: Text -> Maybe Text -> Maybe Text -> KeyMap Value -> Aeson.Value
distObject filename integrity shasum authored =
    Object (verified `KeyMap.union` KeyMap.filterWithKey (\k _ -> k `notElem` registryDistKeys) authored)
  where
    verified =
        KeyMap.fromList
            ( ("tarball", String filename)
                : maybe [] (\i -> [("integrity", String i)]) integrity
                    <> maybe [] (\s -> [("shasum", String s)]) shasum
            )

registryDistKeys :: [Key.Key]
registryDistKeys = ["tarball", "integrity", "shasum", "signatures", "attestations"]

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
