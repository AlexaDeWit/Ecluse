-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Native npm mirror input, publication, and the first-party identity guard.
The checked input keeps publication fields paired with the admitted tarball.
-}
module Ecluse.Core.Registry.Npm.Publish (
    NpmMirrorInput,
    mkNpmMirrorInput,
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

import Ecluse.Core.Credential (ClientCredential, Secret, bareCredential)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (Artifact (artFilename, artHashes, artSize), HashAlg (SHA1, SRI), PackageDetails (pkgArtifacts, pkgName, pkgVersion), PackageName, Scope, hashAlg, hashValue, pkgEcosystem, pkgNamespace, renderPackageName)
import Ecluse.Core.Package.Integrity (assertedAlg, authoritativeDigest)
import Ecluse.Core.Registry (
    FetchFault (FetchUrlUnformable),
    MirrorArtifact (maFilename, maHashes, maSize),
    PublishError (PublishError),
    PublishFault (PublishFetch, PublishRejected, PublishSourceUnavailable),
    UrlFormationError,
    firstHashValue,
    isSuccessStatus,
 )
import Ecluse.Core.Registry.CachedDocument (CachedDoc, npmCached)
import Ecluse.Core.Registry.Metadata (VersionDoc (vdDetails, vdRaw))
import Ecluse.Core.Registry.Npm.Project qualified as Project
import Ecluse.Core.Registry.Npm.Request (MetadataForm (Abbreviated), jsonPutRequest, metadataRequest, packageUrl)
import Ecluse.Core.Registry.Publish (PublishCodec (..), PublishPlan (ppLatest))
import Ecluse.Core.Server.Path (unFilename)
import Ecluse.Core.Version (Version, renderVersion)

-- | A complete npm tarball inventory with its paired native publication fields.
data NpmMirrorInput = NpmMirrorInput
    { nmiName :: PackageName
    , nmiVersion :: Version
    , nmiArtifact :: MirrorArtifact
    , nmiFields :: KeyMap Value
    }
    deriving stock (Eq, Show)

-- | Refuse a mismatched identity, incomplete npm inventory, or missing native source before fetching bytes.
mkNpmMirrorInput :: PackageName -> Version -> VersionDoc -> MirrorArtifact -> Either PublishFault NpmMirrorInput
mkNpmMirrorInput name version doc admitted = do
    unless (pkgEcosystem name == Npm && pkgName details == name && pkgVersion details == version) $
        Left (PublishSourceUnavailable "the admitted snapshot belongs to another package or version")
    case pkgArtifacts details of
        artifact :| []
            | artFilename artifact == unFilename (maFilename admitted)
            , artHashes artifact == toList (maHashes admitted)
            , artSize artifact == maSize admitted ->
                pure ()
        _ -> Left (PublishSourceUnavailable "the admitted descriptor does not cover the complete npm inventory")
    raw <- maybeToRight (PublishSourceUnavailable "the resolver carried no source version object") (vdRaw doc)
    fields <- authoredFields raw
    pure (NpmMirrorInput name version admitted fields)
  where
    details = vdDetails doc

-- | Probe an abbreviated packument and publish verified bytes with their strongest SRI alternatives.
npmPublishCodec :: PublishCodec
npmPublishCodec =
    PublishCodec
        { pcProbeRequest = \targetUrl token -> metadataRequest targetUrl (bareCredential <$> token) Abbreviated
        , pcVersionListParser = Project.versionListParser
        , pcPrepareInput = mkNpmMirrorInput
        , pcPublishRequest = npmPublishRequestFor
        , pcPublishOutcome = classifyPublish
        }

npmPublishRequestFor :: Text -> Maybe Secret -> PublishPlan -> NpmMirrorInput -> ByteString -> Either PublishFault Request
npmPublishRequestFor targetUrl token plan input bytes =
    first (PublishFetch . FetchUrlUnformable) $
        publishRequest targetUrl (bareCredential <$> token) (nmiName input) (npmPublishDocument plan input bytes)

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

-- | Preserve native source fields while replacing identity, attachment controls, and verified distribution facts.
npmPublishDocument :: PublishPlan -> NpmMirrorInput -> ByteString -> ByteString
npmPublishDocument plan input tarball =
    toStrict . Aeson.encode $
        object
            [ "_id" .= rendered
            , "name" .= rendered
            , "dist-tags" .= object ["latest" .= renderVersion (ppLatest plan)]
            , "versions" .= object [Key.fromText versionText .= manifest]
            , "_attachments" .= object [Key.fromText filename .= attachmentObject tarball]
            ]
  where
    artifact = nmiArtifact input
    filename = unFilename (maFilename artifact)
    authored = nmiFields input
    dist = distObject filename (strongestSriValue artifact) (firstHashValue SHA1 artifact) (objectAt "dist" authored)
    manifest = versionManifestObject rendered versionText dist authored
    versionText = renderVersion (nmiVersion input)
    rendered = renderPackageName (nmiName input)

-- Keep the shrinkwrap installation marker, but drop the source registry's bookkeeping.
authoredFields :: CachedDoc -> Either PublishFault (KeyMap Value)
authoredFields doc = case snd npmCached doc of
    Just (Object o) -> Right (KeyMap.filterWithKey (\k _ -> k == "_hasShrinkwrap" || not (T.isPrefixOf "_" (Key.toText k))) o)
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
