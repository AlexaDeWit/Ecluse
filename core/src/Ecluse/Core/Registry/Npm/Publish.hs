-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | npm mirror upload data, publication, and the first-party identity guard.
The checked mirror upload data keeps publication fields paired with the admitted tarball.
-}
module Ecluse.Core.Registry.Npm.Publish (
    NpmUploadMetadata,
    prepareNpmSource,
    prepareNpmUpload,
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
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Lens.Micro ((^?))
import Lens.Micro.Aeson (key, _Object)
import Network.HTTP.Client (Request)

import Ecluse.Core.Credential (ClientCredential)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (Artifact (..), HashAlg (SHA1, SRI), PackageDetails (..), PackageInfo (..), PackageName, Scope, hashAlg, hashValue, pkgEcosystem, pkgNamespace, renderPackageName)
import Ecluse.Core.Package.Hash (canonicalHashValue)
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
import Ecluse.Core.Registry.CachedDocument (npmCached)
import Ecluse.Core.Registry.Metadata (VersionDoc (vdRaw))
import Ecluse.Core.Registry.MirrorUpload
import Ecluse.Core.Registry.MirrorUpload.Internal (checkedMirrorUploadData)
import Ecluse.Core.Registry.Npm.Metadata (npmChargeFactors, npmRead)
import Ecluse.Core.Registry.Npm.Project qualified as Project
import Ecluse.Core.Registry.Npm.Request (MetadataForm (Abbreviated), jsonPutRequest, metadataRequest, packageUrl)
import Ecluse.Core.Registry.Publish (PublishCodec (..), PublishPlan (ppLatest))
import Ecluse.Core.Server.Admission.Types (ChargeFactors (cfFullReadPermille))
import Ecluse.Core.Server.Path (unFilename)
import Ecluse.Core.Version (Version, renderVersion)

-- | Original source fields bind the publication metadata to its admitted snapshot.
newtype NpmUploadMetadata = NpmUploadMetadata (KeyMap Value)
    deriving stock (Eq, Show)

-- | Preserve the paired source fields and refuse an npm inventory with more than one physical tarball.
prepareNpmSource :: PackageName -> Version -> VersionDoc -> Either PublishFault (MirrorInventory, NpmUploadMetadata)
prepareNpmSource name version doc = do
    unless (pkgEcosystem name == Npm) (Left (PublishSourceUnavailable "the mirror snapshot is not npm"))
    raw <- maybeToRight (PublishSourceUnavailable "the resolver carried no source version object") (vdRaw doc)
    fields <- case snd npmCached raw of
        Just (Object objectFields) -> Right objectFields
        _ -> Left (PublishSourceUnavailable "the source version object is not an npm object")
    inventory <- checkedMirrorInventory name version doc (NpmMirrorRead fields)
    case inventoryFiles inventory of
        _ :| [] -> pure ()
        _ -> Left (PublishSourceUnavailable "the npm inventory contains more than one physical tarball")
    pure (inventory, NpmUploadMetadata fields)

-- | Bind npm source fields to this verified member of the complete version inventory.
prepareNpmUpload :: MirrorInventory -> NpmUploadMetadata -> ExpectedMirrorFile -> VerifiedArtifactBytes -> Either PublishFault (MirrorUploadData NpmUploadMetadata)
prepareNpmUpload inventory metadata@(NpmUploadMetadata fields) file verified = do
    unless (inventoryReadData inventory == NpmMirrorRead fields) $
        Left (PublishSourceUnavailable "npm upload fields belong to another source snapshot")
    checkedMirrorUploadData inventory file verified metadata

-- | npm uses its ordinary bounded metadata reader and one immutable tarball upload per version.
npmPublishCodec :: PublishCodec
npmPublishCodec =
    PublishCodec
        { pcPrepareSource = prepareNpmSource
        , pcPrepareUpload = \_ _ inventory metadata file bytes -> pure (prepareNpmUpload inventory metadata file bytes)
        , pcNativeComplete = nativeNpmComplete
        , pcTargetRead = npmRead
        , pcTargetReadPermille = cfFullReadPermille npmChargeFactors
        , pcProbeRequest = \targetUrl credential -> metadataRequest targetUrl credential Abbreviated
        , pcVersionListParser = Just Project.versionListParser
        , pcPublishRequest = npmPublishRequestFor
        , pcPublishOutcome = \status _ -> classifyPublish status
        }

nativeNpmComplete :: PackageName -> Version -> PackageInfo -> Bool
nativeNpmComplete name version info = fromMaybe False $ do
    guard (pkgEcosystem name == Npm && infoName info == name)
    details <- Map.lookup (renderVersion version) (infoVersions info)
    guard (pkgName details == name && renderVersion (pkgVersion details) == renderVersion version)
    case pkgArtifacts details of
        artifact :| [] -> do
            (fileVersion, _) <- Project.tarballCoordinate name (artFilename artifact)
            guard (renderVersion fileVersion == renderVersion version && maybe True (>= 0) (artSize artifact))
            hashes <- nonEmpty (artHashes artifact)
            pure (all (isJust . canonicalHashValue) hashes && isJust (canonicalHashValue (authoritativeDigest hashes)))
        _ -> Nothing

npmPublishRequestFor :: Text -> Maybe ClientCredential -> PublishPlan -> MirrorUploadData NpmUploadMetadata -> Either PublishFault Request
npmPublishRequestFor targetUrl credential plan uploadData =
    first (PublishFetch . FetchUrlUnformable) $
        publishRequest targetUrl credential (inventoryPackage (uploadInventory uploadData)) (npmPublishDocument plan uploadData)

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

-- | Preserve source metadata fields while replacing identity, attachment controls, and verified distribution facts.
npmPublishDocument :: PublishPlan -> MirrorUploadData NpmUploadMetadata -> ByteString
npmPublishDocument plan uploadData =
    toStrict . Aeson.encode $
        object
            [ "_id" .= rendered
            , "name" .= rendered
            , "dist-tags" .= object ["latest" .= renderVersion (ppLatest plan)]
            , "versions" .= object [Key.fromText versionText .= manifest]
            , "_attachments" .= object [Key.fromText filename .= attachmentObject tarball]
            ]
  where
    artifact = expectedDescriptor (uploadFile uploadData)
    tarball = uploadBytes uploadData
    filename = unFilename (maFilename artifact)
    NpmUploadMetadata fields = uploadMetadata uploadData
    authored = authoredFields fields
    dist = distObject filename (strongestSriValue artifact) (firstHashValue SHA1 artifact) (objectAt "dist" authored)
    manifest = versionManifestObject rendered versionText dist authored
    versionText = renderVersion (inventoryVersion (uploadInventory uploadData))
    rendered = renderPackageName (inventoryPackage (uploadInventory uploadData))

-- Keep the shrinkwrap installation marker, but drop the source registry's bookkeeping.
authoredFields :: KeyMap Value -> KeyMap Value
authoredFields = KeyMap.filterWithKey (\keyName _ -> keyName == "_hasShrinkwrap" || not (T.isPrefixOf "_" (Key.toText keyName)))

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
