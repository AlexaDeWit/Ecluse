-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Checked version membership and verified bytes shared by mirror codecs and durable records.
module Ecluse.Core.Registry.MirrorUpload.Internal (
    MirrorReadData (..),
    mirrorReadDocument,
    readDataCompanions,
    MirrorInventory,
    ExpectedMirrorFile,
    VerifiedArtifactBytes,
    MirrorUploadData,
    checkedMirrorInventory,
    restoreMirrorInventory,
    restoreMirrorFragment,
    splitMirrorInventory,
    expectedIdentityMatches,
    expectedTargetMatchesWith,
    targetIdentityConflict,
    inventoryDetails,
    inventoryFiles,
    inventoryReadData,
    expectedArtifact,
    expectedDescriptor,
    verifyMirrorBytes,
    verifiedFile,
    verifiedBytes,
    verifiedTargetHash,
    checkedMirrorUploadData,
    uploadInventory,
    uploadFile,
    uploadBytes,
    uploadTargetHash,
    uploadMetadata,
) where

import Control.Monad (foldM)
import Data.Aeson (FromJSON (parseJSON), Object, Value (Object, String))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI, RubyGems))
import Ecluse.Core.Package (Artifact (..), Hash, HashAlg (SHA256), PackageDetails (..), PackageInfo (infoVersions), PackageName, mkHash, pkgEcosystem)
import Ecluse.Core.Package.Entry (EntryKey, validEntryKey)
import Ecluse.Core.Package.Hash (canonicalHashValue, computeDigest, hexDigestText)
import Ecluse.Core.Package.Integrity (IntegrityResult (..), assertedAlg, authoritativeDigest, verifyIntegrity)
import Ecluse.Core.Registry (CompanionRequirement (DeclaredCompanions), MirrorArtifact (..), PublishFault (PublishIntegrityMismatch, PublishSourceUnavailable))
import Ecluse.Core.Registry.CachedDocument (CachedDoc, cargoCached, npmCached, pypiSimpleCached, rubyGemsCached)
import Ecluse.Core.Registry.Cargo.Document (CargoDocument)
import Ecluse.Core.Registry.Cargo.SourceState (cargoSourceCompanions, restrictCargoDocument, validateCargoSource)
import Ecluse.Core.Registry.Metadata (VersionDoc (..))
import Ecluse.Core.Registry.PyPI.Document (SimpleDocument, restrictSimpleFiles, simpleEnvelope, simpleFiles, simpleSourceCompanions)
import Ecluse.Core.Registry.PyPI.Project (fileCoordinate, projectFile, projectName, projectSimpleIndex)
import Ecluse.Core.Registry.PyPI.Wire (IndexFile (ifEntryKey, ifFilename))
import Ecluse.Core.Registry.RubyGems.Document (GemDocument, restrictGemDocument)
import Ecluse.Core.Registry.RubyGems.SourceState (gemSourceCompanions, validateGemSource)
import Ecluse.Core.Registry.ServedDocument (uniqueEntries)
import Ecluse.Core.Server.Path (mkFilename, unFilename)
import Ecluse.Core.Version (Version, renderVersion)

-- | Supported read fields retain their source coordinates independently of physical uploads.
data MirrorReadData
    = NpmMirrorRead Object
    | PyPIMirrorRead SimpleDocument
    | -- | Original crates.io rows. Their stored source identity never grants target transport authority.
      CargoMirrorRead CargoDocument
    | RubyGemsMirrorRead GemDocument
    deriving stock (Eq, Show)

-- | Restore retained source fields without encoding or reparsing them.
mirrorReadDocument :: MirrorReadData -> CachedDoc
mirrorReadDocument = \case
    NpmMirrorRead fields -> fst npmCached (Object fields)
    PyPIMirrorRead document -> fst pypiSimpleCached document
    CargoMirrorRead document -> fst cargoCached document
    RubyGemsMirrorRead document -> fst rubyGemsCached document

-- | Which files need a retained metadata companion. A durable format's declaration names the same value.
readDataCompanions :: MirrorReadData -> CompanionRequirement
readDataCompanions = \case
    NpmMirrorRead _ -> DeclaredCompanions
    PyPIMirrorRead _ -> simpleSourceCompanions
    CargoMirrorRead _ -> cargoSourceCompanions
    RubyGemsMirrorRead _ -> gemSourceCompanions

-- | Membership comes from the complete paired version snapshot, never a requested-file subset.
data MirrorInventory = MirrorInventory
    { miDetails :: PackageDetails
    , miFiles :: NonEmpty ExpectedMirrorFile
    , miReadData :: MirrorReadData
    }
    deriving stock (Eq, Show)

-- | The original artifact and its validated filename and nonempty digest refinement.
data ExpectedMirrorFile = ExpectedMirrorFile
    { emfArtifact :: Artifact
    , emfDescriptor :: MirrorArtifact
    }
    deriving stock (Eq, Show)

-- | Bytes verified against this exact expected member.
data VerifiedArtifactBytes = VerifiedArtifactBytes
    { vabFile :: ExpectedMirrorFile
    , vabBytes :: ByteString
    , vabTargetHash :: Hash
    }
    deriving stock (Eq, Show)

-- | One checked upload retains whole-version context and mandatory codec-owned metadata.
data MirrorUploadData metadata = MirrorUploadData
    { mudInventory :: MirrorInventory
    , mudFile :: ExpectedMirrorFile
    , mudVerified :: VerifiedArtifactBytes
    , mudMetadata :: metadata
    }
    deriving stock (Eq, Show)

-- | Original admitted policy facts and complete source membership.
inventoryDetails :: MirrorInventory -> PackageDetails
inventoryDetails = miDetails

-- | Expected physical files, with identical duplicate source entries collapsed.
inventoryFiles :: MirrorInventory -> NonEmpty ExpectedMirrorFile
inventoryFiles = miFiles

-- | Supported fields paired with this inventory.
inventoryReadData :: MirrorInventory -> MirrorReadData
inventoryReadData = miReadData

-- | The original admitted file facts and source coordinate.
expectedArtifact :: ExpectedMirrorFile -> Artifact
expectedArtifact = emfArtifact

-- | Validated filename and nonempty expected digest facts.
expectedDescriptor :: ExpectedMirrorFile -> MirrorArtifact
expectedDescriptor = emfDescriptor

-- | The member whose strongest digest verified this body.
verifiedFile :: VerifiedArtifactBytes -> ExpectedMirrorFile
verifiedFile = vabFile

-- | The exact body that passed byte verification.
verifiedBytes :: VerifiedArtifactBytes -> ByteString
verifiedBytes = vabBytes

-- | SHA-256 computed from the verified body, separate from source claims.
verifiedTargetHash :: VerifiedArtifactBytes -> Hash
verifiedTargetHash = vabTargetHash

-- | Whole-version context retained by one checked upload.
uploadInventory :: MirrorUploadData metadata -> MirrorInventory
uploadInventory = mudInventory

-- | The checked member selected for this upload.
uploadFile :: MirrorUploadData metadata -> ExpectedMirrorFile
uploadFile = mudFile

-- | Required ecosystem upload metadata.
uploadMetadata :: MirrorUploadData metadata -> metadata
uploadMetadata = mudMetadata

-- | Reject a mismatched identity or source payload before deriving the complete physical inventory.
checkedMirrorInventory :: PackageName -> Version -> VersionDoc -> MirrorReadData -> Either PublishFault MirrorInventory
checkedMirrorInventory name version doc readData = do
    unless (pkgName details == name && pkgVersion details == version) $
        refused "the source snapshot belongs to another package or version"
    raw <- maybeToRight (PublishSourceUnavailable "the source snapshot has no read metadata") (vdRaw doc)
    unless
        ( case readData of
            NpmMirrorRead fields -> snd npmCached raw == Just (Object fields)
            PyPIMirrorRead document -> snd pypiSimpleCached raw == Just document
            CargoMirrorRead document -> snd cargoCached raw == Just document
            RubyGemsMirrorRead document -> snd rubyGemsCached raw == Just document
        )
        $ refused "the read metadata is not paired with the admitted snapshot"
    restoreMirrorInventory details (restrictReadData (Set.fromList (map artEntryKey (toList (pkgArtifacts details)))) readData)
  where
    details = vdDetails doc

-- | Recheck a trusted durable record without reconstructing policy facts from publication fields.
restoreMirrorInventory :: PackageDetails -> MirrorReadData -> Either PublishFault MirrorInventory
restoreMirrorInventory details readData = do
    files <- restoreMirrorFragment details readData
    pure (MirrorInventory details files readData)

-- | Recheck a stored source fragment without claiming that its owned files form a complete version.
restoreMirrorFragment :: PackageDetails -> MirrorReadData -> Either PublishFault (NonEmpty ExpectedMirrorFile)
restoreMirrorFragment details readData = do
    validateReadData details readData
    traverse expectedFile (pkgArtifacts details) >>= deduplicate

-- | Retain only newly owned files and their original source coordinates for a durable fragment.
splitMirrorInventory :: Set Text -> MirrorInventory -> Maybe (PackageDetails, MirrorReadData)
splitMirrorInventory filenames inventory = do
    selected <- nonEmpty (filter ((`Set.member` filenames) . artFilename) (toList (pkgArtifacts details)))
    let keys = Set.fromList (map artEntryKey (toList selected))
        readData = restrictReadData keys (inventoryReadData inventory)
    pure (details{pkgArtifacts = selected}, readData)
  where
    details = inventoryDetails inventory

restrictReadData :: Set EntryKey -> MirrorReadData -> MirrorReadData
restrictReadData keys = \case
    NpmMirrorRead fields -> NpmMirrorRead fields
    PyPIMirrorRead document -> PyPIMirrorRead (restrictSimpleFiles keys document)
    CargoMirrorRead document -> CargoMirrorRead (restrictCargoDocument keys document)
    RubyGemsMirrorRead document -> RubyGemsMirrorRead (restrictGemDocument keys document)

validateReadData :: PackageDetails -> MirrorReadData -> Either PublishFault ()
validateReadData details = \case
    NpmMirrorRead _ -> unless (pkgEcosystem (pkgName details) == Npm) (refused "the source fields belong to another ecosystem")
    PyPIMirrorRead document -> do
        unless (pkgEcosystem (pkgName details) == PyPI) (refused "the source fields belong to another ecosystem")
        let entries = uniqueEntries (simpleFiles document)
            expectedKeys = Set.fromList (map artEntryKey (toList (pkgArtifacts details)))
        unless (Map.keysSet entries == expectedKeys && Map.size entries == length (simpleFiles document) && Set.size expectedKeys == length (pkgArtifacts details)) $
            refused "the source coordinates do not match the complete admitted entry set"
        case KeyMap.lookup "name" (simpleEnvelope document) of
            Nothing -> Right ()
            Just (String name) | projectName name == Right (pkgName details) -> Right ()
            _ -> refused "the source envelope belongs to another package"
        traverse_ (validatePyPIEntry details entries) (pkgArtifacts details)
    RubyGemsMirrorRead document -> do
        unless (pkgEcosystem (pkgName details) == RubyGems) (refused "the source fields belong to another ecosystem")
        first PublishSourceUnavailable (validateGemSource details document)
    CargoMirrorRead document -> do
        unless (pkgEcosystem (pkgName details) == Cargo) (refused "the source fields belong to another ecosystem")
        first PublishSourceUnavailable (validateCargoSource details document)

validatePyPIEntry :: PackageDetails -> Map EntryKey Value -> Artifact -> Either PublishFault ()
validatePyPIEntry details entries artifact = do
    raw <- maybeToRight (PublishSourceUnavailable "a source coordinate is missing or ambiguous") (Map.lookup (artEntryKey artifact) entries)
    decoded <- first (const (PublishSourceUnavailable "an admitted source file is malformed")) (parseEither parseJSON raw)
    let file = decoded{ifEntryKey = artEntryKey artifact}
        projected = projectSimpleIndex (pkgName details) [] [projectFile file (fileCoordinate (pkgName details) (ifFilename file))]
    version <- maybeToRight (PublishSourceUnavailable "a source filename belongs to another package or version") (Map.lookup (renderVersion (pkgVersion details)) (infoVersions projected))
    let actual :| _ = pkgArtifacts version
    expected <- expectedFile artifact
    actualExpected <- expectedFile actual
    unless (expectedIdentityMatches expected actualExpected && actual{artUrl = artUrl artifact, artHashes = artHashes artifact, artSize = artSize artifact} == artifact) $
        refused "the source file facts do not match the admitted artifact"

expectedFile :: Artifact -> Either PublishFault ExpectedMirrorFile
expectedFile artifact = do
    unless (validEntryKey (artEntryKey artifact)) (refused "an expected artifact has an invalid source coordinate")
    filename <- maybeToRight (PublishSourceUnavailable "an expected artifact has an invalid filename") (mkFilename (artFilename artifact))
    unless (all (isJust . canonicalHashValue) (artHashes artifact)) (refused "an expected artifact has a malformed digest")
    hashes <- maybeToRight (PublishSourceUnavailable "an expected artifact has no integrity digest") (nonEmpty (artHashes artifact))
    pure (ExpectedMirrorFile artifact (MirrorArtifact filename hashes (artSize artifact)))

deduplicate :: NonEmpty ExpectedMirrorFile -> Either PublishFault (NonEmpty ExpectedMirrorFile)
deduplicate (firstFile :| rest) = do
    (_, files) <- foldM add (Map.singleton (fileName firstFile) firstFile, []) rest
    pure (firstFile :| reverse files)
  where
    add (seen, files) candidate = case Map.lookup (fileName candidate) seen of
        Nothing -> Right (Map.insert (fileName candidate) candidate seen, candidate : files)
        Just previous
            | expectedIdentityMatches previous candidate && sameDeclarations previous candidate -> Right (seen, files)
            | otherwise -> refused ("conflicting source entries for " <> fileName candidate)

sameDeclarations :: ExpectedMirrorFile -> ExpectedMirrorFile -> Bool
sameDeclarations left right =
    a{artEntryKey = artEntryKey b, artUrl = artUrl b, artHashes = artHashes b, artSize = artSize b} == b
  where
    a = expectedArtifact left
    b = expectedArtifact right

fileName :: ExpectedMirrorFile -> Text
fileName = unFilename . maFilename . expectedDescriptor

-- | Source locations may change, but filenames, digest identities, and known sizes must agree.
expectedIdentityMatches :: ExpectedMirrorFile -> ExpectedMirrorFile -> Bool
expectedIdentityMatches left right =
    maFilename a == maFilename b
        && compatibleSize (maSize a) (maSize b)
        && sharedDigestsAgree aHashes bHashes
        && case (strongestAlgorithm a, strongestAlgorithm b) of
            (Just leftAlgorithm, Just rightAlgorithm) -> Map.member (min leftAlgorithm rightAlgorithm) (Map.intersection aHashes bHashes)
            _ -> False
  where
    a = expectedDescriptor left
    b = expectedDescriptor right
    aHashes = digestIdentities (toList (maHashes a))
    bHashes = digestIdentities (toList (maHashes b))

-- | A persisted SHA-256 computed from verified bytes can bridge different source and target algorithms.
expectedTargetMatchesWith :: ExpectedMirrorFile -> Maybe Hash -> Artifact -> Bool
expectedTargetMatchesWith expected stored target =
    not (targetIdentityConflict expected stored target)
        && or (Map.intersectionWith (\allowed actual -> not (Set.null actual) && actual `Set.isSubsetOf` allowed) (targetProofs expected stored) (digestIdentities (artHashes target)))

-- | Distinguish contradictory target evidence from a target that advertises no usable proof.
targetIdentityConflict :: ExpectedMirrorFile -> Maybe Hash -> Artifact -> Bool
targetIdentityConflict expected stored target =
    artFilename source /= artFilename target
        || not (compatibleSize (artSize source) (artSize target))
        || or (Map.intersectionWith (\allowed actual -> not (actual `Set.isSubsetOf` allowed)) (targetProofs expected stored) (digestIdentities (artHashes target)))
  where
    source = expectedArtifact expected

targetProofs :: ExpectedMirrorFile -> Maybe Hash -> Map HashAlg (Set Text)
targetProofs expected stored = case stored of
    Just verifiedHash
        | assertedAlg verifiedHash == Just SHA256
        , Just digest <- canonicalHashValue verifiedHash ->
            Map.insertWith Set.intersection SHA256 (Set.singleton digest) strongest
    _ -> strongest
  where
    strongest = case strongestAlgorithm (expectedDescriptor expected) of
        Nothing -> mempty
        Just algorithm -> Map.restrictKeys (digestIdentities (artHashes (expectedArtifact expected))) (Set.singleton algorithm)

strongestAlgorithm :: MirrorArtifact -> Maybe HashAlg
strongestAlgorithm = assertedAlg . authoritativeDigest . maHashes

digestIdentities :: [Hash] -> Map HashAlg (Set Text)
digestIdentities hashes = Map.fromListWith Set.union [(algorithm, Set.singleton digest) | hash <- hashes, Just algorithm <- [assertedAlg hash], Just digest <- [canonicalHashValue hash]]

sharedDigestsAgree :: Map HashAlg (Set Text) -> Map HashAlg (Set Text) -> Bool
sharedDigestsAgree left right = and (Map.intersectionWith (==) left right)

compatibleSize :: Maybe Int -> Maybe Int -> Bool
compatibleSize (Just left) (Just right) = left == right
compatibleSize _ _ = True

-- | Verify the strongest admitted digest without permitting replacement bytes at publication.
verifyMirrorBytes :: ExpectedMirrorFile -> ByteString -> Either PublishFault VerifiedArtifactBytes
verifyMirrorBytes expected bytes = case verifyIntegrity (maHashes (expectedDescriptor expected)) bytes of
    IntegrityMismatch detail -> Left (PublishIntegrityMismatch detail)
    IntegrityVerified -> do
        compute <- maybeToRight (PublishIntegrityMismatch "SHA-256 computation is unavailable") (computeDigest SHA256)
        targetHash <- first PublishIntegrityMismatch (mkHash SHA256 (hexDigestText (compute (toLazy bytes))))
        pure (VerifiedArtifactBytes expected bytes targetHash)

-- | Codec construction alone attaches checked ecosystem metadata to verified inventory membership.
checkedMirrorUploadData :: MirrorInventory -> ExpectedMirrorFile -> VerifiedArtifactBytes -> metadata -> Either PublishFault (MirrorUploadData metadata)
checkedMirrorUploadData inventory file verified metadata = do
    unless (file `elem` inventoryFiles inventory && verifiedFile verified == file) $
        refused "the verified artifact does not belong to the complete mirror inventory"
    pure (MirrorUploadData inventory file verified metadata)

-- | Verified bytes from this upload, without a replacement body.
uploadBytes :: MirrorUploadData metadata -> ByteString
uploadBytes = verifiedBytes . mudVerified

-- | The target digest computed from the same verified upload body.
uploadTargetHash :: MirrorUploadData metadata -> Hash
uploadTargetHash = verifiedTargetHash . mudVerified

refused :: Text -> Either PublishFault a
refused = Left . PublishSourceUnavailable
