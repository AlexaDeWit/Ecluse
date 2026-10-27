-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Whole-version membership, retained source coordinates, and verified publication bytes.
module Ecluse.Core.Registry.MirrorUpload.InternalSpec (spec) where

import Data.Aeson (Value (Bool, Null, Object, String), object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Test.Hspec

import Ecluse.Core.Package (Artifact (..), CoreMetadata (MetadataAvailable), Hash, HashAlg (SHA1, SHA256, SHA512, SRI), PackageDetails (..), hashAlg, hashValue, renderHashAlg)
import Ecluse.Core.Package.Entry (EntryKey (ArrayEntry, RecordEntry))
import Ecluse.Core.Registry (PublishFault (PublishSourceUnavailable))
import Ecluse.Core.Registry.CachedDocument (cargoCached, pypiSimpleCached)
import Ecluse.Core.Registry.Cargo.Document (cargoDocument, cargoRecords)
import Ecluse.Core.Registry.Metadata (VersionDoc (..))
import Ecluse.Core.Registry.MirrorUpload.Internal
import Ecluse.Core.Registry.PyPI.Document (simpleDocument, simpleEnvelope, simpleFiles)
import Ecluse.Test.Package (hexSha256Of, hexSha512Of, pypiVersion, sampleArtifact, sampleDetails, sriSha256Of, unsafeHash, unscopedPyPI, validSha1)
import Ecluse.Test.Registry.Cargo.Metadata (cargoTestRecord)
import Ecluse.Test.Registry.Cargo.Publish (cargoSourceVersion)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = do
    describe "Cargo durable source facts" $ do
        it "preserves the exact row, coordinate, filename and checksum in a selected fragment" $ do
            let row = cargoTestRecord "1.0.0"
                document = cargoDocument [(RecordEntry 7, row)]
            doc <- cargoSourceVersion row
            let admitted = vdDetails doc
            inventory <- expectRight (checkedMirrorInventory (pkgName admitted) (pkgVersion admitted) doc (CargoMirrorRead document))
            let artifact :| _ = pkgArtifacts admitted
                file :| _ = inventoryFiles inventory
            inventoryDetails inventory `shouldBe` admitted
            length (inventoryFiles inventory) `shouldBe` 1
            expectedArtifact file `shouldBe` artifact
            artEntryKey artifact `shouldBe` RecordEntry 7
            artFilename artifact `shouldBe` "fixture-1.0.0.crate"
            artHashes artifact `shouldBe` [unsafeHash SHA256 (hexSha256Of "Owned Cargo metadata fixture bytes")]
            case inventoryReadData inventory of
                CargoMirrorRead kept -> cargoRecords kept `shouldBe` [(RecordEntry 7, row)]
                _ -> expectationFailure "expected Cargo source fields"
            fragment <- maybe (fail "missing Cargo fragment") pure (splitMirrorInventory (Set.singleton "fixture-1.0.0.crate") inventory)
            uncurry restoreMirrorFragment fragment `shouldBe` Right (inventoryFiles inventory)
            snd cargoCached (mirrorReadDocument (snd fragment)) `shouldBe` Just document
        it "refuses foreign ecosystems, mismatched coordinates and multiple source claims" $ do
            let row = cargoTestRecord "1.0.0"
                pairedSource = CargoMirrorRead (cargoDocument [(RecordEntry 7, row)])
            doc <- cargoSourceVersion row
            let admitted = vdDetails doc
                artifact :| _ = pkgArtifacts admitted
            restoreMirrorFragment admitted{pkgName = unscopedPyPI "fixture"} pairedSource
                `shouldBe` Left (PublishSourceUnavailable "the source fields belong to another ecosystem")
            for_ [RecordEntry (-1), RecordEntry 8, ArrayEntry 7] $ \key ->
                restoreMirrorFragment admitted (CargoMirrorRead (cargoDocument [(key, row)]))
                    `shouldBe` Left (PublishSourceUnavailable "Cargo source coordinate disagrees with its artifact")
            for_ [CargoMirrorRead (cargoDocument []), CargoMirrorRead (cargoDocument [(RecordEntry 7, row), (RecordEntry 8, row)]), CargoMirrorRead (cargoDocument [(RecordEntry 7, Null)])] $ \source ->
                restoreMirrorFragment admitted source `shouldBe` Left (PublishSourceUnavailable "Cargo requires exactly one paired source row and archive")
            restoreMirrorFragment admitted{pkgArtifacts = artifact :| [artifact]} pairedSource
                `shouldBe` Left (PublishSourceUnavailable "Cargo requires exactly one paired source row and archive")
        it "refuses changed native identity, digest and policy facts against the original admission" $ do
            let row = cargoTestRecord "1.0.0"
            doc <- cargoSourceVersion row
            let admitted = vdDetails doc
                artifact :| _ = pkgArtifacts admitted
                source = CargoMirrorRead (cargoDocument [(RecordEntry 7, row)])
            case row of
                Object fields -> do
                    for_ [("name", String "another", "Cargo source identity disagrees with the admitted version"), ("vers", String "1.0.1", "Cargo source identity disagrees with the admitted version"), ("cksum", String (hexSha256Of "different"), "Cargo source archive identity disagrees with its artifact"), ("pubtime", String "2026-01-01T00:00:00Z", "Cargo source policy facts disagree with the admitted version"), ("yanked", Bool True, "Cargo source policy facts disagree with the admitted version")] $ \(key, value, reason) ->
                        restoreMirrorFragment admitted (CargoMirrorRead (cargoDocument [(RecordEntry 7, Object (KeyMap.insert key value fields))]))
                            `shouldBe` Left (PublishSourceUnavailable reason)
                    for_ ["name", "vers", "cksum"] $ \key ->
                        restoreMirrorFragment admitted (CargoMirrorRead (cargoDocument [(RecordEntry 7, Object (KeyMap.delete key fields))]))
                            `shouldBe` Left (PublishSourceUnavailable ("invalid Cargo field: " <> Key.toText key))
                _ -> expectationFailure "expected a Cargo row object"
            restoreMirrorFragment admitted{pkgArtifacts = artifact{artFilename = "another-1.0.0.crate"} :| []} source
                `shouldBe` Left (PublishSourceUnavailable "Cargo source archive identity disagrees with its artifact")
            for_ [artifact{artCoreMetadata = Just (MetadataAvailable True)}, artifact{artSize = Just 1}] $ \changed ->
                restoreMirrorFragment admitted{pkgArtifacts = changed :| []} source
                    `shouldBe` Left (PublishSourceUnavailable "Cargo source policy facts disagree with the admitted version")

    it "retains every wheel and sdist in the paired version" $ do
        inventory <- expectRight (sourceInventory (wheel :| [sdist]))
        map (artFilename . expectedArtifact) (toList (inventoryFiles inventory)) `shouldBe` [wheelName, sdistName]

    it "copies an identical filename once while retaining both source entries and locations" $ do
        let alias = (ArrayEntry 2, wheelName, "https://other.example/wheel", "wheel")
        inventory <- expectRight (sourceInventory (wheel :| [alias]))
        length (inventoryFiles inventory) `shouldBe` 1
        case inventoryReadData inventory of
            PyPIMirrorRead document -> length (simpleFiles document) `shouldBe` 2
            _ -> expectationFailure "expected PyPI source fields"

    it "refuses an impossible source coordinate" $
        sourceInventory ((ArrayEntry (-1), wheelName, "https://files.example/wheel", "wheel") :| []) `shouldSatisfy` isLeft

    it "refuses duplicate filenames that assert different bytes" $
        sourceInventory (wheel :| [(ArrayEntry 2, wheelName, "https://files.example/other", "different")]) `shouldSatisfy` isLeft

    it "keeps original source coordinates when selecting a newly owned fragment" $ do
        inventory <- expectRight (sourceInventory (wheel :| [sdist]))
        (details, raw) <- maybe (fail "missing fragment") pure (splitMirrorInventory (Set.singleton sdistName) inventory)
        map artFilename (toList (pkgArtifacts details)) `shouldBe` [sdistName]
        case raw of
            PyPIMirrorRead document -> map fst (simpleFiles document) `shouldBe` [ArrayEntry 7]
            _ -> expectationFailure "expected PyPI source fields"
        restored <- expectRight (restoreMirrorFragment details raw)
        map (artFilename . expectedArtifact) (toList restored) `shouldBe` [sdistName]

    it "does not create an empty fragment" $ do
        inventory <- expectRight (sourceInventory (wheel :| []))
        splitMirrorInventory Set.empty inventory `shouldBe` Nothing

    it "selects the exact supported entries when the raw source also contains excluded files" $ do
        inventory <- expectRight (sourceInventory (wheel :| [sdist]))
        let firstArtifact :| _ = pkgArtifacts (inventoryDetails inventory)
            admitted = (inventoryDetails inventory){pkgArtifacts = firstArtifact :| []}
        case inventoryReadData inventory of
            readData@(PyPIMirrorRead document) -> do
                let doc = VersionDoc admitted (Just (fst pypiSimpleCached document))
                selected <- expectRight (checkedMirrorInventory (pkgName admitted) (pkgVersion admitted) doc readData)
                length (inventoryFiles selected) `shouldBe` 1
                case inventoryReadData selected of
                    PyPIMirrorRead kept -> map fst (simpleFiles kept) `shouldBe` [ArrayEntry 0]
                    _ -> expectationFailure "expected PyPI fields"
            _ -> expectationFailure "expected PyPI source fields"

    it "rejects ambiguous admitted source coordinates" $
        sourceInventory (wheel :| [(ArrayEntry 0, sdistName, "https://files.example/sdist", "sdist")]) `shouldSatisfy` isLeft

    it "refuses a changed source digest at the admitted coordinate" $ do
        inventory <- expectRight (sourceInventory (wheel :| []))
        changed <- expectRight (sourceInventoryWith (const [unsafeHash SHA256 (hexSha256Of "other")]) (wheel :| []))
        let admitted = inventoryDetails inventory
            readData = inventoryReadData changed
            refusal = PublishSourceUnavailable "the source file facts do not match the admitted artifact"
        case readData of
            PyPIMirrorRead document -> do
                map fst (simpleFiles document) `shouldBe` [ArrayEntry 0]
                let doc = VersionDoc admitted (Just (fst pypiSimpleCached document))
                checkedMirrorInventory (pkgName admitted) (pkgVersion admitted) doc readData `shouldBe` Left refusal
                restoreMirrorFragment admitted readData `shouldBe` Left refusal
            _ -> expectationFailure "expected PyPI source fields"

    it "refuses a changed companion declaration with the admitted digest and coordinate" $ do
        inventory <- expectRight (sourceInventory (wheel :| []))
        let admitted = inventoryDetails inventory
            artifact :| _ = pkgArtifacts admitted
            refusal = PublishSourceUnavailable "the source file facts do not match the admitted artifact"
        artCoreMetadata artifact `shouldBe` Nothing
        case inventoryReadData inventory of
            PyPIMirrorRead document
                | [(key, Object fields)] <- simpleFiles document -> do
                    let changed = simpleDocument (simpleEnvelope document) [(key, Object (KeyMap.insert "core-metadata" (Bool True) fields))]
                        readData = PyPIMirrorRead changed
                        doc = VersionDoc admitted (Just (fst pypiSimpleCached changed))
                    checkedMirrorInventory (pkgName admitted) (pkgVersion admitted) doc readData `shouldBe` Left refusal
                    restoreMirrorFragment admitted readData `shouldBe` Left refusal
            _ -> expectationFailure "expected one PyPI source file object"

    it "rejects a fragment carrying an undeclared source entry" $ do
        inventory <- expectRight (sourceInventory (wheel :| [sdist]))
        let firstArtifact :| _ = pkgArtifacts (inventoryDetails inventory)
            reduced = (inventoryDetails inventory){pkgArtifacts = firstArtifact :| []}
        restoreMirrorFragment reduced (inventoryReadData inventory) `shouldSatisfy` isLeft

    it "refuses altered bytes before a writer value exists" $ do
        inventory <- expectRight (sourceInventory (wheel :| []))
        let file :| _ = inventoryFiles inventory
        verifyMirrorBytes file "different" `shouldSatisfy` isLeft

    it "keeps the exact verified body in the checked upload" $ do
        inventory <- expectRight (sourceInventory (wheel :| []))
        let file :| _ = inventoryFiles inventory
        verified <- expectRight (verifyMirrorBytes file "wheel")
        upload <- expectRight (checkedMirrorUploadData inventory file verified ("metadata" :: Text))
        uploadBytes upload `shouldBe` "wheel"
        uploadInventory upload `shouldBe` inventory

    it "refuses verified bytes belonging to another expected member" $ do
        inventory <- expectRight (sourceInventory (wheel :| [sdist]))
        case inventoryFiles inventory of
            firstFile :| [secondFile] -> do
                verified <- expectRight (verifyMirrorBytes firstFile "wheel")
                checkedMirrorUploadData inventory secondFile verified ("metadata" :: Text) `shouldSatisfy` isLeft
            _ -> expectationFailure "expected two files"

    it "requires current target digest evidence and tolerates a changed target location" $ do
        inventory <- expectRight (sourceInventory (wheel :| []))
        let file :| _ = inventoryFiles inventory
            target = (expectedArtifact file){artUrl = "https://private.example/wheel"}
        expectedTargetMatchesWith file Nothing target `shouldBe` True
        expectedTargetMatchesWith file Nothing target{artHashes = []} `shouldBe` False
        expectedTargetMatchesWith file Nothing target{artHashes = [unsafeHash SHA1 validSha1]} `shouldBe` False
        expectedTargetMatchesWith file Nothing target{artHashes = [unsafeHash SHA256 (hexSha256Of "other")]} `shouldBe` False

    it "proves a SHA-512 source against a SHA-256 target using only the digest computed from verified bytes" $ do
        inventory <- expectRight (sourceInventoryWith (\bytes -> [unsafeHash SHA512 (hexSha512Of bytes), unsafeHash SHA256 (hexSha256Of "unverified weaker claim")]) (wheel :| []))
        let file :| _ = inventoryFiles inventory
            target = (expectedArtifact file){artHashes = [unsafeHash SHA256 (hexSha256Of "wheel")]}
        verified <- expectRight (verifyMirrorBytes file "wheel")
        expectedTargetMatchesWith file Nothing target `shouldBe` False
        expectedTargetMatchesWith file (Just (verifiedTargetHash verified)) target `shouldBe` True
        expectedTargetMatchesWith file (Just (verifiedTargetHash verified)) target{artHashes = [unsafeHash SHA512 (hexSha512Of "wheel")]} `shouldBe` True
        targetIdentityConflict file (Just (verifiedTargetHash verified)) target{artHashes = []} `shouldBe` False
        targetIdentityConflict file (Just (verifiedTargetHash verified)) target{artHashes = [unsafeHash SHA256 (hexSha256Of "changed bytes")]} `shouldBe` True
        expectedTargetMatchesWith file (Just (verifiedTargetHash verified)) target{artHashes = [unsafeHash SHA512 (hexSha512Of "wheel"), unsafeHash SHA256 (hexSha256Of "changed bytes")]} `shouldBe` False
        expectedTargetMatchesWith file (Just (verifiedTargetHash verified)) target{artHashes = [unsafeHash SRI (sriSha256Of "wheel")]} `shouldBe` True
        expectedTargetMatchesWith file (Just (verifiedTargetHash verified)) target{artHashes = [unsafeHash SHA256 (hexSha256Of "changed bytes")]} `shouldBe` False

wheelName, sdistName :: Text
wheelName = "demo-1.0-py3-none-any.whl"
sdistName = "demo-1.0.tar.gz"

wheel, sdist :: (EntryKey, Text, Text, ByteString)
wheel = (ArrayEntry 0, wheelName, "https://files.example/wheel", "wheel")
sdist = (ArrayEntry 7, sdistName, "https://files.example/sdist", "sdist")

sourceInventory :: NonEmpty (EntryKey, Text, Text, ByteString) -> Either PublishFault MirrorInventory
sourceInventory = sourceInventoryWith (\bytes -> [unsafeHash SHA256 (hexSha256Of bytes)])

sourceInventoryWith :: (ByteString -> [Hash]) -> NonEmpty (EntryKey, Text, Text, ByteString) -> Either PublishFault MirrorInventory
sourceInventoryWith hashes files = checkedMirrorInventory name version doc (PyPIMirrorRead document)
  where
    name = unscopedPyPI "demo"
    version = pypiVersion "1"
    details = (sampleDetails name version){pkgArtifacts = fmap artifact files}
    document = simpleDocument mempty (map entry (toList files))
    doc = VersionDoc details (Just (fst pypiSimpleCached document))
    artifact (key, filename, location, bytes) = sampleArtifact{artEntryKey = key, artFilename = filename, artUrl = location, artHashes = hashes bytes, artSize = Just (BS.length bytes)}
    entry (key, filename, location, bytes) = (key, object ["filename" .= filename, "url" .= location, "hashes" .= Map.fromList [(renderHashAlg (hashAlg hash), hashValue hash) | hash <- hashes bytes], "size" .= BS.length bytes])
