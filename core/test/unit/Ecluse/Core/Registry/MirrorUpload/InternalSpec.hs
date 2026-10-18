-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Whole-version membership, retained source coordinates, and verified publication bytes.
module Ecluse.Core.Registry.MirrorUpload.InternalSpec (spec) where

import Data.Aeson (Object, Value (Bool, Null, Object, String), object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Test.Hspec

import Ecluse.Core.Package (Artifact (..), CoreMetadata (..), Hash, HashAlg (SHA1, SHA256, SHA512, SRI), PackageDetails (..), hashAlg, hashValue, renderHashAlg)
import Ecluse.Core.Package.Entry (EntryKey (ArrayEntry))
import Ecluse.Core.Registry (PublishFault (PublishSourceUnavailable))
import Ecluse.Core.Registry.CachedDocument (pypiSimpleCached)
import Ecluse.Core.Registry.Metadata (VersionDoc (..))
import Ecluse.Core.Registry.MirrorUpload.Internal
import Ecluse.Core.Registry.PyPI.Document (simpleDocument, simpleEnvelope, simpleFiles)
import Ecluse.Test.Package (hexSha256Of, hexSha512Of, pypiVersion, sampleArtifact, sampleDetails, sriSha256Of, unsafeHash, unscopedPyPI, validSha1)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = do
    sourceFactSpec

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

sourceFactSpec :: Spec
sourceFactSpec = describe "durable source projection equivalence" $
    for_ sourceFactCases $ \(label, changeDetails, changeFields, failure) ->
        it label $ do
            inventory <- expectRight (sourceInventory ((ArrayEntry 7, wheelName, "https://files.example/wheel", "wheel") :| []))
            case inventoryReadData inventory of
                PyPIMirrorRead document | [(key, Object fields)] <- simpleFiles document -> do
                    let details = changeDetails (inventoryDetails inventory)
                        source = PyPIMirrorRead (simpleDocument (simpleEnvelope document) [(key, Object (changeFields fields))])
                        expected = maybe (Right (toList (pkgArtifacts details))) (Left . PublishSourceUnavailable) failure
                    fmap (map expectedArtifact . toList) (restoreMirrorFragment details source) `shouldBe` expected
                    when (isNothing failure) $ do
                        restored <- expectRight (restoreMirrorInventory details source)
                        inventoryReadData restored `shouldBe` source
                _ -> expectationFailure "expected one independently constructed source entry"

sourceFactCases :: [(String, PackageDetails -> PackageDetails, Object -> Object, Maybe Text)]
sourceFactCases =
    [ ("accepts a canonical filename alias at the original nonzero coordinate", filename "demo-01.0-py3-none-any.whl", field "filename" (String "demo-01.0-py3-none-any.whl"), Nothing)
    , ("rejects equivalent admitted versions with different rendered text", \details -> details{pkgVersion = pypiVersion "1.0"}, id, Just wrongVersion)
    , ("rejects a different version", id, field "filename" (String "demo-2.0-py3-none-any.whl"), Just wrongVersion)
    , ("rejects another project", id, field "filename" (String "other-1.0-py3-none-any.whl"), Just wrongVersion)
    , ("rejects unsupported archive names", id, field "filename" (String "demo-1.0.exe"), Just wrongVersion)
    , ("rejects invalid PEP 440 text", id, field "filename" (String "demo-bad-py3-none-any.whl"), Just wrongVersion)
    , ("accepts legacy read archives without the stricter upload grammar", filename "demo-1.0.tgz", field "filename" (String "demo-1.0.tgz"), Nothing)
    , ("rejects a malformed required source field", id, field "filename" (Bool False), Just malformed)
    , ("rejects a malformed URL field before checking the filename", id, field "url" (Bool False) . field "filename" (String "other-2.0.exe"), Just malformed)
    , ("retains changed source URLs without imposing location equality", id, field "url" (String "https://changed.example/file"), Nothing)
    , ("retains unknown source fields without rebuilding their tree", id, field "future-field" (object ["nested" .= ["first", "second" :: Text]]), Nothing)
    , ("ignores unknown digest algorithms beside a supported identity", id, hashes [("sha256", hexSha256Of "wheel"), ("future-hash", "retained")], Nothing)
    , ("refuses a source with only unknown digest algorithms", id, hashes [("future-hash", "retained")], Just noDigest)
    , ("refuses a source with only malformed supported digests", id, hashes [("sha256", "malformed")], Just noDigest)
    , ("drops a malformed weaker digest beside a valid supported identity", id, hashes [("sha256", hexSha256Of "wheel"), ("sha1", "malformed")], Nothing)
    , ("rejects a conflicting supported digest", id, hashes [("sha256", hexSha256Of "changed")], Just mismatch)
    , ("rejects conflicting weaker digests despite matching strongest evidence", artifact (\file -> file{artHashes = [unsafeHash SHA256 (hexSha256Of "wheel"), unsafeHash SHA1 validSha1]}), hashes [("sha256", hexSha256Of "wheel"), ("sha1", "1111111111111111111111111111111111111111")], Just mismatch)
    , ("rejects a shared weaker digest without common strongest evidence", artifact (\file -> file{artHashes = [unsafeHash SHA256 (hexSha256Of "wheel"), unsafeHash SHA1 validSha1]}), hashes [("sha512", hexSha512Of "wheel"), ("sha1", validSha1)], Just mismatch)
    , ("accepts a missing source size", id, KeyMap.delete "size", Nothing)
    , ("accepts a known source size against an unknown admitted size", artifact (\file -> file{artSize = Nothing}), id, Nothing)
    , ("accepts a null source size", id, field "size" Null, Nothing)
    , ("rejects conflicting known sizes", id, field "size" (toJSON (6 :: Int)), Just mismatch)
    , ("accepts a matching companion declaration", artifact (\file -> file{artCoreMetadata = Just (MetadataAvailable True)}), field "core-metadata" (Bool True), Nothing)
    , ("retains unknown companion digest declarations", artifact (\file -> file{artCoreMetadata = Just (MetadataDigests (Map.singleton "future-hash" "retained"))}), field "core-metadata" (object ["future-hash" .= ("retained" :: Text)]), Nothing)
    , ("rejects a changed companion declaration", id, field "core-metadata" (Bool True), Just mismatch)
    , ("rejects a malformed companion declaration", id, field "core-metadata" Null, Just malformed)
    , ("checks the filename before missing admitted digest evidence", artifact (\file -> file{artHashes = []}), field "filename" (String "other-1.0-py3-none-any.whl"), Just wrongVersion)
    , ("checks admitted digest evidence before source digest evidence", artifact (\file -> file{artHashes = []}), hashes [("sha256", "malformed")], Just noDigest)
    ]
  where
    artifact change details = details{pkgArtifacts = fmap change (pkgArtifacts details)}
    filename name = artifact (\file -> file{artFilename = name})
    field = KeyMap.insert
    hashes pairs = field "hashes" (toJSON (Map.fromList pairs :: Map Text Text))
    wrongVersion = "a source filename belongs to another package or version"
    malformed = "an admitted source file is malformed"
    mismatch = "the source file facts do not match the admitted artifact"
    noDigest = "an expected artifact has no integrity digest"

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
