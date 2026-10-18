-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Managed reads validate fresh state and restore fields only after current private identity proof.
module Ecluse.Core.Server.MirrorViewSpec (spec) where

import Control.Monad (foldM)
import Data.Aeson (Value (Object, String))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Map.Strict qualified as Map
import Data.Primitive.SmallArray (smallArrayFromList)
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Mirror.State
import Ecluse.Core.Mirror.State.Internal (MirrorVersion (..), ProjectMirrorRecord (..), StateRevision (StateRevision), StoredMirrorFragment (..))
import Ecluse.Core.Package
import Ecluse.Core.Package.Entry (EntryKey (ArrayEntry))
import Ecluse.Core.Registry.CachedDocument (CachedDoc, npmCached, pypiPacked, pypiSimpleCached, weighCachedDoc)
import Ecluse.Core.Registry.Json.Packed (docTable)
import Ecluse.Core.Registry.Json.Writer (packTree)
import Ecluse.Core.Registry.Metadata (Manifest (..), VersionDoc (..))
import Ecluse.Core.Registry.MirrorUpload
import Ecluse.Core.Registry.MirrorUpload.Internal (restoreMirrorInventory)
import Ecluse.Core.Registry.PyPI.Document (SimpleDocument, packedSimple, packedSimpleDocument, simpleDocument, simpleEnvelope, simpleFiles, urlHole)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Server.Admission.Meter (MemoryTicket, MeterSettings (..), newMemoryMeter, withMemoryEntry)
import Ecluse.Core.Server.Cache.Backend.Local (newLocalPool)
import Ecluse.Core.Server.Cache.Types (StoreBudget (StoreBudget))
import Ecluse.Core.Server.MirrorView
import Ecluse.Core.Version (renderVersion)
import Ecluse.Test.Mirror.State
import Ecluse.Test.Package (pypiVersion)
import Ecluse.Test.Port (noopMetricsPort)
import Ecluse.Test.Snapshot (digestOf)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = do
    describe "managed mirror source restoration" $ do
        it "retains every source entry across colliding fragment coordinates and changed native URLs" $ do
            originalInventory <- mirrorSample [1] False "https://public.test/" "first"
            later <- mirrorSample [2] False "https://public.test/" "second"
            initial <- expectRight (extendProjectRecord defaultLimits target originalInventory Nothing)
            record <- expectRight (extendProjectRecord defaultLimits target later (Just initial))
            targetInventory <- mirrorSample [1, 2] False "https://private.test/moved/" "stripped"
            let current = Just (VersionDoc (inventoryDetails targetInventory) Nothing)
            restored <- expectRight (mirrorVersionView mirrorVersion record current)
            let documents = [document | doc <- maybeToList restored, sourceDoc <- maybeToList (vdRaw doc), document <- maybeToList (snd pypiSimpleCached sourceDoc)]
                values = concatMap simpleFiles documents
            map fst values `shouldBe` [ArrayEntry 0, ArrayEntry 1]
            map (field "note" . snd) values `shouldBe` [Just (String "first"), Just (String "second")]
            map (field "url" . snd) values `shouldBe` [Just (String ("https://private.test/moved/" <> mirrorFilename n)) | n <- [1, 2]]

        it "refuses a native deletion and digestless presence after earlier completion" $ do
            inventory <- mirrorSample [1, 2] False "https://public.test/" "source"
            record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
            let details = inventoryDetails inventory
                firstFile :| _ = pkgArtifacts details
            mirrorVersionView mirrorVersion record (Just (VersionDoc details{pkgArtifacts = firstFile :| []} Nothing)) `shouldSatisfy` isLeft
            mirrorVersionView mirrorVersion record (Just (VersionDoc details{pkgArtifacts = fmap (\artifact -> artifact{artHashes = []}) (pkgArtifacts details)} Nothing)) `shouldSatisfy` isLeft

        it "requires a declared companion to be retained even when every native wheel exists" $ do
            inventory <- mirrorSample [1] True "https://public.test/" "source"
            record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
            mirrorVersionView mirrorVersion record (Just (VersionDoc (inventoryDetails inventory) Nothing)) `shouldSatisfy` isLeft

        it "allows a complete public fallback but rejects an apparently successful shortened source" $ do
            inventory <- mirrorSample [1, 2, 3] False "https://public.test/" "source"
            short <- mirrorSample [1, 2] False "https://public.test/" "later"
            record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
            publicMirrorCoverage (recordVersions record) (info inventory) `shouldBe` True
            publicMirrorCoverage (recordVersions record) (info short) `shouldBe` False

        it "keeps restored listing contributions packed and binds their validator to revision" $ do
            inventory <- mirrorSample [1, 2] False "https://private.test/" "source"
            record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
            let private = Manifest (info inventory) (raw inventory) 1024 (digestOf "private")
            (_, initialManifest, incompleteVersions) <- expectRight (mirrorProjectViews revision record (Just private))
            (_, updatedManifest, _) <- expectRight (mirrorProjectViews (rev "new") record (Just private))
            incompleteVersions `shouldSatisfy` Map.null
            (initialManifest >>= snd pypiPacked . manifestRaw) `shouldSatisfy` isJust
            fmap manifestDigest initialManifest `shouldNotBe` fmap manifestDigest updatedManifest

        it "matches independently constructed project fields across versions, fragments and coordinate gaps" $ do
            (record, private, expected) <- projectFixture
            (unmanaged, managed, pending) <- expectRight (mirrorProjectViews revision record (Just private))
            fmap manifestInfo managed `shouldBe` Just (manifestInfo expected)
            (managed >>= snd pypiPacked . manifestRaw >>= packedSimpleDocument) `shouldBe` snd pypiSimpleCached (manifestRaw expected)
            fmap manifestBodyBytes managed `shouldBe` Just (weighCachedDoc (packDocument (fromMaybe (simpleDocument mempty []) (snd pypiSimpleCached (manifestRaw expected)))))
            fmap manifestRaw unmanaged `shouldBe` Just (manifestRaw private)
            fmap (Map.keys . infoVersions . manifestInfo) unmanaged `shouldBe` Just ["3"]
            Map.keys pending `shouldBe` ["4"]
            let selected = Map.lookup "2" (infoVersions (manifestInfo private))
            restored <- expectRight (mirrorVersionView (pypiVersion "2") record (fmap (`VersionDoc` Nothing) selected))
            fmap (map artEntryKey . toList . pkgArtifacts . vdDetails) restored `shouldBe` Just [ArrayEntry 0]

        it "keeps a version pending after a current private file disappears" $ do
            (record, private, _) <- projectFixture
            let withoutFile details = details{pkgArtifacts = fmap identityArtifact (pkgArtifacts details)}
                identityArtifact artifact = artifact{artHashes = []}
                shortened = private{manifestInfo = (manifestInfo private){infoVersions = Map.adjust withoutFile "1" (infoVersions (manifestInfo private))}}
            (_, managed, pending) <- expectRight (mirrorProjectViews revision record (Just shortened))
            fmap (Map.keys . infoVersions . manifestInfo) managed `shouldBe` Just ["2"]
            Map.keys pending `shouldBe` ["1", "4"]
            let missing = private{manifestInfo = (manifestInfo private){infoVersions = Map.delete "1" (infoVersions (manifestInfo private))}}
            (_, _, absent) <- expectRight (mirrorProjectViews revision record (Just missing))
            Map.keys absent `shouldBe` ["1", "4"]

        it "refuses the whole view when a later complete version has malformed retained fields" $ do
            (record, private, _) <- projectFixture
            let breakVersion mirror = mirror{storedFragments = fmap breakFragment (storedFragments mirror)}
                breakFragment (StoredMirrorFragment details _ files) = StoredMirrorFragment details (PyPIMirrorRead (simpleDocument mempty [(artEntryKey artifact, String "malformed") | artifact <- toList (pkgArtifacts details)])) files
                broken = record{projectVersions = Map.adjust breakVersion "2" (recordVersions record)}
            void (mirrorProjectViews revision broken (Just private)) `shouldBe` Left (StateInvalid "cannot restore paired mirror source")

    describe "per-request project validation" $ do
        it "performs one state exchange on every read and accepts only the requested 304 revision" $ do
            record <- sampleRecord
            requests <- newIORef []
            let readState _ previous = do
                    modifyIORef' requests (<> [previous])
                    pure (Right (maybe (StateLoaded revision record) StateUnchanged previous))
            view <- viewWith readState
            runRead view `shouldReturn` Right (Just (revision, record))
            runRead view `shouldReturn` Right (Just (revision, record))
            readIORef requests `shouldReturn` [Nothing, Just revision]

        it "owns a retained revision without changing its bytes" $ do
            record <- sampleRecord
            let original = BS.take 4 (BS.replicate 65536 97)
            view <- viewWith (\_ _ -> pure (Right (StateLoaded (rev original) record)))
            result <- runRead view >>= expectRight
            case result of
                Just (StateRevision _ _ owned, _) -> do
                    owned `shouldBe` original
                    let (oldBacking, _, _) = BSI.toForeignPtr original
                        (newBacking, _, _) = BSI.toForeignPtr owned
                    newBacking `shouldNotBe` oldBacking
                Nothing -> expectationFailure "loaded state disappeared"

        it "rejects stale cached state during an outage despite a smaller current public inventory" $ do
            record <- sampleRecord
            answer <- newIORef (Right (StateLoaded revision record))
            view <- viewWith (\_ _ -> readIORef answer)
            runRead view `shouldReturn` Right (Just (revision, record))
            writeIORef answer (Left (StateInvalid "outage"))
            runRead view `shouldReturn` Left (StateInvalid "outage")

        it "rejects an unsolicited 304 and a 304 for a different revision" $ do
            view <- viewWith (\_ _ -> pure (Right (StateUnchanged revision)))
            runRead view `shouldReturn` Left StateRevisionMismatch
            record <- sampleRecord
            answer <- newIORef (Right (StateLoaded revision record))
            warmed <- viewWith (\_ _ -> readIORef answer)
            void (runRead warmed)
            writeIORef answer (Right (StateUnchanged (rev "other")))
            runRead warmed `shouldReturn` Left StateRevisionMismatch

        it "does not cache absence and observes a concurrent extension on the next request" $ do
            record <- sampleRecord
            requests <- newIORef (0 :: Int)
            answer <- newIORef (Right StateAbsent)
            view <- viewWith (\_ _ -> modifyIORef' requests (+ 1) >> readIORef answer)
            runRead view `shouldReturn` Right Nothing
            writeIORef answer (Right (StateLoaded revision record))
            runRead view `shouldReturn` Right (Just (revision, record))
            added <- mirrorSample [4] False "https://public.test/" "added"
            extended <- expectRight (extendProjectRecord defaultLimits target added (Just record))
            writeIORef answer (Right (StateLoaded (rev "extended") extended))
            runRead view `shouldReturn` Right (Just (rev "extended", extended))
            readIORef requests `shouldReturn` 3

projectFixture :: IO (ProjectMirrorRecord, Manifest, Manifest)
projectFixture = do
    first <- fixtureInventory "1" [1] False "https://public.test/" "first"
    later <- fixtureInventory "1" [2, 3] False "https://public.test/" "later"
    second <- fixtureInventory "2" [1] False "https://public.test/" "second"
    incomplete <- fixtureInventory "4" [1] True "https://public.test/" "pending"
    initial <- expectRight (extendProjectRecord defaultLimits target first Nothing)
    record <- foldM (\held inventory -> expectRight (extendProjectRecord defaultLimits target inventory (Just held))) initial [later, second, incomplete]
    privateVersions <- traverse (\(version, numbers) -> fixtureInventory version numbers False "https://private.test/moved/" "stripped") [("1", [1, 2, 3]), ("2", [1]), ("3", [1]), ("4", [1])]
    let privateDocument = simpleDocument (recordEnvelope record) (concat [simpleFiles document | inventory <- privateVersions, PyPIMirrorRead document <- [inventoryReadData inventory]])
        private = Manifest (PackageInfo mirrorPackage (Map.fromList [(renderVersion (pkgVersion details), details) | inventory <- privateVersions, let details = inventoryDetails inventory]) Map.empty []) (packDocument privateDocument) 1024 (digestOf "private")
        expectedEntries = concat [simpleFiles document | (start, note, inventory) <- [(0, "first", first), (1, "later", later), (3, "second", second)], PyPIMirrorRead document <- [expectedSource start note inventory]]
        expectedSource start note inventory = mirrorSource (numberedArtifacts start (fmap privateLocation (pkgArtifacts (inventoryDetails inventory)))) note
        privateLocation artifact = artifact{artUrl = "https://private.test/moved/" <> artFilename artifact}
        numberedArtifacts start (artifact :| rest) = artifact{artEntryKey = ArrayEntry start} :| zipWith (\position item -> item{artEntryKey = ArrayEntry position}) [start + 1 ..] rest
        numbered start details = details{pkgArtifacts = numberedArtifacts start (pkgArtifacts details)}
        expectedVersions = Map.fromList [(renderVersion (pkgVersion details), numbered start details) | (start, inventory) <- zip [0, 3] (take 2 privateVersions), let details = inventoryDetails inventory]
        expected = Manifest (PackageInfo mirrorPackage expectedVersions Map.empty []) (fst pypiSimpleCached (simpleDocument (recordEnvelope record) expectedEntries)) 1024 (digestOf "expected")
    pure (record, private, expected)

packDocument :: SimpleDocument -> CachedDoc
packDocument document = fst pypiPacked (packedSimple (simpleEnvelope document) (docTable (smallArrayFromList [])) [(key, packTree urlHole value) | (key, value) <- simpleFiles document])

fixtureInventory :: Text -> [Int] -> Bool -> Text -> Text -> IO MirrorInventory
fixtureInventory version numbers companion location note = do
    base <- mirrorSample numbers companion location note
    let change position artifact =
            let filename = T.replace "1.0" (version <> ".0") (artFilename artifact)
             in artifact{artEntryKey = ArrayEntry (position * 3 + 5), artFilename = filename, artUrl = location <> filename}
        artifacts = case pkgArtifacts (inventoryDetails base) of first :| rest -> change 0 first :| zipWith change [1 ..] rest
        details = (inventoryDetails base){pkgVersion = pypiVersion version, pkgArtifacts = artifacts}
    expectRight (restoreMirrorInventory details (mirrorSource artifacts note))

field :: Key -> Value -> Maybe Value
field key = \case
    Object fields -> KeyMap.lookup key fields
    _ -> Nothing

info :: MirrorInventory -> PackageInfo
info inventory = PackageInfo mirrorPackage (Map.singleton (renderVersion mirrorVersion) (inventoryDetails inventory)) Map.empty []

raw :: MirrorInventory -> CachedDoc
raw inventory = case inventoryReadData inventory of
    PyPIMirrorRead document -> fst pypiSimpleCached document
    NpmMirrorRead fields -> fst npmCached (Object fields)

sampleRecord :: IO ProjectMirrorRecord
sampleRecord = mirrorSample [1, 2, 3] False "https://public.test/" "source" >>= \inventory -> expectRight (extendProjectRecord defaultLimits target inventory Nothing)

target :: StateTarget
target = stateTarget "https://private.test/"

revision :: StateRevision
revision = rev "one"

rev :: ByteString -> StateRevision
rev = StateRevision target mirrorPackage

viewWith :: (PackageName -> Maybe StateRevision -> IO (Either MirrorStateFault StateRead)) -> IO MirrorView
viewWith readState = do
    pool <- newLocalPool 20 10_000_000
    cache <- newMirrorRetention pool 60 (StoreBudget 0 0)
    let store = MirrorState (const readState) (\_ _ _ _ -> pure (Left (StateInvalid "unexpected write"))) (\_ _ -> pure (Left StateMissingCompanion)) (\_ _ -> pure (Left StateMissingCompanion))
    pure (newMirrorView defaultLimits target store cache)

runRead :: MirrorView -> IO (Either MirrorStateFault (Maybe (StateRevision, ProjectMirrorRecord)))
runRead view = withTicket (\ticket -> validateMirrorProject view ticket mirrorPackage)

withTicket :: (MemoryTicket -> IO a) -> IO a
withTicket action = do
    meter <- newMemoryMeter (MeterSettings maxBound 1024 1 1000)
    admitted <- withMemoryEntry noopMetricsPort meter action
    maybe (fail "test memory admission unexpectedly refused") pure admitted
