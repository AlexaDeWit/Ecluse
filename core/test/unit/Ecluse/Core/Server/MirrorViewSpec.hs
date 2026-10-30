-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Managed reads validate fresh state and restore fields only after current private identity proof.
module Ecluse.Core.Server.MirrorViewSpec (spec) where

import Data.Aeson (Value (Array, Bool, Object, String), eitherDecodeStrict, encode)
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Lazy qualified as LBS
import Control.Monad (foldM)
import Data.Map.Strict qualified as Map
import Data.Traversable (for)
import Test.Hspec

import Ecluse.Core.Deadline (newDeadlines)
import Ecluse.Core.Ecosystem (Ecosystem (Cargo))
import Ecluse.Core.Mirror.State
import Ecluse.Core.Mirror.State.Codec (chargedDecodeProjectRecord)
import Ecluse.Core.Mirror.State.Internal (StateRevision (StateRevision), VerifiedCompanionBytes (VerifiedCompanionBytes), retainVerifiedIdentity)
import Ecluse.Core.Package
import Ecluse.Core.Package.Entry (EntryKey (ArrayEntry, RecordEntry))
import Ecluse.Core.Registry.CachedDocument (CachedDoc, cargoCached, pypiPacked, pypiSimpleCached, rubyGemsCached)
import Ecluse.Core.Registry.Cargo.Document (cargoDocument, cargoRecords)
import Ecluse.Core.Registry.Cargo.Mirror (cargoMirrorDocuments)
import Ecluse.Core.Registry.Metadata (Manifest (..), VersionDoc (..))
import Ecluse.Core.Registry.MirrorUpload
import Ecluse.Core.Registry.PyPI.Document (simpleFiles)
import Ecluse.Core.Registry.PyPI.Mirror (pypiMirrorDocuments)
import Ecluse.Core.Registry.RubyGems.Document (GemDocument (..), GemRow (..), selectGemVersion)
import Ecluse.Core.Registry.RubyGems.Mirror (rubyGemsMirrorDocuments)
import Ecluse.Core.Registry.RubyGems.Project (coordinateVersion, projectName)
import Ecluse.Core.Registry.RubyGems.Projection (projectGemRows)
import Ecluse.Core.Registry.RubyGems.Wire (decodeInfoRow)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Server.Admission.Meter (MemoryTicket, MeterSettings (..), newMemoryMeter, withMemoryEntry)
import Ecluse.Core.Server.MirrorView
import Ecluse.Core.Version (renderVersion)
import Ecluse.Test.Mirror.State
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Port (noopMetricsPort)
import Ecluse.Test.Registry.Cargo.Fixtures (CargoRelease (releaseArchive), cargoIndexRecord, minimalCargoRelease)
import Ecluse.Test.Registry.Cargo.Publish (cargoPublicationFixture, cargoSourceVersion)
import Ecluse.Test.Snapshot (digestOf)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = do
    describe "Cargo durable source restoration" $ do
        it "restores original yank, time and additive fields after durable reopen and target replacement" $ do
            (original, inventory) <- cargoInventory
            fmap cargoRecords (snd cargoCached (raw inventory)) `shouldBe` fmap cargoRecords (vdRaw original >>= snd cargoCached)
            record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
            bytes <- expectRight (encodeProjectRecord defaultLimits record)
            reopened <- chargedDecodeProjectRecord (const (pure ())) defaultLimits target (pkgName (vdDetails original)) bytes >>= expectRight
            current <- cargoTarget original
            restored <- expectRight (mirrorVersionView cargoMirrorDocuments (pkgVersion (vdDetails original)) reopened (Just current))
            fmap vdDetails restored `shouldBe` Just (vdDetails original){pkgArtifacts = pkgArtifacts (vdDetails current)}
            fmap (fmap cargoRecords . (snd cargoCached <=< vdRaw)) restored `shouldBe` Just (fmap cargoRecords (vdRaw original >>= snd cargoCached))
            let reopenedRevision = StateRevision target (pkgName (vdDetails original)) "reopened"
                details = vdDetails current
                manifest = Manifest (PackageInfo (pkgName details) (Map.singleton "1.0.0" details) mempty []) (fromMaybe (fst cargoCached (cargoDocument [])) (vdRaw current)) 0 (digestOf "target")
            (_, managed, incomplete) <- expectRight (mirrorProjectViews cargoMirrorDocuments reopenedRevision reopened (Just manifest))
            Map.null incomplete `shouldBe` True
            let rows = concat [cargoRecords source | value <- maybeToList managed, source <- maybeToList (snd cargoCached (manifestRaw value))]
            map fst rows `shouldBe` [RecordEntry 0]
            map snd rows `shouldBe` [value | source <- maybeToList (vdRaw original >>= snd cargoCached), (_, value) <- cargoRecords source]
        it "refuses absent, missing-digest and conflicting target identity" $ do
            (original, inventory) <- cargoInventory
            record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
            current <- cargoTarget original
            let details = vdDetails current
                noHash = current{vdDetails = details{pkgArtifacts = fmap (\artifact -> artifact{artHashes = []}) (pkgArtifacts details)}}
            mirrorVersionView cargoMirrorDocuments (pkgVersion details) record Nothing `shouldSatisfy` isLeft
            mirrorVersionView cargoMirrorDocuments (pkgVersion details) record (Just noHash) `shouldSatisfy` isLeft
            other <- expectRight (mkHash SHA256 ("a" <> toText (replicate 63 '0')))
            let conflicting = current{vdDetails = details{pkgArtifacts = fmap (\artifact -> artifact{artHashes = [other]}) (pkgArtifacts details)}}
            mirrorVersionView cargoMirrorDocuments (pkgVersion details) record (Just conflicting) `shouldSatisfy` isLeft
        it "rejects corrupt retained source facts and retains the first admitted row on re-admission" $ do
            (original, inventory) <- cargoInventory
            record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
            current <- cargoTarget original
            currentRaw <- maybe (fail "missing target row") pure (vdRaw current >>= snd cargoCached)
            admittedAgain <- expectRight (checkedMirrorInventory (pkgName (vdDetails current)) (pkgVersion (vdDetails current)) current (CargoMirrorRead currentRaw))
            extendProjectRecord defaultLimits target admittedAgain (Just record) `shouldBe` Right record
            encoded <- expectRight (encodeProjectRecord defaultLimits record)
            value <- expectRight (eitherDecodeStrict encoded)
            let corrupt = rewriteCargoSource value
            chargedDecodeProjectRecord (const (pure ())) defaultLimits target (pkgName (vdDetails original)) (LBS.toStrict (encode corrupt)) >>= (`shouldSatisfy` isLeft)
    describe "managed mirror source restoration" $ do
        it "retains every source entry across colliding fragment coordinates and changed native URLs" $ do
            originalInventory <- mirrorSample [1] False "https://public.test/" "first"
            later <- mirrorSample [2] False "https://public.test/" "second"
            initial <- expectRight (extendProjectRecord defaultLimits target originalInventory Nothing)
            record <- expectRight (extendProjectRecord defaultLimits target later (Just initial))
            targetInventory <- mirrorSample [1, 2] False "https://private.test/moved/" "stripped"
            let current = Just (VersionDoc (inventoryDetails targetInventory) Nothing)
            restored <- expectRight (mirrorVersionView pypiMirrorDocuments mirrorVersion record current)
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
            mirrorVersionView pypiMirrorDocuments mirrorVersion record (Just (VersionDoc details{pkgArtifacts = firstFile :| []} Nothing)) `shouldSatisfy` isLeft
            mirrorVersionView pypiMirrorDocuments mirrorVersion record (Just (VersionDoc details{pkgArtifacts = fmap (\artifact -> artifact{artHashes = []}) (pkgArtifacts details)} Nothing)) `shouldSatisfy` isLeft

        it "requires a declared companion to be retained even when every native wheel exists" $ do
            inventory <- mirrorSample [1] True "https://public.test/" "source"
            record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
            mirrorVersionView pypiMirrorDocuments mirrorVersion record (Just (VersionDoc (inventoryDetails inventory) Nothing)) `shouldSatisfy` isLeft

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
            (_, initialManifest, incompleteVersions) <- expectRight (mirrorProjectViews pypiMirrorDocuments revision record (Just private))
            (_, updatedManifest, _) <- expectRight (mirrorProjectViews pypiMirrorDocuments (rev "new") record (Just private))
            incompleteVersions `shouldSatisfy` Map.null
            (initialManifest >>= snd pypiPacked . manifestRaw) `shouldSatisfy` isJust
            fmap manifestDigest initialManifest `shouldNotBe` fmap manifestDigest updatedManifest

    describe "managed mirror document order" $ do
        it "orders restored Cargo versions by original index position" $ do
            let name = mkPackageName Cargo Nothing "thing"
                release = minimalCargoRelease "thing"
            record <- cargoMirrorRecord target name (releaseArchive (release "1.9.0")) [(0, cargoIndexRecord (release "1.9.0")), (1, cargoIndexRecord (release "1.10.0"))]
            (_, managed, incomplete) <- expectRight (mirrorProjectViews cargoMirrorDocuments (StateRevision target name "ordered") record (Just (retainedManifest record (fst cargoCached (cargoDocument [])))))
            incomplete `shouldSatisfy` Map.null
            let rows = concat [cargoRecords document | value <- maybeToList managed, document <- maybeToList (snd cargoCached (manifestRaw value))]
            map fst rows `shouldBe` [RecordEntry 0, RecordEntry 1]
            map (field "vers" . snd) rows `shouldBe` [Just (String "1.9.0"), Just (String "1.10.0")]

        it "orders restored RubyGems versions by version text" $ do
            (record, rows) <- rubyGemsRecord
            (_, managed, incomplete) <- expectRight (mirrorProjectViews rubyGemsMirrorDocuments (StateRevision target (recordPackage record) "ordered") record (Just (retainedManifest record (fst rubyGemsCached (GemDocument (recordPackage record) "---\n" [])))))
            incomplete `shouldSatisfy` Map.null
            let restored = [row | value <- maybeToList managed, document <- maybeToList (snd rubyGemsCached (manifestRaw value)), row <- gemRows document]
            map (renderVersion . coordinateVersion . grCoordinate) restored `shouldBe` ["10.0.0", "9.0.0"]
            map grEntryKey restored `shouldBe` [RecordEntry 0, RecordEntry 1]
            map grSource restored `shouldBe` map grSource (reverse rows)

    describe "per-request project validation" $ do
        it "performs one state exchange on every read and accepts only the requested 304 revision" $ do
            record <- sampleRecord
            requests <- newIORef []
            let readState _ previous = do
                    modifyIORef' requests (<> [previous])
                    pure (Right (maybe (StateLoaded revision record) StateUnchanged previous))
            view <- mirrorViewWith pypiMirrorDocuments target readState
            runRead view `shouldReturn` Right (Just (revision, record))
            runRead view `shouldReturn` Right (Just (revision, record))
            readIORef requests `shouldReturn` [Nothing, Just revision]

        it "owns a retained revision without changing its bytes" $ do
            record <- sampleRecord
            let original = BS.take 4 (BS.replicate 65536 97)
            view <- mirrorViewWith pypiMirrorDocuments target (\_ _ -> pure (Right (StateLoaded (rev original) record)))
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
            view <- mirrorViewWith pypiMirrorDocuments target (\_ _ -> readIORef answer)
            runRead view `shouldReturn` Right (Just (revision, record))
            writeIORef answer (Left (StateInvalid "outage"))
            runRead view `shouldReturn` Left (StateInvalid "outage")

        it "rejects an unsolicited 304 and a 304 for a different revision" $ do
            view <- mirrorViewWith pypiMirrorDocuments target (\_ _ -> pure (Right (StateUnchanged revision)))
            runRead view `shouldReturn` Left StateRevisionMismatch
            record <- sampleRecord
            answer <- newIORef (Right (StateLoaded revision record))
            warmed <- mirrorViewWith pypiMirrorDocuments target (\_ _ -> readIORef answer)
            void (runRead warmed)
            writeIORef answer (Right (StateUnchanged (rev "other")))
            runRead warmed `shouldReturn` Left StateRevisionMismatch

        it "does not cache absence and observes a concurrent extension on the next request" $ do
            record <- sampleRecord
            requests <- newIORef (0 :: Int)
            answer <- newIORef (Right StateAbsent)
            view <- mirrorViewWith pypiMirrorDocuments target (\_ _ -> modifyIORef' requests (+ 1) >> readIORef answer)
            runRead view `shouldReturn` Right Nothing
            writeIORef answer (Right (StateLoaded revision record))
            runRead view `shouldReturn` Right (Just (revision, record))
            added <- mirrorSample [4] False "https://public.test/" "added"
            extended <- expectRight (extendProjectRecord defaultLimits target added (Just record))
            writeIORef answer (Right (StateLoaded (rev "extended") extended))
            runRead view `shouldReturn` Right (Just (rev "extended", extended))
            readIORef requests `shouldReturn` 3

field :: Key -> Value -> Maybe Value
field key = \case
    Object fields -> KeyMap.lookup key fields
    _ -> Nothing

info :: MirrorInventory -> PackageInfo
info inventory = PackageInfo mirrorPackage (Map.singleton (renderVersion mirrorVersion) (inventoryDetails inventory)) Map.empty []

raw :: MirrorInventory -> CachedDoc
raw = mirrorReadDocument . inventoryReadData

sampleRecord :: IO ProjectMirrorRecord
sampleRecord = mirrorSample [1, 2, 3] False "https://public.test/" "source" >>= \inventory -> expectRight (extendProjectRecord defaultLimits target inventory Nothing)

target :: StateTarget
target = stateTarget "https://private.test/"

revision :: StateRevision
revision = rev "one"

rev :: ByteString -> StateRevision
rev = StateRevision target mirrorPackage

runRead :: MirrorView -> IO (Either MirrorStateFault (Maybe (StateRevision, ProjectMirrorRecord)))
runRead view = withTicket (\ticket -> validateMirrorProject view ticket mirrorPackage)

withTicket :: (MemoryTicket -> IO a) -> IO a
withTicket action = do
    meter <- newDeadlines >>= \deadlines -> newMemoryMeter deadlines (MeterSettings maxBound 1024 1 1000)
    admitted <- withMemoryEntry noopMetricsPort meter action
    either (const (fail "test memory admission unexpectedly refused")) pure admitted

-- The target holds exactly the retained files, so every recorded version is complete.
retainedManifest :: ProjectMirrorRecord -> CachedDoc -> Manifest
retainedManifest record listing = Manifest (PackageInfo (recordPackage record) (fmap retained (recordVersions record)) Map.empty []) listing 0 (digestOf "target")
  where
    retained version = let fragment :| _ = versionFragments version in fragmentDetails fragment

-- Two complete versions whose original rows put 9.0.0 before 10.0.0.
rubyGemsRecord :: IO (ProjectMirrorRecord, [GemRow])
rubyGemsRecord = do
    name <- expectRight (projectName "ordered")
    rows <- for (zip [0 ..] ["9.0.0", "10.0.0"]) $ \(position, version) ->
        expectRight (decodeInfoRow name (RecordEntry position) (encodeUtf8 (version <> " |checksum:" <> hexSha256Of (gemArchive version) <> ",ruby:>= 3.2,rubygems:>= 3.4,created_at:2020-01-0" <> show (position + 1 :: Int) <> "T00:00:00Z")))
    let source = GemDocument name "---\n" rows
    projected <- expectRight (projectGemRows defaultLimits name "https://public.test" rows)
    record <- foldM (retainGemVersion source) Nothing (Map.elems (infoVersions projected))
    maybe (fail "no RubyGems versions retained") (\complete -> pure (complete, rows)) record
  where
    gemArchive version = encodeUtf8 ("gem archive " <> version)
    retainGemVersion source previous details = do
        selected <- maybe (fail "missing RubyGems version rows") pure (selectGemVersion (pkgVersion details) source)
        inventory <- expectRight (checkedMirrorInventory (pkgName details) (pkgVersion details) (VersionDoc details (Just (fst rubyGemsCached selected))) (RubyGemsMirrorRead selected))
        expected <- expectRight (extendProjectRecord defaultLimits target inventory previous)
        Just <$> foldM (completeGemFile inventory) expected (inventoryFiles inventory)
    completeGemFile inventory record file = do
        verified <- expectRight (verifyMirrorBytes file (gemArchive (renderVersion (inventoryVersion inventory))))
        withIdentity <- expectRight (retainVerifiedIdentity (inventoryVersion inventory) verified record)
        checked@(VerifiedCompanionBytes _ reference _) <- expectRight (verifySourceCompanionBytes defaultLimits (inventoryReadData inventory) file "compact")
        expectRight (retainCompanionReference (inventoryVersion inventory) checked reference withIdentity)

cargoInventory :: IO (VersionDoc, MirrorInventory)
cargoInventory = do
    release <- cargoPublicationFixture
    let row = case cargoIndexRecord release of
            Object fields -> Object (KeyMap.insert "yanked" (Bool True) fields)
            other -> other
    doc <- cargoSourceVersion row
    source <- maybe (fail "missing Cargo source") pure (vdRaw doc >>= snd cargoCached)
    inventory <- expectRight (checkedMirrorInventory (pkgName (vdDetails doc)) (pkgVersion (vdDetails doc)) doc (CargoMirrorRead source))
    pure (doc, inventory)

cargoTarget :: VersionDoc -> IO VersionDoc
cargoTarget original = do
    source <- maybe (fail "missing Cargo source") pure (vdRaw original >>= snd cargoCached)
    row <- maybe (fail "missing Cargo row") (pure . snd) (listToMaybe (cargoRecords source))
    let changed = case row of
            Object fields -> Object (KeyMap.delete "ecluse-fixture" (KeyMap.insert "pubtime" (String "2026-10-25T00:00:00Z") (KeyMap.insert "yanked" (Bool False) fields)))
            other -> other
    doc <- cargoSourceVersion changed
    pure doc{vdDetails = (vdDetails doc){pkgArtifacts = fmap (\artifact -> artifact{artUrl = "https://target.test/current/archive"}) (pkgArtifacts (vdDetails doc))}}

rewriteCargoSource :: Value -> Value
rewriteCargoSource = \case
    Object fields
        | KeyMap.lookup "kind" fields == Just (String "cargo") -> Object (KeyMap.insert "sourceIndex" (String "https://unapproved.test/index") fields)
        | otherwise -> Object (fmap rewriteCargoSource fields)
    Array values -> Array (fmap rewriteCargoSource values)
    value -> value
