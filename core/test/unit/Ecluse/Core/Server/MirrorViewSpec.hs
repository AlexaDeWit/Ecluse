-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Managed reads validate fresh state and restore fields only after current private identity proof.
module Ecluse.Core.Server.MirrorViewSpec (spec) where

import Data.Aeson (Value (Object, String))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Core.Deadline (newDeadlines)
import Ecluse.Core.Mirror.State
import Ecluse.Core.Mirror.State.Internal (StateRevision (StateRevision))
import Ecluse.Core.Package
import Ecluse.Core.Package.Entry (EntryKey (ArrayEntry))
import Ecluse.Core.Registry.CachedDocument (CachedDoc, npmCached, pypiPacked, pypiSimpleCached)
import Ecluse.Core.Registry.Metadata (Manifest (..), VersionDoc (..))
import Ecluse.Core.Registry.MirrorUpload
import Ecluse.Core.Registry.PyPI.Document (simpleFiles)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Server.Admission.Meter (MemoryTicket, MeterSettings (..), newMemoryMeter, withMemoryEntry)
import Ecluse.Core.Server.MirrorView
import Ecluse.Core.Version (renderVersion)
import Ecluse.Test.Mirror.State
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

    describe "per-request project validation" $ do
        it "performs one state exchange on every read and accepts only the requested 304 revision" $ do
            record <- sampleRecord
            requests <- newIORef []
            let readState _ previous = do
                    modifyIORef' requests (<> [previous])
                    pure (Right (maybe (StateLoaded revision record) StateUnchanged previous))
            view <- mirrorViewWith target readState
            runRead view `shouldReturn` Right (Just (revision, record))
            runRead view `shouldReturn` Right (Just (revision, record))
            readIORef requests `shouldReturn` [Nothing, Just revision]

        it "owns a retained revision without changing its bytes" $ do
            record <- sampleRecord
            let original = BS.take 4 (BS.replicate 65536 97)
            view <- mirrorViewWith target (\_ _ -> pure (Right (StateLoaded (rev original) record)))
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
            view <- mirrorViewWith target (\_ _ -> readIORef answer)
            runRead view `shouldReturn` Right (Just (revision, record))
            writeIORef answer (Left (StateInvalid "outage"))
            runRead view `shouldReturn` Left (StateInvalid "outage")

        it "rejects an unsolicited 304 and a 304 for a different revision" $ do
            view <- mirrorViewWith target (\_ _ -> pure (Right (StateUnchanged revision)))
            runRead view `shouldReturn` Left StateRevisionMismatch
            record <- sampleRecord
            answer <- newIORef (Right (StateLoaded revision record))
            warmed <- mirrorViewWith target (\_ _ -> readIORef answer)
            void (runRead warmed)
            writeIORef answer (Right (StateUnchanged (rev "other")))
            runRead warmed `shouldReturn` Left StateRevisionMismatch

        it "does not cache absence and observes a concurrent extension on the next request" $ do
            record <- sampleRecord
            requests <- newIORef (0 :: Int)
            answer <- newIORef (Right StateAbsent)
            view <- mirrorViewWith target (\_ _ -> modifyIORef' requests (+ 1) >> readIORef answer)
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

runRead :: MirrorView -> IO (Either MirrorStateFault (Maybe (StateRevision, ProjectMirrorRecord)))
runRead view = withTicket (\ticket -> validateMirrorProject view ticket mirrorPackage)

withTicket :: (MemoryTicket -> IO a) -> IO a
withTicket action = do
    meter <- newDeadlines >>= \deadlines -> newMemoryMeter deadlines (MeterSettings maxBound 1024 1 1000)
    admitted <- withMemoryEntry noopMetricsPort meter action
    either (const (fail "test memory admission unexpectedly refused")) pure admitted
