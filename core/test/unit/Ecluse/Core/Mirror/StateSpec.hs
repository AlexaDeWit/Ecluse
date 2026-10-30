-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Interrupted copies retain every admitted identity and source fragment across conditional updates.
module Ecluse.Core.Mirror.StateSpec (spec) where

import Control.Monad (foldM)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Mirror.State
import Ecluse.Core.Mirror.State.Codec (chargedDecodeProjectRecord, chargedEncodeProjectRecord)
import Ecluse.Core.Mirror.State.Internal (CompanionRef (..), StateRevision (..), VerifiedCompanionBytes (..), retainVerifiedIdentity)
import Ecluse.Core.Package
import Ecluse.Core.Package.Entry (EntryKey (ArrayEntry))
import Ecluse.Core.Registry.MirrorUpload
import Ecluse.Core.Registry.MirrorUpload.Internal (restoreMirrorInventory)
import Ecluse.Core.Registry.PyPI.Document (simpleEnvelope, simpleFiles)
import Ecluse.Core.Registry.PyPI.Mirror (pypiStateFormat)
import Ecluse.Core.Security (Limits (..), defaultLimits)
import Ecluse.Core.Server.Admission.Meter (newChargeRegion)
import Ecluse.Core.Server.MemoryModel (bufferedChunkCharge, decodedDocumentCharge)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Mirror.State (mirrorFilename, mirrorPackage, mirrorSample, mirrorSource, mirrorVersion, withMirrorEnvelope)
import Ecluse.Test.Package (hexSha256Of, hexSha512Of)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "durable mirror expectations" $ do
    it "charges repeated same-size state rewrites once and grows with the largest live encoding" $ do
        inventory <- mirrorSample [1] False "https://first.test/" "original"
        added <- mirrorSample [1, 2] False "https://first.test/" "original"
        small <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target inventory Nothing)
        large <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target added (Just small))
        paid <- newIORef (0 :: Int)
        charge <- newChargeRegion (\cost -> modifyIORef' paid (+ cost))
        replicateM_ 3 (chargedEncodeProjectRecord pypiStateFormat charge defaultLimits small >>= expectRight)
        smallPaid <- readIORef paid
        void (chargedEncodeProjectRecord pypiStateFormat charge defaultLimits large >>= expectRight)
        largePaid <- readIORef paid
        largePaid `shouldSatisfy` (> smallPaid)
        freshPaid <- newIORef (0 :: Int)
        freshCharge <- newChargeRegion (\cost -> modifyIORef' freshPaid (+ cost))
        void (chargedEncodeProjectRecord pypiStateFormat freshCharge defaultLimits large >>= expectRight)
        readIORef freshPaid `shouldReturn` largePaid

    it "bases decode charges on the actual body rather than the configured maximum" $ do
        inventory <- mirrorSample [1] False "https://first.test/" "original"
        record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target inventory Nothing)
        bytes <- expectRight (encodeProjectRecord pypiStateFormat defaultLimits record)
        payments <- newIORef ([] :: [Int])
        chargedDecodeProjectRecord pypiStateFormat (\cost -> modifyIORef' payments (cost :)) defaultLimits target mirrorPackage bytes `shouldReturn` Right record
        observed <- readIORef payments
        observed `shouldSatisfy` all (< maxMetadataBytes defaultLimits)
        observed `shouldSatisfy` all (>= bufferedChunkCharge (BS.length bytes) + decodedDocumentCharge bytes)
        projectRecordWeight pypiStateFormat defaultLimits{maxNestingDepth = 1} record `shouldSatisfy` isRight

    it "prepays typed restoration separately when long version keys outweigh the JSON tree model" $ do
        original <- mirrorSample [1] False "https://first.test/" "original"
        let versionText = T.intercalate "." (replicate 512 "1")
            details = inventoryDetails original
            artifacts = fmap (\artifact -> artifact{artFilename = T.replace "1.0" versionText (artFilename artifact), artUrl = T.replace "1.0" versionText (artUrl artifact)}) (pkgArtifacts details)
        inventory <- expectRight (restoreMirrorInventory details{pkgVersion = mkVersion PyPI versionText, pkgArtifacts = artifacts} (mirrorSource artifacts "original"))
        record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target inventory Nothing)
        bytes <- expectRight (encodeProjectRecord pypiStateFormat defaultLimits record)
        retained <- expectRight (projectRecordWeight pypiStateFormat defaultLimits record)
        payments <- newIORef ([] :: [Int])
        chargedDecodeProjectRecord pypiStateFormat (\cost -> modifyIORef' payments (cost :)) defaultLimits target mirrorPackage bytes `shouldReturn` Right record
        readIORef payments >>= \case
            [domainPaid, treePaid] -> do
                treePaid `shouldSatisfy` (< retained)
                domainPaid `shouldSatisfy` (> retained)
            other -> expectationFailure ("expected prepaid tree and domain phases, got " <> show other)

    it "preserves old files when a later admitted source removes them" $ do
        a <- mirrorSample [1, 2] False "https://first.test/" "original"
        b <- mirrorSample [2, 3] False "https://later.test/" "changed"
        old <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target a Nothing)
        updated <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target b (Just old))
        version <- getVersion updated
        Map.keys (versionFiles version) `shouldBe` map mirrorFilename [1, 2, 3]
        length (versionFragments version) `shouldBe` 2
        let fields = [value | fragment <- toList (versionFragments version), PyPIMirrorRead document <- [fragmentReadData fragment], (_, value) <- simpleFiles document]
        fields `shouldSatisfy` any (hasNote "original")
        fields `shouldSatisfy` any (hasNote "changed")
        sum [length (pkgArtifacts (fragmentDetails fragment)) | fragment <- toList (versionFragments version)] `shouldBe` 3

    it "keeps equal ArrayEntry coordinates in separate admitted fragments" $ do
        a <- mirrorSample [1] False "https://first.test/" "original"
        b <- mirrorSample [2] False "https://later.test/" "added"
        old <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target a Nothing)
        updated <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target b (Just old))
        version <- getVersion updated
        map (map artEntryKey . toList . pkgArtifacts . fragmentDetails) (toList (versionFragments version)) `shouldBe` [[ArrayEntry 0], [ArrayEntry 0]]
        encoded <- expectRight (encodeProjectRecord pypiStateFormat defaultLimits updated)
        chargedDecodeProjectRecord pypiStateFormat (const (pure ())) defaultLimits target mirrorPackage encoded `shouldReturn` Right updated

    it "does not replace original fields when only source URLs change" $ do
        a <- mirrorSample [1] False "https://first.test/" "original"
        b <- mirrorSample [1] False "https://later.test/" "withdrawn"
        old <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target a Nothing)
        extendProjectRecord pypiStateFormat defaultLimits target b (Just old) `shouldBe` Right old

    it "refuses a conflicting hash without mutating the old record" $ do
        a <- mirrorSample [1] False "https://first.test/" "original"
        let details = inventoryDetails a
        otherHash <- expectRight (mkHash SHA256 (T.replicate 64 "b"))
        let changed = fmap (\artifact -> artifact{artHashes = [otherHash]}) (pkgArtifacts details)
            source = mirrorSource changed "changed"
        b <- expectRight (restoreMirrorInventory details{pkgArtifacts = changed} source)
        old <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target a Nothing)
        extendProjectRecord pypiStateFormat defaultLimits target b (Just old) `shouldBe` Left (StateFileConflict (mirrorFilename 1))

    it "retains one source entry per filename through repeated incremental admissions" $ do
        inventories <- traverse (\n -> mirrorSample [1 .. n] False "https://first.test/" "original") [1 .. 20]
        records <-
            foldM
                ( \records inventory -> do
                    record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target inventory (viaNonEmpty NE.head records))
                    pure (record : records)
                )
                []
                inventories
        sizes <- traverse (fmap BS.length . expectRight . encodeProjectRecord pypiStateFormat defaultLimits) records
        case nonEmpty sizes of
            Nothing -> expectationFailure "missing incremental records"
            Just (value :| values) -> foldl' max value values `shouldSatisfy` (<= 25 * foldl' min value values)
        for_ (viaNonEmpty NE.head records) $ \record -> do
            version <- getVersion record
            sum [length (pkgArtifacts (fragmentDetails fragment)) | fragment <- toList (versionFragments version)] `shouldBe` 20

    it "refuses a project or target identity mismatch" $ do
        inventory <- mirrorSample [1] False "https://first.test/" "original"
        record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target inventory Nothing)
        bytes <- expectRight (encodeProjectRecord pypiStateFormat defaultLimits record)
        chargedDecodeProjectRecord pypiStateFormat (const (pure ())) defaultLimits (stateTarget "https://other.test") mirrorPackage bytes `shouldReturn` Left StateIdentityMismatch
        chargedDecodeProjectRecord pypiStateFormat (const (pure ())) defaultLimits target (mkPackageName PyPI Nothing "other") bytes `shouldReturn` Left StateIdentityMismatch

    it "requires all current target identities and rejects digestless target presence" $ do
        inventory <- mirrorSample [1, 2] False "https://first.test/" "original"
        record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target inventory Nothing)
        version <- getVersion record
        let files = map expectedArtifact (toList (inventoryFiles inventory))
        checkMirrorCompletion version files `shouldBe` MirrorComplete
        checkMirrorCompletion version (take 1 files) `shouldBe` MirrorIncomplete (MissingMirrorFile (mirrorFilename 2) :| [])
        checkMirrorCompletion version (map (\file -> file{artHashes = []}) files) `shouldBe` MirrorIncomplete (MissingMirrorProof (mirrorFilename 1) :| [MissingMirrorProof (mirrorFilename 2)])
        checkMirrorCompletion version (map (\file -> file{artUrl = "https://private.test/file"}) files) `shouldBe` MirrorComplete

    it "does not let a matching target entry hide a conflicting or unproved duplicate" $ do
        inventory <- mirrorSample [1] False "https://first.test/" "original"
        record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target inventory Nothing)
        version <- getVersion record
        wrong <- expectRight (mkHash SHA256 (T.replicate 64 "b"))
        let file :| _ = inventoryFiles inventory
            matching = expectedArtifact file
        checkMirrorCompletion version [matching, matching{artHashes = [wrong]}]
            `shouldBe` MirrorIncomplete (ConflictingMirrorFile (mirrorFilename 1) :| [])
        checkMirrorCompletion version [matching, matching{artHashes = []}]
            `shouldBe` MirrorIncomplete (MissingMirrorProof (mirrorFilename 1) :| [])

    it "proves target SHA256 from bytes verified against source SHA512" $ do
        original <- mirrorSample [1] False "https://first.test/" "original"
        sha512 <- expectRight (mkHash SHA512 (hexSha512Of "wheel"))
        sha256 <- expectRight (mkHash SHA256 (hexSha256Of "wheel"))
        let details = inventoryDetails original
            files = fmap (\artifact -> artifact{artHashes = [sha512]}) (pkgArtifacts details)
        inventory <- expectRight (restoreMirrorInventory details{pkgArtifacts = files} (mirrorSource files "original"))
        let file :| _ = inventoryFiles inventory
            targetFile = (expectedArtifact file){artHashes = [sha256], artUrl = "https://private.test/file"}
        record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target inventory Nothing)
        unverifiedVersion <- getVersion record
        checkMirrorCompletion unverifiedVersion [targetFile] `shouldBe` MirrorIncomplete (MissingMirrorProof (mirrorFilename 1) :| [])
        verified <- expectRight (verifyMirrorBytes file "wheel")
        retainVerifiedIdentity (mkVersion PyPI "2") verified record `shouldBe` Left (StateInvalid "missing expected version")
        anotherInventory <- mirrorSample [2] False "https://first.test/" "other"
        anotherRecord <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target anotherInventory Nothing)
        retainVerifiedIdentity mirrorVersion verified anotherRecord `shouldBe` Left (StateFileConflict (mirrorFilename 1))
        updated <- expectRight (retainVerifiedIdentity mirrorVersion verified record)
        retainVerifiedIdentity mirrorVersion verified updated `shouldBe` Right updated
        verifiedVersion <- getVersion updated
        checkMirrorCompletion verifiedVersion [targetFile] `shouldBe` MirrorComplete
        checkMirrorCompletion verifiedVersion [targetFile{artHashes = [sha512]}] `shouldBe` MirrorComplete
        map (artHashes . expectedArtifact) (Map.elems (versionFiles verifiedVersion)) `shouldBe` [[sha512]]
        wrong <- expectRight (mkHash SHA256 (hexSha256Of "other"))
        conflictingSource <- expectRight (mkHash SHA512 (hexSha512Of "other"))
        checkMirrorCompletion verifiedVersion [targetFile{artHashes = [sha256, conflictingSource]}] `shouldBe` MirrorIncomplete (ConflictingMirrorFile (mirrorFilename 1) :| [])
        checkMirrorCompletion verifiedVersion [targetFile{artHashes = [wrong]}] `shouldBe` MirrorIncomplete (ConflictingMirrorFile (mirrorFilename 1) :| [])
        recordEnvelope updated `shouldBe` recordEnvelope record
        encoded <- expectRight (encodeProjectRecord pypiStateFormat defaultLimits updated)
        chargedDecodeProjectRecord pypiStateFormat (const (pure ())) defaultLimits target mirrorPackage encoded `shouldReturn` Right updated

    it "updates one project envelope across versions without replacing earlier file fields" $ do
        let firstEnvelope = KeyMap.singleton "project-status" (object ["status" .= ("active" :: Text)])
            nextEnvelope = KeyMap.singleton "project-status" (object ["status" .= ("archived" :: Text)])
        initialInventory <- mirrorSample [1] False "https://first.test/" "original" >>= withMirrorEnvelope firstEnvelope
        let details = inventoryDetails initialInventory
            newVersion = mkVersion PyPI "2"
            renamed = fmap (\artifact -> artifact{artFilename = T.replace "1.0" "2.0" (artFilename artifact), artUrl = T.replace "1.0" "2.0" (artUrl artifact)}) (pkgArtifacts details)
        nextInventory <- expectRight (restoreMirrorInventory details{pkgVersion = newVersion, pkgArtifacts = renamed} (mirrorSource renamed "second")) >>= withMirrorEnvelope nextEnvelope
        old <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target initialInventory Nothing)
        updated <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target nextInventory (Just old))
        recordEnvelope old `shouldBe` firstEnvelope
        recordEnvelope updated `shouldBe` nextEnvelope
        for_ (Map.elems (recordVersions updated)) $ \version ->
            for_ (versionFragments version) $ \fragment -> case fragmentReadData fragment of
                PyPIMirrorRead document -> simpleEnvelope document `shouldBe` mempty
                NpmMirrorRead _ -> expectationFailure "unexpected npm fragment"
                CargoMirrorRead _ -> expectationFailure "unexpected Cargo fragment"
                RubyGemsMirrorRead _ -> expectationFailure "unexpected RubyGems fragment"
        encoded <- expectRight (encodeProjectRecord pypiStateFormat defaultLimits updated)
        chargedDecodeProjectRecord pypiStateFormat (const (pure ())) defaultLimits target mirrorPackage encoded `shouldReturn` Right updated
        Map.keys (recordVersions updated) `shouldBe` ["1", "2"]
        recordVersion mirrorVersion updated `shouldBe` recordVersion mirrorVersion old

    it "blocks completion while a declared companion remains pending" $ do
        inventory <- mirrorSample [1] True "https://first.test/" "original"
        record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target inventory Nothing)
        version <- getVersion record
        checkMirrorCompletion version (map expectedArtifact (toList (inventoryFiles inventory))) `shouldBe` MirrorIncomplete (PendingMirrorCompanion (mirrorFilename 1) :| [])
        Map.lookup (mirrorFilename 1) (versionCompanions version) `shouldBe` Just CompanionPending

    it "writes companion bytes before referencing them and verifies retained content" $ do
        inventory <- mirrorSample [1] True "https://first.test/" "original"
        record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target inventory Nothing)
        let file :| _ = inventoryFiles inventory
        verified@(VerifiedCompanionBytes _ reference _) <- expectRight (verifyCompanionBytes defaultLimits file "metadata")
        verifyRetainedCompanion reference "metadata" `shouldBe` Right "metadata"
        verifyRetainedCompanion reference "corrupt" `shouldSatisfy` isLeft
        retained <- newIORef False
        held <- newIORef record
        let revision = StateRevision target mirrorPackage "one"
            store =
                inertState
                    { msReadProject = \_ _ _ -> Right . StateLoaded revision <$> readIORef held
                    , msRetainCompanion = \_ _ -> writeIORef retained True $> Right reference
                    , msWriteProject = \_ _ _ updated -> do
                        readIORef retained `shouldReturn` True
                        writeIORef held updated
                        pure (Right (StateStored revision))
                    }
        (_, updated) <- retainProjectCompanion defaultLimits (const pass) store mirrorPackage mirrorVersion verified >>= expectRight
        recordEnvelope updated `shouldBe` recordEnvelope record
        version <- getVersion updated
        checkMirrorCompletion version [expectedArtifact file] `shouldBe` MirrorComplete
        bytes <- expectRight (encodeProjectRecord pypiStateFormat defaultLimits updated)
        chargedDecodeProjectRecord pypiStateFormat (const (pure ())) defaultLimits target mirrorPackage bytes `shouldReturn` Right updated

    it "refuses undeclared and oversized companion retention" $ do
        inventory <- mirrorSample [1] False "https://first.test/" "original"
        let file :| _ = inventoryFiles inventory
        verifyCompanionBytes defaultLimits file "metadata" `shouldSatisfy` isLeft
        declared <- mirrorSample [1] True "https://first.test/" "original"
        let declaredFile :| _ = inventoryFiles declared
        verifyCompanionBytes defaultLimits{maxMetadataBytes = 2} declaredFile "metadata" `shouldSatisfy` isLeft

    it "checks dimensions before retaining the source tree" $ do
        inventory <- mirrorSample [1, 2] False "https://first.test/" "original"
        record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
        bytes <- expectRight (encodeProjectRecord defaultLimits record)
        chargedDecodeProjectRecord (const (pure ())) defaultLimits{mirrorStateFileCeiling = 1} target mirrorPackage bytes >>= (`shouldSatisfy` isLeft)
        chargedDecodeProjectRecord (const (pure ())) defaultLimits{maxVersionCount = 0} target mirrorPackage bytes >>= (`shouldSatisfy` isLeft)

    it "round trips every admission fact and original source field" $ do
        inventory <- mirrorSample [1, 2] True "https://first.test/" "original"
        record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target inventory Nothing)
        bytes <- expectRight (encodeProjectRecord pypiStateFormat defaultLimits record)
        chargedDecodeProjectRecord pypiStateFormat (const (pure ())) defaultLimits target mirrorPackage bytes `shouldReturn` Right record

    it "rejects unsupported schema, malformed JSON, and excessive nesting" $ do
        chargedDecodeProjectRecord pypiStateFormat (const (pure ())) defaultLimits target mirrorPackage "{\"schema\":999}" >>= (`shouldSatisfy` isLeft)
        chargedDecodeProjectRecord pypiStateFormat (const (pure ())) defaultLimits target mirrorPackage "{" >>= (`shouldSatisfy` isLeft)
        chargedDecodeProjectRecord pypiStateFormat (const (pure ())) defaultLimits{maxNestingDepth = 2} target mirrorPackage "{\"a\":{\"b\":{}}}" >>= (`shouldSatisfy` isLeft)

    it "rejects trailing non-JSON data" $
        chargedDecodeProjectRecord pypiStateFormat (const (pure ())) defaultLimits target mirrorPackage "{}trailing" >>= (`shouldSatisfy` isLeft)

    it "rejects a response above its byte cap before record decoding" $
        chargedDecodeProjectRecord pypiStateFormat (const (pure ())) defaultLimits{maxMetadataBytes = 2} target mirrorPackage "{broken" >>= (`shouldSatisfy` isLeft)

    it "reconciles an ambiguous successful write by rereading its union" $ do
        inventory <- mirrorSample [1] False "https://first.test/" "original"
        held <- newIORef Nothing
        writes <- newIORef (0 :: Int)
        let revision = StateRevision target mirrorPackage "one"
            store =
                inertState
                    { msReadProject = \_ _ _ -> readIORef held <&> Right . maybe StateAbsent (StateLoaded revision)
                    , msWriteProject = \_ _ _ record -> do
                        modifyIORef' writes (+ 1)
                        writeIORef held (Just record)
                        pure (Left (StateInvalid "lost reply"))
                    }
        result <- recordExpectedInventory pypiStateFormat defaultLimits (const pass) target store inventory >>= expectRight
        fst result `shouldBe` revision
        readIORef writes `shouldReturn` 1

    it "rereads after a collision and commits its envelope after the competing admission" $ do
        let committed = KeyMap.singleton "project-status" (object ["status" .= ("active" :: Text)])
            competing = KeyMap.singleton "project-status" (object ["status" .= ("archived" :: Text)])
        a <- mirrorSample [1] False "https://first.test/" "original" >>= withMirrorEnvelope committed
        b <- mirrorSample [2] False "https://later.test/" "new" >>= withMirrorEnvelope competing
        other <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target b Nothing)
        held <- newIORef Nothing
        collision <- newIORef True
        let revision = StateRevision target mirrorPackage "one"
            store =
                inertState
                    { msReadProject = \_ _ _ -> readIORef held <&> Right . maybe StateAbsent (StateLoaded revision)
                    , msWriteProject = \_ _ _ record ->
                        readIORef collision >>= \firstWrite ->
                            if firstWrite
                                then do
                                    writeIORef collision False
                                    writeIORef held (Just other)
                                    pure (Right StateConflict)
                                else writeIORef held (Just record) $> Right (StateStored revision)
                    }
        (_, record) <- recordExpectedInventory pypiStateFormat defaultLimits (const pass) target store a >>= expectRight
        recordEnvelope record `shouldBe` committed
        version <- getVersion record
        Map.keys (versionFiles version) `shouldBe` map mirrorFilename [1, 2]

    it "refuses a retained reference for a different version, file or declaration" $ do
        inventory <- mirrorSample [1] True "https://first.test/" "original"
        otherInventory <- mirrorSample [2] True "https://first.test/" "original"
        changedInventory <- mirrorSample [1] True "https://other.test/" "original"
        record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target inventory Nothing)
        let file :| _ = inventoryFiles inventory
            otherFile :| _ = inventoryFiles otherInventory
            changedFile :| _ = inventoryFiles changedInventory
        verified@(VerifiedCompanionBytes _ reference _) <- expectRight (verifyCompanionBytes defaultLimits file "metadata")
        other@(VerifiedCompanionBytes _ otherReference _) <- expectRight (verifyCompanionBytes defaultLimits otherFile "metadata")
        changed@(VerifiedCompanionBytes _ changedReference _) <- expectRight (verifyCompanionBytes defaultLimits changedFile "metadata")
        retainCompanionReference (mkVersion PyPI "2") verified reference record `shouldBe` Left StateMissingCompanion
        retainCompanionReference mirrorVersion other otherReference record `shouldBe` Left StateMissingCompanion
        retainCompanionReference mirrorVersion changed changedReference record `shouldBe` Left (StateCompanionConflict (mirrorFilename 1))
        retained <- expectRight (retainCompanionReference mirrorVersion verified reference record)
        retainCompanionReference mirrorVersion verified reference retained `shouldBe` Right retained
        replacement@(VerifiedCompanionBytes _ replacementReference _) <- expectRight (verifyCompanionBytes defaultLimits file "different")
        retainCompanionReference mirrorVersion verified replacementReference retained `shouldBe` Left (StateCompanionConflict (mirrorFilename 1))
        retainCompanionReference mirrorVersion replacement replacementReference retained `shouldBe` Left (StateCompanionConflict (mirrorFilename 1))
        version <- getVersion retained
        Map.lookup (mirrorFilename 1) (versionCompanions version) `shouldBe` Just (CompanionRetained reference)
        checkMirrorCompletion version [expectedArtifact file] `shouldBe` MirrorComplete

    it "verifies supported companion digests and retains unknown declarations without inventing a digest rule" $ do
        original <- mirrorSample [1] True "https://first.test/" "original"
        let details = inventoryDetails original
        for_ [Map.singleton "sha256" (hexSha256Of "metadata"), Map.singleton "future-hash" "opaque"] $ \digests -> do
            let artifacts = fmap (\artifact -> artifact{artCoreMetadata = Just (MetadataDigests digests)}) (pkgArtifacts details)
            inventory <- expectRight (restoreMirrorInventory details{pkgArtifacts = artifacts} (mirrorSource artifacts "original"))
            let file :| _ = inventoryFiles inventory
            verified@(VerifiedCompanionBytes _ reference _) <- expectRight (verifyCompanionBytes defaultLimits file "metadata")
            companionBody verified `shouldBe` "metadata"
            companionDigest reference `shouldBe` hexSha256Of "metadata"
            record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target inventory Nothing)
            retained <- expectRight (retainCompanionReference mirrorVersion verified reference record)
            encoded <- expectRight (encodeProjectRecord pypiStateFormat defaultLimits retained)
            chargedDecodeProjectRecord pypiStateFormat (const pass) defaultLimits target mirrorPackage encoded `shouldReturn` Right retained
            verifyRetainedCompanion reference "Metadata" `shouldBe` Left (StateInvalid "retained companion identity mismatch")
            let CompanionRef digest size = reference
            verifyRetainedCompanion (CompanionRef digest (size + 1)) "metadata" `shouldBe` Left (StateInvalid "retained companion identity mismatch")

    for_ [(Right StateConflict, StateRetryExhausted), (Left (StateInvalid "write refused"), StateInvalid "write refused")] $ \(writeResult, expectedFault) ->
        it ("keeps the committed inventory when conditional writes exhaust retries: " <> show expectedFault) $ do
            original <- mirrorSample [1] False "https://first.test/" "original"
            addition <- mirrorSample [1, 2] False "https://first.test/" "original"
            record <- expectRight (extendProjectRecord pypiStateFormat defaultLimits target original Nothing)
            readCount <- newIORef (0 :: Int)
            writes <- newIORef (0 :: Int)
            let revision = StateRevision target mirrorPackage "committed"
                store =
                    inertState
                        { msReadProject = \_ name expected -> do
                            name `shouldBe` mirrorPackage
                            expected `shouldBe` Nothing
                            modifyIORef' readCount (+ 1)
                            pure (Right (StateLoaded revision record))
                        , msWriteProject = \_ name expected attempted -> do
                            name `shouldBe` mirrorPackage
                            expected `shouldBe` Just revision
                            version <- getVersion attempted
                            Map.keys (versionFiles version) `shouldBe` map mirrorFilename [1, 2]
                            modifyIORef' writes (+ 1)
                            pure writeResult
                        }
            recordExpectedInventory pypiStateFormat defaultLimits (const pass) target store addition `shouldReturn` Left expectedFault
            readIORef readCount `shouldReturn` 8
            readIORef writes `shouldReturn` 8
            msReadProject store (const pass) mirrorPackage Nothing `shouldReturn` Right (StateLoaded revision record)

    it "stops an update on read uncertainty without writing a smaller inventory" $ do
        inventory <- mirrorSample [1] False "https://first.test/" "original"
        let revision = StateRevision target mirrorPackage "unavailable"
        for_ [(Left (StateInvalid "unreadable"), StateInvalid "unreadable"), (Right (StateUnchanged revision), StateRevisionMismatch)] $ \(readResult, expectedFault) -> do
            let store =
                    inertState
                        { msReadProject = \_ _ _ -> pure readResult
                        , msWriteProject = \_ _ _ _ -> fail "must not write after an uncertain read"
                        }
            recordExpectedInventory pypiStateFormat defaultLimits (const pass) target store inventory `shouldReturn` Left expectedFault

    it "does not reference a companion when its durable write fails" $ do
        inventory <- mirrorSample [1] True "https://first.test/" "original"
        let file :| _ = inventoryFiles inventory
        verified <- expectRight (verifyCompanionBytes defaultLimits file "metadata")
        let store =
                inertState
                    { msRetainCompanion = \_ _ -> pure (Left (StateInvalid "companion write refused"))
                    , msReadProject = \_ _ _ -> fail "must not update a record before durable companion storage"
                    }
        retainProjectCompanion defaultLimits (const pass) store mirrorPackage mirrorVersion verified
            `shouldReturn` Left (StateInvalid "companion write refused")

target :: StateTarget
target = stateTarget "https://private.test/simple"

getVersion :: ProjectMirrorRecord -> IO MirrorVersion
getVersion = maybe (fail "missing version") pure . recordVersion mirrorVersion

hasNote :: Text -> Value -> Bool
hasNote expected (Object fields) = KeyMap.lookup "note" fields == Just (String expected)
hasNote _ _ = False

inertState :: MirrorState
inertState =
    MirrorState
        { msReadProject = \_ _ _ -> pure (Right StateAbsent)
        , msWriteProject = \_ _ _ _ -> pure (Left (StateInvalid "unexpected write"))
        , msRetainCompanion = \_ _ -> pure (Left (StateInvalid "unexpected companion write"))
        , msReadCompanion = \_ _ -> pure (Left StateMissingCompanion)
        }
