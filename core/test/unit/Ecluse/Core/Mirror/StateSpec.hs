-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Interrupted copies retain every admitted identity and source fragment across conditional updates.
module Ecluse.Core.Mirror.StateSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Mirror.State
import Ecluse.Core.Mirror.State.Internal (StateRevision (..), VerifiedCompanionBytes (..), retainVerifiedIdentity)
import Ecluse.Core.Package
import Ecluse.Core.Package.Entry (EntryKey (ArrayEntry))
import Ecluse.Core.Registry.MirrorUpload
import Ecluse.Core.Registry.MirrorUpload.Internal (restoreMirrorInventory)
import Ecluse.Core.Registry.PyPI.Document (simpleDocument, simpleFiles)
import Ecluse.Core.Security (Limits (..), defaultLimits)
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Test.Package (hexSha256Of, hexSha512Of)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "durable mirror expectations" $ do
    it "preserves old files when a later admitted source removes them" $ do
        a <- sample [1, 2] False "https://first.test/" "original"
        b <- sample [2, 3] False "https://later.test/" "changed"
        old <- expectRight (extendProjectRecord defaultLimits target a Nothing)
        updated <- expectRight (extendProjectRecord defaultLimits target b (Just old))
        version <- getVersion updated
        Map.keys (versionFiles version) `shouldBe` map filename [1, 2, 3]
        length (versionFragments version) `shouldBe` 2
        let fields = [value | fragment <- toList (versionFragments version), PyPIMirrorRead document <- [fragmentReadData fragment], (_, value) <- simpleFiles document]
        fields `shouldSatisfy` any (hasNote "original")
        fields `shouldSatisfy` any (hasNote "changed")
        sum [length (pkgArtifacts (fragmentDetails fragment)) | fragment <- toList (versionFragments version)] `shouldBe` 3

    it "keeps equal ArrayEntry coordinates in separate admitted fragments" $ do
        a <- sample [1] False "https://first.test/" "original"
        b <- sample [2] False "https://later.test/" "added"
        old <- expectRight (extendProjectRecord defaultLimits target a Nothing)
        updated <- expectRight (extendProjectRecord defaultLimits target b (Just old))
        version <- getVersion updated
        map (map artEntryKey . toList . pkgArtifacts . fragmentDetails) (toList (versionFragments version)) `shouldBe` [[ArrayEntry 0], [ArrayEntry 0]]
        encoded <- expectRight (encodeProjectRecord defaultLimits updated)
        decodeProjectRecord defaultLimits target name encoded `shouldBe` Right updated

    it "does not replace original fields when only source URLs change" $ do
        a <- sample [1] False "https://first.test/" "original"
        b <- sample [1] False "https://later.test/" "withdrawn"
        old <- expectRight (extendProjectRecord defaultLimits target a Nothing)
        extendProjectRecord defaultLimits target b (Just old) `shouldBe` Right old

    it "refuses a conflicting hash without mutating the old record" $ do
        a <- sample [1] False "https://first.test/" "original"
        let details = inventoryDetails a
        otherHash <- expectRight (mkHash SHA256 (T.replicate 64 "b"))
        let changed = fmap (\artifact -> artifact{artHashes = [otherHash]}) (pkgArtifacts details)
            source = sourceFor changed "changed"
        b <- expectRight (restoreMirrorInventory details{pkgArtifacts = changed} source)
        old <- expectRight (extendProjectRecord defaultLimits target a Nothing)
        extendProjectRecord defaultLimits target b (Just old) `shouldBe` Left (StateFileConflict (filename 1))

    it "retains one source entry per filename through repeated incremental admissions" $ do
        inventories <- traverse (\n -> sample [1 .. n] False "https://first.test/" "original") [1 .. 20]
        records <-
            foldM
                ( \records inventory -> do
                    record <- expectRight (extendProjectRecord defaultLimits target inventory (viaNonEmpty NE.head records))
                    pure (record : records)
                )
                []
                inventories
        sizes <- traverse (fmap BS.length . expectRight . encodeProjectRecord defaultLimits) records
        case nonEmpty sizes of
            Nothing -> expectationFailure "missing incremental records"
            Just (value :| values) -> foldl' max value values `shouldSatisfy` (<= 25 * foldl' min value values)
        for_ (viaNonEmpty NE.head records) $ \record -> do
            version <- getVersion record
            sum [length (pkgArtifacts (fragmentDetails fragment)) | fragment <- toList (versionFragments version)] `shouldBe` 20

    it "refuses a project or target identity mismatch" $ do
        inventory <- sample [1] False "https://first.test/" "original"
        record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
        bytes <- expectRight (encodeProjectRecord defaultLimits record)
        decodeProjectRecord defaultLimits (stateTarget "https://other.test") name bytes `shouldBe` Left StateIdentityMismatch
        decodeProjectRecord defaultLimits target (mkPackageName PyPI Nothing "other") bytes `shouldBe` Left StateIdentityMismatch

    it "requires all current target identities and rejects digestless target presence" $ do
        inventory <- sample [1, 2] False "https://first.test/" "original"
        record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
        version <- getVersion record
        let files = map expectedArtifact (toList (inventoryFiles inventory))
        checkMirrorCompletion version files `shouldBe` True
        checkMirrorCompletion version (take 1 files) `shouldBe` False
        checkMirrorCompletion version (map (\file -> file{artHashes = []}) files) `shouldBe` False
        checkMirrorCompletion version (map (\file -> file{artUrl = "https://private.test/file"}) files) `shouldBe` True

    it "proves target SHA256 from bytes verified against source SHA512" $ do
        original <- sample [1] False "https://first.test/" "original"
        sha512 <- expectRight (mkHash SHA512 (hexSha512Of "wheel"))
        sha256 <- expectRight (mkHash SHA256 (hexSha256Of "wheel"))
        let details = inventoryDetails original
            files = fmap (\artifact -> artifact{artHashes = [sha512]}) (pkgArtifacts details)
        inventory <- expectRight (restoreMirrorInventory details{pkgArtifacts = files} (sourceFor files "original"))
        let file :| _ = inventoryFiles inventory
            targetFile = (expectedArtifact file){artHashes = [sha256], artUrl = "https://private.test/file"}
        record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
        before <- getVersion record
        checkMirrorCompletion before [targetFile] `shouldBe` False
        verified <- expectRight (verifyMirrorBytes file "wheel")
        updated <- expectRight (retainVerifiedIdentity versionOne verified record)
        after <- getVersion updated
        checkMirrorCompletion after [targetFile] `shouldBe` True
        map (artHashes . expectedArtifact) (Map.elems (versionFiles after)) `shouldBe` [[sha512]]
        wrong <- expectRight (mkHash SHA256 (hexSha256Of "other"))
        checkMirrorCompletion after [targetFile{artHashes = [wrong]}] `shouldBe` False
        encoded <- expectRight (encodeProjectRecord defaultLimits updated)
        decodeProjectRecord defaultLimits target name encoded `shouldBe` Right updated

    it "unions concurrent versions without replacing an earlier version" $ do
        first <- sample [1] False "https://first.test/" "original"
        let details = inventoryDetails first
            newVersion = mkVersion PyPI "2"
            renamed = fmap (\artifact -> artifact{artFilename = T.replace "1.0" "2.0" (artFilename artifact), artUrl = T.replace "1.0" "2.0" (artUrl artifact)}) (pkgArtifacts details)
        second <- expectRight (restoreMirrorInventory details{pkgVersion = newVersion, pkgArtifacts = renamed} (sourceFor renamed "second"))
        old <- expectRight (extendProjectRecord defaultLimits target first Nothing)
        updated <- expectRight (extendProjectRecord defaultLimits target second (Just old))
        Map.keys (recordVersions updated) `shouldBe` ["1", "2"]
        recordVersion versionOne updated `shouldBe` recordVersion versionOne old

    it "blocks completion while a declared companion remains pending" $ do
        inventory <- sample [1] True "https://first.test/" "original"
        record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
        version <- getVersion record
        checkMirrorCompletion version (map expectedArtifact (toList (inventoryFiles inventory))) `shouldBe` False
        Map.lookup (filename 1) (versionCompanions version) `shouldBe` Just CompanionPending

    it "writes companion bytes before referencing them and verifies retained content" $ do
        inventory <- sample [1] True "https://first.test/" "original"
        record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
        let file :| _ = inventoryFiles inventory
        verified@(VerifiedCompanionBytes _ reference _) <- expectRight (verifyCompanionBytes defaultLimits file "metadata")
        verifyRetainedCompanion reference "metadata" `shouldBe` Right "metadata"
        verifyRetainedCompanion reference "corrupt" `shouldSatisfy` isLeft
        retained <- newIORef False
        held <- newIORef record
        let revision = StateRevision target name "one"
            store =
                inertState
                    { msReadProject = \_ _ -> Right . StateLoaded revision <$> readIORef held
                    , msRetainCompanion = \_ -> writeIORef retained True $> Right reference
                    , msWriteProject = \_ _ updated -> do
                        readIORef retained `shouldReturn` True
                        writeIORef held updated
                        pure (Right (StateStored revision))
                    }
        (_, updated) <- retainProjectCompanion defaultLimits store name versionOne verified >>= expectRight
        version <- getVersion updated
        checkMirrorCompletion version [expectedArtifact file] `shouldBe` True
        bytes <- expectRight (encodeProjectRecord defaultLimits updated)
        decodeProjectRecord defaultLimits target name bytes `shouldBe` Right updated

    it "refuses undeclared and oversized companion retention" $ do
        inventory <- sample [1] False "https://first.test/" "original"
        let file :| _ = inventoryFiles inventory
        verifyCompanionBytes defaultLimits file "metadata" `shouldSatisfy` isLeft
        declared <- sample [1] True "https://first.test/" "original"
        let declaredFile :| _ = inventoryFiles declared
        verifyCompanionBytes defaultLimits{maxMetadataBytes = 2} declaredFile "metadata" `shouldSatisfy` isLeft

    it "checks dimensions before retaining the source tree" $ do
        inventory <- sample [1, 2] False "https://first.test/" "original"
        record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
        bytes <- expectRight (encodeProjectRecord defaultLimits record)
        decodeProjectRecord defaultLimits{maxArtifactCount = 1} target name bytes `shouldSatisfy` isLeft
        decodeProjectRecord defaultLimits{maxVersionCount = 0} target name bytes `shouldSatisfy` isLeft

    it "round trips every admission fact and original source field" $ do
        inventory <- sample [1, 2] True "https://first.test/" "original"
        record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
        bytes <- expectRight (encodeProjectRecord defaultLimits record)
        decodeProjectRecord defaultLimits target name bytes `shouldBe` Right record

    it "rejects unsupported schema, malformed JSON, and excessive nesting" $ do
        decodeProjectRecord defaultLimits target name "{\"schema\":2}" `shouldSatisfy` isLeft
        decodeProjectRecord defaultLimits target name "{" `shouldSatisfy` isLeft
        decodeProjectRecord defaultLimits{maxNestingDepth = 2} target name "{\"a\":{\"b\":{}}}" `shouldSatisfy` isLeft

    it "rejects trailing non-JSON data" $
        decodeProjectRecord defaultLimits target name "{}trailing" `shouldSatisfy` isLeft

    it "rejects a response above its byte cap before record decoding" $
        decodeProjectRecord defaultLimits{maxMetadataBytes = 2} target name "{broken" `shouldSatisfy` isLeft

    it "reconciles an ambiguous successful write by rereading its union" $ do
        inventory <- sample [1] False "https://first.test/" "original"
        held <- newIORef Nothing
        writes <- newIORef (0 :: Int)
        let revision = StateRevision target name "one"
            store =
                inertState
                    { msReadProject = \_ _ -> readIORef held <&> Right . maybe StateAbsent (StateLoaded revision)
                    , msWriteProject = \_ _ record -> do
                        modifyIORef' writes (+ 1)
                        writeIORef held (Just record)
                        pure (Left (StateInvalid "lost reply"))
                    }
        result <- recordExpectedInventory defaultLimits target store inventory >>= expectRight
        fst result `shouldBe` revision
        readIORef writes `shouldReturn` 1

    it "rereads after a collision and preserves a concurrent filename addition" $ do
        a <- sample [1] False "https://first.test/" "original"
        b <- sample [2] False "https://later.test/" "new"
        other <- expectRight (extendProjectRecord defaultLimits target b Nothing)
        held <- newIORef Nothing
        collision <- newIORef True
        let revision = StateRevision target name "one"
            store =
                inertState
                    { msReadProject = \_ _ -> readIORef held <&> Right . maybe StateAbsent (StateLoaded revision)
                    , msWriteProject = \_ _ record ->
                        readIORef collision >>= \firstWrite ->
                            if firstWrite
                                then do
                                    writeIORef collision False
                                    writeIORef held (Just other)
                                    pure (Right StateConflict)
                                else writeIORef held (Just record) $> Right (StateStored revision)
                    }
        (_, record) <- recordExpectedInventory defaultLimits target store a >>= expectRight
        version <- getVersion record
        Map.keys (versionFiles version) `shouldBe` map filename [1, 2]

name :: PackageName
name = mkPackageName PyPI Nothing "demo"

versionOne :: Version
versionOne = mkVersion PyPI "1"

target :: StateTarget
target = stateTarget "https://private.test/simple"

filename :: Int -> Text
filename build = "demo-1.0-" <> show build <> "-py3-none-any.whl"

sample :: [Int] -> Bool -> Text -> Text -> IO MirrorInventory
sample numbers companion location note = do
    digest <- expectRight (mkHash SHA256 (T.replicate 64 "a"))
    artifacts <-
        maybe (fail "empty test inventory") pure . nonEmpty $
            [Artifact (ArrayEntry position) (filename number) (location <> filename number) [digest] (if companion then Just (MetadataAvailable True) else Nothing) (Just 5) | (position, number) <- zip [0 ..] numbers]
    let details = PackageDetails name versionOne Nothing NoCodeOnInstall Available artifacts
    expectRight (restoreMirrorInventory details (sourceFor artifacts note))

sourceFor :: NonEmpty Artifact -> Text -> MirrorReadData
sourceFor artifacts note =
    PyPIMirrorRead
        ( simpleDocument
            (KeyMap.fromList [("name", String "demo"), ("meta", object ["api-version" .= ("1.0" :: Text)])])
            [ ( artEntryKey artifact
              , object
                    ( ["filename" .= artFilename artifact, "url" .= artUrl artifact, "hashes" .= object [Key.fromText (renderHashAlg (hashAlg digest)) .= hashValue digest | digest <- artHashes artifact], "size" .= artSize artifact, "requires-python" .= (">=3.8" :: Text), "yanked" .= False, "note" .= note]
                        <> ["core-metadata" .= True | isJust (artCoreMetadata artifact)]
                    )
              )
            | artifact <- toList artifacts
            ]
        )

getVersion :: ProjectMirrorRecord -> IO MirrorVersion
getVersion = maybe (fail "missing version") pure . recordVersion versionOne

hasNote :: Text -> Value -> Bool
hasNote expected (Object fields) = KeyMap.lookup "note" fields == Just (String expected)
hasNote _ _ = False

inertState :: MirrorState
inertState =
    MirrorState
        { msReadProject = \_ _ -> pure (Right StateAbsent)
        , msWriteProject = \_ _ _ -> pure (Left (StateInvalid "unexpected write"))
        , msRetainCompanion = \_ -> pure (Left (StateInvalid "unexpected companion write"))
        , msReadCompanion = \_ -> pure (Left StateMissingCompanion)
        }
