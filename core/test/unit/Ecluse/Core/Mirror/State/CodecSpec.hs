-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Stored records preserve admission evidence and reject contradictory membership.
module Ecluse.Core.Mirror.State.CodecSpec (spec) where

import Data.Aeson (Value (..), eitherDecodeStrict, encode, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Mirror.State (recordExpectedInventory)
import Ecluse.Core.Mirror.State.Codec
import Ecluse.Core.Mirror.State.Internal
import Ecluse.Core.Package (Availability (..), CodeExecSignal (..), DependencySources (..), PackageDetails (..))
import Ecluse.Core.Registry.MirrorUpload (inventoryDetails, inventoryFiles, inventoryReadData)
import Ecluse.Core.Registry.MirrorUpload.Internal (restoreMirrorInventory)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (..), Limits (..), defaultLimits)
import Ecluse.Core.Worker.Managed (readMirrorRecord)
import Ecluse.Core.Worker.Types (WorkerMirrorState (..), WorkerStateBudget (..))
import Ecluse.Test.Json (changeAt)
import Ecluse.Test.Mirror.State
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "durable record codec" $ do
    it "keeps the old inventory, update, worker and encoder charges" $ do
        inventory <- mirrorSample [1] False "https://first.test/" "original"
        record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
        projectRecordWorkWeight defaultLimits record `shouldBe` Right 25698
        mirrorInventoryWeight inventory `shouldBe` 21074
        projectRecordWeight defaultLimits record `shouldSatisfy` either (const False) (< 25698)
        paid <- newIORef ([] :: [Int])
        let charge cost = modifyIORef' paid (cost :)
            store =
                MirrorState
                    { msReadProject = \_ _ _ -> pure (Right (StateLoaded revision record))
                    , msWriteProject = \_ _ _ _ -> fail "idempotent admission must not write"
                    , msRetainCompanion = \_ _ -> fail "unexpected companion write"
                    , msReadCompanion = \_ _ -> fail "unexpected companion read"
                    }
            budget = WorkerStateBudget charge charge charge charge charge charge charge
            binding = WorkerMirrorState store target defaultLimits (\action -> Just <$> action budget)
        void (recordExpectedInventory defaultLimits charge target store inventory >>= expectRight)
        readIORef paid `shouldReturn` [97640]
        writeIORef paid []
        void (readMirrorRecord budget binding mirrorPackage >>= expectRight)
        readIORef paid `shouldReturn` [77094]
        writeIORef paid []
        void (chargedEncodeProjectRecord charge defaultLimits record >>= expectRight)
        readIORef paid `shouldReturn` [135560]

    for_ [(NoCodeOnInstall, Available), (CodeExecUnknown, Deprecated), (RunsCodeOnInstall "source build", Yanked)] $ \(install, availability) ->
        it ("preserves admission facts through restart: " <> show (install, availability)) $ do
            original <- mirrorSample [1] False "https://first.test/" "original"
            let details = inventoryDetails original
            inventory <- expectRight (restoreMirrorInventory details{pkgInstallCode = install, pkgAvailability = availability} (inventoryReadData original))
            record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
            bytes <- expectRight (encodeProjectRecord defaultLimits record)
            decode bytes `shouldReturn` Right record

    for_ [RegistryDependencies, FetchesOutsideRegistry "dependency 'left-pad' in dependencies names a git source", DependencySourcesUnknown] $ \dependencies ->
        it ("preserves the dependency fact through restart: " <> show dependencies) $ do
            original <- mirrorSample [1] False "https://first.test/" "original"
            let details = (inventoryDetails original){pkgDependencySources = dependencies}
            inventory <- expectRight (restoreMirrorInventory details (inventoryReadData original))
            record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
            bytes <- expectRight (encodeProjectRecord defaultLimits record)
            decode bytes `shouldReturn` Right record

    for_ invalidFields $ \(label, path, replacement, reason) ->
        it label $ do
            (record, value) <- recordFixture
            bytes <- expectRight (encodeProjectRecord defaultLimits record)
            decode bytes `shouldReturn` Right record
            changed <- changeAt path (const (pure replacement)) value
            decode (LBS.toStrict (encode changed)) >>= (`shouldSatisfy` invalidBecause reason)
            decode bytes `shouldReturn` Right record

    for_ duplicateFields $ \(label, path) ->
        it label $ do
            (record, value) <- recordFixture
            decode (LBS.toStrict (encode value)) `shouldReturn` Right record
            changed <- changeAt path duplicateArray value
            decode (LBS.toStrict (encode changed)) >>= (`shouldSatisfy` invalidBecause "duplicate state entry")

    it "counts retained companion bytes together with the encoded record" $ do
        inventory <- mirrorSample [1] True "https://first.test/" "original"
        record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
        let file :| _ = inventoryFiles inventory
        verified@(VerifiedCompanionBytes _ reference _) <- expectRight (verifyCompanionBytes defaultLimits file "metadata")
        retained <- expectRight (retainCompanionReference mirrorVersion verified reference record)
        bytes <- expectRight (encodeProjectRecord defaultLimits retained)
        let cap = BS.length bytes + 8
            exact = defaultLimits{maxMetadataBytes = cap}
            small = exact{maxMetadataBytes = cap - 1}
            refused = StateBound (BodyTooLarge (MetadataBodyLimit (cap - 1)))
        encodeProjectRecord exact retained `shouldBe` Right bytes
        chargedDecodeProjectRecord (const pass) exact target mirrorPackage bytes `shouldReturn` Right retained
        encodeProjectRecord small retained `shouldBe` Left refused
        chargedDecodeProjectRecord (const pass) small target mirrorPackage bytes `shouldReturn` Left refused
        encodeProjectRecord defaultLimits{maxMetadataBytes = 7} retained
            `shouldBe` Left (StateBound (BodyTooLarge (MetadataBodyLimit 7)))

    it "rejects excessive dimensions before paying for domain restoration" $ do
        (record, _) <- recordFixture
        bytes <- expectRight (encodeProjectRecord defaultLimits record)
        for_ [(defaultLimits{maxVersionCount = 0}, TooManyVersions 1 0), (defaultLimits{mirrorStateFileCeiling = 0}, TooManyArtifacts 1 0)] $ \(bounds, fault) -> do
            paid <- newIORef ([] :: [Int])
            chargedDecodeProjectRecord (\cost -> modifyIORef' paid (cost :)) bounds target mirrorPackage bytes
                `shouldReturn` Left (StateBound fault)
            length <$> readIORef paid `shouldReturn` 1

recordFixture :: IO (ProjectMirrorRecord, Value)
recordFixture = do
    inventory <- mirrorSample [1] True "https://first.test/" "original"
    record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
    bytes <- expectRight (encodeProjectRecord defaultLimits record)
    value <- expectRight (eitherDecodeStrict bytes)
    pure (record, value)

decode :: ByteString -> IO (Either MirrorStateFault ProjectMirrorRecord)
decode = chargedDecodeProjectRecord (const pass) defaultLimits target mirrorPackage

invalidBecause :: Text -> Either MirrorStateFault a -> Bool
invalidBecause reason (Left (StateInvalid detail)) = reason `T.isSuffixOf` detail
invalidBecause _ _ = False

duplicateArray :: Value -> IO Value
duplicateArray (Array values) = pure (Array (values <> values))
duplicateArray _ = fail "expected fixture array"

invalidFields :: [(String, [Key], Value, Text)]
invalidFields =
    [ ("refuses an unsupported durable schema", ["schema"], Number 999, "unsupported mirror state schema")
    , ("refuses an unknown package ecosystem", ["package", "ecosystem"], String "unknown", "unknown ecosystem")
    , ("refuses another package in the project envelope", ["envelope", "name"], String "other", "invalid project envelope identity")
    , ("refuses a version key that disagrees with its fragment", ["versions", "version"], String "2", "fragment identity mismatch")
    , ("refuses another package in an admission fragment", details <> ["package", "name"], String "other", "fragment identity mismatch")
    , ("refuses a version without fragments", ["versions", "fragments"], toJSON ([] :: [Value]), "empty version fragments")
    , ("refuses a fragment without artifacts", details <> ["artifacts"], toJSON ([] :: [Value]), "empty source fragment")
    , ("refuses a negative artifact size", artifacts <> ["size"], Number (-1), "negative artifact size")
    , ("refuses an unknown install fact", details <> ["install", "kind"], String "invalid", "unknown install fact")
    , ("refuses an unknown availability fact", details <> ["availability"], String "invalid", "unknown availability fact")
    , ("refuses an unknown source shape", source <> ["kind"], String "invalid", "unknown source shape")
    , ("refuses a source entry with another filename", source <> ["entries", "value", "filename"], String "other-1.0.tar.gz", "invalid source fragment pairing")
    , ("refuses missing declared companion membership", ["versions", "companions"], toJSON ([] :: [Value]), "companion set does not match declared source companions")
    , ("refuses companion membership for another filename", ["versions", "companions", "filename"], String "other", "companion set does not match declared source companions")
    , ("refuses a verified digest without an expected file", ["versions", "verified"], toJSON [object ["filename" .= ("other" :: Text), "digest" .= digest "sha256" 64]], "verified digest has no expected file")
    , ("refuses an unsupported verified digest algorithm", ["versions", "verified"], toJSON [object ["filename" .= mirrorFilename 1, "digest" .= digest "sha512" 128]], "unsupported verified artifact digest")
    , ("refuses a negative retained companion size", ["versions", "companions", "retained"], object ["digest" .= digest "sha256" 64, "size" .= (-1 :: Int)], "invalid retained companion identity")
    , ("refuses a retained companion with another digest algorithm", ["versions", "companions", "retained"], object ["digest" .= digest "sha512" 128, "size" .= (8 :: Int)], "invalid retained companion identity")
    ]
  where
    details = ["versions", "fragments", "details"]
    artifacts = details <> ["artifacts"]
    source = ["versions", "fragments", "source"]
    digest algorithm size = object ["algorithm" .= (algorithm :: Text), "value" .= T.replicate size "a"]

duplicateFields :: [(String, [Key])]
duplicateFields =
    [ ("refuses duplicate versions", ["versions"])
    , ("refuses duplicate filenames across fragments", ["versions", "fragments"])
    , ("refuses duplicate companion entries", ["versions", "companions"])
    , ("refuses duplicate source coordinates", ["versions", "fragments", "source", "entries"])
    ]

target :: StateTarget
target = stateTarget "https://private.test"

revision :: StateRevision
revision = StateRevision target mirrorPackage "etag"
