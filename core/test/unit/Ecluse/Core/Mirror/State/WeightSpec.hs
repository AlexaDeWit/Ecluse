-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Retained graph growth checks are independent of the active-work accounting formula.
module Ecluse.Core.Mirror.State.WeightSpec (spec) where

import Data.Aeson (Value (Object, String))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Mirror.State.Codec (chargedDecodeProjectRecord, saturatingStateCharge)
import Ecluse.Core.Mirror.State.Internal
import Ecluse.Core.Mirror.State.Weight
import Ecluse.Core.Package
import Ecluse.Core.Registry.MirrorUpload (MirrorReadData (RubyGemsMirrorRead), inventoryDetails)
import Ecluse.Core.Registry.MirrorUpload.Internal (restoreMirrorInventory)
import Ecluse.Core.Registry.RubyGems.Document (GemDocument (gemRows), GemRow (grSource))
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Server.Cache.Types (Source (Source), fullKey)
import Ecluse.Core.Server.MirrorView (mirrorRetentionWeight)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Mirror.RubyGems
import Ecluse.Test.Mirror.State
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "durable graph accounting" $ do
    nativeStateWeightSpec
    it "grows with admitted files and fragments but leaves repeated admission unchanged" $ do
        initialInventory <- mirrorSample [1] False "https://first.test/" "original"
        next <- mirrorSample [2] False "https://first.test/" "original"
        previousRecord <- expectRight (extendProjectRecord defaultLimits target initialInventory Nothing)
        repeated <- expectRight (extendProjectRecord defaultLimits target initialInventory (Just previousRecord))
        added <- expectRight (extendProjectRecord defaultLimits target next (Just previousRecord))
        retainedRecordWeight repeated `shouldBe` retainedRecordWeight previousRecord
        retainedRecordWeight added `shouldSatisfy` (> retainedRecordWeight previousRecord)

    it "charges repeated expected descriptors even when their complete payloads alias" $ do
        record <- sampleRecord
        version <- maybe (fail "missing fixture version") pure (recordVersion mirrorVersion record)
        let fragment = NE.head (storedFragments version)
            StoredMirrorFragment details source expected = fragment
            repeated = StoredMirrorFragment details source (expected <> expected)
        retainedFragmentWeight repeated - retainedFragmentWeight fragment `shouldSatisfy` (> 512)

    it "counts every verified hash, companion reference and repeated backing allocation" $ do
        record <- sampleRecord
        digest <- expectRight (mkHash SHA256 (T.take 64 (T.replicate 65536 "a")))
        let withHashes n = record{projectVersions = fmap (\version -> version{storedIdentities = Map.fromList [(show k, digest) | k <- [1 .. n :: Int]]}) (projectVersions record)}
            pendingRecord = record{projectVersions = fmap (\version -> version{storedCompanions = Map.singleton "file" CompanionPending}) (projectVersions record)}
            retained = record{projectVersions = fmap (\version -> version{storedCompanions = Map.singleton "file" (CompanionRetained (CompanionRef digest 5))}) (projectVersions record)}
        retainedRecordWeight (withHashes 2) - retainedRecordWeight (withHashes 1) `shouldSatisfy` (>= 65536)
        retainedRecordWeight retained - retainedRecordWeight pendingRecord `shouldSatisfy` (>= 65536)

    it "charges one envelope per project and the full backing of JSON keys and values" $ do
        record <- sampleRecord
        let sliced = T.take 1 (T.replicate 65536 "x")
            fields text = KeyMap.singleton (Key.fromText text) (String text)
            large = Object (fields sliced)
            small = Object (fields (T.copy sliced))
        valueBackingExcess large - valueBackingExcess small `shouldBe` 2 * 65535
        let changed = record{projectEnvelope = fields sliced}
        retainedRecordWeight changed `shouldSatisfy` (>= retainedRecordWeight record + 65536)

    it "keeps the parsed-version allowance beyond the retained raw spelling" $ do
        record <- sampleRecord
        version <- maybe (fail "missing fixture version") pure (recordVersion mirrorVersion record)
        let original@(StoredMirrorFragment details source expected) = NE.head (storedFragments version)
            expanded = StoredMirrorFragment details{pkgVersion = mkVersion PyPI (T.intercalate "." (replicate 512 "1"))} source expected
        retainedFragmentWeight expanded - retainedFragmentWeight original `shouldSatisfy` (>= 256 * 1022)

    it "charges UTF-8 backing growth" $ do
        initialInventory <- mirrorSample [1] False "https://first.test/" "original"
        let details = inventoryDetails initialInventory
            withNote note = restoreMirrorInventory details (mirrorSource (pkgArtifacts details) note)
        ascii <- expectRight (withNote (T.replicate 1024 "a")) >>= \inventory -> expectRight (extendProjectRecord defaultLimits target inventory Nothing)
        unicode <- expectRight (withNote (T.replicate 1024 "\x1f600")) >>= \inventory -> expectRight (extendProjectRecord defaultLimits target inventory Nothing)
        retainedRecordWeight unicode `shouldSatisfy` (> retainedRecordWeight ascii + 3072)

    it "saturates nonnegative arithmetic monotonically" $
        hedgehog $ do
            let intLimit = toInteger (maxBound :: Int)
                term = Gen.choice [Gen.integral (Range.linear 0 (4 * intLimit)), Gen.element [0, 1, intLimit - 1, intLimit, intLimit + 1]]
            a <- forAll term
            b <- forAll term
            (saturatingStateCharge (a + b) >= saturatingStateCharge a) === True

    it "keeps saturated entries uncacheable" $ do
        let key = fullKey (Source "test") mirrorPackage
            revision = StateRevision target mirrorPackage "etag"
        saturatingStateCharge (toInteger (maxBound :: Int) * 4 + 32768) `shouldBe` maxBound
        mirrorRetentionWeight key revision maxBound `shouldBe` maxBound
        mirrorRetentionWeight key (StateRevision target mirrorPackage (encodeUtf8 (T.replicate 65536 "x"))) 0
            - mirrorRetentionWeight key revision 0
            `shouldBe` 65532

nativeStateWeightSpec :: Spec
nativeStateWeightSpec = describe "genuine RubyGems state" $ beforeAll rubyStateProbeFixture $ do
    it "reopens every platform row and every original compact object" $ \fixture -> do
        record <- reopenNative fixture
        let versions = Map.elems (recordVersions record)
            rows = [row | version <- versions, StoredMirrorFragment _ (RubyGemsMirrorRead source) _ <- toList (versionFragments version), row <- gemRows source]
            companions = [reference | version <- versions, CompanionRetained reference <- Map.elems (versionCompanions version)]
            objects = rubyStateCompanionObjects fixture
        length rows `shouldBe` 4
        length companions `shouldBe` 4
        Set.fromList (map companionDigest companions) `shouldBe` Map.keysSet objects
        for_ companions $ \reference -> do
            bytes <- maybe (fail "missing native compact object") pure (Map.lookup (companionDigest reference) objects)
            companionSize reference `shouldBe` BS.length bytes
            verifyRetainedCompanion reference bytes `shouldBe` Right bytes

    it "detects omitted native source and companion-reference charges" $ \fixture -> do
        record <- reopenNative fixture
        let versions = Map.elems (recordVersions record)
            sourceBytes = sum [BS.length (grSource row) | version <- versions, StoredMirrorFragment _ (RubyGemsMirrorRead source) _ <- toList (versionFragments version), row <- gemRows source]
            companionCount = sum [Map.size (versionCompanions version) | version <- versions]
            actual = retainedRecordWeight record
        sourceBytes `shouldSatisfy` (> 0)
        companionCount `shouldBe` 4
        actual - rubyStateSourceOmissionCharge record `shouldSatisfy` (>= toInteger sourceBytes)
        actual - rubyStateCompanionOmissionCharge record `shouldSatisfy` (>= 64 * toInteger companionCount)

    it "charges labelled source padding without changing compact objects" $ \fixture -> do
        original <- reopenNative fixture
        paddedFixture <- padRubyStateProbeSource 1024 fixture
        padded <- reopenNative paddedFixture
        rubyStateCompanionObjects paddedFixture `shouldBe` rubyStateCompanionObjects fixture
        retainedRecordWeight padded - retainedRecordWeight original `shouldSatisfy` (>= 4 * 1024)
        let originalSource = retainedRecordWeight original - rubyStateSourceOmissionCharge original
            paddedSource = retainedRecordWeight padded - rubyStateSourceOmissionCharge padded
        paddedSource - originalSource `shouldSatisfy` (>= 4 * 1024)

    it "keeps external compact-object lengths out of the retained record heap" $ \fixture -> do
        record <- reopenNative fixture
        let expand (CompanionRetained (CompanionRef digest size)) = CompanionRetained (CompanionRef digest (size + 1024 * 1024))
            expand CompanionPending = CompanionPending
            counterfact = record{projectVersions = fmap (\version -> version{storedCompanions = fmap expand (storedCompanions version)}) (projectVersions record)}
        retainedRecordWeight counterfact `shouldBe` retainedRecordWeight record

reopenNative :: RubyStateProbeFixture -> IO ProjectMirrorRecord
reopenNative fixture = chargedDecodeProjectRecord (const pass) defaultLimits (rubyStateTarget fixture) (rubyStatePackage fixture) (rubyStateEncodedRecord fixture) >>= expectRight

sampleRecord :: IO ProjectMirrorRecord
sampleRecord = do
    inventory <- mirrorSample [1] False "https://first.test/" "original"
    expectRight (extendProjectRecord defaultLimits target inventory Nothing)

target :: StateTarget
target = stateTarget "https://private.test"
