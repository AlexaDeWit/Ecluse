-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Retained graph growth checks are independent of the active-work accounting formula.
module Ecluse.Core.Mirror.State.WeightSpec (spec) where

import Data.Aeson (Value (Object, String))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec
import Test.QuickCheck (NonNegative (..), property)

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Mirror.State.Codec (saturatingStateCharge)
import Ecluse.Core.Mirror.State.Internal
import Ecluse.Core.Mirror.State.Weight
import Ecluse.Core.Package
import Ecluse.Core.Registry.MirrorUpload (inventoryDetails)
import Ecluse.Core.Registry.MirrorUpload.Internal (restoreMirrorInventory)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Server.Cache.Types (Source (Source), fullKey)
import Ecluse.Core.Server.MirrorView (mirrorRetentionWeight)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Mirror.State
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "durable graph accounting" $ do
    it "grows with admitted files and fragments but leaves repeated admission unchanged" $ do
        first <- mirrorSample [1] False "https://first.test/" "original"
        next <- mirrorSample [2] False "https://first.test/" "original"
        before <- expectRight (extendProjectRecord defaultLimits target first Nothing)
        repeated <- expectRight (extendProjectRecord defaultLimits target first (Just before))
        added <- expectRight (extendProjectRecord defaultLimits target next (Just before))
        retainedRecordWeight repeated `shouldBe` retainedRecordWeight before
        retainedRecordWeight added `shouldSatisfy` (> retainedRecordWeight before)

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
            pending = record{projectVersions = fmap (\version -> version{storedCompanions = Map.singleton "file" CompanionPending}) (projectVersions record)}
            retained = record{projectVersions = fmap (\version -> version{storedCompanions = Map.singleton "file" (CompanionRetained (CompanionRef digest 5))}) (projectVersions record)}
        retainedRecordWeight (withHashes 2) - retainedRecordWeight (withHashes 1) `shouldSatisfy` (>= 65536)
        retainedRecordWeight retained - retainedRecordWeight pending `shouldSatisfy` (>= 65536)

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
        first <- mirrorSample [1] False "https://first.test/" "original"
        let details = inventoryDetails first
            withNote note = restoreMirrorInventory details (mirrorSource (pkgArtifacts details) note)
        ascii <- expectRight (withNote (T.replicate 1024 "a")) >>= \inventory -> expectRight (extendProjectRecord defaultLimits target inventory Nothing)
        unicode <- expectRight (withNote (T.replicate 1024 "\x1f600")) >>= \inventory -> expectRight (extendProjectRecord defaultLimits target inventory Nothing)
        retainedRecordWeight unicode `shouldSatisfy` (> retainedRecordWeight ascii + 3072)

    it "saturates nonnegative arithmetic monotonically" $
        property $
            \(NonNegative a) (NonNegative b) -> saturatingStateCharge (a + b) >= saturatingStateCharge a

    it "keeps saturated entries uncacheable" $ do
        let key = fullKey (Source "test") mirrorPackage
            revision = StateRevision target mirrorPackage "etag"
        saturatingStateCharge (toInteger (maxBound :: Int) * 4 + 32768) `shouldBe` maxBound
        mirrorRetentionWeight key revision maxBound `shouldBe` maxBound
        mirrorRetentionWeight key (StateRevision target mirrorPackage (encodeUtf8 (T.replicate 65536 "x"))) 0
            - mirrorRetentionWeight key revision 0
            `shouldBe` 65532

sampleRecord :: IO ProjectMirrorRecord
sampleRecord = do
    inventory <- mirrorSample [1] False "https://first.test/" "original"
    expectRight (extendProjectRecord defaultLimits target inventory Nothing)

target :: StateTarget
target = stateTarget "https://private.test"
