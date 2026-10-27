-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Cargo's artifact admission and native maintenance capabilities.
module Ecluse.Core.Registry.Cargo.AdapterSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Registry.Adapter.Capability (AdapterArtifact (artifactPrivateAddressing), AdapterMaintenance (maintenanceAlphabet), PrivateArtifactAddressing (AdmittedIndexedArtifactLocation))
import Ecluse.Core.Registry.Adapter.Types (RegistryAdapter (adapterArtifact, adapterMaintenance))
import Ecluse.Core.Registry.Cargo.Adapter (cargoAdapter)
import Ecluse.Core.Registry.Cargo.Project (projectName)
import Ecluse.Core.Registry.Maintenance.NameSpace (extendBucket, inBucket, initialBuckets, parseNamePrefix, renderNamePrefix)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (ChargeFactors))
import Ecluse.Test.Support (expectRight)

-- | Pin artifact admission and the native name partitions.
spec :: Spec
spec = describe "Cargo adapter" $ do
    let adapter = cargoAdapter (ChargeFactors 1000 1000 1000)
        alphabet = maintenanceAlphabet (adapterMaintenance adapter)
    it "requires metadata admission before private artifact selection" $
        artifactPrivateAddressing (adapterArtifact adapter) `shouldBe` AdmittedIndexedArtifactLocation
    it "partitions native names instead of using one whole-store bucket" $ do
        let buckets = toList (initialBuckets alphabet)
        map renderNamePrefix buckets `shouldMatchList` map T.singleton (['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9'] <> "-_")
        for_ ["Upper_Name", "lower-name", "A0", "a-", "a_", "Z9", "z9"] $ \raw -> do
            name <- expectRight (projectName raw)
            length (filter (`inBucket` name) buckets) `shouldBe` 1
    it "subdivides through case, digit, hyphen and underscore continuations without losing names" $ do
        prefix <- maybe (fail "Cargo alphabet lost its uppercase prefix") pure (parseNamePrefix alphabet "A")
        let children = extendBucket alphabet prefix
        for_ ["AAcrate", "Aacrate", "A0crate", "A-crate", "A_crate"] $ \raw -> do
            name <- expectRight (projectName raw)
            length (filter (`inBucket` name) children) `shouldBe` 1
    it "round-trips a full native cursor without changing published case or separators" $ do
        let maximumName = "A0_-" <> T.replicate 60 "z"
        fmap renderNamePrefix (parseNamePrefix alphabet maximumName) `shouldBe` Just maximumName
        fmap renderNamePrefix (parseNamePrefix alphabet "Upper_Name-0") `shouldBe` Just "Upper_Name-0"
