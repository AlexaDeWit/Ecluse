-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | RubyGems' artifact admission and native maintenance capabilities.
module Ecluse.Core.Registry.RubyGems.AdapterSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Registry.Adapter.Capability (AdapterArtifact (artifactHosts, artifactPrivateAddressing), AdapterMaintenance (maintenanceAlphabet), PrivateArtifactAddressing (AdmittedIndexedArtifactLocation))
import Ecluse.Core.Registry.Adapter.Types (RegistryAdapter (adapterArtifact, adapterMaintenance))
import Ecluse.Core.Registry.Maintenance.NameSpace (extendBucket, inBucket, initialBuckets, parseNamePrefix, renderNamePrefix)
import Ecluse.Core.Registry.RubyGems.Adapter (rubyGemsAdapter)
import Ecluse.Core.Registry.RubyGems.Project (projectName)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (ChargeFactors))
import Ecluse.Test.Support (expectRight)

-- | Pin artifact admission, the absent artifact host list and the native name partitions.
spec :: Spec
spec = describe "RubyGems adapter" $ do
    let adapter = rubyGemsAdapter (ChargeFactors 1000 1000 1000)
        alphabet = maintenanceAlphabet (adapterMaintenance adapter)
    it "requires metadata admission before private artifact selection and trusts no artifact host" $ do
        artifactPrivateAddressing (adapterArtifact adapter) `shouldBe` AdmittedIndexedArtifactLocation
        artifactHosts (adapterArtifact adapter) `shouldBe` []
    it "partitions native names, dot-led names included, instead of using one whole-store bucket" $ do
        let buckets = toList (initialBuckets alphabet)
        map renderNamePrefix buckets `shouldMatchList` map T.singleton (['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9'] <> "._-")
        for_ [".cat", "-", "_private", "Rack", "rack", "0gem", "a.b-c_d"] $ \raw -> do
            name <- expectRight (projectName raw)
            length (filter (`inBucket` name) buckets) `shouldBe` 1
    it "subdivides a dot-led bucket through case, digit and punctuation continuations without losing names" $ do
        prefix <- maybe (fail "RubyGems alphabet lost its dot prefix") pure (parseNamePrefix alphabet ".")
        let children = extendBucket alphabet prefix
        for_ [".cat", ".Cat", ".0gem", ".-gem", "._gem"] $ \raw -> do
            name <- expectRight (projectName raw)
            length (filter (`inBucket` name) children) `shouldBe` 1
