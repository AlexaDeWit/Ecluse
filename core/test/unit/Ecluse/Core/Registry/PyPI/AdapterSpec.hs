-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native capabilities and cross-ecosystem dispatch remain explicit.
module Ecluse.Core.Registry.PyPI.AdapterSpec (spec) where

import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems))
import Ecluse.Core.Registry.Adapter (
    RegistryAdapter (adapterArtifact, adapterEcosystem, adapterMaintenance, adapterPublish),
    adapterFor,
 )
import Ecluse.Core.Registry.Adapter.Capability (
    AdapterArtifact (artifactHosts, artifactPrivateAddressing),
    AdapterMaintenance (maintenanceListing, maintenanceVersionDelete),
    AdapterPublish (publishIdentity, publishMirrorRequirements, publishUploadEndpoint),
    MirrorRequirements (mirrorStateRequirement, mirrorWorkerRead),
    MirrorStateRequirement (..),
    PrivateArtifactAddressing (..),
    UploadEndpointRequirement (..),
    UploadIdentityRequirement (..),
    WorkerMetadataRead (..),
 )

spec :: Spec
spec = describe "the PyPI adapter" $ do
    it "is what the registry resolves the ecosystem to, so a pypi mount serves" $
        fmap adapterEcosystem (adapterFor PyPI) `shouldBe` Just PyPI

    it "declares the files host public PyPI serves distribution bytes from" $
        -- The artifact-host gate folds this in, so the secure-default same-host policy admits
        -- the files host with no operator naming a hostname.
        fmap (artifactHosts . adapterArtifact) (adapterFor PyPI)
            `shouldBe` Just ["https://files.pythonhosted.org"]

    it "declares indexed private addressing independently of its public files host" $
        fmap (artifactPrivateAddressing . adapterArtifact) (adapterFor PyPI) `shouldBe` Just IndexedArtifactLocation

    it "keeps npm's conventional private path and empty public host list" $ do
        fmap (artifactPrivateAddressing . adapterArtifact) (adapterFor Npm) `shouldBe` Just ConventionalArtifactPath
        fmap (artifactHosts . adapterArtifact) (adapterFor Npm) `shouldBe` Just []

    it "carries the native upload capability for a configured publication target" $
        fmap (isJust . adapterPublish) (adapterFor PyPI) `shouldBe` Just True

    it "declares PyPI's charged read, durable state, explicit endpoint, and named identity" $ do
        let selected = adapterFor PyPI >>= adapterPublish
        fmap (mirrorStateRequirement . publishMirrorRequirements) selected `shouldBe` Just DurableVersionState
        fmap publishUploadEndpoint selected `shouldBe` Just ExplicitUploadEndpoint
        fmap publishIdentity selected `shouldBe` Just NamedIdentity
        fmap (charged . mirrorWorkerRead . publishMirrorRequirements) selected `shouldBe` Just True

    it "declares npm's cached read, native state, root endpoint, and token identity" $ do
        let selected = adapterFor Npm >>= adapterPublish
        fmap (mirrorStateRequirement . publishMirrorRequirements) selected `shouldBe` Just NativeVersionState
        fmap publishUploadEndpoint selected `shouldBe` Just ReadRootUpload
        fmap publishIdentity selected `shouldBe` Just TokenIdentity
        fmap (charged . mirrorWorkerRead . publishMirrorRequirements) selected `shouldBe` Just False

    it "spells neither store-maintenance verb, so a Dredger against such a store refuses" $ do
        -- PyPI has no public wire endpoint for listing a store's projects or deleting a
        -- release, so the Dredger names the missing verb rather than sweeping half the store.
        fmap (isJust . maintenanceListing . adapterMaintenance) (adapterFor PyPI) `shouldBe` Just False
        fmap (isJust . maintenanceVersionDelete . adapterMaintenance) (adapterFor PyPI) `shouldBe` Just False

    it "leaves npm's publish capability filled, so npm keeps writing as it did" $
        fmap (isJust . adapterPublish) (adapterFor Npm) `shouldBe` Just True

    it "keeps the registered RubyGems capability distinct from PyPI" $
        fmap adapterEcosystem (adapterFor RubyGems) `shouldBe` Just RubyGems
  where
    charged CachedWorkerRead = False
    charged (ChargedWorkerRead _) = True
    charged (NativeChargedWorkerRead _) = True
