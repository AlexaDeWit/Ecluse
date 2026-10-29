-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Ecosystem dispatch and unsupported rule declarations.
module Ecluse.Core.Registry.AdapterSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI, RubyGems))
import Ecluse.Core.Registry.Adapter (adapterEcosystem, adapterFor, adapterMetadata, adapterRuleBlocks)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataChargeFactors))
import Ecluse.Core.Registry.Cargo.Metadata (cargoChargeFactors)
import Ecluse.Core.Registry.RubyGems.Metadata (rubyGemsChargeFactors)

import Ecluse.Core.Rules.Types (RuleType (AllowScopeType, DenyIfDeprecatedType, DenyIfYankedType, DenyInstallTimeExecutionType))

-- | Pin ecosystem dispatch and the adapter's declared rule blocks.
spec :: Spec
spec = describe "adapterFor (the ecosystem adapter registry)" $ do
    it "resolves npm to the adapter registered under its own ecosystem tag" $
        (adapterEcosystem <$> adapterFor Npm) `shouldBe` Just Npm

    it "resolves PyPI to the adapter registered under its own ecosystem tag" $
        (adapterEcosystem <$> adapterFor PyPI) `shouldBe` Just PyPI

    it "resolves Cargo with its measured native charge factors" $ do
        (adapterEcosystem <$> adapterFor Cargo) `shouldBe` Just Cargo
        (metadataChargeFactors . adapterMetadata <$> adapterFor Cargo) `shouldBe` Just cargoChargeFactors

    it "blocks exactly Cargo's absent scope and deprecation capabilities" $
        (adapterRuleBlocks <$> adapterFor Cargo)
            `shouldBe` Just
                ( Map.fromList
                    [ (AllowScopeType, "Cargo package names have no scope.")
                    , (DenyIfDeprecatedType, "Cargo's sparse index supplies no deprecation flag.")
                    ]
                )

    it "blocks the unsupported yank signal on npm" $
        (adapterRuleBlocks <$> adapterFor Npm) `shouldBe` Just (Map.singleton DenyIfYankedType "npm reports no yank flag. Removed releases disappear from its metadata.")

    it "blocks exactly the unsuitable PyPI types with their reasons" $
        (adapterRuleBlocks <$> adapterFor PyPI)
            `shouldBe` Just
                ( Map.fromList
                    [ (DenyInstallTimeExecutionType, "Most PyPI releases offer a source distribution, which runs its own build on install, so this rule would deny nearly every release.")
                    , (DenyIfDeprecatedType, "The PyPI adapter supplies no deprecation flag.")
                    , (AllowScopeType, "A PyPI project name has no scope, so this rule can never match one.")
                    ]
                )

    it "resolves RubyGems with its initial measured native charge factors" $ do
        (adapterEcosystem <$> adapterFor RubyGems) `shouldBe` Just RubyGems
        (metadataChargeFactors . adapterMetadata <$> adapterFor RubyGems) `shouldBe` Just rubyGemsChargeFactors

    it "blocks exactly RubyGems' install, deprecation and scope types with their reasons" $
        (adapterRuleBlocks <$> adapterFor RubyGems)
            `shouldBe` Just
                ( Map.fromList
                    [ (DenyInstallTimeExecutionType, "RubyGems native builds are normal installation behaviour. This rule is unsupported for the ecosystem.")
                    , (DenyIfDeprecatedType, "RubyGems metadata supplies no supported deprecation flag.")
                    , (AllowScopeType, "RubyGems package names have no scope.")
                    ]
                )
