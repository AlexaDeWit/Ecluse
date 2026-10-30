-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Ecosystem dispatch and unsupported rule declarations.
module Ecluse.Core.Registry.AdapterSpec (spec) where

import Prelude hiding (universe)

import Data.Map.Strict qualified as Map
import Data.Universe.Class (Universe (universe))
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI, RubyGems))
import Ecluse.Core.Mirror.Documents (MirrorDocuments (mdCompanions))
import Ecluse.Core.Package (mkPackageName)
import Ecluse.Core.Registry (CompanionRequirement (DeclaredCompanions, NativeCompanions))
import Ecluse.Core.Registry.Adapter (adapterEcosystem, adapterFor, adapterMetadata, adapterPublish, adapterRuleBlocks, uploadEndpointFor)
import Ecluse.Core.Registry.Adapter.Capability (
    AdapterMetadata (metadataChargeFactors),
    AdapterPublish (publishMirrorRequirements),
    MirrorRequirements (mirrorStateRequirement),
    UploadEndpointRequirement (ExplicitUploadEndpoint, ReadRootUpload),
    companionsAgree,
    durableMirrorDocuments,
 )
import Ecluse.Core.Registry.Cargo.Document (cargoDocument)
import Ecluse.Core.Registry.Cargo.Metadata (cargoChargeFactors)
import Ecluse.Core.Registry.MirrorUpload (MirrorReadData (CargoMirrorRead, PyPIMirrorRead, RubyGemsMirrorRead), readDataCompanions)
import Ecluse.Core.Registry.PyPI.Document (simpleDocument)
import Ecluse.Core.Registry.RubyGems.Document (GemDocument (GemDocument))
import Ecluse.Core.Registry.RubyGems.Metadata (rubyGemsChargeFactors)

import Ecluse.Core.Rules.Types (RuleType (AllowScopeType, DenyExternalDependenciesType, DenyIfDeprecatedType, DenyIfYankedType, DenyInstallTimeExecutionType))

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

    it "blocks exactly Cargo's install, dependency, deprecation and scope types with their reasons" $
        (adapterRuleBlocks <$> adapterFor Cargo)
            `shouldBe` Just
                ( Map.fromList
                    [ (DenyInstallTimeExecutionType, "Cargo's sparse index does not declare build scripts or procedural macros. This rule is unsupported for the ecosystem.")
                    , (DenyExternalDependenciesType, "The Cargo adapter does not project where a public crate's dependencies install from into rule evidence.")
                    , (DenyIfDeprecatedType, "Cargo's sparse index supplies no deprecation flag.")
                    , (AllowScopeType, "Cargo package names have no scope.")
                    ]
                )

    it "blocks the unsupported yank signal on npm" $
        (adapterRuleBlocks <$> adapterFor Npm) `shouldBe` Just (Map.singleton DenyIfYankedType "npm reports no yank flag. Removed releases disappear from its metadata.")

    it "blocks exactly the unsuitable PyPI types with their reasons" $
        (adapterRuleBlocks <$> adapterFor PyPI)
            `shouldBe` Just
                ( Map.fromList
                    [ (DenyInstallTimeExecutionType, "Most PyPI releases offer a source distribution, which runs its own build on install, so this rule would deny nearly every release.")
                    , (DenyExternalDependenciesType, "The PyPI adapter does not read where a release's dependencies install from.")
                    , (DenyIfDeprecatedType, "The PyPI adapter supplies no deprecation flag.")
                    , (AllowScopeType, "A PyPI project name has no scope, so this rule can never match one.")
                    ]
                )

    it "resolves RubyGems with its initial measured native charge factors" $ do
        (adapterEcosystem <$> adapterFor RubyGems) `shouldBe` Just RubyGems
        (metadataChargeFactors . adapterMetadata <$> adapterFor RubyGems) `shouldBe` Just rubyGemsChargeFactors

    it "prepares native companions exactly where each registered adapter's durable declaration requires them" $
        map (fmap companionsAgree . (adapterFor >=> adapterPublish)) [Npm, PyPI, Cargo, RubyGems]
            `shouldBe` [Just True, Just True, Just True, Just True]

    it "requires native companions only for RubyGems' durable mirror" $
        map (fmap mdCompanions . durableDocuments) [Npm, PyPI, Cargo, RubyGems]
            `shouldBe` [Nothing, Just DeclaredCompanions, Just DeclaredCompanions, Just NativeCompanions]

    it "declares each durable adapter's companion requirement as mirror state applies it to the format's read data" $
        Map.fromList [(eco, mdCompanions documents) | eco <- universe, Just documents <- [durableDocuments eco]]
            `shouldBe` Map.map readDataCompanions retainedReadData

    it "declares separate read and write endpoints for exactly PyPI and Cargo" $
        map uploadEndpointFor [Npm, PyPI, RubyGems, Cargo]
            `shouldBe` [Just ReadRootUpload, Just ExplicitUploadEndpoint, Just ReadRootUpload, Just ExplicitUploadEndpoint]

    it "blocks exactly RubyGems' install, yank, dependency, deprecation and scope types with their reasons" $
        (adapterRuleBlocks <$> adapterFor RubyGems)
            `shouldBe` Just
                ( Map.fromList
                    [ (DenyInstallTimeExecutionType, "RubyGems native builds are normal installation behaviour. This rule is unsupported for the ecosystem.")
                    , (DenyIfYankedType, "RubyGems reports no yank flag in a gem's metadata. Yanked versions disappear from it.")
                    , (DenyExternalDependenciesType, "A RubyGems dependency names only a gem and its requirements, never a source, so this rule can never match one.")
                    , (DenyIfDeprecatedType, "RubyGems metadata supplies no supported deprecation flag.")
                    , (AllowScopeType, "RubyGems package names have no scope.")
                    ]
                )

-- | The documents an ecosystem's registered adapter declares for durable version state.
durableDocuments :: Ecosystem -> Maybe MirrorDocuments
durableDocuments = adapterFor >=> adapterPublish >=> durableMirrorDocuments . mirrorStateRequirement . publishMirrorRequirements

-- | One read-data value per durable format. A durable adapter without one here fails the agreement case.
retainedReadData :: Map Ecosystem MirrorReadData
retainedReadData =
    Map.fromList
        [ (PyPI, PyPIMirrorRead (simpleDocument mempty []))
        , (Cargo, CargoMirrorRead (cargoDocument []))
        , (RubyGems, RubyGemsMirrorRead (GemDocument (mkPackageName RubyGems Nothing "thing") "" []))
        ]
