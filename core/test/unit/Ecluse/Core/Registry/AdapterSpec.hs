-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Ecosystem dispatch and unsupported rule declarations.
module Ecluse.Core.Registry.AdapterSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems))
import Ecluse.Core.Registry.Adapter (adapterEcosystem, adapterFor, adapterRuleBlocks)

import Ecluse.Core.Rules.Types (RuleType (AllowScopeType, DenyIfDeprecatedType, DenyIfYankedType, DenyInstallTimeExecutionType))

-- | Pin ecosystem dispatch and the adapter's declared rule blocks.
spec :: Spec
spec = describe "adapterFor (the ecosystem adapter registry)" $ do
    it "resolves npm to the adapter registered under its own ecosystem tag" $
        (adapterEcosystem <$> adapterFor Npm) `shouldBe` Just Npm

    it "resolves PyPI to the adapter registered under its own ecosystem tag" $
        (adapterEcosystem <$> adapterFor PyPI) `shouldBe` Just PyPI

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

    it "resolves RubyGems to no adapter (unsupported by the build, a loud miss)" $
        (adapterEcosystem <$> adapterFor RubyGems) `shouldBe` Nothing
