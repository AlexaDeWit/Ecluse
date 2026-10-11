-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Registry.AdapterSpec (spec) where

import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems))
import Ecluse.Core.Registry.Adapter (adapterEcosystem, adapterFor, adapterRuleBlocks)

import Ecluse.Core.Rules.Types (RuleType (AllowScopeType, DenyInstallTimeExecutionType))

-- | Pin ecosystem dispatch and the adapter's declared rule blocks.
spec :: Spec
spec = describe "adapterFor (the ecosystem adapter registry)" $ do
    it "resolves npm to the adapter registered under its own ecosystem tag" $
        (adapterEcosystem <$> adapterFor Npm) `shouldBe` Just Npm

    it "resolves PyPI to the adapter registered under its own ecosystem tag" $
        (adapterEcosystem <$> adapterFor PyPI) `shouldBe` Just PyPI

    it "blocks no rule on npm" $
        (adapterRuleBlocks <$> adapterFor Npm) `shouldBe` Just Map.empty

    it "blocks exactly the two unsuitable PyPI types with their reasons" $
        (adapterRuleBlocks <$> adapterFor PyPI)
            `shouldBe` Just
                ( Map.fromList
                    [ (DenyInstallTimeExecutionType, "Most PyPI releases offer a source distribution, which runs its own build on install, so this rule would deny nearly every release.")
                    , (AllowScopeType, "A PyPI project name has no scope, so this rule can never match one.")
                    ]
                )

    it "resolves RubyGems to no adapter (unsupported by the build, a loud miss)" $
        (adapterEcosystem <$> adapterFor RubyGems) `shouldBe` Nothing
