-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | RubyGems ownership uses native identities without claiming adjacent names.
module Ecluse.Core.Registry.RubyGems.FirstPartySpec (spec) where

import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems))
import Ecluse.Core.Package (mkPackageName, mkScope)
import Ecluse.Core.Registry.RubyGems.FirstParty (projectFirstPartyEntry, rubyGemsFirstPartyName)

-- | Native RubyGems ownership and prefix examples.
spec :: Spec
spec = do
    for_ ["Acme_Tools", "acme-*", "ACME_*", "acme--*", "acme.*"] $ \entry ->
        it ("accepts " <> show entry) $
            projectFirstPartyEntry entry `shouldSatisfy` isRight
    for_ ["", "*", "acme*", "@acme", "acme/tools", "acme-**"] $ \entry ->
        it ("refuses " <> show entry) $
            projectFirstPartyEntry entry `shouldSatisfy` isLeft
    it "preserves case and separators in exact names" $
        verdicts "Acme_Tools" ["Acme_Tools", "ACME_TOOLS", "acme__tools", "acme.tools", "acme-toolsmith"]
            `shouldBe` [True, False, False, False, False]
    it "matches prefixes only at the exact declared separator" $
        verdicts "acme_*" ["acme_tools", "Acme_Tools", "acme--tools", "acme", "acmeco", "acme.tools"]
            `shouldBe` [True, False, False, False, False, False]
    it "matches a dot prefix without claiming a hyphen or bare name" $
        verdicts "acme.*" ["acme.tools", "acme-tools", "acme", "acmeco.tools"]
            `shouldBe` [True, False, False, False]
    it "preserves repeated separators in prefix declarations" $
        verdicts "acme--*" ["acme-tools", "acme__tools", "acme--tools"]
            `shouldBe` [False, False, True]
    it "excludes other ecosystems and scoped names" $
        case projectFirstPartyEntry "acme-*" of
            Left err -> expectationFailure (show err)
            Right entry ->
                map
                    (rubyGemsFirstPartyName (entry :| []))
                    [ mkPackageName Npm Nothing "acme-tools"
                    , mkPackageName PyPI Nothing "acme-tools"
                    , mkPackageName RubyGems (Just (mkScope "acme")) "tools"
                    ]
                    `shouldBe` [False, False, False]

verdicts :: Text -> [Text] -> [Bool]
verdicts declaration names = case projectFirstPartyEntry declaration of
    Left _ -> []
    Right entry -> map (rubyGemsFirstPartyName (entry :| []) . mkPackageName RubyGems Nothing) names
