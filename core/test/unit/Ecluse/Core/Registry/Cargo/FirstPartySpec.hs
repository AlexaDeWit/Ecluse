-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Cargo ownership uses native aliases without claiming adjacent names.
module Ecluse.Core.Registry.Cargo.FirstPartySpec (spec) where

import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI))
import Ecluse.Core.Package (mkPackageName, mkScope)
import Ecluse.Core.Registry.Cargo.FirstParty (cargoFirstPartyName, projectFirstPartyEntry)

-- | Native Cargo ownership and prefix examples.
spec :: Spec
spec = do
    for_ ["Acme_Tools", "acme-*", "ACME_*", "acme--*"] $ \entry ->
        it ("accepts " <> show entry) $
            projectFirstPartyEntry entry `shouldSatisfy` isRight
    for_ ["", "*", "acme*", "acme.*", "@acme", "acme/tools", "acme-**", "1crate"] $ \entry ->
        it ("refuses " <> show entry) $
            projectFirstPartyEntry entry `shouldSatisfy` isLeft
    it "matches exact ownership aliases without claiming repeated separators" $
        verdicts "Acme_Tools" ["acme-tools", "ACME_TOOLS", "acme__tools", "acme.tools", "acme-toolsmith"]
            `shouldBe` [True, True, False, False, False]
    it "matches prefixes at their declared separator without claiming the bare name" $
        verdicts "acme_*" ["acme-tools", "Acme_Tools", "acme--tools", "acme", "acmeco", "acme.tools"]
            `shouldBe` [True, True, True, False, False, False]
    it "preserves repeated separators in prefix declarations" $
        verdicts "acme--*" ["acme-tools", "acme__tools", "acme--tools"]
            `shouldBe` [False, True, True]
    it "excludes other ecosystems and scoped names" $
        case projectFirstPartyEntry "acme-*" of
            Left err -> expectationFailure (show err)
            Right entry ->
                map
                    (cargoFirstPartyName (entry :| []))
                    [ mkPackageName Npm Nothing "acme-tools"
                    , mkPackageName PyPI Nothing "acme-tools"
                    , mkPackageName Cargo (Just (mkScope "acme")) "tools"
                    ]
                    `shouldBe` [False, False, False]

verdicts :: Text -> [Text] -> [Bool]
verdicts declaration names = case projectFirstPartyEntry declaration of
    Left _ -> []
    Right entry -> map (cargoFirstPartyName (entry :| []) . mkPackageName Cargo Nothing) names
