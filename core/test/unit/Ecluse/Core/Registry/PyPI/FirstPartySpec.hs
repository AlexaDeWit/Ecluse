-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Registry.PyPI.FirstPartySpec (spec) where

import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (mkPackageName)
import Ecluse.Core.Registry.PyPI.FirstParty (
    PyPIFirstParty (PyPIOwnedName, PyPIOwnedPrefix),
    mkPyPIPrefix,
    projectFirstPartyEntry,
    pypiFirstPartyName,
    underPyPIPrefix,
 )

import Ecluse.Test.Package (unscopedPyPI)
import Ecluse.Test.Registry.PyPI (pypiEntryVerdicts)

spec :: Spec
spec = do
    describe "PyPIPrefix" $ do
        it "canonicalises a prefix, so one spelling has one verdict" $
            mkPyPIPrefix "Acme_Tools" `shouldBe` mkPyPIPrefix "acme.tools"
        it "refuses text no PyPI name can start with" $
            -- An empty or separator-only prefix would cover every name on PyPI, and a
            -- character outside PEP 503's alphabet covers none.
            map mkPyPIPrefix ["", ".", "-_-", "*", "@acme", "acme/tools"] `shouldBe` replicate 6 Nothing
        it "covers a name under the prefix, at the separator and no further" $
            -- A prefix that ran past the separator would privilege acmeco, a name the
            -- deployment does not own. The bare prefix is a name, not a family.
            map (maybe False (`underPyPIPrefix` unscopedPyPI "acme-tools") . mkPyPIPrefix) ["acme", "acme-", "acme-tools", "acmeco"]
                `shouldBe` [True, True, False, False]
        it "covers only its own ecosystem" $
            maybe False (`underPyPIPrefix` mkPackageName Npm Nothing "acme-tools") (mkPyPIPrefix "acme")
                `shouldBe` False

    describe "projectFirstPartyEntry" $ do
        for_ pypiEntryVerdicts $ \(entry, valid) ->
            it (show entry <> (if valid then " is an entry" else " is refused")) $
                isRight (projectFirstPartyEntry entry) `shouldBe` valid

        it "canonicalises exact names with case and internal separator aliases" $
            map projectFirstPartyEntry ["Acme_Tools", "acme.tools", "ACME-TOOLS", "acme._-tools"]
                `shouldBe` replicate 4 (Right (PyPIOwnedName (unscopedPyPI "acme-tools")))

        it "canonicalises all supported prefix spellings" $
            map projectFirstPartyEntry ["acme-*", "acme_*", "acme.*", "ACME-*"]
                `shouldBe` replicate 4 (projectFirstPartyEntry "acme-*")

        it "reads a bare name as a canonical distribution and a starred one as a prefix" $
            case mkPyPIPrefix "widgets" of
                Nothing -> expectationFailure "widgets is a valid prefix"
                Just prefix ->
                    (projectFirstPartyEntry "Acme_Tools", projectFirstPartyEntry "widgets-*")
                        `shouldBe` (Right (PyPIOwnedName (unscopedPyPI "Acme_Tools")), Right (PyPIOwnedPrefix prefix))

    describe "pypiFirstPartyName" $ do
        it "matches a declared name on its canonical form, so one spelling has one verdict" $
            map
                (pypiFirstPartyName (PyPIOwnedName (unscopedPyPI "Acme_Tools") :| []) . unscopedPyPI)
                ["acme-tools", "Acme.TOOLS", "acme_tools", "acme-toolsmith"]
                `shouldBe` [True, True, True, False]

        it "matches under a declared prefix at the separator, and not the bare prefix" $
            -- A prefix that ran past the separator would privilege acmeco, a name the
            -- deployment does not own. The bare name is a separate declaration.
            maybe
                (expectationFailure "acme is a valid prefix")
                (\prefix -> map (pypiFirstPartyName (PyPIOwnedPrefix prefix :| []) . unscopedPyPI) ["acme-tools", "Acme.Tools", "acmeco", "acme"] `shouldBe` [True, True, False, False])
                (mkPyPIPrefix "acme")

        it "denies every name a declaration does not cover" $
            pypiFirstPartyName (PyPIOwnedName (unscopedPyPI "acme") :| []) (unscopedPyPI "beta") `shouldBe` False
