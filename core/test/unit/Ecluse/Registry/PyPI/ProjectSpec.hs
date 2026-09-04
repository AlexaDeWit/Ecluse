-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Registry.PyPI.ProjectSpec (spec) where

import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Package (PackageName, mkPackageName)
import Ecluse.Core.Registry.PyPI.Project (
    PyPIFirstParty (PyPIOwnedName, PyPIOwnedPrefix),
    mkPyPIPrefix,
    projectFirstPartyEntry,
    pypiFirstPartyName,
    underPyPIPrefix,
 )

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
            map (maybe False (`underPyPIPrefix` pypiName "acme-tools") . mkPyPIPrefix) ["acme", "acme-", "acme-tools", "acmeco"]
                `shouldBe` [True, True, False, False]
        it "covers only its own ecosystem" $
            maybe False (`underPyPIPrefix` mkPackageName Npm Nothing "acme-tools") (mkPyPIPrefix "acme")
                `shouldBe` False

    describe "projectFirstPartyEntry" $ do
        -- The grammar the configured list is read through. A segment no distribution name or
        -- prefix can equal privileges nothing, so it fails here rather than binding at request
        -- time. "Ecluse.Config.AesonSpec" pins the same verdicts through the config surface.
        for_ entryVerdicts $ \(entry, valid) ->
            it (show entry <> (if valid then " is an entry" else " is refused")) $
                isRight (projectFirstPartyEntry entry) `shouldBe` valid

        it "reads a bare name as a canonical distribution and a starred one as a prefix" $
            case mkPyPIPrefix "widgets" of
                Nothing -> expectationFailure "widgets is a valid prefix"
                Just prefix ->
                    (projectFirstPartyEntry "Acme_Tools", projectFirstPartyEntry "widgets-*")
                        `shouldBe` (Right (PyPIOwnedName (pypiName "Acme_Tools")), Right (PyPIOwnedPrefix prefix))

    describe "pypiFirstPartyName" $ do
        it "matches a declared name on its canonical form, so one spelling has one verdict" $
            map
                (pypiFirstPartyName (PyPIOwnedName (pypiName "Acme_Tools") :| []) . pypiName)
                ["acme-tools", "Acme.TOOLS", "acme_tools", "acme-toolsmith"]
                `shouldBe` [True, True, True, False]

        it "matches under a declared prefix at the separator, and not the bare prefix" $
            -- A prefix that ran past the separator would privilege acmeco, a name the
            -- deployment does not own. The bare name is a separate declaration.
            maybe
                (expectationFailure "acme is a valid prefix")
                (\prefix -> map (pypiFirstPartyName (PyPIOwnedPrefix prefix :| []) . pypiName) ["acme-tools", "Acme.Tools", "acmeco", "acme"] `shouldBe` [True, True, False, False])
                (mkPyPIPrefix "acme")

        it "denies every name a declaration does not cover" $
            pypiFirstPartyName (PyPIOwnedName (pypiName "acme") :| []) (pypiName "beta") `shouldBe` False

-- A PyPI name, in the ecosystem whose canonical form is PEP 503's.
pypiName :: Text -> PackageName
pypiName = mkPackageName PyPI Nothing

{- Every entry the grammar must refuse or accept. A prefix is a separator then @*@, and anything
outside PEP 503's name alphabet, or that canonicalises to nothing, is refused. -}
entryVerdicts :: [(Text, Bool)]
entryVerdicts =
    [ ("acme", True)
    , ("Acme_Tools", True)
    , ("acme-*", True)
    , ("acme_*", True)
    , ("acme.*", True)
    , ("acme*", False)
    , ("-*", False)
    , ("*acme", False)
    , ("*", False)
    , ("@acme", False)
    , ("acme/tools", False)
    , ("acme tools", False)
    , (",", False)
    , (".", False)
    ]
