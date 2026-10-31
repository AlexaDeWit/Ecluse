-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The first-party predicate reads a mount's declaration through its adapter's own matching rules.
module Ecluse.Core.Registry.FirstPartySpec (spec) where

import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, RubyGems))
import Ecluse.Core.Package (mkPackageName, mkScope)
import Ecluse.Core.Registry.Cargo.FirstParty (cargoFirstPartyNames)
import Ecluse.Core.Registry.FirstParty (FirstParty (FirstParty), FirstPartyEntry (OwnedName), firstPartyName)
import Ecluse.Core.Registry.Npm.Adapter (npmFirstPartyNames)
import Ecluse.Core.Registry.PyPI.FirstParty (PyPIFirstParty (PyPIOwnedName), pypiFirstPartyNames)
import Ecluse.Core.Registry.RubyGems.FirstParty (rubyGemsFirstPartyNames)
import Ecluse.Test.Package (scopedNpm, thingName, unscopedPyPI)

-- | Each ecosystem's declaration owns its names, deny by default: a disagreement would be dependency confusion.
spec :: Spec
spec = describe "firstPartyName (the derived first-party predicate)" $ do
    it "matches an npm scope exactly, refusing a lookalike and an unscoped name" $
        map
            (firstPartyName (FirstParty npmFirstPartyNames (mkScope "acme" :| [mkScope "beta"])))
            [scopedNpm "acme" "thing", scopedNpm "beta" "thing", scopedNpm "acme-evil" "thing", thingName]
            `shouldBe` [True, True, False, False]

    -- PyPI's matching rules are pinned in "Ecluse.Core.Registry.PyPI.FirstPartySpec".
    it "owns names through the PyPI declaration" $
        map (firstPartyName (FirstParty pypiFirstPartyNames (PyPIOwnedName (unscopedPyPI "Acme_Tools") :| [])) . unscopedPyPI) ["acme-tools", "beta"]
            `shouldBe` [True, False]

    it "owns RubyGems names by their native identity" $
        let owned = mkPackageName RubyGems Nothing "Acme_Tools"
         in map
                (firstPartyName (FirstParty rubyGemsFirstPartyNames (OwnedName owned :| [])))
                [owned, mkPackageName RubyGems Nothing "acme-tools", mkPackageName Npm Nothing "Acme_Tools"]
                `shouldBe` [True, False, False]

    it "owns Cargo names through their collision key, refusing another ecosystem" $
        let owned = mkPackageName Cargo Nothing "acme_tools"
         in map
                (firstPartyName (FirstParty cargoFirstPartyNames (OwnedName owned :| [])))
                [owned, mkPackageName Cargo Nothing "acme-tools", mkPackageName Cargo Nothing "acme-toolsx", mkPackageName RubyGems Nothing "acme_tools"]
                `shouldBe` [True, True, False, False]
