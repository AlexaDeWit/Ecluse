-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Registry.AdapterSpec (spec) where

import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems))
import Ecluse.Core.Registry.Adapter (adapterEcosystem, adapterFor)

{- | Pins for the adapter registry's dispatch. Additivity itself is
compiler-enforced ('Ecluse.Core.Registry.Adapter.adapterFor' is a total case over
the closed 'Ecosystem' sum, so an added ecosystem is a compile error until its arm
exists); what these examples pin is each arm's answer -- the build-support fact,
independent of configuration. An unsupported ecosystem answers 'Nothing' here; a
supported-but-unconfigured one is simply never activated (no registry involvement);
a configured-but-unsupported one is the loud boot error
"Ecluse.CompositionSpec" pins over 'Ecluse.Composition.planMounts'.
-}
spec :: Spec
spec = describe "adapterFor (the ecosystem adapter registry)" $ do
    it "resolves npm to the adapter registered under its own ecosystem tag" $
        (adapterEcosystem <$> adapterFor Npm) `shouldBe` Just Npm

    it "resolves PyPI to no adapter (unsupported by the build, a loud miss)" $
        (adapterEcosystem <$> adapterFor PyPI) `shouldBe` Nothing

    it "resolves RubyGems to no adapter (unsupported by the build, a loud miss)" $
        (adapterEcosystem <$> adapterFor RubyGems) `shouldBe` Nothing
