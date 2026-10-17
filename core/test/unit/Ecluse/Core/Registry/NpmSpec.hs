-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The npm origin fixture's wiring.
module Ecluse.Core.Registry.NpmSpec (spec) where

import Network.HTTP.Client (defaultManagerSettings, newManager)
import Test.Hspec (Spec, describe, it, shouldBe)
import UnliftIO (evaluate)

import Ecluse.Core.Registry.Origin (OriginClient (..))
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress (mkRegistryUrl, registryUrlText)
import Ecluse.Test.Registry.Npm (defaultNpmConfig, publicRegistryBaseUrl)

spec :: Spec
spec = configAndWiringSpec

configAndWiringSpec :: Spec
configAndWiringSpec = describe "config wiring" $ do
    it "defaultNpmConfig targets the public registry anonymously over the given manager" $ do
        manager <- newManager defaultManagerSettings
        -- The production https-only former accepts the public registry, so this fixture needs
        -- no loopback opt-in to reach it.
        base <- either (fail . toString) pure (mkRegistryUrl publicRegistryBaseUrl)
        let config = defaultNpmConfig base manager
        registryUrlText (ocBaseUrl config) `shouldBe` publicRegistryBaseUrl
        isJust (ocToken config) `shouldBe` False
        -- The secure-default bounds apply to an anonymous public fetch out of the box. A
        -- deployment overrides them per its budget.
        ocLimits config `shouldBe` defaultLimits
        -- A 'Manager' is opaque (no Eq/Show), so forcing it to WHNF is the
        -- assertion that the field carries the manager we passed, not a bottom.
        _ <- evaluate (ocManager config)
        pure ()
