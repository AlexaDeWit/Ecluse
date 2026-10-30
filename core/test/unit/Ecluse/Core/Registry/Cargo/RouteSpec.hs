-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Documented native Cargo paths keep one publication route at the registry's API base.
module Ecluse.Core.Registry.Cargo.RouteSpec (spec) where

import Network.HTTP.Types.Method (StdMethod (PUT))
import Test.Hspec

import Ecluse.Core.Registry.Cargo.Route (cargoRouteSpecs)
import Ecluse.Core.Server.RouteDescription (PathSeg (Lit), RouteSpec (rsMethod, rsPattern))

-- | Publication has exactly one documented route.
spec :: Spec
spec =
    describe "cargoRouteSpecs" $
        it "exposes publication only at the registry's API base" $
            [rsPattern route | route <- toList cargoRouteSpecs, rsMethod route == PUT]
                `shouldBe` [map Lit ["api", "v1", "crates", "new"]]
