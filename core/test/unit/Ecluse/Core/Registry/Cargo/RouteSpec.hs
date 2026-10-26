-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Documented native Cargo view paths preserve the private publication boundary.
module Ecluse.Core.Registry.Cargo.RouteSpec (spec) where

import Network.HTTP.Types.Method (StdMethod (PUT))
import Test.Hspec

import Ecluse.Core.Registry.Cargo.Route (cargoRouteSpecs)
import Ecluse.Core.Server.RouteDescription (PathSeg (Lit), RouteSpec (rsMethod, rsPattern))

-- | Publication remains restricted to the private API view.
spec :: Spec
spec =
    describe "cargoRouteSpecs" $
        it "exposes publication only below the private API base" $
            [rsPattern route | route <- toList cargoRouteSpecs, rsMethod route == PUT]
                `shouldBe` [map Lit ["private", "api", "v1", "crates", "new"]]
