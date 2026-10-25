-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Cargo archive renderings and documented native view paths.
module Ecluse.Core.Registry.Cargo.RouteSpec (spec) where

import Network.HTTP.Types.Method (StdMethod (PUT))
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Cargo))
import Ecluse.Core.Package (mkPackageName)
import Ecluse.Core.Registry.Cargo.Route (cargoArtifactPath, cargoRouteSpecs)
import Ecluse.Core.Server.RouteDescription (PathSeg (Lit), RouteSpec (rsMethod, rsPattern))
import Ecluse.Core.Version (mkVersion)

spec :: Spec
spec = do
    describe "cargoArtifactPath" $ do
        it "keeps all native archive claims in explicit captures" $
            cargoArtifactPath (mkPackageName Cargo Nothing "My-crate") (mkVersion Cargo "1.2.3-rc.1+build.4")
                `shouldBe` Just "crates/My-crate/1.2.3-rc.1+build.4/My-crate-1.2.3-rc.1+build.4.crate"
        it "refuses an invalid native version" $
            cargoArtifactPath (mkPackageName Cargo Nothing "crate") (mkVersion Cargo "1.2") `shouldBe` Nothing

    describe "cargoRouteSpecs" $
        it "exposes publication only below the private API base" $
            [rsPattern route | route <- toList cargoRouteSpecs, rsMethod route == PUT]
                `shouldBe` [map Lit ["private", "api", "v1", "crates", "new"]]
