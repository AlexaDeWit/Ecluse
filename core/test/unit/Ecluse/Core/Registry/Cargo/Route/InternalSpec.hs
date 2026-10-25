-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Sparse path captures and native archive identity validation.
module Ecluse.Core.Registry.Cargo.Route.InternalSpec (spec) where

import Network.HTTP.Types (Method, methodGet, methodHead, methodPut)
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm))
import Ecluse.Core.Package (mkPackageName, renderPackageName)
import Ecluse.Core.Registry.Cargo.Route.Internal (artifactCoordinate, cargoRoutes, takeIndexPath)
import Ecluse.Core.Server.Path (unFilename)
import Ecluse.Core.Server.Route (Route (routeName), RouteName (RouteName), matchRoute)
import Ecluse.Core.Version (mkVersion)

spec :: Spec
spec = do
    describe "takeIndexPath" $ do
        it "checks native prefixes for every name length" $
            for_ [["1", "a"], ["2", "ab"], ["3", "a", "abc"], ["ab", "cd", "abcde"]] $ \path ->
                fmap (first renderPackageName) (takeIndexPath path) `shouldBe` ((\raw -> (raw, [])) <$> listToMaybe (reverse path))
        it "rejects wrong prefixes, upper-case lookups and excess segments" $
            for_ [["ab", "xx", "abcde"], ["AB", "cd", "abcde"], ["ab", "cd", "Abcde"], ["3", "a", "abc", "extra"], [".."], []] $ \path ->
                takeIndexPath path `shouldBe` Nothing

    describe "artifactCoordinate" $ do
        let name = mkPackageName Cargo Nothing "my-crate"
            version = mkVersion Cargo "1.2.3-beta.4"
        it "preserves the exact native filename" $
            fmap unFilename (artifactCoordinate name version "my-crate-1.2.3-beta.4.crate")
                `shouldBe` Just "my-crate-1.2.3-beta.4.crate"
        it "rejects disagreement in any archive claim" $
            for_ ["other-1.2.3-beta.4.crate", "my-crate-1.2.3.crate", "My-crate-1.2.3-beta.4.crate", "../my-crate-1.2.3-beta.4.crate"] $ \file ->
                artifactCoordinate name version file `shouldBe` Nothing
        it "rejects a foreign ecosystem with the same rendered name" $
            artifactCoordinate (mkPackageName Npm Nothing "my-crate") version "my-crate-1.2.3-beta.4.crate" `shouldBe` Nothing

    describe "native route selection" $ do
        for_ [methodGet, methodHead] $ \method -> do
            it "claims each view's discovery and sparse records" $ do
                claimed method ["config.json"] `shouldBe` Just (RouteName "public.discovery")
                claimed method ["private", "config.json"] `shouldBe` Just (RouteName "private.discovery")
                claimed method ["se", "rd", "serde"] `shouldBe` Just (RouteName "public.index")
                claimed method ["private", "se", "rd", "serde"] `shouldBe` Just (RouteName "private.index")
            it "claims the explicit archive route" $
                claimed method ["crates", "my-crate", "1.2.3-beta.4", "my-crate-1.2.3-beta.4.crate"]
                    `shouldBe` Just (RouteName "public.archive")
        it "refuses the public publication path" $
            claimed methodPut ["api", "v1", "crates", "new"] `shouldBe` Nothing
        it "claims only the native private publication path" $
            claimed methodPut ["private", "api", "v1", "crates", "new"] `shouldBe` Just (RouteName "private.publish")

claimed :: Method -> [Text] -> Maybe RouteName
claimed method path = fmap (routeName . fst) (matchRoute cargoRoutes method [] path)
