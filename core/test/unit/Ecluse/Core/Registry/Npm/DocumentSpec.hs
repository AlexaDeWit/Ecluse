-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The packed packument's tree and heap bytes, and the one path a served release's tarball URL takes.
module Ecluse.Core.Registry.Npm.DocumentSpec (spec) where

import Data.Aeson (Value (Null, Number, Object, String), object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Ecluse.Core.Registry.Json.Packed (docTable, packedResident, tableResident)
import Ecluse.Core.Registry.Json.Shape (Shape (Generic))
import Ecluse.Core.Registry.JsonStream (StreamResult (..))
import Ecluse.Core.Registry.Npm.Document (PackedPackument (..), packumentResident, packumentValue, tarballHole, tarballUrl, withTarball)
import Ecluse.Test.Json (genJsonText, genValue)
import Ecluse.Test.Registry.Packed (packValue)

spec :: Spec
spec = do
    describe "tarballUrl" $ do
        it "reads the string at the release's dist.tarball" $
            tarballUrl (release (String "https://registry.npmjs.org/thing/-/thing-1.0.0.tgz"))
                `shouldBe` Just "https://registry.npmjs.org/thing/-/thing-1.0.0.tgz"

        it "reads nothing where the path holds no string" $
            map tarballUrl [release Null, release (Number 1), object ["dist" .= Null], object [], String "thing"] `shouldBe` replicate 5 Nothing

    describe "withTarball" $
        it "replaces only a string at the path, so the new URL is what tarballUrl reads" $
            hedgehog $ do
                value <- forAll (Gen.choice [release . String <$> genJsonText, genValue ["dist", "tarball", "name"]])
                url <- forAll genJsonText
                tarballUrl (withTarball url value) === (url <$ tarballUrl value)
                (if isJust (tarballUrl value) then pass else withTarball url value === value)

    describe "packumentValue" $
        it "holds each packed release under its version beside the top-level members, or refuses a damaged one" $ do
            let packedRelease = release (String "https://registry.npmjs.org/thing/-/thing-1.0.0.tgz")
            case packValue (Generic 64) tarballHole packedRelease of
                Right (StreamResult (Right (table, form)) _) -> do
                    let document = PackedPackument (KeyMap.singleton "name" "thing") table (KeyMap.singleton "1.0.0" form)
                    packumentValue document `shouldBe` Just (object ["name" .= ("thing" :: Text), "versions" .= object ["1.0.0" .= packedRelease]])
                    packumentValue document{packumentTable = docTable mempty} `shouldBe` Nothing
                    packumentResident document `shouldSatisfy` (>= tableResident table + packedResident form + 104)
                _ -> expectationFailure "did not pack the release"
  where
    release tarball = Object (KeyMap.fromList [("name", "thing"), ("dist", Object (KeyMap.singleton "tarball" tarball))])
