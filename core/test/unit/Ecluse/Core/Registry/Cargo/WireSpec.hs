-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Strict row envelopes and supported native schema protect per-record projection.
module Ecluse.Core.Registry.Cargo.WireSpec (spec) where

import Data.Aeson (Value (..), encode, object, toJSON, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Test.Hspec

import Ecluse.Core.Registry.Cargo.Wire (CargoRecordError (..), decodeCargoRecord, recordIdentity, validateCargoRecord)
import Ecluse.Core.Security (LimitError (TooDeeplyNested))
import Ecluse.Test.Registry.Cargo.Metadata (cargoFeatureRecord, cargoTestRecord)

spec :: Spec
spec = describe "native row validation" $ do
    it "rejects duplicate members and trailing JSON values" $
        for_ ["{\"name\":\"a\",\"name\":\"b\"}", "{}{}", "{\"deps\":[{\"req\":\"*\",\"req\":\"1\"}]}"] $ \bytes ->
            decodeCargoRecord 64 bytes `shouldBe` Left (CargoRecordMalformed "malformed Cargo index record")
    it "preserves additive fields, explicit null and optional omissions" $ do
        let row = cargoTestRecord "1.0.0+build"
        decodeCargoRecord 64 (toStrict (encode row)) `shouldBe` Right row
        validateCargoRecord row `shouldBe` Right ()
        recordIdentity row `shouldBe` Just "1.0.0"
    it "accepts native default omissions without inserting fields" $ do
        let row = object ["name" .= ("fixture" :: Text), "vers" .= ("1.0.0" :: Text), "cksum" .= ("digest" :: Text), "deps" .= [object ["name" .= ("dependency" :: Text), "req" .= ("*" :: Text)]]]
        validateCargoRecord row `shouldBe` Right ()
        decodeCargoRecord 64 (toStrict (encode row)) `shouldBe` Right row
    it "preserves escaped additive text while scanning structural bytes" $ do
        let text = "a quoted \"value\", a backslash \\ and brackets {[[]]}" :: Text
        case cargoFeatureRecord "1.0.0" 2 of
            Object fields -> do
                let row = Object (KeyMap.insert "future-text" (String text) fields)
                decodeCargoRecord 4 (toStrict (encode row)) `shouldBe` Right row
                validateCargoRecord row `shouldBe` Right ()
            _ -> expectationFailure "feature fixture is not an object"
    it "rejects explicit null and wrong types for defaulted fields" $ do
        let dependency = object ["name" .= ("dependency" :: Text), "req" .= ("*" :: Text)]
        case cargoTestRecord "1.0.0" of
            Object fields -> do
                for_ [Null, Array mempty, Bool False] $ \value ->
                    validateCargoRecord (Object (KeyMap.insert "features" value fields)) `shouldBe` Left "invalid Cargo field: features"
                case dependency of
                    Object dependencyFields -> for_ ["features", "optional", "default_features"] $ \key ->
                        for_ [Null, String "invalid"] $ \value ->
                            validateCargoRecord (Object (KeyMap.insert "deps" (toJSON [Object (KeyMap.insert key value dependencyFields)]) fields)) `shouldBe` Left ("invalid Cargo field: " <> Key.toText key)
                    _ -> expectationFailure "dependency fixture is not an object"
            _ -> expectationFailure "index fixture is not an object"
    it "rejects unsupported schema without discarding its native uniqueness claim" $ do
        let row = case cargoTestRecord "1.0.0" of
                Object fields -> Object (KeyMap.insert "v" (Number 3) fields)
                other -> other
        validateCargoRecord row `shouldBe` Left "unsupported or invalid Cargo index schema"
        recordIdentity row `shouldBe` Just "1.0.0"
    it "distinguishes non-object rows and dependencies from missing required fields" $ do
        decodeCargoRecord 2 "[]" `shouldBe` Left (CargoRecordMalformed "Cargo index record is not an object")
        validateCargoRecord Null `shouldBe` Left "Cargo index record is not an object"
        recordIdentity Null `shouldBe` Nothing
        case cargoTestRecord "1.0.0" of
            Object fields -> do
                validateCargoRecord (Object (KeyMap.insert "deps" (toJSON [Null]) fields)) `shouldBe` Left "invalid Cargo dependency"
                for_ ["name", "vers", "cksum", "deps"] $ \key -> do
                    validateCargoRecord (Object (KeyMap.delete key fields)) `shouldBe` Left ("invalid Cargo field: " <> Key.toText key)
                    validateCargoRecord (Object (KeyMap.insert key Null fields)) `shouldBe` Left ("invalid Cargo field: " <> Key.toText key)
                let dependency = KeyMap.fromList [("name", String "dependency"), ("req", String "*")]
                for_ ["name", "req"] $ \key ->
                    for_ [KeyMap.delete key dependency, KeyMap.insert key Null dependency] $ \invalid ->
                        validateCargoRecord (Object (KeyMap.insert "deps" (toJSON [Object invalid]) fields)) `shouldBe` Left ("invalid Cargo field: " <> Key.toText key)
                for_ [("features2", object ["extra" .= [Bool False]]), ("features2", object ["extra" .= ("value" :: Text)]), ("yanked", String "false"), ("links", Bool False), ("rust_version", Number 174)] $ \(key, value) ->
                    validateCargoRecord (Object (KeyMap.insert key value fields)) `shouldBe` Left ("invalid Cargo field: " <> Key.toText key)
                recordIdentity (Object (KeyMap.insert "vers" (String "1.0") fields)) `shouldBe` Nothing
                recordIdentity (Object (KeyMap.insert "vers" Null fields)) `shouldBe` Nothing
                recordIdentity (Object (KeyMap.delete "vers" fields)) `shouldBe` Nothing
            _ -> expectationFailure "index fixture is not an object"
    it "refuses deep containers before constructing their JSON tree" $
        decodeCargoRecord 2 "{\"a\":[[[]]]}" `shouldBe` Left (CargoRecordBound (TooDeeplyNested 2))
    it "checks the decoded depth of scalar values after scanning containers" $ do
        decodeCargoRecord 1 "{}" `shouldBe` Right (object [])
        decodeCargoRecord 1 "{\"a\":true}" `shouldBe` Left (CargoRecordBound (TooDeeplyNested 1))
