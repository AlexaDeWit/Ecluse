-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Strict row envelopes and supported native schema protect per-record projection.
module Ecluse.Core.Registry.Cargo.WireSpec (spec) where

import Data.Aeson (Value (..), encode, object, toJSON, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Test.Hspec

import Ecluse.Core.Registry.Cargo.Wire (CargoRecordError (..), decodeCargoRecord, recordIdentity, validateCargoRecord)
import Ecluse.Core.Security (LimitError (TooDeeplyNested))
import Ecluse.Test.Registry.Cargo.Metadata (cargoTestRecord)

spec :: Spec
spec = describe "native row validation" $ do
    it "rejects duplicate members and trailing JSON values" $
        for_ ["{\"name\":\"a\",\"name\":\"b\"}", "{}{}", "{\"deps\":[{\"req\":\"*\",\"req\":\"1\"}]}"] $ \bytes ->
            decodeCargoRecord 64 bytes `shouldSatisfy` isLeft
    it "preserves additive fields, explicit null and optional omissions" $ do
        let row = cargoTestRecord "1.0.0+build"
        decodeCargoRecord 64 (toStrict (encode row)) `shouldBe` Right row
        validateCargoRecord row `shouldBe` Right ()
        recordIdentity row `shouldBe` Just "1.0.0"
    it "accepts native default omissions without inserting fields" $ do
        let row = object ["name" .= ("fixture" :: Text), "vers" .= ("1.0.0" :: Text), "cksum" .= ("digest" :: Text), "deps" .= [object ["name" .= ("dependency" :: Text), "req" .= ("*" :: Text)]]]
        validateCargoRecord row `shouldBe` Right ()
        decodeCargoRecord 64 (toStrict (encode row)) `shouldBe` Right row
    it "rejects explicit null and wrong types for defaulted fields" $ do
        let dependency = object ["name" .= ("dependency" :: Text), "req" .= ("*" :: Text)]
        case cargoTestRecord "1.0.0" of
            Object fields -> do
                for_ [Null, Array mempty, Bool False] $ \value ->
                    validateCargoRecord (Object (KeyMap.insert "features" value fields)) `shouldSatisfy` isLeft
                case dependency of
                    Object dependencyFields -> for_ ["features", "optional", "default_features"] $ \key ->
                        for_ [Null, String "invalid"] $ \value ->
                            validateCargoRecord (Object (KeyMap.insert "deps" (toJSON [Object (KeyMap.insert key value dependencyFields)]) fields)) `shouldSatisfy` isLeft
                    _ -> expectationFailure "dependency fixture is not an object"
            _ -> expectationFailure "index fixture is not an object"
    it "rejects unsupported schema without discarding its native uniqueness claim" $ do
        let row = case cargoTestRecord "1.0.0" of
                Object fields -> Object (KeyMap.insert "v" (Number 3) fields)
                other -> other
        validateCargoRecord row `shouldSatisfy` isLeft
        recordIdentity row `shouldBe` Just "1.0.0"
    it "refuses deep containers before constructing their JSON tree" $
        decodeCargoRecord 2 "{\"a\":[[[]]]}" `shouldBe` Left (CargoRecordBound (TooDeeplyNested 2))
