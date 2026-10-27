-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.QueueSpec (spec) where

import Data.Aeson (Value (..), eitherDecodeStrict', encode, object, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KM
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Package (PackageName, mkPackageName, mkScope)
import Ecluse.Core.Queue (
    DeadLetterTerminus (TerminusAbsent, TerminusAttached),
    DeliveryBudget (DeliveryBudget),
    MirrorJob (jobTriggerFilename),
    QueueMessage (QueueMessage, msgJob, msgLease, msgReceipt, msgReceiveCount),
    decodeJob,
    deliveryBudgetSpent,
    effectiveDeliveryBudget,
    encodeJob,
    mkReceiptHandle,
 )
import Ecluse.Core.Security.Egress (mkRegistryUrl)
import Ecluse.Test.Queue (sampleJob)
import Ecluse.Test.Support (expectRight)

-- | Version triggers, wire compatibility, and delivery-budget decisions.
spec :: Spec
spec = do
    describe "whole-version queue compatibility" $ do
        it "encodes v2 without an artifact location" $ do
            let body = encodeJob sampleJob
            body `shouldSatisfy` T.isInfixOf "\"schemaVersion\":2"
            body `shouldSatisfy` (not . T.isInfixOf "artifactUrl")
            decodeJob packageName mkRegistryUrl body `shouldBe` Right sampleJob
        it "accepts a v2 version trigger without a filename or legacy URL validation" $
            decodeJob packageName (const (Left "legacy URL validator consulted")) versionBody `shouldBe` Right sampleJob{jobTriggerFilename = Nothing}
        it "validates and discards a legacy location while preserving the trigger" $
            decodeJob packageName mkRegistryUrl legacyBody `shouldBe` Right sampleJob
        it "still refuses an invalid legacy location" $
            decodeJob packageName (const (Left "invalid legacy location")) legacyBody `shouldSatisfy` isLeft
        it "refuses unsupported wire schemas" $
            decodeJob packageName mkRegistryUrl (T.replace "\"schemaVersion\":2" "\"schemaVersion\":3" versionBody) `shouldSatisfy` isLeft
    describe "decodeJob field refusals" $ do
        for_ [("schema 1", legacyBody, "filename"), ("schema 2", encodeJob sampleJob, "triggerFilename")] $ \(schema, body, filenameKey) -> do
            it (schema <> " control decodes") $
                decodeJob packageName mkRegistryUrl body `shouldBe` Right sampleJob
            for_
                [ ("unsafe filename", filenameKey, Just (String "../../etc/passwd"), "artifact filename")
                , ("missing traceparent", "traceContext", Just (object ["tracestate" .= ("ecluse=1" :: Text)]), "traceparent")
                , ("non-object traceContext", "traceContext", Just (String "just-a-string"), "RemoteSpanContext")
                ]
                $ \(label, key, replacement, errorField) ->
                    it (schema <> " rejects " <> label <> " at its field") $ do
                        invalid <- changeField key replacement body
                        assertFieldError errorField invalid
        it "rejects a missing legacy filename after validating the URL" $ do
            invalid <- changeField "filename" Nothing legacyBody
            assertFieldError "filename" invalid
        it "rejects non-JSON" $
            decodeJob packageName mkRegistryUrl "not json at all" `shouldSatisfy` isLeft
        it "rejects a JSON value that is not an object" $
            decodeJob packageName mkRegistryUrl "[1,2,3]" `shouldSatisfy` isLeft
        it "rejects a missing legacy artifactUrl at its field" $ do
            invalid <- changeField "artifactUrl" Nothing legacyBody
            assertFieldError "artifactUrl" invalid
        it "rejects an unknown ecosystem at its field" $ do
            invalid <- changeField "ecosystem" (Just (String "unknown")) legacyBody
            assertFieldError "unknown" invalid
    describe "deliveryBudgetSpent -- the shared redelivery verdict" $ do
        it "grants every delivery below the budget" $
            map (deliveryBudgetSpent (DeliveryBudget 5) . deliveredTimes) [1, 2, 3, 4]
                `shouldBe` [False, False, False, False]

        it "is spent at the budget, and stays spent past it" $
            map (deliveryBudgetSpent (DeliveryBudget 5) . deliveredTimes) [5, 6, 50]
                `shouldBe` [True, True, True]

        it "still grants a first delivery under any budget, however small" $
            -- A budget of one (or zero, or below) would otherwise retire a job that never
            -- ran. The worker retires no message before it tries that message at least once.
            map (\budget -> deliveryBudgetSpent budget (deliveredTimes 1)) [DeliveryBudget 1, DeliveryBudget 0, DeliveryBudget (-3)]
                `shouldBe` [False, False, False]

        it "retires on the second delivery under a budget too small to reach" $
            map (\budget -> deliveryBudgetSpent budget (deliveredTimes 2)) [DeliveryBudget 1, DeliveryBudget 0]
                `shouldBe` [True, True]

    describe "effectiveDeliveryBudget -- the dead-letter queue captures first" $ do
        it "raises the configured floor one delivery past an attached terminus's capture count" $
            -- Écluse must not retire the message at the configured 5 and rob the
            -- dead-letter queue: the operator's redrive policy captures it at 10.
            effectiveDeliveryBudget (DeliveryBudget 5) (TerminusAttached (Just (DeliveryBudget 10)))
                `shouldBe` DeliveryBudget 11

        it "keeps the configured floor when it already sits above the capture count" $
            effectiveDeliveryBudget (DeliveryBudget 20) (TerminusAttached (Just (DeliveryBudget 3)))
                `shouldBe` DeliveryBudget 20

        it "keeps the configured floor when a terminus declares no capture count" $
            effectiveDeliveryBudget (DeliveryBudget 5) (TerminusAttached Nothing)
                `shouldBe` DeliveryBudget 5

        it "keeps the configured floor when nothing captures poison messages" $
            -- The no-terminus case the budget exists for: the budget is the only terminus.
            effectiveDeliveryBudget (DeliveryBudget 5) TerminusAbsent `shouldBe` DeliveryBudget 5
  where
    -- A delivery of the sample job on its n-th receive. These verdicts read only the
    -- count, so the rest of the message stays fixed.
    deliveredTimes :: Int -> QueueMessage
    deliveredTimes n =
        QueueMessage{msgJob = sampleJob, msgReceipt = mkReceiptHandle "receipt", msgReceiveCount = n, msgLease = Nothing}

versionBody :: Text
versionBody = "{\"schemaVersion\":2,\"ecosystem\":\"npm\",\"name\":\"thing\",\"version\":\"1.0.0\"}"

legacyBody :: Text
legacyBody = "{\"ecosystem\":\"npm\",\"name\":\"thing\",\"version\":\"1.0.0\",\"filename\":\"thing-1.0.0.tgz\",\"artifactUrl\":\"https://old.example/thing.tgz\"}"

packageName :: Ecosystem -> Maybe Text -> Text -> Either Text PackageName
packageName ecosystem scope name = Right (mkPackageName ecosystem (mkScope <$> scope) name)

changeField :: Key -> Maybe Value -> Text -> IO Text
changeField key replacement body = do
    value <- expectRight (eitherDecodeStrict' (encodeUtf8 body))
    case value of
        Object fields -> pure (decodeUtf8 (encode (Object (maybe (KM.delete key fields) (\v -> KM.insert key v fields) replacement))))
        _ -> fail "queue control body is not an object"

assertFieldError :: Text -> Text -> Expectation
assertFieldError field body = case decodeJob packageName mkRegistryUrl body of
    Left err -> err `shouldSatisfy` T.isInfixOf field
    Right job -> expectationFailure ("expected a field refusal, got " <> show job)
