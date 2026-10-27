-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Fault controls mutate existing mirror objects through the emulator's authenticated HTTP interface.
module Ecluse.E2E.Harness.MirrorState (mutateMirrorStateObject, mirrorRetryAttempts) where

import Network.HTTP.Client qualified as Http
import Network.HTTP.Types (statusCode)
import Test.Hspec (shouldBe, shouldSatisfy)

import Ecluse.Core.Clock (secondsToMicros)
import Ecluse.Core.Queue (Seconds (Seconds))
import Ecluse.E2E.Harness.Advisories (advisoryBucket, s3CredentialScope)
import Ecluse.E2E.Harness.Container (containerLogPollMicros)
import Ecluse.E2E.Harness.Types (GlobalDataPlane, ministackPort)
import Ecluse.Runtime.Queue.Sqs.Internal (SqsConfig (sqsVisibilityTimeout, sqsWaitSeconds), defaultSqsConfig)

-- | Prove that the object exists before deleting or replacing its exact fixture key.
mutateMirrorStateObject :: GlobalDataPlane -> Text -> ByteString -> ByteString -> IO ()
mutateMirrorStateObject plane key verb body = do
    port <- ministackPort plane
    manager <- Http.newManager Http.defaultManagerSettings
    signature <- s3CredentialScope
    let url = "http://127.0.0.1:" <> show port <> "/" <> advisoryBucket <> "/" <> key
    initial <- Http.parseRequest (toString url)
    before <- Http.httpLbs initial{Http.requestHeaders = [("Authorization", encodeUtf8 signature)]} manager
    statusCode (Http.responseStatus before) `shouldBe` 200
    response <- Http.httpLbs initial{Http.method = verb, Http.requestBody = Http.RequestBodyBS body, Http.requestHeaders = [("Authorization", encodeUtf8 signature)]} manager
    statusCode (Http.responseStatus response) `shouldSatisfy` (`elem` [200, 204])

-- | A failed visibility reset can keep a retry hidden until its original queue lease expires.
mirrorRetryAttempts :: Text -> Int
mirrorRetryAttempts queue = 120 + secondsToMicros (fromIntegral (visibilitySeconds + sqsWaitSeconds config)) `div` containerLogPollMicros
  where
    config = defaultSqsConfig queue "us-east-1"
    Seconds visibilitySeconds = sqsVisibilityTimeout config
