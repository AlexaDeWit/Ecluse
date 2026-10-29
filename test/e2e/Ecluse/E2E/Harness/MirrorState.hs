-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Fault controls mutate existing mirror objects through the emulator's authenticated HTTP interface.
module Ecluse.E2E.Harness.MirrorState (readMirrorStateObject, mutateMirrorStateObject, updateMirrorStateObject, mirrorRetryAttempts) where

import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Client qualified as Http
import Network.HTTP.Types (statusCode)
import Test.Hspec (shouldBe, shouldSatisfy)

import Ecluse.Core.Clock (secondsToMicros)
import Ecluse.Core.Queue (Seconds (Seconds))
import Ecluse.E2E.Harness.Advisories (advisoryBucket, s3CredentialScope)
import Ecluse.E2E.Harness.Container (containerLogPollMicros)
import Ecluse.E2E.Harness.Types (GlobalDataPlane, ministackPort)
import Ecluse.Runtime.Queue.Sqs.Internal (SqsConfig (sqsVisibilityTimeout, sqsWaitSeconds), defaultSqsConfig)

-- | Read an existing object through the same authenticated endpoint as the fault controls.
readMirrorStateObject :: GlobalDataPlane -> Text -> IO ByteString
readMirrorStateObject plane key = withMirrorStateRequest plane key readExistingObject

-- | Prove that the object exists, mutate its exact key, then read back the result.
mutateMirrorStateObject :: GlobalDataPlane -> Text -> ByteString -> ByteString -> IO ()
mutateMirrorStateObject plane key verb body = writeExistingObject plane key verb (const (pure body))

-- | Derive a replacement from the existing object without changing other state keys.
updateMirrorStateObject :: GlobalDataPlane -> Text -> (ByteString -> IO ByteString) -> IO ()
updateMirrorStateObject plane key = writeExistingObject plane key "PUT"

writeExistingObject :: GlobalDataPlane -> Text -> ByteString -> (ByteString -> IO ByteString) -> IO ()
writeExistingObject plane key verb change = withMirrorStateRequest plane key $ \request manager -> do
    body <- readExistingObject request manager >>= change
    response <- Http.httpLbs request{Http.method = verb, Http.requestBody = Http.RequestBodyBS body} manager
    statusCode (Http.responseStatus response) `shouldSatisfy` (`elem` [200, 204])
    after <- Http.httpLbs request manager
    if verb == "DELETE"
        then statusCode (Http.responseStatus after) `shouldBe` 404
        else do
            statusCode (Http.responseStatus after) `shouldBe` 200
            Http.responseBody after `shouldBe` LBS.fromStrict body

readExistingObject :: Http.Request -> Http.Manager -> IO ByteString
readExistingObject request manager = do
    response <- Http.httpLbs request manager
    statusCode (Http.responseStatus response) `shouldBe` 200
    pure (LBS.toStrict (Http.responseBody response))

withMirrorStateRequest :: GlobalDataPlane -> Text -> (Http.Request -> Http.Manager -> IO a) -> IO a
withMirrorStateRequest plane key action = do
    port <- ministackPort plane
    manager <- Http.newManager Http.defaultManagerSettings
    signature <- s3CredentialScope
    let url = "http://127.0.0.1:" <> show port <> "/" <> advisoryBucket <> "/" <> key
    initial <- Http.parseRequest (toString url)
    action initial{Http.requestHeaders = [("Authorization", encodeUtf8 signature)]} manager

-- | A failed visibility reset can keep a retry hidden until its original queue lease expires.
mirrorRetryAttempts :: Text -> Int
mirrorRetryAttempts queue = 120 + secondsToMicros (fromIntegral (visibilitySeconds + sqsWaitSeconds config)) `div` containerLogPollMicros
  where
    config = defaultSqsConfig queue "us-east-1"
    Seconds visibilitySeconds = sqsVisibilityTimeout config
