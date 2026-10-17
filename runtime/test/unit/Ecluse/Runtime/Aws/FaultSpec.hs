-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | How an @amazonka@ failure leaves an adapter: typed, with no request in its text.
module Ecluse.Runtime.Aws.FaultSpec (spec) where

import Network.HTTP.Client (defaultManagerSettings, httpNoBody, newManager, parseRequest, requestHeaders)
import Test.Hspec

import Ecluse.Core.Fault (TransportCause (TransportUnreachable))
import Ecluse.Runtime.Aws.Fault (AwsFault (AwsTransport), AwsRequestFailed (AwsRequestFailed), retypeAwsFailure)
import Ecluse.Runtime.Test.CodeArtifact (closedEndpoint, refusedRequest)

spec :: Spec
spec = describe "retypeAwsFailure" $ do
    it "raises an amazonka transport failure by its cause at the dialled host:port, never the signed request" $
        retypeAwsFailure refusedRequest `shouldThrow` (== refusedAtClosedEndpoint)

    it "raises an http-client failure, as the container credential fetch throws, the same way" $ do
        request <- parseRequest "http://127.0.0.1:1/v2/credentials/path-secret"
        manager <- newManager defaultManagerSettings
        retypeAwsFailure (void (httpNoBody request{requestHeaders = [("X-Amz-Security-Token", "token-secret")]} manager))
            `shouldThrow` (== refusedAtClosedEndpoint)
  where
    refusedAtClosedEndpoint = AwsRequestFailed closedEndpoint (AwsTransport TransportUnreachable)
