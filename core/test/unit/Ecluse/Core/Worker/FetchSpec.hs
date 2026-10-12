-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.Worker.FetchSpec (spec) where

import Data.ByteString qualified as BS
import Network.HTTP.Types.Status (status200, status503)
import Test.Hspec

import Ecluse.Core.Fault (TransportCause (TransportProtocol), TransportFault (tfCause))
import Ecluse.Core.Registry (FetchFault (FetchBoundExceeded, FetchTransport))
import Ecluse.Core.Registry.Npm.Request (artifactRequestByUrl)
import Ecluse.Core.Security (Limits (maxMirrorArtifactBytes), defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Worker.Fetch (fetchArtifactBytesCharged)
import Ecluse.Test.Stub (headerValue, lastCaptured, stubBaseUrl, withStub)
import Ecluse.Worker.Support (runWM, withRuntime)

spec :: Spec
spec = describe "charged worker artifact downloads" $ do
    it "retains exact bytes under a proportional reservation and sends no credential" $
        withStub status200 (toLazy body) $ \upstream ->
            withRuntime (Right ()) $ \runtime _ _ -> do
                charges <- newIORef []
                runWM runtime (fetchArtifactBytesCharged (\n -> modifyIORef' charges (n :)) defaultLimits artifactRequestByUrl (loopbackRegistryUrl (stubBaseUrl upstream)))
                    `shouldReturn` Right body
                recorded <- readIORef charges
                recorded `shouldSatisfy` (not . null)
                recorded `shouldSatisfy` any (>= BS.length body)
                headerValue "authorization" <$> lastCaptured upstream `shouldReturn` Nothing
    it "refuses a non-success response even when its body is the expected artifact" $
        withStub status503 (toLazy body) $ \upstream ->
            withRuntime (Right ()) $ \runtime _ _ -> do
                charges <- newIORef []
                outcome <- runWM runtime (fetchArtifactBytesCharged (\n -> modifyIORef' charges (n :)) defaultLimits artifactRequestByUrl (loopbackRegistryUrl (stubBaseUrl upstream)))
                outcome `shouldSatisfy` \case
                    Left (FetchTransport fault) -> tfCause fault == TransportProtocol
                    _ -> False
                readIORef charges `shouldReturn` []
    it "keeps the configured artifact bound while charging received chunks" $
        withStub status200 (toLazy body) $ \upstream ->
            withRuntime (Right ()) $ \runtime _ _ -> do
                outcome <- runWM runtime (fetchArtifactBytesCharged (const (pure ())) defaultLimits{maxMirrorArtifactBytes = 4} artifactRequestByUrl (loopbackRegistryUrl (stubBaseUrl upstream)))
                outcome `shouldSatisfy` \case
                    Left FetchBoundExceeded{} -> True
                    _ -> False

body :: ByteString
body = "verified-distribution"
