-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Egress, TLS validation, and credential-redirect checks against in-process npm upstreams.
module Ecluse.Core.Security.EgressIntegrationSpec (spec) where

import Data.Aeson (Value, encode, object, (.=))
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Network.HTTP.Client (Manager, Request (redirectCount), defaultManagerSettings, newManager)
import Network.HTTP.Types (status200, status302, status503)
import Network.HTTP.Types.Header (hLocation)
import Network.Wai.Handler.Warp (Port)
import Test.Hspec

import Ecluse.Composition.Plan (BootInputs (..), BootReport (brOutcome), resolveBootPlan)
import Ecluse.Composition.Types (BootRole (BootMirrorPipeline), MirrorRole (ServeOnly))
import Ecluse.Config (loadConfig, renderConfigError)
import Ecluse.Core.Credential (bareCredential, mkSecret)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Fault (TransportCause (TransportTls), TransportFault (tfCause))
import Ecluse.Core.Package (PackageName, mkPackageName)
import Ecluse.Core.Registry (FetchFault (FetchTransport), RegistryResponse (responseBody, responseStatusCode))
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient)
import Ecluse.Core.Registry.Npm.Request (MetadataForm (Abbreviated), metadataRequest)
import Ecluse.Core.Registry.Origin (OriginClient (ocToken))
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), Limits (maxMetadataBytes, progressFloor), defaultLimits)
import Ecluse.Core.Security.Egress (RegistryUrl, mkRegistryUrl)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Rts (EffectiveAxis (..), EffectiveRuntimePlan (..), Provenance (FromRts))
import Ecluse.Service.Internal (dataPlaneManagers)
import Ecluse.Test.Deadline (withExchangeClient)
import Ecluse.Test.Registry.Metadata.Fetch (fetchTestBody)
import Ecluse.Test.Registry.Npm (defaultNpmConfig)
import Ecluse.Test.Registry.Npm.Metadata (fetchMetadataFormBounded)
import Ecluse.Test.Stub (Stub, allCaptured, headerValue, stubBaseUrl, stubPort, withStub, withStubHeaders)
import Ecluse.Test.Stub.Socket (withTlsServer)
import Ecluse.Test.Support (expectRight)

-- | Exercise test-only loopback HTTP, TLS refusal, and refused redirects through the npm fetch path.
spec :: Spec
spec = do
    describe "egress over the validating manager (loopback http opt-in)" $ do
        it (caseLabel "integration/egress/native-001" "reaches a loopback upstream addressed through the test-only http opt-in") $
            withUpstream $ \upstream -> do
                withExchangeClient (newManager defaultManagerSettings) $ \exchanges -> do
                    response <- fetchMetadata exchanges (loopback (stubPort upstream)) Nothing
                    fmap responseBody response `shouldBe` Right (toStrict (encode packument))
                    map (headerValue "Authorization") <$> allCaptured upstream `shouldReturn` [Nothing]

        it (caseLabel "integration/egress/native-002" "uses the same validating TLS settings for a credential-forwarding (private-origin) fetch") $ do
            (public, private) <- productManagers
            -- A refused handshake leaves the upstream with no completed session, so no request and no token reaches it.
            refusals <- forM [(public, Nothing), (private, Just "tok")] $ \(manager, token) ->
                withUntrustedUpstream $ \port sessions ->
                    withExchangeClient (pure manager) $ \exchanges -> do
                        upstream <- expectRight (mkRegistryUrl ("https://127.0.0.1:" <> show port))
                        result <- fetchMetadata exchanges upstream token
                        (transportCause result,) <$> sessions
            refusals `shouldBe` [(Just TransportTls, 0), (Just TransportTls, 0)]
            -- The same private manager forwards the token to an upstream it accepts.
            withUpstream $ \upstream ->
                withExchangeClient (pure private) $ \exchanges -> do
                    response <- fetchMetadata exchanges (loopback (stubPort upstream)) (Just "tok")
                    fmap responseBody response `shouldBe` Right (toStrict (encode packument))
                    map (headerValue "Authorization") <$> allCaptured upstream `shouldReturn` [Just "Bearer tok"]

    describe "no upstream redirect is followed (redirectCount = 0)" $ do
        it (caseLabel "integration/egress/native-003" "returns the original 302 without contacting the local target") $
            withRedirector $ \redirector target -> do
                withExchangeClient (newManager defaultManagerSettings) $ \exchanges -> do
                    result <- fetchMetadata exchanges (loopback (stubPort redirector)) Nothing
                    fmap responseStatusCode result `shouldBe` Right 302
                    length <$> allCaptured redirector `shouldReturn` 1
                    allCaptured target `shouldReturn` []

        it (caseLabel "integration/egress/native-004" "records a followed redirect even when the target rejects the fetch") $
            withRedirector $ \redirector target -> do
                withExchangeClient (newManager defaultManagerSettings) $ \exchanges -> do
                    request <- expectRight (metadataRequest (stubBaseUrl redirector) Nothing Abbreviated thing)
                    result <- fetchTestBody exchanges (progressFloor defaultLimits) (MetadataBodyLimit (maxMetadataBytes defaultLimits)) request{redirectCount = 1}
                    fmap responseStatusCode result `shouldBe` Right 503
                    length <$> allCaptured redirector `shouldReturn` 1
                    length <$> allCaptured target `shouldReturn` 1

fetchMetadata :: ExchangeClient -> RegistryUrl -> Maybe Text -> IO (Either FetchFault RegistryResponse)
fetchMetadata manager base token =
    fetchMetadataFormBounded (clientConfig manager base token) Abbreviated thing

clientConfig :: ExchangeClient -> RegistryUrl -> Maybe Text -> OriginClient
clientConfig manager base token =
    (defaultNpmConfig base manager)
        { ocToken = bareCredential . mkSecret <$> token
        }

-- The loopback upstream on @port@, through the test-only plain-HTTP opt-in, a constructor a release build does not have.
loopback :: Port -> RegistryUrl
loopback port = loopbackRegistryUrl ("http://127.0.0.1:" <> show port)

transportCause :: Either FetchFault a -> Maybe TransportCause
transportCause = \case
    Left (FetchTransport fault) -> Just (tfCause fault)
    _ -> Nothing

{- The public and private managers a serving role builds, from the smallest configuration that
serves. The unit tier's plan fixtures live in that suite's own source tree, which this one does not build.
-}
productManagers :: IO (Manager, Manager)
productManagers = do
    config <- either (fail . toString . unlines . map renderConfigError) pure (loadConfig servingEnv Nothing)
    plan <- expectRight (brOutcome (resolveBootPlan (BootMirrorPipeline ServeOnly) (BootInputs servingEnv Nothing config fixedRuntime 1024)))
    dataPlaneManagers plan
  where
    servingEnv = [("ECLUSE_MOUNTS__NPM__ENABLED", "true"), ("ECLUSE_SERVER__PUBLIC_URL", "https://registry.example.test"), ("ECLUSE_SERVER__PORT", "0")]
    fixedRuntime =
        EffectiveRuntimePlan
            { erpCapabilities = EffectiveAxis{axDesired = 2, axObserved = 2, axProvenance = FromRts}
            , erpMaxHeapBytes = EffectiveAxis{axDesired = Nothing, axObserved = Nothing, axProvenance = FromRts}
            , erpAllocAreaBytes = 4 * 1024 * 1024
            , erpAllocAreaProvenance = FromRts
            , erpNurseryChunkBytes = Nothing
            , erpContainerMemoryBytes = Nothing
            }

-- An HTTPS upstream whose certificate no trust store holds, with the count of TLS sessions it completed.
withUntrustedUpstream :: (Port -> IO Int -> IO a) -> IO a
withUntrustedUpstream action = do
    sessions <- newIORef 0
    withTlsServer (\_ _ -> atomicModifyIORef' sessions (\n -> (n + 1, ()))) $ \port -> action port (readIORef sessions)

-- Run an action against an in-process upstream serving the packument on loopback.
withUpstream :: (Stub -> IO a) -> IO a
withUpstream = withStub status200 (encode packument)

withRedirector :: (Stub -> Stub -> IO a) -> IO a
withRedirector action =
    withStub status503 "target rejected" $ \target ->
        withStubHeaders status302 [(hLocation, encodeUtf8 (stubBaseUrl target <> "/elsewhere"))] "" $ \redirector ->
            action redirector target

-- A minimal packument body the upstream serves. The test asserts on the bytes, not
-- their structure, so an opaque object is enough.
packument :: Value
packument = object ["name" .= ("thing" :: Text), "versions" .= object []]

thing :: PackageName
thing = mkPackageName Npm Nothing "thing"
