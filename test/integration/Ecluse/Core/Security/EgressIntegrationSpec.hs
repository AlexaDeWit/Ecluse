-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Egress and credential-redirect checks against an in-process npm upstream.
module Ecluse.Core.Security.EgressIntegrationSpec (spec) where

import Data.Aeson (Value, encode, object, (.=))
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Network.HTTP.Client (Request (redirectCount), defaultManagerSettings, newManager)
import Network.HTTP.Types (status200, status302, status503)
import Network.HTTP.Types.Header (hLocation)
import Network.Wai.Handler.Warp (Port)
import Test.Hspec

import Ecluse.Core.Credential (bareCredential, mkSecret)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageName, mkPackageName)
import Ecluse.Core.Registry (FetchFault, RegistryResponse (responseBody, responseStatusCode))
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient)
import Ecluse.Core.Registry.Npm.Request (MetadataForm (Abbreviated), metadataRequest)
import Ecluse.Core.Registry.Origin (OriginClient (ocToken))
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), Limits (maxMetadataBytes, progressFloor), defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Test.Deadline (withExchangeClient)
import Ecluse.Test.Registry.Metadata.Fetch (fetchTestBody)
import Ecluse.Test.Registry.Npm (defaultNpmConfig)
import Ecluse.Test.Registry.Npm.Metadata (fetchMetadataFormBounded)
import Ecluse.Test.Stub (Stub, allCaptured, headerValue, stubBaseUrl, stubPort, withStub, withStubHeaders)
import Ecluse.Test.Support (expectRight)

-- | Exercise test-only loopback HTTP and refused redirects through the npm fetch path.
spec :: Spec
spec = do
    describe "egress over the validating manager (loopback http opt-in)" $ do
        it (caseLabel "integration/egress/native-001" "reaches a loopback upstream addressed through the test-only http opt-in") $
            withUpstream $ \upstream -> do
                withExchangeClient (newManager defaultManagerSettings) $ \exchanges -> do
                    response <- fetchMetadata exchanges (stubPort upstream) Nothing
                    fmap responseBody response `shouldBe` Right (toStrict (encode packument))
                    map (headerValue "Authorization") <$> allCaptured upstream `shouldReturn` [Nothing]

        it (caseLabel "integration/egress/native-002" "uses the same validating manager for a credential-forwarding (private-origin) fetch") $
            -- The split is the credential, not the manager: a token-forwarding read reaches
            -- the same loopback upstream over the same validating manager, carrying the token.
            withUpstream $ \upstream -> do
                withExchangeClient (newManager defaultManagerSettings) $ \exchanges -> do
                    response <- fetchMetadata exchanges (stubPort upstream) (Just "tok")
                    fmap responseBody response `shouldBe` Right (toStrict (encode packument))
                    map (headerValue "Authorization") <$> allCaptured upstream `shouldReturn` [Just "Bearer tok"]

    describe "no upstream redirect is followed (redirectCount = 0)" $ do
        it (caseLabel "integration/egress/native-003" "returns the original 302 without contacting the local target") $
            withRedirector $ \redirector target -> do
                withExchangeClient (newManager defaultManagerSettings) $ \exchanges -> do
                    result <- fetchMetadata exchanges (stubPort redirector) Nothing
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

fetchMetadata :: ExchangeClient -> Port -> Maybe Text -> IO (Either FetchFault RegistryResponse)
fetchMetadata manager port token =
    fetchMetadataFormBounded (clientConfig manager port token) Abbreviated thing

-- An origin pointed at the loopback upstream on @port@. Its base URL comes from the
-- test-only plain-HTTP opt-in, a constructor a release build does not have.
clientConfig :: ExchangeClient -> Port -> Maybe Text -> OriginClient
clientConfig manager port token =
    (defaultNpmConfig (loopbackRegistryUrl ("http://127.0.0.1:" <> show port)) manager)
        { ocToken = bareCredential . mkSecret <$> token
        }

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
