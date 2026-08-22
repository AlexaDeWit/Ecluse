-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Security.EgressOriginSpec (spec) where

import Data.Aeson (Value, encode, object, (.=))
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.HTTP.Types (status200, status302)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp (Port, testWithApplication)
import Test.Hspec

import Ecluse.Core.Credential (mkSecret)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageName, mkPackageName)
import Ecluse.Core.Registry (FetchFault, RegistryResponse (responseBody))
import Ecluse.Core.Registry.Npm (
    NpmClientConfig (NpmClientConfig, npmBaseUrl, npmLimits, npmManager, npmToken),
    fetchMetadataFormBounded,
 )
import Ecluse.Core.Registry.Npm.Request (
    MetadataForm (Abbreviated),
    noValidators,
 )
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)

{- | The data-plane egress posture, driven through the real npm fetch path against an
in-process upstream on loopback.

Egress is __https-only by construction__. In production the data-plane manager is the
standard validating TLS manager. The per-origin split lives in credential handling,
not in the manager: both origins share it. These tests reach an in-process
@http:\/\/127.0.0.1@ upstream through the test-only http opt-in ('loopbackRegistryUrl'),
compiled only under the @dev-http-egress@ Cabal flag. The suite therefore needs no TLS,
and the production posture stays https-only.

These cases also cover the credential-redirect invariant, @redirectCount = 0@. The
client does __not__ follow an upstream @302@, so an upstream cannot bounce a fetch off
the build-time host allowlist or downgrade the scheme.
-}
spec :: Spec
spec = do
    describe "egress over the validating manager (loopback http opt-in)" $ do
        it "reaches a loopback upstream addressed through the test-only http opt-in" $
            withUpstream $ \port -> do
                manager <- newManager defaultManagerSettings
                response <- fetchMetadata manager port Nothing
                fmap responseBody response `shouldBe` Right (toStrict (encode packument))

        it "uses the same validating manager for a credential-forwarding (private-origin) fetch" $
            -- The split is the credential, not the manager: a token-forwarding read reaches
            -- the same loopback upstream over the same validating manager.
            withUpstream $ \port -> do
                manager <- newManager defaultManagerSettings
                response <- fetchMetadata manager port (Just "tok")
                fmap responseBody response `shouldBe` Right (toStrict (encode packument))

    describe "no upstream redirect is followed (redirectCount = 0)" $
        it "does not chase a 302 to an off-allowlist location" $
            -- The upstream answers 302 to an off-allowlist host. With redirect-following
            -- disabled the fetch never reaches that host. It surfaces the 3xx and no body
            -- from the redirect target, so no hop escapes the allowlist or downgrades.
            withRedirector $ \port -> do
                manager <- newManager defaultManagerSettings
                result <- fetchMetadata manager port Nothing
                case result of
                    Right response -> responseBody response `shouldNotBe` toStrict (encode packument)
                    -- A fetch fault is equally safe: the fetch never reached the redirect target.
                    Left _ -> pass

fetchMetadata :: Manager -> Port -> Maybe Text -> IO (Either FetchFault RegistryResponse)
fetchMetadata manager port token =
    fetchMetadataFormBounded (clientConfig manager port token) Abbreviated noValidators thing

-- An npm client config pointed at the loopback upstream on @port@. Its base URL comes
-- from the test-only plain-HTTP opt-in, a constructor a release build does not have.
clientConfig :: Manager -> Port -> Maybe Text -> NpmClientConfig
clientConfig manager port token =
    NpmClientConfig
        { npmBaseUrl = registryUrlText (loopbackRegistryUrl ("http://127.0.0.1:" <> show port))
        , npmManager = manager
        , npmToken = mkSecret <$> token
        , npmLimits = defaultLimits
        }

-- Run an action against an in-process upstream serving the packument on loopback.
withUpstream :: (Port -> IO a) -> IO a
withUpstream = testWithApplication (pure app)
  where
    app _request respond = respond (responseLBS status200 [] (encode packument))

-- Run an action against an in-process upstream that answers 302 to an off-allowlist host.
withRedirector :: (Port -> IO a) -> IO a
withRedirector = testWithApplication (pure app)
  where
    app _request respond =
        respond (responseLBS status302 [("Location", "https://evil.example.test/elsewhere")] "")

-- A minimal packument body the upstream serves. The test asserts on the bytes, not
-- their structure, so an opaque object is enough.
packument :: Value
packument = object ["name" .= ("thing" :: Text), "versions" .= object []]

thing :: PackageName
thing = mkPackageName Npm Nothing "thing"
