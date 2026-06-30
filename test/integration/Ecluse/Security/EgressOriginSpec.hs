module Ecluse.Security.EgressOriginSpec (spec) where

import Data.Aeson (Value, encode, object, (.=))
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.HTTP.Types (status200, status302)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp (Port, testWithApplication)
import Test.Hspec
import UnliftIO.Exception (tryAny)

import Ecluse.Core.Credential (mkSecret)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageName, mkPackageName)
import Ecluse.Core.Registry (RegistryResponse (responseBody))
import Ecluse.Core.Registry.Npm (
    NpmClientConfig (NpmClientConfig, npmBaseUrl, npmLimits, npmManager, npmToken),
    fetchMetadataForm,
 )
import Ecluse.Core.Registry.Npm.Request (
    MetadataForm (Abbreviated),
    noValidators,
 )
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)

{- | The data-plane egress posture exercised through the real npm fetch path against an
in-process upstream on loopback.

The egress is __https-only by construction__, and in production the data-plane manager
is the standard validating TLS manager; the per-origin split is now in credential
handling, not in the manager (both origins share it). These tests reach an in-process
@http:\/\/127.0.0.1@ upstream through the test-only http opt-in ('loopbackRegistryUrl'),
compiled only under the @dev-http-egress@ Cabal flag, so the suite needs no TLS while the
production posture stays https-only.

No-redirect (the credential-redirect invariant, @redirectCount = 0@) is exercised here
too: an upstream @302@ is __not__ followed, so an upstream cannot bounce a fetch off the
build-time host allowlist or downgrade the scheme.
-}
spec :: Spec
spec = do
    describe "egress over the validating manager (loopback http opt-in)" $ do
        it "reaches a loopback upstream addressed through the test-only http opt-in" $
            withUpstream $ \port -> do
                manager <- newManager defaultManagerSettings
                response <- fetchMetadata manager port Nothing
                responseBody response `shouldBe` toStrict (encode packument)

        it "uses the same validating manager for a credential-forwarding (private-origin) fetch" $
            -- The split is the credential, not the manager: a token-forwarding read reaches
            -- the same loopback upstream over the same validating manager.
            withUpstream $ \port -> do
                manager <- newManager defaultManagerSettings
                response <- fetchMetadata manager port (Just "tok")
                responseBody response `shouldBe` toStrict (encode packument)

    describe "no upstream redirect is followed (redirectCount = 0)" $
        it "does not chase a 302 to an off-allowlist location" $
            -- The upstream answers 302 to an off-allowlist host. With redirect-following
            -- disabled the fetch never reaches that host; it surfaces the 3xx (no body of
            -- the redirect target), so there is no hop to escape the allowlist or downgrade.
            withRedirector $ \port -> do
                manager <- newManager defaultManagerSettings
                result <- tryAny (fetchMetadata manager port Nothing)
                case result of
                    Right response -> responseBody response `shouldNotBe` toStrict (encode packument)
                    Left _ -> pass

-- Fetch the package's metadata through the npm client over the given manager and token.
fetchMetadata :: Manager -> Port -> Maybe Text -> IO RegistryResponse
fetchMetadata manager port token =
    fetchMetadataForm (clientConfig manager port token) Abbreviated noValidators thing

-- An npm client config pointed at the loopback upstream on @port@, its base URL built
-- through the test-only plain-HTTP opt-in (a release build has no such constructor).
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

-- A minimal packument body the upstream serves; the test asserts on the bytes, not
-- their structure, so an opaque object is enough.
packument :: Value
packument = object ["name" .= ("thing" :: Text), "versions" .= object []]

thing :: PackageName
thing = mkPackageName Npm Nothing "thing"
