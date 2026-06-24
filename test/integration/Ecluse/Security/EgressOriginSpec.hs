module Ecluse.Security.EgressOriginSpec (spec) where

import Data.Aeson (Value, encode, object, (.=))
import Network.HTTP.Client (Manager)
import Network.HTTP.Types (status200)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp (Port, testWithApplication)
import Test.Hspec
import UnliftIO.Exception (try)

import Ecluse.Ecosystem (Ecosystem (Npm))
import Ecluse.Package (PackageName, mkPackageName)
import Ecluse.Registry (RegistryResponse (responseBody))
import Ecluse.Registry.Npm (
    MetadataForm (Abbreviated),
    NpmClientConfig (NpmClientConfig, npmBaseUrl, npmManager, npmToken),
    fetchMetadataForm,
    noValidators,
 )
import Ecluse.Security (lowerCaseHosts)
import Ecluse.Security.Egress (
    BlockedTarget (BlockedTarget),
    newGuardedTlsManager,
    newTrustedTlsManager,
 )

{- | Per-origin trust split, exercised through the real npm fetch path against an
in-process upstream on loopback.

@127.0.0.1@ is itself an internal address, so it doubles as the "internal target"
the split must treat differently per origin — no Docker, no external network. The
public\/artifact fetches run over the __guarded__ manager (resolved-IP recheck) and
the private origin over the __trusted__ manager (no recheck), so:

* the public-origin fetch __refuses__ a fetch to the loopback upstream ('BlockedTarget'),
  proving an untrusted target on an internal address is blocked at connect time;
* the private-origin fetch __reaches__ the same loopback upstream, proving an
  operator-trusted private registry on an internal address still works.

This is the integration proof that the manager split behaves correctly per origin.
-}
spec :: Spec
spec = describe "egress trust split (per-origin, against a loopback upstream)" $ do
    it "the guarded public-origin fetch refuses a target that resolves to an internal address" $
        -- An empty internal opt-in, as the composition root installs: loopback is
        -- refused. Drives the same fetch primitive the public-origin fetch uses.
        withUpstream $ \port -> do
            manager <- newGuardedTlsManager (lowerCaseHosts mempty)
            result <- try (fetchMetadata manager port) :: IO (Either SomeException RegistryResponse)
            case result of
                Left e -> case fromException e of
                    Just (BlockedTarget host _) -> host `shouldBe` "127.0.0.1"
                    Nothing -> expectationFailure ("expected BlockedTarget, got " <> show e)
                Right _ -> expectationFailure "expected the guarded public-origin fetch to refuse loopback"

    it "the trusted private-origin fetch reaches a target on an internal address" $
        -- The private base URL is operator-configured and trusted, so its manager
        -- carries no recheck: the same loopback upstream the public-origin fetch refused is
        -- reached and its body returned.
        withUpstream $ \port -> do
            manager <- newTrustedTlsManager
            response <- fetchMetadata manager port
            responseBody response `shouldBe` toStrict (encode packument)

-- Fetch the package's metadata through the npm client over the given manager — the
-- exact primitive both serve fetches use, differing only in which manager they carry.
fetchMetadata :: Manager -> Port -> IO RegistryResponse
fetchMetadata manager port =
    fetchMetadataForm (clientConfig manager port) Abbreviated noValidators thing

-- An npm client config pointed at the loopback upstream on @port@.
clientConfig :: Manager -> Port -> NpmClientConfig
clientConfig manager port =
    NpmClientConfig
        { npmBaseUrl = "http://127.0.0.1:" <> show port
        , npmManager = manager
        , npmToken = Nothing
        }

-- Run an action against an in-process upstream serving the packument on loopback.
withUpstream :: (Port -> IO a) -> IO a
withUpstream = testWithApplication (pure app)
  where
    app _request respond = respond (responseLBS status200 [] (encode packument))

-- A minimal packument body the upstream serves; the test asserts on the bytes, not
-- their structure, so an opaque object is enough.
packument :: Value
packument = object ["name" .= ("thing" :: Text), "versions" .= object []]

thing :: PackageName
thing = mkPackageName Npm Nothing "thing"
