-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | HTTP outcomes reach every metadata reader before body projection.
Successful responses retain each ecosystem's identity and decode checks.
-}
module Ecluse.Core.Registry.MetadataSpec (spec) where

import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (mkStatus)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec
import UnliftIO.Exception (throwIO, try)

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Fault (TransportCause (TransportUnreachable), transportFault)
import Ecluse.Core.Package (PackageDetails, PackageName, mkPackageName)
import Ecluse.Core.Registry (
    FetchFault (FetchBoundExceeded, FetchTransport),
    RegistryResponse (RegistryResponse),
 )
import Ecluse.Core.Registry.Metadata (
    MetadataClient (MetadataClient, fetchFullManifest, fetchVersionMetadata),
    MetadataError (MetadataAbsent, MetadataAuthorisationFailure, MetadataFetch, MetadataHttpFailure, MetadataNameMismatch, MetadataUndecodable),
    VersionEvaluation (VersionMetadataUnavailable, VersionMissing, VersionPresent),
    VersionRead,
    fetchThenProject,
    fetchVersionDetails,
 )
import Ecluse.Core.Registry.Npm.Metadata (newNpmMetadataReads)
import Ecluse.Core.Registry.Origin (perCallerOrigin)
import Ecluse.Core.Registry.PyPI.Metadata (newPyPIMetadataReads)
import Ecluse.Core.Security (LimitError (BodyTooLarge), defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Metadata (privateMetadataClient)
import Ecluse.Core.Telemetry.Span (TracingPort (spanMetadataDecode, spanMetadataFetch))
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Test.Package (sampleDetails, thingName, unscopedNpm, v1_0_0)
import Ecluse.Test.Port (noopMetricsPort, passthroughTracingPort)
import Ecluse.Test.Snapshot (versionDocOf, versionReadOf)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))

-- | Exercise error preservation and projection through the adapters' shared read step.
spec :: Spec
spec = do
    fetchStepSpec
    rawReadersSpec
    versionEvaluationSpec

fetchStepSpec :: Spec
fetchStepSpec = describe "fetchThenProject" $ do
    for_ statusOutcomes $ \(code, expected) ->
        it ("traces the requested package and decodes only successful HTTP " <> show code) $ do
            events <- newIORef ([] :: [(Text, PackageName)])
            let name = unscopedNpm "left-pad"
                record phase who action = modifyIORef' events (<> [(phase, who)]) >> action
                tracing = passthroughTracingPort{spanMetadataFetch = record "fetch", spanMetadataDecode = record "decode"}
            outcome <- fetchThenProject tracing (const (pure (Right (RegistryResponse code "body")))) name Right
            void outcome `shouldBe` expected
            readIORef events `shouldReturn` ([("fetch", name)] <> [("decode", name) | isRight expected])

    it "hands the fetched body to the projection" $
        runStep (Right (RegistryResponse 200 "the-body")) Right `shouldReturn` Right "the-body"

    it "folds an exchange fault into MetadataFetch, discarding the projection" $
        runStep (Left bodyTooLarge) (const (Right "projected"))
            `shouldReturn` (Left (MetadataFetch bodyTooLarge) :: Either MetadataError ByteString)

    it "returns a projection refusal as the mount phrased it" $
        runStep (Right (RegistryResponse 200 "junk")) (const (Left MetadataUndecodable))
            `shouldReturn` (Left MetadataUndecodable :: Either MetadataError ByteString)

    it "retains a successful response's identity refusal" $
        runStep (Right (RegistryResponse 200 "other-package")) (const (Left (MetadataNameMismatch "other-package")))
            `shouldReturn` (Left (MetadataNameMismatch "other-package") :: Either MetadataError ByteString)

    it "asks the fetch action for the requested package, once" $ do
        asked <- newIORef ([] :: [PackageName])
        let fetch name = modifyIORef' asked (name :) $> Right (RegistryResponse 200 "b")
        _ <- fetchThenProject passthroughTracingPort fetch (unscopedNpm "left-pad") Right
        readIORef asked `shouldReturn` [unscopedNpm "left-pad"]

rawReadersSpec :: Spec
rawReadersSpec = describe "raw metadata readers" $
    for_ [Npm, PyPI] $ \ecosystem ->
        for_ statusOutcomes $ \(code, expected) ->
            it (show ecosystem <> " preserves HTTP " <> show code <> " over a valid manifest body on full and version reads") $
                testWithApplication (pure (\_ respond -> respond (responseLBS (mkStatus code "test") [] (bodyFor ecosystem)))) $ \port -> do
                    manager <- newManager defaultManagerSettings
                    let name = mkPackageName ecosystem Nothing "thing"
                        origin = perCallerOrigin defaultLimits manager (loopbackRegistryUrl ("http://localhost:" <> show port)) Nothing
                        makeReads = case ecosystem of
                            PyPI -> newPyPIMetadataReads
                            _ -> newNpmMetadataReads
                        client = privateMetadataClient (makeReads passthroughTracingPort noopMetricsPort (\_ _ -> pass) (\_ _ -> pass) (const pass) origin)
                    full <- fetchFullManifest client name
                    void full `shouldBe` expected
                    single <- fetchVersionMetadata client name (mkVersion ecosystem "1.0.0")
                    void single `shouldBe` expected
  where
    bodyFor PyPI = "{\"meta\":{\"api-version\":\"1.0\"},\"name\":\"thing\",\"files\":[]}"
    bodyFor _ = "{\"name\":\"thing\",\"versions\":{}}"

versionEvaluationSpec :: Spec
versionEvaluationSpec = describe "fetchVersionDetails: the shared single-version evaluation boundary" $ do
    -- The serve-time tarball gate and the worker both resolve a version through this one
    -- function, so these cases pin its classification directly.
    it "classifies a resolved version as present" $
        fetchVersionDetails (versionClient (Right (versionReadOf (Just theDetails) (Just otherVersion)))) thingName v1_0_0
            `shouldReturn` VersionPresent (versionDocOf theDetails) (Just otherVersion)

    it "carries the document's own latest onto the present verdict" $
        fetchVersionDetails (versionClient (Right (versionReadOf (Just theDetails) Nothing))) thingName v1_0_0
            `shouldReturn` VersionPresent (versionDocOf theDetails) Nothing

    it "classifies an absent version (resolved, but no such version) as missing" $
        fetchVersionDetails (versionClient (Right (versionReadOf Nothing Nothing))) thingName v1_0_0
            `shouldReturn` VersionMissing

    it "classifies a metadata error as unavailable (the transient degrade)" $
        fetchVersionDetails (versionClient (Left MetadataUndecodable)) thingName v1_0_0
            `shouldReturn` VersionMetadataUnavailable

    it "classifies an unreachable upstream as unavailable (transport in the typed channel)" $
        fetchVersionDetails (versionClient (Left (MetadataFetch (FetchTransport (transportFault TransportUnreachable "refused"))))) thingName v1_0_0
            `shouldReturn` VersionMetadataUnavailable

    it "propagates a client that escapes its total contract (the invariant channel)" $ do
        -- The typed channel reports every real failure, so a throw out of the fetch is an
        -- invariant break. It must reach the caller's supervision, never be laundered into the
        -- transient degrade.
        outcome <- try (fetchVersionDetails throwingVersionClient thingName v1_0_0) :: IO (Either SomeException VersionEvaluation)
        case outcome of
            Left escaped -> fromException escaped `shouldBe` Just (TestContractEscape "simulated contract escape")
            Right evaluation -> expectationFailure ("expected the client's throw to reach the caller, got " <> show evaluation)

-- | The release a resolved read carries. Nothing here decides from its contents.
theDetails :: PackageDetails
theDetails = sampleDetails thingName v1_0_0

{- | A different version of the same package, so a present verdict's own latest is
distinguishable from the version that was asked for.
-}
otherVersion :: Version
otherVersion = mkVersion Npm "0.9.0"

{- | A 'MetadataClient' whose single-version read returns a fixed result. The full-manifest read
is unused here and refuses loudly.
-}
versionClient :: Either MetadataError VersionRead -> MetadataClient
versionClient result =
    MetadataClient
        { fetchFullManifest = const (throwIO (TestContractEscape "versionClient: fetchFullManifest is unused"))
        , fetchVersionMetadata = \_ _ -> pure result
        }

-- | Break the metadata handle's value-error contract, to pin exception propagation.
throwingVersionClient :: MetadataClient
throwingVersionClient =
    MetadataClient
        { fetchFullManifest = const (throwIO (TestContractEscape "throwingVersionClient: fetchFullManifest is unused"))
        , fetchVersionMetadata = \_ _ -> throwIO (TestContractEscape "simulated contract escape")
        }

statusOutcomes :: [(Int, Either MetadataError ())]
statusOutcomes =
    [(code, Right ()) | code <- [200, 201, 299]]
        <> [(code, Left (MetadataAuthorisationFailure code)) | code <- [401, 403]]
        <> [(404, Left MetadataAbsent)]
        <> [(code, Left (MetadataHttpFailure code)) | code <- [301, 304, 400, 408, 410, 429, 500, 503, 599]]

bodyTooLarge :: FetchFault
bodyTooLarge = FetchBoundExceeded (BodyTooLarge 12)

runStep ::
    Either FetchFault RegistryResponse ->
    (ByteString -> Either MetadataError a) ->
    IO (Either MetadataError a)
runStep outcome =
    fetchThenProject passthroughTracingPort (const (pure outcome)) (unscopedNpm "left-pad")
