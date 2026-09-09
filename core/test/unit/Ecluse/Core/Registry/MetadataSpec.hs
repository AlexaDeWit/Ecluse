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

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Package (PackageName, mkPackageName)
import Ecluse.Core.Registry (
    FetchFault (FetchBoundExceeded),
    RegistryResponse (RegistryResponse),
 )
import Ecluse.Core.Registry.Metadata (
    MetadataClient (fetchFullManifest, fetchVersionMetadata),
    MetadataError (MetadataAbsent, MetadataAuthorisationFailure, MetadataFetch, MetadataHttpFailure, MetadataNameMismatch, MetadataUndecodable),
    fetchThenProject,
 )
import Ecluse.Core.Registry.Npm.Metadata (newNpmMetadataClient)
import Ecluse.Core.Registry.Origin (originClient)
import Ecluse.Core.Registry.PyPI.Metadata (newPyPIMetadataClient)
import Ecluse.Core.Security (LimitError (BodyTooLarge), defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Metadata (ManifestCaching (Uncached))
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Span (TracingPort (spanMetadataDecode, spanMetadataFetch))
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Package (unscopedNpm)
import Ecluse.Test.Port (noopMetricsPort, passthroughTracingPort)

-- | Exercise error preservation and projection through the adapters' shared read step.
spec :: Spec
spec = do
    fetchStepSpec
    rawReadersSpec

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

    for_ [401, 403] $ \code ->
        it ("retains HTTP " <> show code <> " before projecting a usable body") $
            runStep (Right (RegistryResponse code "usable")) Right
                `shouldReturn` (Left (MetadataAuthorisationFailure code) :: Either MetadataError ByteString)

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
                        origin = originClient defaultLimits manager (loopbackRegistryUrl ("http://localhost:" <> show port)) Nothing
                        makeClient = case ecosystem of
                            PyPI -> newPyPIMetadataClient
                            _ -> newNpmMetadataClient
                        client = makeClient passthroughTracingPort noopMetricsPort Metric.Private Uncached (\_ _ -> pass) (\_ _ -> pass) (const pass) origin
                    full <- fetchFullManifest client name
                    void full `shouldBe` expected
                    single <- fetchVersionMetadata client name (mkVersion ecosystem "1.0.0")
                    void single `shouldBe` expected
  where
    bodyFor PyPI = "{\"meta\":{\"api-version\":\"1.0\"},\"name\":\"thing\",\"files\":[]}"
    bodyFor _ = "{\"name\":\"thing\",\"versions\":{}}"

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
