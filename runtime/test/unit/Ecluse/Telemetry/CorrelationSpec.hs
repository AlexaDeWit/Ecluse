-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Telemetry.CorrelationSpec (spec) where

import Test.Hspec

import Ecluse.Runtime.Log (DdContext (DdContext))
import Ecluse.Runtime.Telemetry.Correlation (activeDdSpan, ddContextNow, ddIdentity)
import Ecluse.Runtime.Telemetry.Resolve (
    EndpointSource (DefaultedEndpoint),
    ResolvedTelemetry (..),
    TelemetryEndpoint (TelemetryEndpoint),
 )

{- | Tests for the log↔trace correlation glue. A unit test runs outside any span, with
no SDK installed, so there is no active span. The trace\/span ids are absent, and the
resolved @service@\/@env@\/@version@ identity still stamps the @dd@ object. These pin
that identity-present, ids-absent shape. The complementary __active-span__ path is a
real span yielding a non-zero @dd.trace_id@ on a rendered log line. It needs a live SDK
span, so the integration tier asserts it (@Ecluse.TelemetryMetricsSpec@'s sibling,
@Ecluse.TelemetryTracingSpec@). "Ecluse.LogSpec" covers the id /format/. Pure of any
exporter.
-}
spec :: Spec
spec = describe "Ecluse.Telemetry.Correlation" $ do
    it "projects a resolved telemetry identity to a span-less dd context" $
        ddIdentity fullIdentity `shouldBe` DdContext "ecluse" (Just "prod") (Just "1.4.2") Nothing

    it "carries no env/version when the resolution left them unset" $
        ddIdentity bareIdentity `shouldBe` DdContext "ecluse" Nothing Nothing Nothing

    it "reports no active span outside any span scope" $ do
        active <- activeDdSpan
        active `shouldBe` Nothing

    it "fills no span ids onto the identity when none is active" $ do
        ctx <- ddContextNow (ddIdentity fullIdentity)
        ctx `shouldBe` DdContext "ecluse" (Just "prod") (Just "1.4.2") Nothing

-- A fully-populated resolved identity (service, env, version).
fullIdentity :: ResolvedTelemetry
fullIdentity =
    ResolvedTelemetry
        { rtServiceName = "ecluse"
        , rtEnvironment = Just "prod"
        , rtVersion = Just "1.4.2"
        , rtEndpoint = TelemetryEndpoint "http://localhost:4318" DefaultedEndpoint
        }

-- An identity with the optional environment/version unset, as a vanilla deployment
-- that named neither leaves them.
bareIdentity :: ResolvedTelemetry
bareIdentity = fullIdentity{rtEnvironment = Nothing, rtVersion = Nothing}
