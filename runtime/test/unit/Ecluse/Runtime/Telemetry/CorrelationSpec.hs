-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Runtime.Telemetry.CorrelationSpec (spec) where

import Test.Hspec

import Ecluse.Runtime.Log (DdContext (DdContext))
import Ecluse.Runtime.Telemetry.Correlation (activeDdSpan, ddContextNow, ddIdentity)
import Ecluse.Runtime.Telemetry.Resolve (
    EndpointSource (DefaultedEndpoint),
    ResolvedTelemetry (..),
    TelemetryEndpoint (TelemetryEndpoint),
 )

{- | Tests the @dd@ correlation object outside any span: ids absent, resolved identity present.
The active-span path needs a live SDK, so @Ecluse.Runtime.Telemetry.TracingIntegrationSpec@ covers it.
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
