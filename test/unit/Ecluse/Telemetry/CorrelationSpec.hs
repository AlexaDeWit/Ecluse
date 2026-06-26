module Ecluse.Telemetry.CorrelationSpec (spec) where

import Test.Hspec

import Ecluse.Log (DdContext (DdContext))
import Ecluse.Telemetry.Correlation (activeDdSpan, ddContextNow, ddIdentity)
import Ecluse.Telemetry.Resolve (
    EndpointSource (DefaultedEndpoint),
    ResolvedTelemetry (..),
    TelemetryEndpoint (TelemetryEndpoint),
 )

{- | Tests for the log↔trace correlation glue. Outside any span — which is the state a
unit test runs in, with no SDK installed — there is no active span, so the trace\/span
ids are absent and the resolved @service@\/@env@\/@version@ identity still stamps the
@dd@ object. These pin that identity-present, ids-absent shape (the active-span path is
proven end to end in the integration tier; the id /format/ is covered by
"Ecluse.LogSpec"). Pure of any exporter.
-}
spec :: Spec
spec = describe "Ecluse.Telemetry.Correlation" $ do
    it "projects a resolved telemetry identity to a span-less dd context" $
        ddIdentity identity `shouldBe` DdContext "ecluse" (Just "prod") (Just "1.4.2") Nothing

    it "carries no env/version when the resolution left them unset" $
        ddIdentity bareIdentity `shouldBe` DdContext "ecluse" Nothing Nothing Nothing

    it "reports no active span outside any span scope" $ do
        active <- activeDdSpan
        active `shouldBe` Nothing

    it "fills no span ids onto the identity when none is active" $ do
        ctx <- ddContextNow (ddIdentity identity)
        ctx `shouldBe` DdContext "ecluse" (Just "prod") (Just "1.4.2") Nothing

-- A fully-populated resolved identity (service, env, version).
identity :: ResolvedTelemetry
identity =
    ResolvedTelemetry
        { rtServiceName = "ecluse"
        , rtEnvironment = Just "prod"
        , rtVersion = Just "1.4.2"
        , rtEndpoint = TelemetryEndpoint "http://localhost:4318" DefaultedEndpoint
        }

-- An identity with the optional environment/version unset, as a vanilla deployment
-- that named neither leaves them.
bareIdentity :: ResolvedTelemetry
bareIdentity = identity{rtEnvironment = Nothing, rtVersion = Nothing}
