-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The pull-side metrics transport: a Prometheus exposition on its own listener, never on the
proxy's data port. A backend that scrapes selects it with @OTEL_METRICS_EXPORTER=prometheus@,
which the SDK answers with a no-op push exporter, leaving the endpoint to the application. The
exposition carries the whole OpenTelemetry resource, so it names the host, the process, and any
cloud or cluster identity the SDK detected. That is why it never shares the port untrusted
registry clients reach, and why it binds @localhost@ until an operator widens it.
@OTEL_EXPORTER_PROMETHEUS_HOST@ and @OTEL_EXPORTER_PROMETHEUS_PORT@ address it, as the
OpenTelemetry specification defines them.
-}
module Ecluse.Runtime.Telemetry.Scrape (
    -- * The collection handle
    MetricScrape (..),
    metricScrapeFor,
    scrapeSelected,

    -- * The dedicated listener
    ScrapeListener (..),
    scrapeListenerFrom,
    scrapeListenerWarnings,
    scrapeApplication,
    withScrapeListener,
) where

import Data.List (lookup)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Katip (LogEnv, Severity (ErrorS, InfoS, WarningS))
import Network.HTTP.Types (hContentType, status404)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp qualified as Warp
import System.Environment (getEnvironment)
import UnliftIO (catchAny)
import UnliftIO.Async (race, wait, withAsync)

import OpenTelemetry.Environment (MetricsExporterSelection (MetricsExporterPrometheus), lookupMetricsExporterSelection)
import OpenTelemetry.Exporter.Metric (ResourceMetricsExport)
import OpenTelemetry.Exporter.Prometheus.WAI (prometheusMiddleware)
import OpenTelemetry.MeterProvider (SdkMeterEnv, collectResourceMetrics)

import Ecluse.Core.Text (displayExceptionT, nonBlank)
import Ecluse.Runtime.Log (moduleLog)

{- | One on-demand collection of the meter's current series. The substrate builds one only where
the operator asked for the scrape transport.
-}
newtype MetricScrape = MetricScrape
    { runMetricScrape :: IO (Vector ResourceMetricsExport)
    }

{- | Whether @OTEL_METRICS_EXPORTER@ names the Prometheus transport. It reads the SDK's own parse
of the variable, so the listener and the exporter the SDK resolves cannot disagree over one value.
-}
scrapeSelected :: IO Bool
scrapeSelected = (Just MetricsExporterPrometheus ==) <$> lookupMetricsExporterSelection

{- | Build the scrape handle, or 'Nothing' when the transport stayed on OTLP push. Collecting
beside the periodic reader is safe only under cumulative temporality: delta would split the points.
-}
metricScrapeFor :: SdkMeterEnv -> IO (Maybe MetricScrape)
metricScrapeFor meterEnv = do
    selected <- scrapeSelected
    pure (if selected then Just (MetricScrape collect) else Nothing)
  where
    collect :: IO (Vector ResourceMetricsExport)
    collect = V.fromList <$> collectResourceMetrics meterEnv

-- | Where the scrape listener binds.
data ScrapeListener = ScrapeListener
    { slHost :: Text
    -- ^ The bind address, loopback unless the operator widened it.
    , slPort :: Int
    -- ^ The TCP port, 9464 by the OpenTelemetry specification's default.
    }
    deriving stock (Eq, Show)

{- | Resolve the listener from an environment. The defaults reach no interface but the loopback,
so publishing the exposition any wider is an operator's deliberate act.
-}
scrapeListenerFrom :: [(String, String)] -> ScrapeListener
scrapeListenerFrom environment =
    ScrapeListener
        { slHost = fromMaybe defaultScrapeHost (declared hostVar environment)
        , slPort = case declaredPort environment of
            PortDeclared port -> port
            _ -> defaultScrapePort
        }

{- | The warnings this environment raises, as values, so a test pins the message without a @katip@
scribe. 'withScrapeListener' surfaces them before it binds.
-}
scrapeListenerWarnings :: [(String, String)] -> [Text]
scrapeListenerWarnings environment = case declaredPort environment of
    PortUnusable raw -> [unusablePortMessage raw]
    _ -> []

-- What the operator's port variable amounts to. One reading feeds both the resolution and the
-- warning, so the two can never disagree about which values are usable.
data PortSource
    = PortAbsent
    | PortDeclared Int
    | PortUnusable Text

declaredPort :: [(String, String)] -> PortSource
declaredPort environment = case declared portVar environment of
    Nothing -> PortAbsent
    Just raw -> maybe (PortUnusable raw) PortDeclared (readMaybe (toString raw))

-- A present but blank value counts as unset, as it does across the telemetry resolution.
declared :: String -> [(String, String)] -> Maybe Text
declared name environment = nonBlank . toText =<< lookup name environment

hostVar :: String
hostVar = "OTEL_EXPORTER_PROMETHEUS_HOST"

portVar :: String
portVar = "OTEL_EXPORTER_PROMETHEUS_PORT"

defaultScrapeHost :: Text
defaultScrapeHost = "localhost"

defaultScrapePort :: Int
defaultScrapePort = 9464

unusablePortMessage :: Text -> Text
unusablePortMessage raw =
    toText portVar
        <> " is not a port number ("
        <> raw
        <> "). Serving the scrape exposition on "
        <> show defaultScrapePort
        <> " instead."

{- | Answer @\/metrics@ with the Prometheus text exposition of the current series, and every other
path with a plain @404@. This is the whole surface of the dedicated listener.
-}
scrapeApplication :: MetricScrape -> Application
scrapeApplication scrape = prometheusMiddleware (runMetricScrape scrape) unmatchedPath

-- The listener serves one path, so anything else gets a body with nothing in it to parse.
unmatchedPath :: Application
unmatchedPath _request respond =
    respond (responseLBS status404 [(hContentType, "text/plain; charset=utf-8")] "Not Found\n")

{- | Run @act@ with the scrape listener alive when the transport selected one, and unchanged when
it did not. A listener that cannot bind is reported and abandoned, never a failed boot.
-}
withScrapeListener :: LogEnv -> Maybe MetricScrape -> IO a -> IO a
withScrapeListener _ Nothing act = act
withScrapeListener logEnv (Just scrape) act = do
    environment <- getEnvironment
    traverse_ (scrapeLog logEnv WarningS) (scrapeListenerWarnings environment)
    bound <- newEmptyMVar
    withAsync (runScrapeListener logEnv (scrapeListenerFrom environment) scrape bound) $ \started -> do
        -- Whichever lands first: the port is bound, or the attempt gave up and logged. Racing
        -- them is what keeps a bind failure from parking the caller on a signal never sent.
        _ <- race (wait started) (takeMVar bound)
        act

runScrapeListener :: LogEnv -> ScrapeListener -> MetricScrape -> MVar () -> IO ()
runScrapeListener logEnv listener scrape bound =
    Warp.runSettings settings (scrapeApplication scrape)
        `catchAny` (say ErrorS . failedMessage listener)
  where
    settings :: Warp.Settings
    settings =
        Warp.setPort (slPort listener)
            . Warp.setHost (fromString (toString (slHost listener)))
            . Warp.setBeforeMainLoop (say InfoS (boundMessage listener) >> putMVar bound ())
            $ Warp.defaultSettings

    say :: Severity -> Text -> IO ()
    say = scrapeLog logEnv

scrapeLog :: LogEnv -> Severity -> Text -> IO ()
scrapeLog logEnv = moduleLog logEnv "Ecluse.Runtime.Telemetry.Scrape"

-- The bind line states what the exposition carries, because the posture is the operator's to hold.
boundMessage :: ScrapeListener -> Text
boundMessage listener =
    "prometheus scrape exposition listening on "
        <> address listener
        <> ". It carries host, process, and cloud identity, so keep the port inside your network."

failedMessage :: ScrapeListener -> SomeException -> Text
failedMessage listener failure =
    "prometheus scrape exposition could not listen on "
        <> address listener
        <> ". Serving continues without it: "
        <> displayExceptionT failure

address :: ScrapeListener -> Text
address listener = slHost listener <> ":" <> show (slPort listener)
