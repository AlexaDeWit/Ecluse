-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The attribution and saturation arithmetic behind the load report, apart from the live shell
so the unit suite can test it. At concurrency one a latency splits into the upstream baseline and
the Écluse overhead ('attribute'). Under load the queuing delay above the concurrency-one service
time is a capacity signal ('deriveSaturation'). Every latency here is a successful response's.
-}
module Ecluse.BenchLoad.Normalise (
    -- * The public-leg baseline
    publicLegMultiple,
    BaselineSource (..),

    -- * Service-time attribution
    Attribution (..),
    attribute,
    NormalisedRow (..),
    renderNormalised,

    -- * Load saturation
    queuingDominanceThreshold,
    SaturationInput (..),
    Saturation (..),
    deriveSaturation,
    renderSaturation,
) where

import Data.Text qualified as T
import Numeric (showFFloat)

{- | The upstream wait per request in public round trips: one, because the origin legs run
concurrently and the public leg is single-flight. Another ecosystem's fixtures must re-check it.
-}
publicLegMultiple :: Double
publicLegMultiple = 1.0

{- | Where the subtracted upstream baseline came from, a live probe or the configured
injected latency. Only the label differs. The arithmetic is the same.
-}
data BaselineSource
    = -- | A live probe: the mean round trip in milliseconds and how many samples it averaged.
      MeasuredRtt Double Int
    | -- | The probe was unavailable, so the configured injected latency (ms) stood in.
      InjectedFallback Double
    deriving stock (Eq, Show)

-- The baseline round trip in milliseconds, whatever its source.
baselineMs :: BaselineSource -> Double
baselineMs = \case
    MeasuredRtt rtt _ -> rtt
    InjectedFallback ms -> ms

{- | One measured latency split into its upstream and Écluse-overhead parts, each
absolute (milliseconds) and as a fraction of the total in @[0, 1]@.
-}
data Attribution = Attribution
    { attrTotalMs :: Double
    -- ^ The measured latency.
    , attrUpstreamMs :: Double
    -- ^ The upstream baseline: the public round trip, capped at the total.
    , attrOverheadMs :: Double
    -- ^ The Écluse overhead: what remains after the upstream baseline.
    , attrUpstreamFraction :: Double
    -- ^ The upstream share of the total, in @[0, 1]@.
    , attrOverheadFraction :: Double
    -- ^ The Écluse-overhead share of the total, in @[0, 1]@.
    }
    deriving stock (Eq, Show)

{- | Split a measured latency into upstream baseline and Écluse overhead. The baseline is
capped at the total, so a measurement below it yields zero overhead rather than a negative.
-}
attribute :: Double -> Double -> Attribution
attribute rttMs totalMs =
    Attribution
        { attrTotalMs = totalMs
        , attrUpstreamMs = upstream
        , attrOverheadMs = overhead
        , attrUpstreamFraction = fraction upstream
        , attrOverheadFraction = fraction overhead
        }
  where
    upstream = max 0 (min totalMs (rttMs * publicLegMultiple))
    overhead = max 0 (totalMs - upstream)
    fraction part = if totalMs <= 0 then 0 else part / totalMs

{- | One scenario's measured p50 and p99 latency, before attribution. A percentile is
'Nothing' when the run recorded no successful request at it.
-}
data NormalisedRow = NormalisedRow
    { nrName :: Text
    , nrP50Ms :: Maybe Double
    , nrP99Ms :: Maybe Double
    }
    deriving stock (Eq, Show)

-- | Render the service-time attribution as a Markdown section, one row per scenario.
renderNormalised :: BaselineSource -> [NormalisedRow] -> Text
renderNormalised source rows =
    T.unlines $
        [ "## Service-time attribution: upstream against Écluse overhead (concurrency 1)"
        , ""
        , "Concurrency-1 pass, so queuing does not contaminate the split: each successful latency is "
            <> "the upstream baseline plus the Écluse overhead. Baseline = "
            <> baselineLabel source
            <> ", "
            <> "subtracted once per request (the public leg, with concurrent fan-out and single-flight). "
            <> "Overhead is everything Écluse adds on top of hitting the public registry (the private "
            <> "leg, the merge, the decode, the re-serialise). p50 is primary. p99 is the tail, GC included."
        , ""
        , "| scenario | p50 total | p50 upstream | p50 overhead | p99 total | p99 upstream | p99 overhead |"
        , "| --- | --- | --- | --- | --- | --- | --- |"
        ]
            <> map (renderRow (baselineMs source)) rows

renderRow :: Double -> NormalisedRow -> Text
renderRow rttMs row =
    "| "
        <> T.intercalate
            " | "
            ( nrName row
                : cellsFor (nrP50Ms row)
                    <> cellsFor (nrP99Ms row)
            )
        <> " |"
  where
    cellsFor :: Maybe Double -> [Text]
    cellsFor Nothing = ["n/a", "n/a", "n/a"]
    cellsFor (Just totalMs) =
        let a = attribute rttMs totalMs
         in [ msCell (attrTotalMs a)
            , split (attrUpstreamMs a) (attrUpstreamFraction a)
            , split (attrOverheadMs a) (attrOverheadFraction a)
            ]
    split ms frac = msCell ms <> " (" <> pctCell frac <> ")"

{- | The fraction of the loaded latency above which queuing delay dominates it: past that
point the client sees mostly waiting in line, neither upstream nor Écluse's own work.
-}
queuingDominanceThreshold :: Double
queuingDominanceThreshold = 0.5

{- | The scalars 'deriveSaturation' works from. Both passes run at the same injected upstream
latency, so the difference of their successful-response p50s is the queuing delay alone.
-}
data SaturationInput = SaturationInput
    { siName :: Text
    , siThroughput :: Double
    -- ^ Requests per second under load (the throughput-plateau signal).
    , siDeadlineAborts :: Int
    -- ^ Requests the load generator abandoned at the deadline (a backlog it never drained).
    , siC1ServiceP50Ms :: Maybe Double
    -- ^ The p50 service time from the concurrency-one pass.
    , siLoadedP50Ms :: Maybe Double
    -- ^ The successful-response p50 from the loaded pass. 'Nothing' when nothing succeeded.
    }
    deriving stock (Eq, Show)

-- | One scenario's saturation view, derived by 'deriveSaturation'.
data Saturation = Saturation
    { satName :: Text
    , satThroughput :: Double
    , satDeadlineAborts :: Int
    , satC1ServiceP50Ms :: Maybe Double
    , satLoadedP50Ms :: Maybe Double
    , satQueuingDelayMs :: Maybe Double
    -- ^ @loaded p50 − concurrency-one service p50@, floored at zero. 'Nothing' when either p50 is absent.
    , satQueuingFraction :: Maybe Double
    -- ^ The queuing delay's share of the loaded p50, in @[0, 1]@. 'Nothing' when undefined.
    , satQueuingDominates :: Bool
    -- ^ Whether the queuing fraction exceeds the given threshold.
    }
    deriving stock (Eq, Show)

{- | Derive a scenario's saturation view. A missing p50 leaves the delay, the fraction, and
the dominance undefined: an absent measurement, never a breach and never a slow one.
-}
deriveSaturation :: Double -> SaturationInput -> Saturation
deriveSaturation threshold si =
    Saturation
        { satName = siName si
        , satThroughput = siThroughput si
        , satDeadlineAborts = siDeadlineAborts si
        , satC1ServiceP50Ms = siC1ServiceP50Ms si
        , satLoadedP50Ms = siLoadedP50Ms si
        , satQueuingDelayMs = delay
        , satQueuingFraction = fraction
        , satQueuingDominates = maybe False (> threshold) fraction
        }
  where
    delay = (\loaded service -> max 0 (loaded - service)) <$> siLoadedP50Ms si <*> siC1ServiceP50Ms si
    fraction = do
        d <- delay
        loaded <- siLoadedP50Ms si
        if loaded > 0 then Just (d / loaded) else Nothing

{- | Render the saturation view as a Markdown section, with a loud summary line when any
scenario is queuing-bound.
-}
renderSaturation :: Double -> [Saturation] -> Text
renderSaturation threshold sats =
    T.unlines $
        [ "## Load saturation: queuing delay"
        , ""
        , "The queuing delay is the loaded p50 of successful responses less the concurrency-1 service p50 (both at the "
            <> "same injected upstream latency), so it is the time a request spends waiting in line. "
            <> "It is neither upstream nor per-request overhead, but a capacity signal. It is flagged "
            <> "queuing-bound when it exceeds "
            <> pctCell threshold
            <> " of the loaded p50. Inform-only: "
            <> "a flag points at connection-pool and admission-bound work, never at a per-request cost."
        , ""
        , "| scenario | throughput | deadline aborts | service p50 | loaded p50 | queuing delay | flag |"
        , "| --- | --- | --- | --- | --- | --- | --- |"
        ]
            <> map renderSat sats
            <> ["", summaryLine]
            <> [unmeasuredLine | not (null unmeasured)]
  where
    bound = filter satQueuingDominates sats
    unmeasured = filter (isNothing . satLoadedP50Ms) sats
    summaryLine
        | null bound =
            "No measured scenario is queuing-bound: the loaded latency is upstream plus per-request overhead, not backlog."
        | otherwise =
            "FLAG, queuing-bound: "
                <> T.intercalate ", " (map satName bound)
                <> ". The loaded latency is mostly backlog (connection-pool or admission-bound), not per-request work."
    unmeasuredLine =
        "No successful response under load, so no queuing reading: " <> T.intercalate ", " (map satName unmeasured) <> "."

renderSat :: Saturation -> Text
renderSat s =
    "| "
        <> T.intercalate
            " | "
            [ satName s
            , fmt1 (satThroughput s) <> " req/s"
            , show (satDeadlineAborts s)
            , msMaybe (satC1ServiceP50Ms s)
            , msMaybe (satLoadedP50Ms s)
            , delayCell
            , flag
            ]
        <> " |"
  where
    flag
        | satQueuingDominates s = "queuing-bound"
        | isNothing (satLoadedP50Ms s) = "no successes"
        | isNothing (satQueuingFraction s) = "n/a"
        | otherwise = "ok"
    delayCell = case (satQueuingDelayMs s, satQueuingFraction s) of
        (Just d, Just f) -> msCell d <> " (" <> pctCell f <> ")"
        (Just d, Nothing) -> msCell d
        _ -> "n/a"

-- A latency in milliseconds to one decimal place.
msCell :: Double -> Text
msCell ms = fmt1 ms <> " ms"

-- A latency in milliseconds, or "n/a" when absent.
msMaybe :: Maybe Double -> Text
msMaybe = maybe "n/a" msCell

-- A fraction in [0, 1] rendered as a whole-number percentage.
pctCell :: Double -> Text
pctCell frac = fmt0 (frac * 100) <> "%"

baselineLabel :: BaselineSource -> Text
baselineLabel = \case
    MeasuredRtt rtt n ->
        "measured public RTT " <> fmt1 rtt <> " ms (mean of " <> show n <> " live samples)"
    InjectedFallback ms ->
        "injected fallback " <> fmt1 ms <> " ms (live probe unavailable)"

fmt0, fmt1 :: Double -> Text
fmt0 x = toText (showFFloat (Just 0) x "")
fmt1 x = toText (showFFloat (Just 1) x "")
