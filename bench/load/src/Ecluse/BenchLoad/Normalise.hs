-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Attribute service latency where the public baseline applies: the latency the stubs inject is the
upstream share. Derive saturation from loaded latency above concurrency-one service time.
-}
module Ecluse.BenchLoad.Normalise (
    -- * The public-leg baseline
    publicLegMultiple,
    LiveFetch (..),

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

import Ecluse.Report.Markdown (cells, fixed)

-- | One public round trip per request, with concurrent origin legs and a single-flight public leg.
publicLegMultiple :: Double
publicLegMultiple = 1.0

-- | The live public fetch time a probe measured, which places the overhead at a real deployment's distance.
data LiveFetch
    = -- | The mean fetch time in milliseconds and how many samples it averaged.
      LiveFetch Double Int
    | -- | The probe was off, or no fetch succeeded.
      NoLiveFetch
    deriving stock (Eq, Show)

-- | A latency split into public upstream and overhead, in milliseconds and fractions in @[0, 1]@.
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

-- | Subtract the public baseline, capped at the total, to leave non-negative Écluse overhead.
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

-- | Raw successful p50 and p99 latency, with a reason when public-leg attribution is unavailable.
data NormalisedRow = NormalisedRow
    { nrName :: Text
    , nrP50Ms :: Maybe Double
    , nrP99Ms :: Maybe Double
    , nrPublicLegUnavailableReason :: Maybe Text
    }
    deriving stock (Eq, Show)

{- | Render the service-time attribution as a Markdown section, one row per scenario: the injected
latency in milliseconds is the upstream share, and the live fetch time places the overhead at a deployment's distance.
-}
renderNormalised :: Double -> LiveFetch -> [NormalisedRow] -> Text
renderNormalised injectedMs live rows =
    T.unlines $
        [ "## Service-time attribution: upstream against Écluse overhead (concurrency 1)"
        , ""
        , "Concurrency-1 pass. For scenarios with an applicable public baseline, successful latency is "
            <> "the upstream baseline plus the Écluse overhead. Baseline = the injected upstream latency, "
            <> fmt1 injectedMs
            <> " ms, subtracted once per request (the public leg, with concurrent fan-out and single-flight). "
            <> "Overhead is everything Écluse adds on top of hitting the public registry (the private "
            <> "leg, the merge, the decode, the re-serialise). p50 is primary. p99 is the tail, GC included."
        , ""
        , liveLine
        , ""
        , "Rows without an applicable public baseline retain their raw totals. Their upstream and overhead attribution is unavailable."
        , ""
        , "| scenario | p50 total | p50 upstream | p50 overhead | p99 total | p99 upstream | p99 overhead | p50 at the live distance | public-leg attribution |"
        , "| --- | --- | --- | --- | --- | --- | --- | --- | --- |"
        ]
            <> map (renderRow injectedMs live) rows
  where
    liveLine = case live of
        LiveFetch ms n ->
            "Live public fetch time: "
                <> fmt1 ms
                <> " ms, the mean of "
                <> show n
                <> " live samples. A deployment at that distance from the public registry would see a p50 of "
                <> "the p50 overhead plus that time, which the column \"p50 at the live distance\" shows."
        NoLiveFetch -> "Live public fetch time: unavailable, because the probe was off or no fetch succeeded."

renderRow :: Double -> LiveFetch -> NormalisedRow -> Text
renderRow injectedMs live row =
    cells
        ( nrName row
            : cellsFor (nrP50Ms row)
                <> cellsFor (nrP99Ms row)
                <> [atDistance]
                <> [maybe "available" ("unavailable: " <>) (nrPublicLegUnavailableReason row)]
        )
  where
    cellsFor :: Maybe Double -> [Text]
    cellsFor Nothing = ["n/a", "n/a", "n/a"]
    cellsFor (Just totalMs) | isJust (nrPublicLegUnavailableReason row) = [msCell totalMs, "n/a", "n/a"]
    cellsFor (Just totalMs) =
        let a = attribute injectedMs totalMs
         in [ msCell (attrTotalMs a)
            , split (attrUpstreamMs a) (attrUpstreamFraction a)
            , split (attrOverheadMs a) (attrOverheadFraction a)
            ]
    split ms frac = msCell ms <> " (" <> pctCell frac <> ")"
    atDistance = case (live, nrP50Ms row, nrPublicLegUnavailableReason row) of
        (LiveFetch liveMs _, Just totalMs, Nothing) -> msCell (attrOverheadMs (attribute injectedMs totalMs) + liveMs)
        _ -> "n/a"

-- | The loaded-latency fraction above which queuing dominates per-request work.
queuingDominanceThreshold :: Double
queuingDominanceThreshold = 0.5

-- | Successful p50s at the same fixture latency, whose difference estimates queuing delay.
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

-- | A missing p50 leaves the delay and fraction undefined.
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

-- | Summarise usable queuing fractions and identify unavailable comparisons.
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
    measured = filter (isJust . satQueuingFraction) sats
    bound = filter satQueuingDominates measured
    unmeasured = filter (isNothing . satQueuingFraction) sats
    summaryLine
        | null measured = "No usable queuing comparison is available."
        | null bound =
            "No measured queuing fraction exceeds " <> pctCell threshold <> ". This observation does not rule out backlog."
        | otherwise =
            "FLAG, queuing-bound: "
                <> T.intercalate ", " (map satName bound)
                <> ". The loaded latency is mostly backlog (connection-pool or admission-bound), not per-request work."
    unmeasuredLine =
        "Queuing comparison unavailable: " <> T.intercalate ", " (map unavailableLabel unmeasured) <> "."

unavailableLabel :: Saturation -> Text
unavailableLabel s = satName s <> " (" <> unavailableReason s <> ")"

unavailableReason :: Saturation -> Text
unavailableReason s
    | isNothing (satLoadedP50Ms s) = "no successful response under load"
    | isNothing (satC1ServiceP50Ms s) = "concurrency-1 service p50 unavailable"
    | otherwise = "loaded p50 is not positive"

renderSat :: Saturation -> Text
renderSat s =
    cells
        [ satName s
        , fmt1 (satThroughput s) <> " req/s"
        , show (satDeadlineAborts s)
        , msMaybe (satC1ServiceP50Ms s)
        , msMaybe (satLoadedP50Ms s)
        , delayCell
        , flag
        ]
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

msCell :: Double -> Text
msCell ms = fmt1 ms <> " ms"

msMaybe :: Maybe Double -> Text
msMaybe = maybe "n/a" msCell

pctCell :: Double -> Text
pctCell frac = fmt0 (frac * 100) <> "%"

fmt0, fmt1 :: Double -> Text
fmt0 = fixed 0
fmt1 = fixed 1
