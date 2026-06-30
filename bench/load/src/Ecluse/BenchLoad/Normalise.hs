{- | The pure attribution and saturation maths behind the Layer B load harness's two
analysis views, kept apart from the live measurement shell so they are exercised
deterministically.

Two complementary views split a measured latency into parts a capacity planner can act
on:

  * __service-time attribution__ ('attribute') -- at concurrency one (no queuing), a
    measured latency is @upstream baseline + Écluse overhead@. The baseline is the real
    public-registry round trip; the overhead is everything Écluse adds on top of just
    hitting the public registry (the private leg, the merge, the decode, the
    re-serialise). Reported absolute and as a fraction of the total, so the
    upstream-bound floor is told apart from the achievable-gain portion;

  * __load saturation__ ('deriveSaturation') -- under concurrent load the same latency
    grows by a queuing delay that is neither upstream nor per-request overhead but a
    capacity signal. It is recovered as @loaded p50 − concurrency-one service p50@ and
    flagged when it dominates the loaded latency, alongside the achieved throughput and
    the deadline-abort count.

Both operate on plain scalars lifted out of a scenario's report, so this module carries
none of the harness's socket or load-generator dependencies and stays unit-testable.
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

{- | The per-request upstream wait as a multiple of the public-registry round trip. The
two origin legs are fetched concurrently and the public leg is single-flight amortised,
so a request waits one round trip on the upstream, whichever scenario it is. A scenario
that ever fetched its legs serially would wait two; this single multiple holds for every
npm scenario and is re-checked when another ecosystem's fixtures land.
-}
publicLegMultiple :: Double
publicLegMultiple = 1.0

{- | Where the upstream baseline subtracted from each measured latency came from: a live
probe of the public registry (its mean round trip and the number of timed samples), or
the configured injected latency used as a fallback when the probe was unavailable. Only
the label differs; the arithmetic is the same.
-}
data BaselineSource
    = -- | A live probe: the mean round trip in milliseconds and how many samples it averaged.
      MeasuredRtt Double Int
    | -- | The probe was unavailable; the configured injected latency (ms) stood in.
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
    -- ^ The Écluse overhead: the remainder once the upstream baseline is removed.
    , attrUpstreamFraction :: Double
    -- ^ The upstream share of the total, in @[0, 1]@.
    , attrOverheadFraction :: Double
    -- ^ The Écluse-overhead share of the total, in @[0, 1]@.
    }
    deriving stock (Eq, Show)

{- | Split a measured latency into its upstream baseline and the Écluse overhead. The
baseline is the public round trip times 'publicLegMultiple', capped at the total so the
overhead is never negative (a measurement below the baseline -- noise, or a path faster
than the live registry -- attributes the whole latency to upstream and zero overhead).
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

{- | One scenario's measured latency at the primary (p50) and tail (p99) percentiles,
before attribution. A percentile is 'Nothing' when the run recorded no successful request
at it. 'renderNormalised' applies 'attribute' against the baseline.
-}
data NormalisedRow = NormalisedRow
    { nrName :: Text
    , nrP50Ms :: Maybe Double
    , nrP99Ms :: Maybe Double
    }
    deriving stock (Eq, Show)

{- | Render the service-time attribution as a Markdown section: a header naming the
baseline source and the concurrency-one pass, then a row per scenario with the p50
(primary) split into total \/ upstream \/ overhead, and the p99 (tail, GC included) split
alongside.
-}
renderNormalised :: BaselineSource -> [NormalisedRow] -> Text
renderNormalised source rows =
    T.unlines $
        [ "## Service-time attribution -- upstream vs Écluse overhead (Layer B, concurrency 1)"
        , ""
        , "Concurrency-1 pass, so queuing does not contaminate the split: each latency is "
            <> "the upstream baseline plus the Écluse overhead. Baseline = "
            <> baselineLabel source
            <> ", "
            <> "subtracted once per request (the public leg; concurrent fan-out, single-flight). "
            <> "Overhead is everything Écluse adds on top of hitting the public registry (the private "
            <> "leg, the merge, the decode, the re-serialise). p50 is primary; p99 is the tail (GC included)."
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

{- | The fraction of the loaded latency above which the queuing delay is judged to
dominate it -- the point where the latency a client sees is mostly the request waiting in
line, not upstream and not Écluse's per-request work.
-}
queuingDominanceThreshold :: Double
queuingDominanceThreshold = 0.5

{- | The scalars one scenario's saturation view is derived from: its name, the achieved
throughput and deadline-abort count under load, and the p50 latency from each pass (the
concurrency-one service pass and the loaded pass), both at the same injected upstream
latency so their difference is the queuing delay alone.
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
    -- ^ The p50 latency from the loaded pass.
    }
    deriving stock (Eq, Show)

{- | One scenario's saturation view: the load-pass throughput and deadline aborts, the
two p50s, the queuing delay between them, its share of the loaded latency, and whether it
dominates.
-}
data Saturation = Saturation
    { satName :: Text
    , satThroughput :: Double
    , satDeadlineAborts :: Int
    , satC1ServiceP50Ms :: Maybe Double
    , satLoadedP50Ms :: Maybe Double
    , satQueuingDelayMs :: Maybe Double
    -- ^ @loaded p50 − concurrency-one service p50@, floored at zero; 'Nothing' when either p50 is absent.
    , satQueuingFraction :: Maybe Double
    -- ^ The queuing delay's share of the loaded p50, in @[0, 1]@; 'Nothing' when undefined.
    , satQueuingDominates :: Bool
    -- ^ Whether the queuing fraction exceeds the given threshold.
    }
    deriving stock (Eq, Show)

{- | Derive a scenario's saturation view from its scalars. The queuing delay is the
loaded p50 less the concurrency-one service p50 (floored at zero), its fraction is that
delay over the loaded p50, and it dominates when the fraction exceeds the threshold. A
missing p50 leaves the delay, the fraction, and the dominance undefined (not a breach --
an absent measurement, not a slow one).
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

{- | Render the saturation view as a Markdown section: a header, a per-scenario table
(throughput, deadline aborts, the two p50s, the queuing delay and its share, and a
per-row flag), then a loud summary line when any scenario is queuing-bound.
-}
renderSaturation :: Double -> [Saturation] -> Text
renderSaturation threshold sats =
    T.unlines $
        [ "## Load saturation -- queuing delay (Layer B)"
        , ""
        , "The queuing delay is the loaded p50 less the concurrency-1 service p50 (both at the "
            <> "same injected upstream latency), so it is the time a request spends waiting in line -- "
            <> "neither upstream nor per-request overhead, but a capacity signal. It is flagged "
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
  where
    bound = filter satQueuingDominates sats
    summaryLine
        | null bound =
            "No scenario is queuing-bound: the loaded latency is upstream plus per-request overhead, not backlog."
        | otherwise =
            "FLAG -- queuing-bound: "
                <> T.intercalate ", " (map satName bound)
                <> " -- the loaded latency is mostly backlog (connection-pool / admission-bound), not per-request work."

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
            , if satQueuingDominates s then "queuing-bound" else "ok"
            ]
        <> " |"
  where
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
