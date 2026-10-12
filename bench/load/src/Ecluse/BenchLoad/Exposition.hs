-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE DeriveAnyClass #-}

-- | Advisory readiness, admission gauges, and cache outcomes from the proxy's Prometheus scrape.
module Ecluse.BenchLoad.Exposition (
    -- * Samples
    Sample (..),
    parseExposition,
    seriesTotal,
    commonLabels,
    renderSample,

    -- * Gauge summaries
    GaugeSummary (..),
    summariseGauge,

    -- * Advisory database and rule failures
    advisoryDatabaseInstalled,
    ruleFailuresWindow,

    -- * Metadata cache outcomes
    expositionName,
    storeLabel,
    CacheOutcomes (..),
    storeOutcomes,
    cacheWindow,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as T
import Data.Universe.Class qualified as Universe

import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Telemetry.Catalogue (MetricName (AdvisoryDatabaseAgeSeconds, AssembledCacheRequests, MetadataCacheRequests, RuleEffectfulFailures, SingleVersionCacheRequests), metricName)
import Ecluse.Core.Telemetry.Metrics (CacheResult (Collapsed, Hit, Miss), CacheStore (AssembledStore, FullStore, VersionStore), Label (LCacheResult, LCacheStore, LEcosystem), renderLabel)
import Ecluse.Test.Exposition (Sample (..), parseExposition)

-- | Sum matching series. An absent series stays distinct from a zero value.
seriesTotal :: Text -> [(Text, Text)] -> [Sample] -> Maybe Double
seriesTotal name wanted samples = case matching of
    [] -> Nothing
    found -> Just (sum (map sampleValue found))
  where
    matching = [s | s <- samples, sampleName s == name, all (`elem` sampleLabels s) wanted]

-- | The label pairs every sample carries: the resource labels the exporter repeats on each series.
commonLabels :: [Sample] -> [Text]
commonLabels = \case
    [] -> []
    sample : rest -> [key | pair@(key, _) <- sampleLabels sample, all (elem pair . sampleLabels) rest]

-- | One sample as a report line, without the given labels.
renderSample :: [Text] -> Sample -> Text
renderSample resourceLabels s =
    sampleName s <> labelPart <> " " <> show (sampleValue s)
  where
    kept = [(k, v) | (k, v) <- sampleLabels s, k `notElem` resourceLabels]
    labelPart
        | null kept = ""
        | otherwise = "{" <> T.intercalate "," [k <> "=" <> v | (k, v) <- kept] <> "}"

-- | A gauge sampled across a window. A missed sample is a scrape that failed or timed out.
data GaugeSummary = GaugeSummary
    { gsSamples :: Int
    , gsMissed :: Int
    , gsMax :: Maybe Double
    , gsMean :: Maybe Double
    , gsLast :: Maybe Double
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Summarise samples in the order taken, 'Nothing' marking a miss.
summariseGauge :: [Maybe Double] -> GaugeSummary
summariseGauge readings =
    GaugeSummary
        { gsSamples = length taken
        , gsMissed = length readings - length taken
        , gsMax = case taken of
            [] -> Nothing
            highest : rest -> Just (foldl' max highest rest)
        , gsMean = if null taken then Nothing else Just (sum taken / fromIntegral (length taken))
        , gsLast = listToMaybe (reverse taken)
        }
  where
    taken = catMaybes readings

-- | Whether a scrape shows the ecosystem's advisory database age, which the proxy reports once one is installed.
advisoryDatabaseInstalled :: Ecosystem -> [Sample] -> Bool
advisoryDatabaseInstalled ecosystem = isJust . seriesTotal (expositionName AdvisoryDatabaseAgeSeconds) [renderLabel (LEcosystem ecosystem)]

-- | The undecidable version decisions a rule recorded between two scrapes of one process.
ruleFailuresWindow :: [Sample] -> [Sample] -> Int
ruleFailuresWindow start end = max 0 (failures end - failures start)
  where
    failures = round . fromMaybe 0 . seriesTotal (expositionName RuleEffectfulFailures) []

-- | A catalogue metric's name in the exposition, which spells each dot as an underscore.
expositionName :: MetricName -> Text
expositionName = T.replace "." "_" . metricName

-- | A cache store's @store@ label value.
storeLabel :: CacheStore -> Text
storeLabel = snd . renderLabel . LCacheStore

-- | One metadata cache store's request outcomes. A collapsed request waited on another's fetch.
data CacheOutcomes = CacheOutcomes
    { coHits :: Int
    , coMisses :: Int
    , coCollapsed :: Int
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | One store's outcomes in one scrape. A store with no series yet reads zero.
storeOutcomes :: [Sample] -> CacheStore -> CacheOutcomes
storeOutcomes samples store = CacheOutcomes (count Hit) (count Miss) (count Collapsed)
  where
    count result = round (fromMaybe 0 (seriesTotal (expositionName (requests store)) [renderLabel (LCacheResult result)] samples))
    requests = \case
        FullStore -> MetadataCacheRequests
        VersionStore -> SingleVersionCacheRequests
        AssembledStore -> AssembledCacheRequests

-- | Each store's outcomes between two scrapes of one process, keyed by its @store@ label value.
cacheWindow :: [Sample] -> [Sample] -> [(Text, CacheOutcomes)]
cacheWindow start end = [(storeLabel store, since (storeOutcomes end store) (storeOutcomes start store)) | store <- Universe.universe]
  where
    since now before =
        CacheOutcomes
            { coHits = max 0 (coHits now - coHits before)
            , coMisses = max 0 (coMisses now - coMisses before)
            , coCollapsed = max 0 (coCollapsed now - coCollapsed before)
            }
