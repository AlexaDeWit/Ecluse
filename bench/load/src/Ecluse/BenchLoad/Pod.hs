-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE DeriveAnyClass #-}

{- | The pod shapes a load run bounds the proxy with, and the cgroup v2 files it reads back.
A shape becomes the proxy's own cgroup limits, so its boot derives the runtime posture and memory
plan as it would in a pod. The load generator and the stub upstreams stay outside the limit.
-}
module Ecluse.BenchLoad.Pod (
    -- * Pod shapes
    PodShape (..),
    parsePodShape,
    renderPodShape,
    cpuMaxValue,

    -- * Cgroup readings
    CgroupReading (..),
    keyedCounters,
    counter,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isDigit)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

-- | Either no limit, or a CPU quota and a memory limit applied together.
data PodShape
    = Unlimited
    | -- | Whole cores, and the memory limit in bytes.
      Limited Int Int
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Parse @unlimited@, @\<cores\>cpu-\<size\>mib@ or @\<cores\>cpu-\<size\>gib@.
parsePodShape :: Text -> Either Text PodShape
parsePodShape raw = case T.splitOn "-" (T.toLower (T.strip raw)) of
    ["unlimited"] -> Right Unlimited
    [cpuPart, memoryPart] -> Limited <$> cores cpuPart <*> memory memoryPart
    _ -> Left refusal
  where
    cores part = maybe (Left refusal) Right (T.stripSuffix "cpu" part >>= positive)
    memory part
        | Just n <- T.stripSuffix "gib" part >>= positive = Right (n * 1024 * 1024 * 1024)
        | Just n <- T.stripSuffix "mib" part >>= positive = Right (n * 1024 * 1024)
        | otherwise = Left refusal
    positive digits
        | not (T.null digits) && T.all isDigit digits = mfilter (> 0) (readMaybe (toString digits))
        | otherwise = Nothing
    refusal = "pod shape " <> show raw <> " is not unlimited or <cores>cpu-<size>mib or <cores>cpu-<size>gib"

-- | The inverse of 'parsePodShape', in gibibytes when the limit is a whole number of them.
renderPodShape :: PodShape -> Text
renderPodShape = \case
    Unlimited -> "unlimited"
    Limited cpus bytes
        | bytes `mod` gib == 0 -> show cpus <> "cpu-" <> show (bytes `div` gib) <> "gib"
        | otherwise -> show cpus <> "cpu-" <> show (bytes `div` mib) <> "mib"
  where
    mib = 1024 * 1024
    gib = 1024 * mib

-- | The @cpu.max@ body granting whole cores over the kernel's default 100 ms period.
cpuMaxValue :: Int -> Text
cpuMaxValue cpus = show (cpus * 100_000) <> " 100000"

-- | What the proxy's cgroup reported. Byte counts are 'Nothing' where the file is absent or unlimited.
data CgroupReading = CgroupReading
    { crMemoryMax :: Maybe Int
    , crMemoryPeak :: Maybe Int
    , crMemoryCurrent :: Maybe Int
    , crMemoryEvents :: Map Text Int
    -- ^ @memory.events@: @low@, @high@, @max@, @oom@, @oom_kill@.
    , crMemoryStat :: Map Text Int
    -- ^ @memory.stat@: @anon@, the memory an OOM kill follows, beside @file@, @kernel@, and @sock@.
    , crCpuStat :: Map Text Int
    -- ^ @cpu.stat@: @usage_usec@, @nr_throttled@, @throttled_usec@ and the rest.
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

{- | Parse a flat-keyed cgroup file such as @memory.events@ or @cpu.stat@. A line that is not
one key and one integer is skipped, so a kernel that adds a field never breaks the read.
-}
keyedCounters :: Text -> Map Text Int
keyedCounters body = Map.fromList (mapMaybe entry (lines body))
  where
    entry line = case words line of
        [key, value] -> (key,) <$> readMaybe (toString value)
        _ -> Nothing

-- | A counter from a flat-keyed file, zero when the kernel did not report it.
counter :: Text -> Map Text Int -> Int
counter = Map.findWithDefault 0
