-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE DeriveAnyClass #-}

{- | The pod shapes a load run bounds the proxy with, the CPUs and runtime settings it may pin on
the proxy, and the cgroup v2 files it writes and reads back. A shape becomes the proxy's own cgroup
limits, so its boot derives the runtime posture and memory plan as it would in a pod. The load
generator and the stub upstreams stay outside the limit.
-}
module Ecluse.BenchLoad.Pod (
    -- * Pod shapes
    PodShape (..),
    parsePodShape,
    renderPodShape,
    podCpuQuota,
    podMemoryLimit,
    cpuMaxValue,
    mib,
    positive,

    -- * CPU placement and runtime settings
    CpuList,
    parseCpuList,
    renderCpuList,
    cpuListMembers,
    cpusLeft,
    ProxyTuning (..),
    noTuning,
    proxyCgroupFiles,
    needsCgroup,
    memoryBackstopBytes,

    -- * Cgroup readings
    CgroupReading (..),
    keyedCounters,
    counter,
    BurstMemory (..),
    burstMemory,
) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withText)
import Data.Char (isDigit)
import Data.Foldable1 qualified as Foldable1
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T

-- | The cgroup limits a run puts on the proxy: none, a CPU-time quota, a memory limit, or both.
data PodShape
    = Unlimited
    | -- | A CPU-time quota in whole CPU equivalents, and a memory limit in bytes.
      Limited Int Int
    | -- | A CPU-time quota in whole CPU equivalents, with no memory limit.
      QuotaOnly Int
    | -- | A memory limit in bytes, with no CPU-time quota.
      MemoryOnly Int
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | A mebibyte in bytes.
mib :: Int
mib = 1024 * 1024

gib :: Int
gib = 1024 * mib

-- | Parse @unlimited@, a CPU-time quota such as @2cpu@, a memory limit such as @1gib@ or @512mib@, or both, as @2cpu-1gib@.
parsePodShape :: Text -> Either Text PodShape
parsePodShape raw = maybeToRight refusal $ case T.splitOn "-" (T.toLower (T.strip raw)) of
    ["unlimited"] -> Just Unlimited
    [part] -> QuotaOnly <$> cpuEquivalents part <|> MemoryOnly <$> memory part
    [cpuPart, memoryPart] -> Limited <$> cpuEquivalents cpuPart <*> memory memoryPart
    _ -> Nothing
  where
    cpuEquivalents part = T.stripSuffix "cpu" part >>= positive
    memory part = (* gib) <$> (T.stripSuffix "gib" part >>= positive) <|> (* mib) <$> (T.stripSuffix "mib" part >>= positive)
    refusal = "pod shape " <> show raw <> " is not unlimited, <cpu-equivalents>cpu, <size>mib or <size>gib, or <cpu-equivalents>cpu-<size>mib or <cpu-equivalents>cpu-<size>gib"

-- | The inverse of 'parsePodShape', in gibibytes when the limit is a whole number of them.
renderPodShape :: PodShape -> Text
renderPodShape = \case
    Unlimited -> "unlimited"
    Limited cpus bytes -> quota cpus <> "-" <> limit bytes
    QuotaOnly cpus -> quota cpus
    MemoryOnly bytes -> limit bytes
  where
    quota cpus = show cpus <> "cpu"
    limit bytes
        | bytes `mod` gib == 0 = show (bytes `div` gib) <> "gib"
        | otherwise = show (bytes `div` mib) <> "mib"

-- | The shape's CPU-time quota in CPU equivalents, if it sets one.
podCpuQuota :: PodShape -> Maybe Int
podCpuQuota = \case
    Limited cpus _ -> Just cpus
    QuotaOnly cpus -> Just cpus
    _ -> Nothing

-- | The shape's memory limit in bytes, if it sets one.
podMemoryLimit :: PodShape -> Maybe Int
podMemoryLimit = \case
    Limited _ bytes -> Just bytes
    MemoryOnly bytes -> Just bytes
    _ -> Nothing

-- | The @cpu.max@ body allowing 100 ms of CPU time per CPU equivalent in each 100 ms period.
cpuMaxValue :: Int -> Text
cpuMaxValue cpus = show (cpus * 100_000) <> " 100000"

-- | A non-empty set of logical CPUs, in the list form @cpuset.cpus@ takes, such as @2-3@ or @0,2-3@.
newtype CpuList = CpuList (Set Int)
    deriving stock (Eq, Ord, Show)

instance ToJSON CpuList where
    toJSON = toJSON . renderCpuList

instance FromJSON CpuList where
    parseJSON = withText "CPU list" (either (fail . toString) pure . parseCpuList)

-- | Parse a comma-separated list of CPU numbers and inclusive ranges.
parseCpuList :: Text -> Either Text CpuList
parseCpuList raw = do
    cpus <- Set.unions <$> traverse members (T.splitOn "," (T.strip raw))
    if Set.null cpus then Left refusal else Right (CpuList cpus)
  where
    members part = case map T.strip (T.splitOn "-" part) of
        [single] -> Set.singleton <$> cpu single
        [low, high] -> do
            from <- cpu low
            to <- cpu high
            if from <= to then Right (Set.fromList [from .. to]) else Left refusal
        _ -> Left refusal
    cpu = maybeToRight refusal . natural
    refusal = "CPU list " <> show raw <> " is not a comma-separated list of CPU numbers and ranges, such as 2-3 or 0,2-3"

-- | The canonical list form, with each run of consecutive CPUs as one range.
renderCpuList :: CpuList -> Text
renderCpuList (CpuList cpus) = T.intercalate "," (map range (runs (Set.toAscList cpus)))
  where
    runs = foldr extend []
    extend cpu = \case
        (from, to) : rest | from == cpu + 1 -> (cpu, to) : rest
        rest -> (cpu, cpu) : rest
    range (from, to) = if from == to then show from else show from <> "-" <> show to

-- | The CPUs in the list.
cpuListMembers :: CpuList -> Set Int
cpuListMembers (CpuList cpus) = cpus

-- | The CPUs of the first list that the second does not take, 'Nothing' when it takes them all.
cpusLeft :: CpuList -> CpuList -> Maybe CpuList
cpusLeft (CpuList every) (CpuList taken) = CpuList left <$ guard (not (Set.null left))
  where
    left = every `Set.difference` taken

-- | What a run pins on the proxy beyond its shape. 'Nothing' leaves a setting to the proxy's own boot.
data ProxyTuning = ProxyTuning
    { ptCpus :: Maybe CpuList
    -- ^ The CPUs the proxy's cgroup may run on, which also bound the processors its RTS sees.
    , ptCores :: Maybe Int
    -- ^ @runtime.cores@.
    , ptMaxHeapBytes :: Maybe Int
    -- ^ @runtime.maxHeapBytes@.
    , ptAllocAreaMib :: Maybe Int
    -- ^ The allocation area per capability, passed as @-A@ in the proxy's @GHCRTS@.
    }
    deriving stock (Eq, Show)

-- | The proxy as the scheduled runs boot it.
noTuning :: ProxyTuning
noTuning = ProxyTuning Nothing Nothing Nothing Nothing

-- | The files the harness writes into the proxy's cgroup. Only the untuned unlimited control gets no memory bound.
proxyCgroupFiles :: PodShape -> ProxyTuning -> [(FilePath, Text)]
proxyCgroupFiles shape tuning =
    [("cpu.max", cpuMaxValue cpus) | Just cpus <- [podCpuQuota shape]]
        <> [("memory.max", show bytes) | Just bytes <- [podMemoryLimit shape]]
        <> [("memory.high", show memoryBackstopBytes) | isNothing (podMemoryLimit shape), shape /= Unlimited || tuning /= noTuning]
        <> [("cpuset.cpus", renderCpuList cpus) | Just cpus <- [ptCpus tuning]]

-- | Whether the proxy needs a cgroup of its own: for a limit, or for a CPU placement.
needsCgroup :: PodShape -> ProxyTuning -> Bool
needsCgroup shape tuning = shape /= Unlimited || isJust (ptCpus tuning)

-- | The @memory.high@ backstop that protects the runner. The proxy's runtime sizing reads @memory.max@ alone.
memoryBackstopBytes :: Int
memoryBackstopBytes = 4 * gib

-- A run of decimal digits as a number.
natural :: Text -> Maybe Int
natural digits
    | not (T.null digits) && T.all isDigit digits = readMaybe (toString digits)
    | otherwise = Nothing

-- | A run of decimal digits as a number above zero.
positive :: Text -> Maybe Int
positive = mfilter (> 0) . natural

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
    , crCpus :: Maybe CpuList
    -- ^ @cpuset.cpus.effective@: the CPUs the cgroup may run on, 'Nothing' without the cpuset controller.
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Parse a flat-keyed cgroup file such as @cpu.stat@, skipping a line that is not one key and one integer.
keyedCounters :: Text -> Map Text Int
keyedCounters body = Map.fromList (mapMaybe entry (lines body))
  where
    entry line = case words line of
        [key, value] -> (key,) <$> readMaybe (toString value)
        _ -> Nothing

-- | A counter from a flat-keyed file, zero when the kernel did not report it.
counter :: Text -> Map Text Int -> Int
counter = Map.findWithDefault 0

-- | The proxy cgroup's memory read at a fixed interval through a burst, kept as two of its readings.
data BurstMemory = BurstMemory
    { bmIntervalMs :: Int
    , bmSamples :: Int
    , bmHighestCurrent :: Maybe CgroupReading
    -- ^ The reading with the most @memory.current@.
    , bmHighestSock :: Maybe CgroupReading
    -- ^ The reading with the most @sock@ in @memory.stat@: kernel socket buffers.
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Keep the readings taken this many milliseconds apart that hold the most memory and the most socket memory.
burstMemory :: Int -> [CgroupReading] -> BurstMemory
burstMemory intervalMs readings =
    BurstMemory
        { bmIntervalMs = intervalMs
        , bmSamples = length readings
        , bmHighestCurrent = highest (fromMaybe 0 . crMemoryCurrent)
        , bmHighestSock = highest (counter "sock" . crMemoryStat)
        }
  where
    highest by = viaNonEmpty (Foldable1.maximumBy (comparing by)) readings
