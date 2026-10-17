-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE DeriveAnyClass #-}

{- | CPU time over a measured window: the proxy's cgroup, the harness cgroup that holds the load
generator and the stub upstreams, and each logical CPU from @/proc/stat@. The generator has
headroom while its cgroup and 'ohaCpu' each stay below 'headroomLimit' busy.
-}
module Ecluse.BenchLoad.CpuUse (
    -- * Readings
    CpuTicks (..),
    parseProcStat,
    CpuSample (..),

    -- * The window
    CpuWindow (..),
    cpuWindow,
    proxyCpuRate,
    proxyCpuShare,
    harnessCpuRate,
    harnessCpuShare,
    cpuBusyShare,
    foreignCpuSeconds,

    -- * Generator headroom
    ohaCpu,
    headroomLimit,
    generatorSaturated,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T

import Ecluse.BenchLoad.Pod (CpuList, cpuListMembers)

-- | One logical CPU's clock ticks: those spent busy, and all of them, idle and I/O wait included.
data CpuTicks = CpuTicks
    { ctBusy :: Int
    , ctTotal :: Int
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Each logical CPU's ticks from a @/proc/stat@ body. Guest time is already inside user time.
parseProcStat :: Text -> Map Int CpuTicks
parseProcStat body = Map.fromList (mapMaybe cpuLine (lines body))
  where
    cpuLine line = case words line of
        label : fields -> do
            cpu <- readMaybe . toString =<< T.stripPrefix "cpu" label
            counts <- traverse (readMaybe . toString) (take 8 fields)
            case counts of
                [user, nice, system, idle, iowait, irq, softirq, steal] ->
                    let busy = user + nice + system + irq + softirq + steal
                     in Just (cpu, CpuTicks busy (busy + idle + iowait))
                _ -> Nothing
        [] -> Nothing

-- | The counters at one end of a window.
data CpuSample = CpuSample
    { csSeconds :: Double
    -- ^ The monotonic clock.
    , csProxyUsec :: Maybe Int
    -- ^ The proxy cgroup's @usage_usec@, 'Nothing' without a cgroup.
    , csHarnessUsec :: Maybe Int
    -- ^ The harness cgroup's @usage_usec@, 'Nothing' without a cgroup.
    , csTicks :: Map Int CpuTicks
    }
    deriving stock (Eq, Show)

-- | The CPU time spent between two samples, with the CPUs each cgroup could run on.
data CpuWindow = CpuWindow
    { cwSeconds :: Double
    , cwProxyCpus :: Maybe CpuList
    , cwProxySeconds :: Maybe Double
    -- ^ CPU-seconds the proxy's cgroup used.
    , cwHarnessCpus :: Maybe CpuList
    , cwHarnessSeconds :: Maybe Double
    -- ^ CPU-seconds the harness cgroup used: the load generator, the stubs, and the harness.
    , cwTicks :: Map Int CpuTicks
    -- ^ Each logical CPU's ticks in the window.
    , cwTickSeconds :: Double
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | The window from a start and an end sample, given the clock ticks per second and each cgroup's CPUs.
cpuWindow :: Int -> Maybe CpuList -> Maybe CpuList -> CpuSample -> CpuSample -> CpuWindow
cpuWindow ticksPerSecond proxyCpus harnessCpus start end =
    CpuWindow
        { cwSeconds = max 0 (csSeconds end - csSeconds start)
        , cwProxyCpus = proxyCpus
        , cwProxySeconds = spent csProxyUsec
        , cwHarnessCpus = harnessCpus
        , cwHarnessSeconds = spent csHarnessUsec
        , cwTicks = Map.intersectionWith since (csTicks start) (csTicks end)
        , cwTickSeconds = 1 / fromIntegral (max 1 ticksPerSecond)
        }
  where
    spent usage = (\before after -> fromIntegral (max 0 (after - before)) / 1_000_000) <$> usage start <*> usage end
    since before after = CpuTicks (max 0 (ctBusy after - ctBusy before)) (max 0 (ctTotal after - ctTotal before))

-- | The proxy's CPU-seconds per second of the window.
proxyCpuRate :: CpuWindow -> Maybe Double
proxyCpuRate w = perSecond w =<< cwProxySeconds w

-- | The proxy's CPU use as a share of the CPUs its cgroup may run on.
proxyCpuShare :: CpuWindow -> Maybe Double
proxyCpuShare w = shareOf <$> cwProxyCpus w <*> proxyCpuRate w

-- | The harness cgroup's CPU-seconds per second of the window.
harnessCpuRate :: CpuWindow -> Maybe Double
harnessCpuRate w = perSecond w =<< cwHarnessSeconds w

-- | The harness cgroup's CPU use as a share of the CPUs it may run on.
harnessCpuShare :: CpuWindow -> Maybe Double
harnessCpuShare w = shareOf <$> cwHarnessCpus w <*> harnessCpuRate w

-- | The share of one logical CPU's ticks spent busy in the window.
cpuBusyShare :: Int -> CpuWindow -> Maybe Double
cpuBusyShare cpu w = do
    ticks <- Map.lookup cpu (cwTicks w)
    guard (ctTotal ticks > 0)
    pure (fromIntegral (ctBusy ticks) / fromIntegral (ctTotal ticks))

-- | Busy time on the proxy's CPUs that the proxy did not spend: runner services and the kernel.
foreignCpuSeconds :: CpuWindow -> Maybe Double
foreignCpuSeconds w = do
    cpus <- Set.toList . cpuListMembers <$> cwProxyCpus w
    proxy <- cwProxySeconds w
    busy <- traverse (\cpu -> ctBusy <$> Map.lookup cpu (cwTicks w)) cpus
    pure (max 0 (fromIntegral (sum busy) * cwTickSeconds w - proxy))

-- | The logical CPU @oha@ is pinned to.
ohaCpu :: Int
ohaCpu = 0

-- | The busy share at which the load generator no longer has headroom.
headroomLimit :: Double
headroomLimit = 0.85

-- | Whether the harness cgroup or 'ohaCpu' reached 'headroomLimit', 'Nothing' when neither was read.
generatorSaturated :: CpuWindow -> Maybe Bool
generatorSaturated w = case catMaybes [harnessCpuShare w, cpuBusyShare ohaCpu w] of
    [] -> Nothing
    shares -> Just (any (>= headroomLimit) shares)

perSecond :: CpuWindow -> Double -> Maybe Double
perSecond w seconds = seconds / cwSeconds w <$ guard (cwSeconds w > 0)

shareOf :: CpuList -> Double -> Double
shareOf cpus rate = rate / fromIntegral (max 1 (Set.size (cpuListMembers cpus)))
