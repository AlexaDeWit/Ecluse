-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Where the harness and the measured proxy run, and the CPU counters read at each end of a
window. A placed proxy gets its own CPUs, and the harness cgroup keeps the rest until the run ends.
-}
module Ecluse.BenchLoad.Placement (
    -- * CPU placement
    harnessLeaf,
    readCpuList,
    runnerProcessors,
    harnessCpus,
    withHarnessPlacement,

    -- * CPU counters
    cpuSampleNow,
    cpuWindowSince,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import GHC.Clock (getMonotonicTime)
import GHC.Conc (getNumProcessors)
import System.FilePath ((</>))
import System.Posix.Unistd (SysVar (ClockTick), getSysVar)
import UnliftIO (bracket_)

import Ecluse.BenchLoad.CpuUse (CpuSample (CpuSample), CpuWindow, cpuWindow, parseProcStat)
import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Pod (CgroupReading (crCpuStat, crCpus), CpuList, cpuListMembers, cpusLeft, keyedCounters, parseCpuList, renderCpuList)
import Ecluse.Rts.Cgroup (readIfExists)

-- | The cgroup @scripts/load-cgroup.sh@ places the harness in, under @BENCH_LOAD_CGROUP@.
harnessLeaf :: FilePath
harnessLeaf = "harness"

-- | A CPU list file such as @cpuset.cpus.effective@, 'Nothing' when it is absent or holds no list.
readCpuList :: FilePath -> IO (Maybe CpuList)
readCpuList path = (rightToMaybe . parseCpuList =<<) <$> readIfExists path

-- | The runner's online processors, whatever CPUs this process is confined to.
runnerProcessors :: IO Int
runnerProcessors = readCpuList "/sys/devices/system/cpu/online" >>= maybe getNumProcessors (pure . Set.size . cpuListMembers)

-- | The CPUs the harness keeps beside a placed proxy, given the CPU @oha@ is pinned to, or why the proxy cannot have its CPUs.
harnessCpus :: Maybe Int -> CpuList -> CpuList -> Either Text CpuList
harnessCpus ohaPinned available proxy
    | not (cpuListMembers proxy `Set.isSubsetOf` cpuListMembers available) = Left (named <> " names CPUs outside BENCH_LOAD_CGROUP's " <> renderCpuList available)
    | Just cpu <- ohaPinned, cpu `Set.member` cpuListMembers proxy = Left (named <> " holds CPU " <> show cpu <> ", where BENCH_LOAD_ISOLATE_OHA=1 pins oha")
    | otherwise = maybeToRight (named <> " leaves the harness no CPU") (cpusLeft available proxy)
  where
    named = "BENCH_LOAD_PROXY_CPUS " <> renderCpuList proxy

-- | Run with the harness cgroup off the proxy's CPUs, then empty its list, which also undoes a killed run's placement.
withHarnessPlacement :: Maybe FilePath -> Maybe Int -> Maybe CpuList -> IO a -> IO a
withHarnessPlacement root ohaPinned proxyCpus action = case (root, proxyCpus) of
    (Nothing, Nothing) -> action
    (Nothing, Just _) -> benchFail "BENCH_LOAD_PROXY_CPUS needs BENCH_LOAD_CGROUP, a cgroup v2 directory delegated to this user"
    (Just base, _) -> do
        let leaf = base </> harnessLeaf </> "cpuset.cpus"
            -- An empty list makes the cgroup use its parent's CPUs, as one never placed does.
            restore = writeFileText leaf "\n"
        available <- readCpuList (base </> "cpuset.cpus.effective")
        case (proxyCpus, available) of
            (Nothing, _) -> do
                placed <- maybe False (not . T.null . T.strip) <$> readIfExists leaf
                when placed restore
                action
            (Just _, Nothing) -> benchFail "BENCH_LOAD_PROXY_CPUS needs the cpuset controller enabled on BENCH_LOAD_CGROUP"
            (Just cpus, Just every) -> do
                kept <- either benchFail pure (harnessCpus ohaPinned every cpus)
                bracket_ (writeFileText leaf (renderCpuList kept)) restore action

-- | The counters at one end of a window: the proxy's from its cgroup reading, the harness cgroup's, and each CPU's ticks.
cpuSampleNow :: Maybe CgroupReading -> IO CpuSample
cpuSampleNow proxy = do
    now <- getMonotonicTime
    harness <- traverse (fmap (usage . keyedCounters =<<) . readIfExists) =<< harnessFile "cpu.stat"
    ticks <- maybe mempty parseProcStat <$> readIfExists "/proc/stat"
    pure (CpuSample now (usage . crCpuStat =<< proxy) (join harness) ticks)
  where
    usage = Map.lookup "usage_usec"

-- | The window from a start sample to now, with the CPUs the proxy's cgroup and the harness cgroup may run on.
cpuWindowSince :: CpuSample -> Maybe CgroupReading -> IO CpuWindow
cpuWindowSince start proxy = do
    end <- cpuSampleNow proxy
    harness <- traverse readCpuList =<< harnessFile "cpuset.cpus.effective"
    ticksPerSecond <- getSysVar ClockTick
    pure (cpuWindow (fromIntegral ticksPerSecond) (crCpus =<< proxy) (join harness) start end)

-- A file of the harness cgroup, when the run has one.
harnessFile :: FilePath -> IO (Maybe FilePath)
harnessFile name = fmap (\base -> base </> harnessLeaf </> name) <$> lookupEnv "BENCH_LOAD_CGROUP"
