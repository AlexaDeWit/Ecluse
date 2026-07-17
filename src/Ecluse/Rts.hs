-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Resolving and applying the process's runtime posture -- how many capabilities
Écluse claims and what heap ceiling it runs under -- from first-class configuration
with a cgroup-derived fallback, logged at boot with each decision's provenance.

The GHC RTS sizes itself from what the /machine/ looks like: bare @-N@ claims a
capability per visible processor, and the heap is unbounded unless @-M@ says
otherwise. In a container neither default matches the pod: a CPU __limit__ is a
cgroup quota that does not shrink the visible processor count, so the RTS claims a
whole node's worth of capabilities under a two-CPU quota, and the only memory
backstop is the kernel OOM killer. This module closes that gap the way Go's
@automaxprocs@ does, but config-first:

1. __Explicit configuration wins__: @cores@ (@ECLUSE_RUNTIME__CORES@) and @maxHeapBytes@
   (@ECLUSE_RUNTIME__MAX_HEAP_BYTES@).
2. __Omitted values fall back to the cgroup__ (v2): @cpu.max@'s quota, __floored__
   (at least one) and clamped to the visible processors, and @memory.max@ less the
   nursery budget and slack ('deriveMaxHeapBytes'). Flooring follows Go's
   @automaxprocs@: a capability count above the budget lets a stop-the-world
   collection outrun the CFS quota and freeze mid-pause, so a fractional
   entitlement is stranded rather than borrowed against.
3. __No limit found either way__: the posture the RTS already resolved (its baked
   defaults plus any @GHCRTS@ the operator set) stands, and the log says so.

Every decision is logged through the standard boot log with its provenance
('renderRuntimePosture'), so an operator reads what was decided or interpreted
straight from the start-up lines.

This resolution is __role-agnostic on purpose, and only the resolution__: cores and
the heap ceiling derive from the container's limits, which bind every role (proxy,
Pilot, Dredger) alike. Workload-shaped tuning -- the allocation area, sized for the
proxy's serve path -- is deliberately not modelled per role here; a role whose
profile diverges is tuned per-deployment via @GHCRTS@ until its shape earns a
default of its own.

== Applying the plan: 'setNumCapabilities', or one exec-in-place

A capability change is applied in-process ('GHC.Conc.setNumCapabilities'). The heap
ceiling has no in-process setter -- @-M@ is fixed when the RTS starts -- so when the
plan requires one, the boot __re-executes its own binary once__ with the resolved
flags appended to @GHCRTS@ (later flags win, verified against GHC 9.10). The exec
replaces the program image in the same process: the PID never exits, so a container
supervisor sees an uninterrupted process, exactly as an @exec@-ing entrypoint script
behaves. A marker variable ('reexecMarker') guards against loops: the re-launched
process sees it, skips any further exec, and only logs (a warning, if the RTS still
diverges from the plan -- an operator's @GHCRTS@ fighting the config, or a flag the
RTS rejected). A failure of the exec call itself is likewise degraded to a warning
and an unenforced posture: tuning never loops the boot and never takes the service
down.

The pure resolution ('resolveRuntimePlan'), the cgroup parsing ('parseCpuMax',
'parseMemoryMax'), and the rendering are separated from the thin IO shell
('applyRuntimePosture') so the precedence and arithmetic are unit-tested without a
cgroup in sight. Sizes are bytes everywhere here; the RTS flag fields count 4 KiB
blocks and are converted at the read boundary ('rtsBlockBytes').
-}
module Ecluse.Rts (
    -- * Applying the resolved posture at boot
    applyRuntimePosture,

    -- * The pure resolution core
    RtsPosture (..),
    CgroupLimits (..),
    Provenance (..),
    RuntimePlan (..),
    provenanceClause,
    resolveRuntimePlan,
    currentRtsPosture,
    readCgroupLimits,
    deriveMaxHeapBytes,
    requiredRtsFlags,
    renderRuntimePosture,

    -- * Cgroup v2 parsing
    parseCpuMax,
    parseMemoryMax,
    parseCgroupSelfPath,
    ancestorPaths,
) where

import Data.Text qualified as T
import GHC.Conc (getNumCapabilities, getNumProcessors, setNumCapabilities)
import GHC.RTS.Flags (GCFlags (maxHeapSize, minAllocAreaSize, nurseryChunkSize), getGCFlags)
import System.Environment (getEnvironment, getExecutablePath)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Process (executeFile)
import UnliftIO (tryIO, tryJust)

{- | The RTS posture the process is actually running with, in bytes. Read once at
boot ('currentRtsPosture'); the plan is resolved against it and the log renders it.
-}
data RtsPosture = RtsPosture
    { rpCapabilities :: Int
    -- ^ Capabilities claimed ('getNumCapabilities' at boot).
    , rpProcessors :: Int
    -- ^ Processors the RTS can see -- the ceiling a derived capability count clamps to.
    , rpAllocAreaBytes :: Int
    -- ^ The per-capability allocation area (@-A@), bytes.
    , rpNurseryChunkBytes :: Maybe Int
    -- ^ The nursery chunk size (@-n@), bytes; 'Nothing' when unset.
    , rpMaxHeapBytes :: Maybe Int
    -- ^ The heap ceiling (@-M@), bytes; 'Nothing' when unlimited.
    }
    deriving stock (Eq, Show)

{- | What the cgroup (v2) grants this process: the CPU quota in cores
(@cpu.max@, quota over period) and the memory ceiling in bytes (@memory.max@).
'Nothing' per axis when the file is absent (not a cgroup-v2 environment) or the
value is the unlimited @max@ sentinel.
-}
data CgroupLimits = CgroupLimits
    { cgCpuCores :: Maybe Double
    , cgMemoryMaxBytes :: Maybe Int
    }
    deriving stock (Eq, Show)

-- | Where a resolved value came from, for the boot log's provenance clause.
data Provenance
    = -- | Explicit Écluse configuration (@cores@ \/ @maxHeapBytes@).
      FromConfig
    | -- | Derived from the cgroup limits.
      FromCgroup
    | -- | Left as the RTS resolved it (baked defaults plus any operator @GHCRTS@).
      FromRts
    deriving stock (Eq, Show)

{- | The resolved runtime posture: the capability count to run with and the heap
ceiling to enforce, each with its provenance. A 'FromRts' entry means "leave it
alone": the plan never overrides a posture it has no better information than.
-}
data RuntimePlan = RuntimePlan
    { planCapabilities :: (Int, Provenance)
    , planMaxHeapBytes :: (Maybe Int, Provenance)
    }
    deriving stock (Eq, Show)

{- | Resolve the runtime plan from the three layers, strongest first: explicit
config, then the cgroup, then the live RTS posture.

Capabilities: an explicit @cores@ wins; else the cgroup CPU quota rounded __up__
(a 0.5-CPU pod still needs one capability) and clamped to the visible processors;
else the RTS's own count stands. Always at least 1.

Heap ceiling: an explicit @maxHeapBytes@ wins; else 'deriveMaxHeapBytes' over the
cgroup memory limit and the __planned__ capability count (the nursery the process
will actually run with); else the RTS posture stands -- notably, an operator's
@GHCRTS -M@ is never overridden by mere derivation, and an absent limit is left
absent rather than fabricated.
-}
resolveRuntimePlan :: Maybe Int -> Maybe Int -> CgroupLimits -> RtsPosture -> RuntimePlan
resolveRuntimePlan cfgCores cfgMaxHeap cgroup rts =
    RuntimePlan
        { planCapabilities = capabilities
        , planMaxHeapBytes = maxHeap
        }
  where
    capabilities = case (cfgCores, cgCpuCores cgroup) of
        (Just n, _) -> (max 1 n, FromConfig)
        -- Floored, as Go's automaxprocs does: claiming above the budget would let a
        -- stop-the-world collection (all capabilities at once) outrun the CFS quota
        -- and freeze mid-pause. The clamp's floor of one covers sub-1 quotas.
        (Nothing, Just quota) -> (clamp (floor quota), FromCgroup)
        (Nothing, Nothing) -> (rpCapabilities rts, FromRts)

    clamp n = max 1 (min (rpProcessors rts) n)

    maxHeap = case (cfgMaxHeap, cgMemoryMaxBytes cgroup) of
        (Just bytes, _) -> (Just (alignToBlock bytes), FromConfig)
        (Nothing, Just memMax) ->
            (Just (deriveMaxHeapBytes memMax (fst capabilities) (rpAllocAreaBytes rts)), FromCgroup)
        (Nothing, Nothing) -> (rpMaxHeapBytes rts, FromRts)

{- | The heap ceiling derived from a cgroup memory limit: the limit less the
nursery budget (capabilities x allocation area -- memory the process spends over
and above the heap) less 10% slack for stacks, buffers, and the RTS itself,
floored at half the limit so a nursery mis-sized for a tiny pod still yields a
sane ceiling rather than a vanishing (or negative) one.
-}
deriveMaxHeapBytes :: Int -> Int -> Int -> Int
deriveMaxHeapBytes memMax capabilities allocAreaBytes =
    alignToBlock (max (memMax - nursery - slack) (memMax `div` 2))
  where
    nursery = capabilities * allocAreaBytes
    slack = memMax `div` 10

{- A heap ceiling rounded down to the RTS's 4 KiB block granularity (and at least
one block): the RTS stores @-M@ in blocks, so a non-multiple value would read back
rounded and the plan would forever look unapplied after the re-exec. -}
alignToBlock :: Int -> Int
alignToBlock bytes = max rtsBlockBytes (bytes - bytes `mod` rtsBlockBytes)

{- | The RTS flags the plan requires beyond the live posture, in @GHCRTS@ syntax:
a @-N@ when the capability count must change, a @-M@ when a ceiling must be
enforced that is not already in force. Empty when the process is already running
the plan. A 'FromRts' entry never contributes a flag (it /is/ the live posture).
-}
requiredRtsFlags :: RtsPosture -> RuntimePlan -> [Text]
requiredRtsFlags rts plan =
    catMaybes [capsFlag, heapFlag]
  where
    capsFlag = case planCapabilities plan of
        (_, FromRts) -> Nothing
        (n, _)
            | n == rpCapabilities rts -> Nothing
            | otherwise -> Just ("-N" <> show n)

    heapFlag = case planMaxHeapBytes plan of
        (_, FromRts) -> Nothing
        (Nothing, _) -> Nothing
        (Just bytes, _)
            | Just bytes == rpMaxHeapBytes rts -> Nothing
            | otherwise -> Just ("-M" <> show bytes)

{- | The boot log's posture lines, one decision per line with its provenance, plus
the allocation-area line (always RTS-sourced; it is deliberately not config-surfaced).
Rendered from the __plan__, so the lines describe what the process runs with after
the plan is applied.
-}
renderRuntimePosture :: RuntimePlan -> RtsPosture -> [Text]
renderRuntimePosture plan rts =
    [ "runtime: capabilities " <> show (fst (planCapabilities plan)) <> renderProvenance (snd (planCapabilities plan))
    , case planMaxHeapBytes plan of
        (Just bytes, prov) -> "runtime: max heap " <> renderMiB bytes <> renderProvenance prov
        (Nothing, _) -> "runtime: max heap unbounded (the container memory limit is the only backstop; set maxHeapBytes or -M for a graceful ceiling)"
    , "runtime: allocation area "
        <> renderMiB (rpAllocAreaBytes rts)
        <> "/capability"
        <> maybe "" (\c -> ", nursery chunks " <> renderMiB c) (rpNurseryChunkBytes rts)
        <> " (RTS; tune with GHCRTS, see USAGE.md)"
    ]

renderProvenance :: Provenance -> Text
renderProvenance prov = " (" <> provenanceClause prov <> ")"

{- | The provenance as a bare clause, for consumers composing their own lines
(the memory budget's, beside this module's posture lines).
-}
provenanceClause :: Provenance -> Text
provenanceClause = \case
    FromConfig -> "from config"
    FromCgroup -> "derived from the cgroup limit"
    FromRts -> "as the RTS resolved it"

-- A byte count in MiB: whole when exact, else to one decimal place.
renderMiB :: Int -> Text
renderMiB bytes =
    let mib = fromIntegral bytes / (1024 * 1024) :: Double
     in if fromIntegral (round mib :: Int) == mib
            then show (round mib :: Int) <> " MiB"
            else toText (showRounded mib) <> " MiB"

showRounded :: Double -> String
showRounded x = show (fromIntegral (round (x * 10) :: Int) / 10 :: Double)

{- | Parse a cgroup-v2 @cpu.max@ body: @\"<quota> <period>\"@ yields the granted
cores (quota over period); the @\"max ...\"@ sentinel (no quota) yields 'Nothing'.
A malformed body yields 'Nothing' -- no limit is inferred from noise.
-}
parseCpuMax :: Text -> Maybe Double
parseCpuMax body = case T.words (T.strip body) of
    [quota, period] -> do
        q <- readMaybe (toString quota) :: Maybe Double
        p <- readMaybe (toString period) :: Maybe Double
        guard (q > 0 && p > 0)
        pure (q / p)
    _ -> Nothing

{- | Parse a cgroup-v2 @memory.max@ body: a byte count, or the unlimited @max@
sentinel ('Nothing'). A malformed body yields 'Nothing'.
-}
parseMemoryMax :: Text -> Maybe Int
parseMemoryMax body = do
    n <- readMaybe (toString (T.strip body)) :: Maybe Int
    guard (n > 0)
    pure n

{- | Resolve the runtime plan and apply it, first thing at boot.

Reads the live posture and the cgroup, resolves the plan against the given config
values, and then:

* plan already in force: log the posture lines and return;
* only the capability count differs: apply it in-process
  ('setNumCapabilities'), log, and return;
* a heap ceiling must be enforced: append the required flags to @GHCRTS@ and
  __exec this binary in place__ (same PID, same arguments), once, guarded by
  'reexecMarker'. The re-launched process resolves the same plan, finds it in
  force, and logs the posture lines as normal.

When the marker is already set and the posture /still/ diverges (an operator's
@GHCRTS@ contradicting the config, or a flag the RTS rejected), the divergence is
logged as a warning and the process continues with what the RTS gave it -- boot
never loops and never aborts over tuning.
-}
applyRuntimePosture :: (Text -> IO ()) -> (Text -> IO ()) -> Maybe Int -> Maybe Int -> IO RuntimePlan
applyRuntimePosture logInfo logWarning cfgCores cfgMaxHeap = do
    rts <- currentRtsPosture
    cgroup <- readCgroupLimits
    let plan = resolveRuntimePlan cfgCores cfgMaxHeap cgroup rts
        flags = requiredRtsFlags rts plan
    alreadyApplied <- isJust <$> lookupEnv reexecMarker
    case flags of
        [] -> logPosture plan rts
        _ | alreadyApplied -> do
            warnStillDivergent logWarning flags
            logPosture plan rts
        [capsOnly]
            | "-N" `T.isPrefixOf` capsOnly -> do
                setNumCapabilities (fst (planCapabilities plan))
                logPosture plan rts{rpCapabilities = fst (planCapabilities plan)}
        _ -> do
            reexecOrWarn logInfo logWarning flags
            logPosture plan rts
    -- The resolved plan (capabilities and heap ceiling, each with provenance) is
    -- the datapoint the downstream sizings compute from at the composition root:
    -- admission from capabilities ("Ecluse.Composition.Sizing"), the byte bounds
    -- from the ceiling ("Ecluse.Composition.MemoryBudget").
    pure plan
  where
    logPosture plan rts = traverse_ logInfo (renderRuntimePosture plan rts)

-- The already-re-launched process found its plan still unapplied: warn and
-- continue with the live posture.
warnStillDivergent :: (Text -> IO ()) -> [Text] -> IO ()
warnStillDivergent logWarning flags =
    logWarning
        ( "runtime: the resolved plan still requires "
            <> T.intercalate " " flags
            <> " after re-launch; an operator GHCRTS may be overriding the configuration, or the RTS rejected a flag. Continuing with the live posture."
        )

{- Tuning must never take the service down: a failed exec (essentially
unreachable -- the path is /proc/self/exe -- but not guaranteed) is degraded to a
warning and an unenforced posture, never an abort. The exec itself never returns
on success, so reaching the continuation at all means it failed. -}
reexecOrWarn :: (Text -> IO ()) -> (Text -> IO ()) -> [Text] -> IO ()
reexecOrWarn logInfo logWarning flags =
    tryIO (reexecWith logInfo flags) >>= \case
        Left err ->
            logWarning
                ( "runtime: re-launching to apply "
                    <> T.intercalate " " flags
                    <> " failed ("
                    <> show err
                    <> "); continuing with the live posture, unenforced."
                )
        Right () -> pass

-- The live RTS posture, converted from the flag fields' 4 KiB blocks to bytes.
currentRtsPosture :: IO RtsPosture
currentRtsPosture = do
    capabilities <- getNumCapabilities
    processors <- getNumProcessors
    gc <- getGCFlags
    let blocks n = fromIntegral n * rtsBlockBytes
    pure
        RtsPosture
            { rpCapabilities = capabilities
            , rpProcessors = processors
            , rpAllocAreaBytes = blocks (minAllocAreaSize gc)
            , rpNurseryChunkBytes = nonZero (blocks (nurseryChunkSize gc))
            , rpMaxHeapBytes = nonZero (blocks (maxHeapSize gc))
            }
  where
    nonZero n = if n <= 0 then Nothing else Just n

-- The RTS flag fields ('minAllocAreaSize', 'nurseryChunkSize', 'maxHeapSize')
-- count blocks of this many bytes (verified against GHC 9.10: -A64m reads back as
-- 16384, -M500m as 128000).
rtsBlockBytes :: Int
rtsBlockBytes = 4096

{- The cgroup-v2 limits that bind this process: its own cgroup (resolved from
@\/proc\/self\/cgroup@) and every ancestor up to the mount root, each axis taking the
__tightest__ limit found along the walk. Inside a container with a private cgroup
namespace the process's cgroup /is/ the visible root, so the walk is one step; on a
host (or a pod whose limit sits on a parent slice) the leaf alone would miss the
binding limit, which is why the ancestors are consulted too. Absent files and the
@max@ sentinel read as no limit at that level; a host with no cgroup v2 mounted
yields no limits at all. -}
readCgroupLimits :: IO CgroupLimits
readCgroupLimits = do
    selfCgroup <- readIfExists "/proc/self/cgroup"
    let relative = fromMaybe "/" (selfCgroup >>= parseCgroupSelfPath)
        dirs = [cgroupRoot <> toString suffix | suffix <- ancestorPaths relative]
    cpus <- traverse (limitAt parseCpuMax "/cpu.max") dirs
    memories <- traverse (limitAt parseMemoryMax "/memory.max") dirs
    pure
        CgroupLimits
            { cgCpuCores = tightest cpus
            , cgMemoryMaxBytes = tightest memories
            }
  where
    cgroupRoot = "/sys/fs/cgroup"

    limitAt :: (Text -> Maybe a) -> String -> FilePath -> IO (Maybe a)
    limitAt parse file dir = (>>= parse) <$> readIfExists (dir <> file)

    tightest :: (Ord a) => [Maybe a] -> Maybe a
    tightest found = case catMaybes found of
        [] -> Nothing
        (x : xs) -> Just (foldl' min x xs)

    readIfExists :: FilePath -> IO (Maybe Text)
    readIfExists path =
        rightToMaybe <$> tryJust (guard . isDoesNotExistError) (decodeUtf8 <$> readFileBS path)

{- | The process's cgroup-v2 path from a @\/proc\/self\/cgroup@ body: the @0::@
line's path (@"0::\/a\/b"@ yields @"\/a\/b"@). 'Nothing' when no v2 entry is
present (a pure cgroup-v1 host).
-}
parseCgroupSelfPath :: Text -> Maybe Text
parseCgroupSelfPath body =
    listToMaybe (mapMaybe (T.stripPrefix "0::") (lines (T.strip body)))

{- | A cgroup path and its ancestors, leaf first, ending at the root (the empty
suffix): @"\/a\/b"@ yields @["\/a\/b", "\/a", ""]@; the root path @"\/"@
yields just @[""]@.
-}
ancestorPaths :: Text -> [Text]
ancestorPaths path = case filter (not . T.null) (T.splitOn "/" (T.strip path)) of
    [] -> [""]
    segments ->
        [T.concat ["/" <> seg | seg <- take n segments] | n <- [length segments, length segments - 1 .. 1]] <> [""]

{- The environment marker that makes the exec-in-place a one-shot: set on the
re-launched process's environment, checked before any further exec. Deliberately
__outside__ the @ECLUSE_@ prefix: everything under that prefix is claimed by the
configuration environment layer (and rejected when unknown), and this marker is
process plumbing, not configuration. -}
reexecMarker :: String
reexecMarker = "__ECLUSE_RUNTIME_RTS_APPLIED"

{- Exec this binary in place with the required flags appended to @GHCRTS@ (later
flags win over both the baked defaults and any earlier operator flags) and the
loop-guard marker set. Same executable path, same arguments, same PID -- the
process never exits, so a container supervisor sees one uninterrupted process. -}
reexecWith :: (Text -> IO ()) -> [Text] -> IO ()
reexecWith logInfo flags = do
    self <- getExecutablePath
    args <- getArgs
    env <- getEnvironment
    let prior = snd <$> find ((== "GHCRTS") . fst) env
        appended = maybe newFlags (\p -> toText p <> " " <> newFlags) prior
        env' =
            (("GHCRTS", toString appended) :)
                . ((reexecMarker, "1") :)
                . filter (\(k, _) -> k /= "GHCRTS" && k /= reexecMarker)
                $ env
    logInfo ("runtime: re-launching with GHCRTS " <> appended <> " to apply the resolved plan (same process, exec in place)")
    executeFile self False args (Just env')
  where
    newFlags = T.intercalate " " flags
