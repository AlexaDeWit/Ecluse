-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Resolving and applying the process's runtime posture: how many capabilities Écluse
claims, and what heap ceiling it runs under.

The GHC RTS sizes itself from what the /machine/ looks like. Bare @-N@ claims a capability
per visible processor, and the heap is unbounded unless @-M@ says otherwise. In a container
neither default matches the pod. A CPU limit is a cgroup quota that does not shrink the
visible processor count, so the RTS claims a whole node's worth of capabilities under a
two-CPU quota, and the kernel OOM killer is the only memory backstop.

== The capability ladder

Capabilities resolve down four rungs, best first, and the boot line names the rung that
fired. Every rung below the first clamps to the visible processors.

1. @cores@ (@ECLUSE_RUNTIME__CORES@), the explicit operator lever, is obeyed absolutely.
2. A cgroup CPU quota, __floored__ as Go's @automaxprocs@ floors it: a stop-the-world
   collection claiming above the CFS quota would freeze mid-pause, so a fractional
   entitlement is stranded rather than borrowed against.
3. No quota but a cgroup memory limit: the count that budget can feed
   ('nurseryFittedCapabilities'). The nursery charge is capabilities x the allocation
   area, so a count the memory limit cannot feed is the traffic-surge shape that ends in
   an OOM kill.
4. Nothing binds: @coresCeiling@, default 8. A whole node's worth of capabilities buys
   nothing on the serve path and costs a nursery each.

Rungs 3 and 4 warn ('renderPostureWarnings'), because neither reads an entitlement: the
process cannot tell an unlimited Kubernetes pod from bare metal, and only @cores@ can.

The heap ceiling resolves over @maxHeapBytes@ (@ECLUSE_RUNTIME__MAX_HEAP_BYTES@), else
@memory.max@ less the nursery budget and slack ('deriveMaxHeapBytes'), else the ceiling the
RTS already resolved (its baked defaults plus any operator @GHCRTS@).

The resolution is __role-agnostic on purpose, and only the resolution__: the limits it reads
bind every role (proxy, Pilot, Dredger) alike. Workload-shaped tuning per role is absent
because the allocation area, for one, is sized for the proxy's serve path. Tune a role whose
profile diverges through @GHCRTS@, until its shape earns a default of its own.

== Applying the plan: 'setNumCapabilities', or one exec-in-place

The boot applies a capability change in-process ('GHC.Conc.setNumCapabilities'). The heap
ceiling has no in-process setter, because the RTS fixes @-M@ when it starts. So when the plan
requires one, the boot __re-executes its own binary once__, with the resolved flags appended
to @GHCRTS@. Later flags win, verified against GHC 9.10. The exec replaces the program image
in the same process, so the PID never exits and a container supervisor sees one uninterrupted
process.

A marker variable ('reexecMarker') guards against loops. The re-launched process sees it,
skips any further exec, and only logs. It warns if the RTS still diverges from the plan: an
operator's @GHCRTS@ fighting the config, or a flag the RTS rejected. A failure of the exec
call itself degrades to a warning and an unenforced posture too. Tuning never loops the boot
and never takes the service down.

The pure resolution ('resolveRuntimePlan'), the cgroup parsing, and the rendering sit apart
from the thin IO shell ('applyRuntimePosture'), so a unit test exercises the ladder without a
cgroup in sight. Sizes are bytes everywhere here, and the read boundary converts the RTS flag
fields' 4 KiB blocks ('rtsBlockBytes').
-}
module Ecluse.Rts (
    -- * Applying the resolved posture at boot
    applyRuntimePosture,

    -- * The pure resolution core
    RtsPosture (..),
    CgroupLimits (..),
    RuntimeOverrides (..),
    Provenance (..),
    RuntimePlan (..),
    provenanceClause,
    resolveRuntimePlan,
    currentRtsPosture,
    readCgroupLimits,
    deriveMaxHeapBytes,
    nurseryFittedCapabilities,
    requiredRtsFlags,

    -- * The effective plan (desired reconciled with observed)
    EffectiveAxis (..),
    EffectiveRuntimePlan (..),
    axEnforced,
    reconcileRuntimePlan,
    appliedRuntimePlan,
    effectiveCapabilities,
    effectiveHeapCeiling,
    renderEffectivePosture,
    renderPostureWarnings,

    -- * Cgroup v2 parsing
    parseCpuMax,
    parseMemoryMax,
) where

import Data.Ord (clamp)
import Data.Text qualified as T
import GHC.Conc (getNumCapabilities, getNumProcessors, setNumCapabilities)
import GHC.RTS.Flags (GCFlags (maxHeapSize, minAllocAreaSize, nurseryChunkSize), getGCFlags)
import System.Environment (getEnvironment, getExecutablePath)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Process (executeFile)
import UnliftIO (tryIO, tryJust)

-- | The RTS posture the process is actually running with, read at boot by 'currentRtsPosture'.
data RtsPosture = RtsPosture
    { rpCapabilities :: Int
    -- ^ Capabilities claimed ('getNumCapabilities' at boot).
    , rpProcessors :: Int
    -- ^ Processors the RTS can see: the ceiling a derived capability count clamps to.
    , rpAllocAreaBytes :: Int
    -- ^ The per-capability allocation area (@-A@), bytes.
    , rpNurseryChunkBytes :: Maybe Int
    -- ^ The nursery chunk size (@-n@), bytes. 'Nothing' when unset.
    , rpMaxHeapBytes :: Maybe Int
    -- ^ The heap ceiling (@-M@), bytes. 'Nothing' when unlimited.
    }
    deriving stock (Eq, Show)

{- | What the cgroup (v2) grants this process: the CPU quota in cores (@cpu.max@, quota over
period) and the memory ceiling in bytes (@memory.max@). 'Nothing' per axis when the file is absent
or the value is the unlimited @max@ sentinel.
-}
data CgroupLimits = CgroupLimits
    { cgCpuCores :: Maybe Double
    , cgMemoryMaxBytes :: Maybe Int
    }
    deriving stock (Eq, Show)

{- | The @runtime@ configuration the resolution reads. Each unset field falls to the next rung,
and 'roCoresCeiling' bounds the last rung alone.
-}
data RuntimeOverrides = RuntimeOverrides
    { roCores :: Maybe Int
    , roCoresCeiling :: Maybe Int
    , roMaxHeapBytes :: Maybe Int
    }
    deriving stock (Eq, Show)

-- | Where a resolved value came from, for the boot log's provenance clause.
data Provenance
    = -- | Explicit Écluse configuration (@cores@ \/ @maxHeapBytes@).
      FromConfig
    | -- | Derived from the cgroup CPU quota, or from @memory.max@ on the heap axis.
      FromCgroup
    | -- | Bounded by what the cgroup memory limit can feed, for want of a CPU quota.
      FromCgroupMemory
    | -- | Capped at @coresCeiling@, with no cgroup limit of either kind in force.
      FromCoresCeiling
    | -- | Left as the RTS resolved it (baked defaults plus any operator @GHCRTS@).
      FromRts
    deriving stock (Eq, Show)

{- | The resolved runtime posture: the capability count and heap ceiling to run with, each with
its provenance. A 'FromRts' entry means leave the live posture alone.
-}
data RuntimePlan = RuntimePlan
    { planCapabilities :: (Int, Provenance)
    , planMaxHeapBytes :: (Maybe Int, Provenance)
    }
    deriving stock (Eq, Show)

{- | Resolve the runtime plan: capabilities down the four-rung ladder, and the heap ceiling from
@maxHeapBytes@, else the cgroup memory limit, else the live RTS posture. Derivation never
overrides an operator's @GHCRTS -M@.
-}
resolveRuntimePlan :: RuntimeOverrides -> CgroupLimits -> RtsPosture -> RuntimePlan
resolveRuntimePlan overrides cgroup rts =
    RuntimePlan
        { planCapabilities = capabilities
        , planMaxHeapBytes = maxHeap
        }
  where
    capabilities = case (roCores overrides, cgCpuCores cgroup, cgMemoryMaxBytes cgroup) of
        (Just n, _, _) -> (max 1 n, FromConfig)
        (Nothing, Just quota, _) -> (visible (floor quota), FromCgroup)
        (Nothing, Nothing, Just memMax) ->
            (visible (nurseryFittedCapabilities memMax (rpAllocAreaBytes rts)), FromCgroupMemory)
        (Nothing, Nothing, Nothing) ->
            (visible (fromMaybe defaultCoresCeiling (roCoresCeiling overrides)), FromCoresCeiling)

    -- Every derived rung floors at one capability and ceilings at the visible processors.
    visible = clamp (1, rpProcessors rts)

    maxHeap = case (roMaxHeapBytes overrides, cgMemoryMaxBytes cgroup) of
        (Just bytes, _) -> (Just (alignToBlock bytes), FromConfig)
        (Nothing, Just memMax) ->
            (Just (deriveMaxHeapBytes memMax (fst capabilities) (rpAllocAreaBytes rts)), FromCgroup)
        (Nothing, Nothing) -> (rpMaxHeapBytes rts, FromRts)

-- The last rung's cap, when no cgroup limit says anything. It is a policy stance, not a machine
-- property, so @runtime.coresCeiling@ overrides it rather than any derivation.
defaultCoresCeiling :: Int
defaultCoresCeiling = 8

{- | The capability count a memory budget can feed: the nursery charge is capabilities x the
allocation area, and it may claim at most the @nurseryCeilingShareDiv@ share of the budget.
-}
nurseryFittedCapabilities :: Int -> Int -> Int
nurseryFittedCapabilities budgetBytes allocAreaBytes =
    max 1 (budgetBytes `div` nurseryCeilingShareDiv `div` max 1 allocAreaBytes)

-- The share of a memory budget the nursery may hold before the capability count is what has
-- to give. The memory-plan shed ladder bounds the same charge against the heap ceiling.
nurseryCeilingShareDiv :: Int
nurseryCeilingShareDiv = 4

{- | The heap ceiling derived from a cgroup memory limit. The nursery sits outside the heap, so
it comes off the limit, and the half-limit floor keeps a tiny pod's ceiling from vanishing.
-}
deriveMaxHeapBytes :: Int -> Int -> Int -> Int
deriveMaxHeapBytes memMax capabilities allocAreaBytes =
    alignToBlock (max (memMax - nursery - slack) (memMax `div` 2))
  where
    nursery = capabilities * allocAreaBytes
    slack = memMax `div` 10

{- A heap ceiling rounded down to the RTS's 4 KiB block granularity. The RTS stores @-M@ in
blocks, so a non-multiple would read back rounded and the plan would look unapplied forever. -}
alignToBlock :: Int -> Int
alignToBlock bytes = max rtsBlockBytes (bytes - bytes `mod` rtsBlockBytes)

{- | One axis of the runtime posture after the boot applied the plan. An apply can fail, so
downstream sizings read 'effectiveCapabilities' and 'effectiveHeapCeiling', never the desired plan.
-}
data EffectiveAxis a = EffectiveAxis
    { axDesired :: a
    -- ^ What the resolution wanted ('resolveRuntimePlan').
    , axObserved :: a
    -- ^ What the RTS reports after the apply attempt.
    , axProvenance :: Provenance
    -- ^ Where the desired value came from.
    }
    deriving stock (Eq, Show)

-- | Whether the live RTS backs an axis (desired and observed agree).
axEnforced :: (Eq a) => EffectiveAxis a -> Bool
axEnforced ax = axDesired ax == axObserved ax

{- | The runtime plan reconciled with the posture the RTS actually runs: each planned axis as a
desired\/observed pair, plus the observed-only datapoints downstream sizing needs.
-}
data EffectiveRuntimePlan = EffectiveRuntimePlan
    { erpCapabilities :: EffectiveAxis Int
    , erpMaxHeapBytes :: EffectiveAxis (Maybe Int)
    , erpAllocAreaBytes :: Int
    -- ^ The per-capability allocation area, observed only (never planned here).
    , erpNurseryChunkBytes :: Maybe Int
    -- ^ The nursery chunk size, observed only.
    , erpContainerMemoryBytes :: Maybe Int
    -- ^ The cgroup @memory.max@ datapoint, when one binds this process.
    }
    deriving stock (Eq, Show)

-- | Pair the desired plan with the posture the RTS reports, axis by axis.
reconcileRuntimePlan :: CgroupLimits -> RuntimePlan -> RtsPosture -> EffectiveRuntimePlan
reconcileRuntimePlan cgroup plan posture =
    EffectiveRuntimePlan
        { erpCapabilities =
            EffectiveAxis
                { axDesired = fst (planCapabilities plan)
                , axObserved = rpCapabilities posture
                , axProvenance = snd (planCapabilities plan)
                }
        , erpMaxHeapBytes =
            EffectiveAxis
                { axDesired = fst (planMaxHeapBytes plan)
                , axObserved = rpMaxHeapBytes posture
                , axProvenance = snd (planMaxHeapBytes plan)
                }
        , erpAllocAreaBytes = rpAllocAreaBytes posture
        , erpNurseryChunkBytes = rpNurseryChunkBytes posture
        , erpContainerMemoryBytes = cgMemoryMaxBytes cgroup
        }

{- | The effective plan a successful application would produce, observed equal to desired.
@check-config@ sizes from this because it applies nothing, so its own posture is not the boot's.
-}
appliedRuntimePlan :: CgroupLimits -> RuntimePlan -> RtsPosture -> EffectiveRuntimePlan
appliedRuntimePlan cgroup plan posture =
    (reconcileRuntimePlan cgroup plan posture)
        { erpCapabilities = enforced (planCapabilities plan)
        , erpMaxHeapBytes = enforced (planMaxHeapBytes plan)
        }
  where
    enforced (v, prov) = EffectiveAxis{axDesired = v, axObserved = v, axProvenance = prov}

{- | The live capability count: budgets must never exceed what the RTS actually runs with, so the
observed side is authoritative. An unenforced count degrades the provenance to 'FromRts'.
-}
effectiveCapabilities :: EffectiveRuntimePlan -> (Int, Provenance)
effectiveCapabilities p =
    let ax = erpCapabilities p
     in (axObserved ax, if axEnforced ax then axProvenance ax else FromRts)

{- | The sizing ceiling: the __tighter__ of desired and observed. An observed @-M@ below the plan
binds, and an absent one leaves the desired ceiling standing on the cgroup limit's OOM backstop.
-}
effectiveHeapCeiling :: EffectiveRuntimePlan -> (Maybe Int, Provenance)
effectiveHeapCeiling p =
    let ax = erpMaxHeapBytes p
     in case (axDesired ax, axObserved ax) of
            (Just desired, Just observed)
                | observed < desired -> (Just observed, FromRts)
            (Nothing, Just observed) -> (Just observed, FromRts)
            (desired, _) -> (desired, axProvenance ax)

{- | The RTS flags the plan requires beyond the live posture, in @GHCRTS@ syntax. A 'FromRts'
entry never contributes a flag, because it /is/ the live posture.
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

{- | The boot log's posture lines, one decision per line with its provenance. The allocation area
is always RTS-sourced and deliberately not config-surfaced.
-}
renderEffectivePosture :: EffectiveRuntimePlan -> [Text]
renderEffectivePosture p =
    [ "runtime: capabilities " <> show capabilities <> renderProvenance capsProvenance
    , case effectiveHeapCeiling p of
        (Just bytes, prov) -> "runtime: max heap " <> renderMiB bytes <> renderProvenance prov
        (Nothing, _) -> "runtime: max heap unbounded (the container memory limit is the only backstop; set maxHeapBytes or -M for a graceful ceiling)"
    , "runtime: allocation area "
        <> renderMiB (erpAllocAreaBytes p)
        <> "/capability"
        <> maybe "" (\c -> ", nursery chunks " <> renderMiB c) (erpNurseryChunkBytes p)
        <> " (RTS; tune with GHCRTS, see https://ecluse-proxy.com/docs/operations/)"
    ]
  where
    (capabilities, capsProvenance) = effectiveCapabilities p

{- | The boot log's posture warnings: an axis the RTS is not enforcing, and a capability count
no entitlement backs.
-}
renderPostureWarnings :: EffectiveRuntimePlan -> [Text]
renderPostureWarnings p = unenforcedWarnings p <> capabilityAdvice p

{- The last two rungs bound the count without reading an entitlement, so the advice is
conditional in form: the process cannot tell an unlimited pod from bare metal. -}
capabilityAdvice :: EffectiveRuntimePlan -> [Text]
capabilityAdvice p = case effectiveCapabilities p of
    (n, FromCgroupMemory) ->
        [advice ("no cgroup CPU quota binds this process, so capabilities are bounded at " <> show n <> " by what the cgroup memory limit can feed")]
    (n, FromCoresCeiling) ->
        [advice ("no cgroup CPU or memory limit binds this process, so capabilities are capped at " <> show n <> " by runtime.coresCeiling")]
    -- Listed rather than wildcarded, so a new rung has to decide whether it warns.
    (_, FromConfig) -> []
    (_, FromCgroup) -> []
    (_, FromRts) -> []
  where
    advice reason =
        "runtime: "
            <> reason
            <> ". If this container has a CPU request but no limit, set runtime.cores (ECLUSE_RUNTIME__CORES) to the whole cores requested."

{- One warning per axis the RTS is not enforcing. The budgets size from the effective value, so a
divergence must be legible in the boot log rather than silently absorbed. -}
unenforcedWarnings :: EffectiveRuntimePlan -> [Text]
unenforcedWarnings p =
    catMaybes
        [ warnAxis "capabilities" show (erpCapabilities p)
        , warnAxis "max heap" (maybe "unbounded" renderMiB) (erpMaxHeapBytes p)
        ]
  where
    warnAxis :: (Eq a) => Text -> (a -> Text) -> EffectiveAxis a -> Maybe Text
    warnAxis name render ax
        | axEnforced ax = Nothing
        | otherwise =
            Just
                ( "runtime: "
                    <> name
                    <> " desired "
                    <> render (axDesired ax)
                    <> " but the RTS is running with "
                    <> render (axObserved ax)
                    <> "; budgets use the effective value"
                )

renderProvenance :: Provenance -> Text
renderProvenance prov = " (" <> provenanceClause prov <> ")"

-- | The provenance as a bare clause, for consumers composing their own log lines.
provenanceClause :: Provenance -> Text
provenanceClause = \case
    FromConfig -> "from config"
    FromCgroup -> "derived from the cgroup limit"
    FromCgroupMemory -> "bounded by the cgroup memory limit, no CPU quota set"
    FromCoresCeiling -> "no cgroup CPU or memory limit found, capped at runtime.coresCeiling"
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

{- | Parse a cgroup-v2 @cpu.max@ body. @\"<quota> <period>\"@ yields the granted cores, and the
@max@ sentinel or a malformed body yields 'Nothing': no limit is inferred from noise.
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

{- | Resolve the runtime plan and apply it, first thing at boot. Enforcing a heap ceiling execs
this binary in place, once, guarded by 'reexecMarker', and never aborts the boot. The returned
plan is the effective one, so downstream sizing computes from what the RTS actually runs with.
-}
applyRuntimePosture :: (Text -> IO ()) -> (Text -> IO ()) -> RuntimeOverrides -> IO EffectiveRuntimePlan
applyRuntimePosture logInfo logWarning overrides = do
    rts <- currentRtsPosture
    cgroup <- readCgroupLimits
    let plan = resolveRuntimePlan overrides cgroup rts
        flags = requiredRtsFlags rts plan
    alreadyApplied <- isJust <$> lookupEnv reexecMarker
    case flags of
        [] -> pass
        _ | alreadyApplied -> warnStillDivergent logWarning flags
        [capsOnly]
            | "-N" `T.isPrefixOf` capsOnly ->
                setNumCapabilities (fst (planCapabilities plan))
        _ -> reexecOrWarn logInfo logWarning flags
    -- Reached only when no exec happened or the exec failed: a successful exec never returns.
    applied <- currentRtsPosture
    let effective = reconcileRuntimePlan cgroup plan applied
    traverse_ logInfo (renderEffectivePosture effective)
    traverse_ logWarning (renderPostureWarnings effective)
    pure effective

-- The already-re-launched process found its plan still unapplied: warn and
-- continue with the live posture.
warnStillDivergent :: (Text -> IO ()) -> [Text] -> IO ()
warnStillDivergent logWarning flags =
    logWarning
        ( "runtime: the resolved plan still requires "
            <> T.intercalate " " flags
            <> " after re-launch; an operator GHCRTS may be overriding the configuration, or the RTS rejected a flag. Continuing with the live posture."
        )

{- Tuning must never take the service down. A failed exec degrades to a warning and an unenforced
posture, never an abort. The exec returns only on failure. -}
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

-- The RTS flag fields ('minAllocAreaSize', 'nurseryChunkSize', 'maxHeapSize') count blocks of
-- this many bytes (GHC 9.10: -A64m reads back as 16384, -M500m as 128000).
rtsBlockBytes :: Int
rtsBlockBytes = 4096

{- The cgroup-v2 limits that bind this process: its own cgroup and every ancestor up to the mount
root, each axis taking the __tightest__ limit found. The leaf alone would miss a limit that sits
on a parent slice. Absent files and the @max@ sentinel read as no limit. -}
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

{- The process's cgroup-v2 path from a @\/proc\/self\/cgroup@ body: the @0::@ line's path
(@"0::\/a\/b"@ yields @"\/a\/b"@). 'Nothing' on a pure cgroup-v1 host.
-}
parseCgroupSelfPath :: Text -> Maybe Text
parseCgroupSelfPath body =
    listToMaybe (mapMaybe (T.stripPrefix "0::") (lines (T.strip body)))

{- A cgroup path and its ancestors, leaf first, ending at the root (the empty suffix).
@"\/a\/b"@ yields @["\/a\/b", "\/a", ""]@, and @"\/"@ yields just @[""]@.
-}
ancestorPaths :: Text -> [Text]
ancestorPaths path = case filter (not . T.null) (T.splitOn "/" (T.strip path)) of
    [] -> [""]
    segments ->
        [T.concat ["/" <> seg | seg <- take n segments] | n <- [length segments, length segments - 1 .. 1]] <> [""]

{- The one-shot guard for the exec-in-place. It sits outside the @ECLUSE_@ prefix because the
environment config layer rejects every unknown key under that prefix. -}
reexecMarker :: String
reexecMarker = "__ECLUSE_RUNTIME_RTS_APPLIED"

{- Exec this binary in place with the required flags appended to @GHCRTS@. Later flags win over
the baked defaults and any earlier operator flags. Same arguments and same PID, so a container
supervisor sees one uninterrupted process. -}
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
