-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.RuntimeSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Rts (
    CgroupLimits (CgroupLimits, cgCpuCores, cgMemoryMaxBytes),
    EffectiveRuntimePlan (erpCapabilities, erpMaxHeapBytes),
    Provenance (FromCgroup, FromCgroupMemory, FromConfig, FromCoresCeiling, FromRts),
    RtsPosture (..),
    RuntimeOverrides (RuntimeOverrides, roCores, roCoresCeiling, roMaxHeapBytes),
    RuntimePlan (planCapabilities, planMaxHeapBytes),
    appliedRuntimePlan,
    axEnforced,
    deriveMaxHeapBytes,
    effectiveCapabilities,
    effectiveHeapCeiling,
    parseCpuMax,
    parseMemoryMax,
    reconcileRuntimePlan,
    renderEffectivePosture,
    renderPostureWarnings,
    requiredRtsFlags,
    resolveRuntimePlan,
 )

spec :: Spec
spec = describe "Ecluse.Rts (runtime posture resolution)" $ do
    cgroupParsingSpec
    resolutionSpec
    ladderSpec
    derivationSpec
    flagsSpec
    reconcileSpec
    renderSpec

-- A live posture to resolve against: the shipped defaults on a 4-core box with
-- 4 capabilities claimed and no heap ceiling.
unpinned :: RtsPosture
unpinned =
    RtsPosture
        { rpCapabilities = 4
        , rpProcessors = 4
        , rpAllocAreaBytes = 64 * mib
        , rpNurseryChunkBytes = Just (4 * mib)
        , rpMaxHeapBytes = Nothing
        }

-- A 64-processor host running the shipped -N: far more processors than any rung
-- below the first would grant, so a bound is visible in the result.
bigNode :: RtsPosture
bigNode = unpinned{rpCapabilities = 64, rpProcessors = 64}

noCgroup :: CgroupLimits
noCgroup = CgroupLimits{cgCpuCores = Nothing, cgMemoryMaxBytes = Nothing}

-- Nothing configured, so every axis resolves down its ladder.
noOverrides :: RuntimeOverrides
noOverrides = RuntimeOverrides{roCores = Nothing, roCoresCeiling = Nothing, roMaxHeapBytes = Nothing}

mib :: Int
mib = 1024 * 1024

cgroupParsingSpec :: Spec
cgroupParsingSpec = describe "cgroup v2 parsing" $ do
    it "reads cpu.max quota over period as granted cores" $ do
        parseCpuMax "200000 100000\n" `shouldBe` Just 2.0
        parseCpuMax "50000 100000" `shouldBe` Just 0.5

    it "reads the cpu.max unlimited sentinel as no limit" $
        parseCpuMax "max 100000\n" `shouldBe` Nothing

    it "infers no cpu limit from a malformed body" $ do
        parseCpuMax "" `shouldBe` Nothing
        parseCpuMax "banana" `shouldBe` Nothing
        parseCpuMax "-100000 100000" `shouldBe` Nothing

    it "reads memory.max bytes and the unlimited sentinel" $ do
        parseMemoryMax "536870912\n" `shouldBe` Just (512 * mib)
        parseMemoryMax "max\n" `shouldBe` Nothing
        parseMemoryMax "much" `shouldBe` Nothing

resolutionSpec :: Spec
resolutionSpec = describe "resolveRuntimePlan precedence" $ do
    it "explicit config wins over the cgroup on both axes" $ do
        let cgroup = CgroupLimits{cgCpuCores = Just 4, cgMemoryMaxBytes = Just (1024 * mib)}
            plan = resolveRuntimePlan noOverrides{roCores = Just 2, roMaxHeapBytes = Just (400 * mib)} cgroup unpinned
        planCapabilities plan `shouldBe` (2, FromConfig)
        planMaxHeapBytes plan `shouldBe` (Just (400 * mib), FromConfig)

    it "derives from the cgroup when config is omitted" $ do
        let cgroup = CgroupLimits{cgCpuCores = Just 2, cgMemoryMaxBytes = Just (512 * mib)}
            plan = resolveRuntimePlan noOverrides cgroup unpinned
        planCapabilities plan `shouldBe` (2, FromCgroup)
        planMaxHeapBytes plan
            `shouldBe` (Just (deriveMaxHeapBytes (512 * mib) 2 (64 * mib)), FromCgroup)

    it "floors a fractional cpu quota, so capabilities never exceed the CFS budget" $ do
        let cgroup = noCgroup{cgCpuCores = Just 3.5}
        planCapabilities (resolveRuntimePlan noOverrides cgroup unpinned)
            `shouldBe` (3, FromCgroup)

    it "grants a sub-1 quota one capability rather than zero" $ do
        let cgroup = noCgroup{cgCpuCores = Just 0.5}
        planCapabilities (resolveRuntimePlan noOverrides cgroup unpinned)
            `shouldBe` (1, FromCgroup)

    it "clamps a derived capability count to the visible processors" $ do
        let cgroup = noCgroup{cgCpuCores = Just 64}
        planCapabilities (resolveRuntimePlan noOverrides cgroup unpinned)
            `shouldBe` (4, FromCgroup)

    it "keeps an operator GHCRTS heap ceiling rather than fabricating one" $ do
        -- No config and no cgroup memory limit: an -M the operator set stands.
        let live = unpinned{rpMaxHeapBytes = Just (300 * mib)}
            plan = resolveRuntimePlan noOverrides noCgroup live
        planMaxHeapBytes plan `shouldBe` (Just (300 * mib), FromRts)

    it "sizes the derived heap ceiling from the planned capabilities, not the live count" $ do
        -- The cgroup grants 2 cores while the RTS claimed 4, so the nursery the
        -- process runs with is 2 x allocation area.
        let cgroup = CgroupLimits{cgCpuCores = Just 2, cgMemoryMaxBytes = Just (512 * mib)}
            plan = resolveRuntimePlan noOverrides cgroup unpinned
        planMaxHeapBytes plan
            `shouldBe` (Just (deriveMaxHeapBytes (512 * mib) 2 (64 * mib)), FromCgroup)

ladderSpec :: Spec
ladderSpec = describe "the capability ladder below the cgroup CPU quota" $ do
    it "bounds capabilities by what the cgroup memory limit can feed when no quota binds" $ do
        -- 1 GiB, a quarter of it for nurseries, at 64 MiB each.
        let cgroup = noCgroup{cgMemoryMaxBytes = Just (1024 * mib)}
        planCapabilities (resolveRuntimePlan noOverrides cgroup bigNode)
            `shouldBe` (4, FromCgroupMemory)

    it "grants one capability when the memory limit cannot feed even that" $ do
        let cgroup = noCgroup{cgMemoryMaxBytes = Just (64 * mib)}
        planCapabilities (resolveRuntimePlan noOverrides cgroup bigNode)
            `shouldBe` (1, FromCgroupMemory)

    it "caps capabilities at the cores ceiling when no cgroup limit binds at all" $
        planCapabilities (resolveRuntimePlan noOverrides noCgroup bigNode)
            `shouldBe` (8, FromCoresCeiling)

    it "takes the visible processors when they are fewer than the ceiling" $ do
        let plan = resolveRuntimePlan noOverrides noCgroup unpinned
        planCapabilities plan `shouldBe` (4, FromCoresCeiling)
        planMaxHeapBytes plan `shouldBe` (Nothing, FromRts)

    it "honours a configured cores ceiling" $
        planCapabilities (resolveRuntimePlan noOverrides{roCoresCeiling = Just 16} noCgroup bigNode)
            `shouldBe` (16, FromCoresCeiling)

    describe "the ceiling caps the last rung only" $ do
        let ceilingEight = noOverrides{roCoresCeiling = Just 8}

        it "never clamps an explicit cores" $
            planCapabilities (resolveRuntimePlan ceilingEight{roCores = Just 32} noCgroup bigNode)
                `shouldBe` (32, FromConfig)

        it "never clamps a cgroup CPU quota" $
            planCapabilities (resolveRuntimePlan ceilingEight (noCgroup{cgCpuCores = Just 16}) bigNode)
                `shouldBe` (16, FromCgroup)

        it "never clamps the memory-derived count" $
            planCapabilities (resolveRuntimePlan ceilingEight (noCgroup{cgMemoryMaxBytes = Just (3072 * mib)}) bigNode)
                `shouldBe` (12, FromCgroupMemory)

derivationSpec :: Spec
derivationSpec = describe "deriveMaxHeapBytes" $ do
    it "subtracts the nursery budget and ten percent slack, aligned to the RTS's 4 KiB blocks" $ do
        -- The result rounds down to a whole 4 KiB block, so the ceiling reads back exactly
        -- after the re-exec.
        let raw = 512 * mib - 2 * 64 * mib - (512 * mib) `div` 10
        deriveMaxHeapBytes (512 * mib) 2 (64 * mib)
            `shouldBe` (raw - raw `mod` 4096)

    it "floors at half the memory limit when the nursery would swallow a tiny pod" $
        -- 128 MiB with a 4 x 64 MiB nursery would go negative, so half the limit stands.
        deriveMaxHeapBytes (128 * mib) 4 (64 * mib) `shouldBe` (64 * mib)

flagsSpec :: Spec
flagsSpec = describe "requiredRtsFlags" $ do
    it "is empty when the plan is already in force" $ do
        let live = unpinned{rpCapabilities = 2, rpMaxHeapBytes = Just (400 * mib)}
            plan = resolveRuntimePlan pinnedBoth noCgroup live
        requiredRtsFlags live plan `shouldBe` []

    it "asks only for the capability change when the heap already matches" $ do
        let live = unpinned{rpMaxHeapBytes = Just (400 * mib)}
            plan = resolveRuntimePlan pinnedBoth noCgroup live
        requiredRtsFlags live plan `shouldBe` ["-N2"]

    it "asks for the heap flag in bytes when a ceiling must be enforced" $ do
        let plan = resolveRuntimePlan pinnedBoth{roCores = Just 4} noCgroup unpinned
        requiredRtsFlags unpinned plan `shouldBe` ["-M" <> show (400 * mib)]

    it "asks for both flags when both differ" $ do
        let cgroup = CgroupLimits{cgCpuCores = Just 2, cgMemoryMaxBytes = Just (512 * mib)}
            plan = resolveRuntimePlan noOverrides cgroup unpinned
            derived = deriveMaxHeapBytes (512 * mib) 2 (64 * mib)
        requiredRtsFlags unpinned plan `shouldBe` ["-N2", "-M" <> show derived]

    it "never asks to change a posture the live RTS already matches" $ do
        -- The last rung lands on the visible processors, which is what -N claimed.
        let plan = resolveRuntimePlan noOverrides noCgroup unpinned
        requiredRtsFlags unpinned plan `shouldBe` []
  where
    pinnedBoth = noOverrides{roCores = Just 2, roMaxHeapBytes = Just (400 * mib)}

reconcileSpec :: Spec
reconcileSpec = describe "reconcileRuntimePlan (desired vs observed)" $ do
    it "reads an exactly-applied plan as enforced on both axes" $ do
        let cgroup = CgroupLimits{cgCpuCores = Just 2, cgMemoryMaxBytes = Just (512 * mib)}
            plan = resolveRuntimePlan noOverrides cgroup unpinned
            derived = deriveMaxHeapBytes (512 * mib) 2 (64 * mib)
            applied = unpinned{rpCapabilities = 2, rpMaxHeapBytes = Just derived}
            effective = reconcileRuntimePlan cgroup plan applied
        axEnforced (erpCapabilities effective) `shouldBe` True
        axEnforced (erpMaxHeapBytes effective) `shouldBe` True
        effectiveCapabilities effective `shouldBe` (2, FromCgroup)
        effectiveHeapCeiling effective `shouldBe` (Just derived, FromCgroup)

    it "keeps an unenforced desired ceiling as the sizing datapoint (the cgroup backstops it)" $ do
        -- Partial application: the capability change took, the -M did not.
        let cgroup = CgroupLimits{cgCpuCores = Just 2, cgMemoryMaxBytes = Just (512 * mib)}
            plan = resolveRuntimePlan noOverrides cgroup unpinned
            derived = deriveMaxHeapBytes (512 * mib) 2 (64 * mib)
            partial = unpinned{rpCapabilities = 2}
            effective = reconcileRuntimePlan cgroup plan partial
        axEnforced (erpMaxHeapBytes effective) `shouldBe` False
        effectiveHeapCeiling effective `shouldBe` (Just derived, FromCgroup)

    it "takes the tighter observed ceiling when an operator GHCRTS binds below the plan" $ do
        let plan = resolveRuntimePlan noOverrides{roMaxHeapBytes = Just (400 * mib)} noCgroup unpinned
            live = unpinned{rpMaxHeapBytes = Just (300 * mib)}
            effective = reconcileRuntimePlan noCgroup plan live
        axEnforced (erpMaxHeapBytes effective) `shouldBe` False
        effectiveHeapCeiling effective `shouldBe` (Just (300 * mib), FromRts)

    it "budgets from the live capability count when the desired one never took" $ do
        -- The re-exec failure shape: neither flag applied. Parallelism budgets must
        -- track what the RTS runs, and the provenance degrades to the RTS's own.
        let cgroup = CgroupLimits{cgCpuCores = Just 2, cgMemoryMaxBytes = Just (512 * mib)}
            plan = resolveRuntimePlan noOverrides cgroup unpinned
            effective = reconcileRuntimePlan cgroup plan unpinned
        axEnforced (erpCapabilities effective) `shouldBe` False
        effectiveCapabilities effective `shouldBe` (4, FromRts)

    it "predicts a successful application (appliedRuntimePlan): both axes enforced at the desire" $ do
        let cgroup = CgroupLimits{cgCpuCores = Just 2, cgMemoryMaxBytes = Just (512 * mib)}
            plan = resolveRuntimePlan noOverrides cgroup unpinned
            derived = deriveMaxHeapBytes (512 * mib) 2 (64 * mib)
            effective = appliedRuntimePlan cgroup plan unpinned
        effectiveCapabilities effective `shouldBe` (2, FromCgroup)
        effectiveHeapCeiling effective `shouldBe` (Just derived, FromCgroup)

renderSpec :: Spec
renderSpec = describe "renderEffectivePosture and renderPostureWarnings" $ do
    it "names each decision with its provenance" $ do
        let cgroup = CgroupLimits{cgCpuCores = Just 2, cgMemoryMaxBytes = Just (512 * mib)}
            plan = resolveRuntimePlan noOverrides cgroup unpinned
            rendered = renderEffectivePosture (appliedRuntimePlan cgroup plan unpinned)
        rendered `shouldSatisfy` any (\l -> "capabilities 2" `T.isInfixOf` l && "cgroup" `T.isInfixOf` l)
        rendered `shouldSatisfy` any (\l -> "max heap" `T.isInfixOf` l && "cgroup" `T.isInfixOf` l)
        rendered `shouldSatisfy` any ("allocation area 64 MiB/capability" `T.isInfixOf`)

    it "says the heap is unbounded when nothing granted a ceiling" $ do
        let plan = resolveRuntimePlan noOverrides noCgroup unpinned
        renderEffectivePosture (appliedRuntimePlan noCgroup plan unpinned)
            `shouldSatisfy` any ("max heap unbounded" `T.isInfixOf`)

    it "renders a config-pinned posture as such" $ do
        let plan = resolveRuntimePlan noOverrides{roCores = Just 2, roMaxHeapBytes = Just (400 * mib)} noCgroup unpinned
        renderEffectivePosture (appliedRuntimePlan noCgroup plan unpinned)
            `shouldSatisfy` any (\l -> "capabilities 2" `T.isInfixOf` l && "from config" `T.isInfixOf` l)

    it "renders the observed, not the desired, side of an unenforced capability axis" $ do
        let cgroup = CgroupLimits{cgCpuCores = Just 2, cgMemoryMaxBytes = Nothing}
            plan = resolveRuntimePlan noOverrides cgroup unpinned
            rendered = renderEffectivePosture (reconcileRuntimePlan cgroup plan unpinned)
        rendered `shouldSatisfy` any (\l -> "capabilities 4" `T.isInfixOf` l && "as the RTS resolved it" `T.isInfixOf` l)

    it "states why the memory limit bounded the count, and how to override it" $ do
        let cgroup = noCgroup{cgMemoryMaxBytes = Just (1024 * mib)}
            plan = resolveRuntimePlan noOverrides cgroup bigNode
            effective = appliedRuntimePlan cgroup plan bigNode
        renderEffectivePosture effective
            `shouldSatisfy` any (\l -> "capabilities 4" `T.isInfixOf` l && "cgroup memory limit" `T.isInfixOf` l)
        renderPostureWarnings effective
            `shouldSatisfy` any (\l -> "no cgroup CPU quota" `T.isInfixOf` l && "ECLUSE_RUNTIME__CORES" `T.isInfixOf` l)

    it "states why the ceiling capped the count, and how to override it" $ do
        let plan = resolveRuntimePlan noOverrides noCgroup bigNode
            effective = appliedRuntimePlan noCgroup plan bigNode
        renderEffectivePosture effective
            `shouldSatisfy` any (\l -> "capabilities 8" `T.isInfixOf` l && "runtime.coresCeiling" `T.isInfixOf` l)
        renderPostureWarnings effective
            `shouldSatisfy` any (\l -> "runtime.cores (ECLUSE_RUNTIME__CORES)" `T.isInfixOf` l)

    it "keeps quiet about the count when a quota or the config decided it" $ do
        let quotaCgroup = noCgroup{cgCpuCores = Just 2}
            quota = resolveRuntimePlan noOverrides quotaCgroup bigNode
            pinned = resolveRuntimePlan noOverrides{roCores = Just 2} noCgroup bigNode
        renderPostureWarnings (appliedRuntimePlan quotaCgroup quota bigNode) `shouldBe` []
        renderPostureWarnings (appliedRuntimePlan noCgroup pinned bigNode) `shouldBe` []
