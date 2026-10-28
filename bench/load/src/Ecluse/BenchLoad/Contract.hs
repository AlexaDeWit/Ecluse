-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Load obligations remain independent of the scenarios a provider registers.
module Ecluse.BenchLoad.Contract (
    LoadScope (..),
    LoadLaw (..),
    LoadGap (..),
    requiredLoadLaws,
    onRequestLoadLaws,
    loadScenarioName,
    loadCoverageGaps,
    requiredLoadPasses,
) where

import Data.Map.Strict qualified as Map

import Ecluse.BenchLoad.Floors (FloorKey, Pass (ConcurrencyOne, Loaded))
import Ecluse.BenchLoad.Pod (PodShape (Unlimited))
import Ecluse.BenchLoad.Selection (scenarioKey)
import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI, RubyGems))

-- | The read foundation includes admission stress but does not claim worker coverage.
data LoadScope = ReadLoadFoundation | FullLoad
    deriving stock (Eq, Show)

-- | Load measurements use business identities across native protocol names.
data LoadLaw
    = ColdMetadata
    | ShippedAdvisoryMetadata
    | AllAdvisoryMetadata
    | SlowReaders
    | PrivateCopyFivePercent
    | PrivateCopyQuarter
    | PrivateCopyWhole
    | AssembledResponseHit
    | RevalidateMetadata
    | RevalidateAdvisories
    | CacheFits
    | CacheEvicts
    | PrivateArtifact
    | PublicArtifactOnboarding
    | PatternHotSet
    | PatternColdInstall
    | PatternCiFleet
    | PatternHeterogeneous
    | PatternZipf
    | PatternRestart
    | PatternScan
    | PatternDefaultBodyCap
    | PrivateArtifactCeiling
    | AdmissionHerd
    | HerdUnderPrivateCopy
    | WarmUnderCold
    | AdmissionRamp
    | MirrorWorker
    | ManagedMirrorBaseline
    | ManagedMirrorWarm
    | ManagedMirrorRevision
    | BatchMetadata
    | QuickSpecification
    | VerifiedKnownLengthArtifact
    | VerifiedSlowArtifact
    | RepeatedDependencies
    | OneProjectDependencies
    | ManyProjectDependencies
    deriving stock (Eq, Ord, Show)

-- | Missing measurements describe test coverage, not evidence of a production defect.
data LoadGap
    = MissingLoadScenario LoadLaw Text
    | DuplicateLoadScenario Text
    | NoLoadProtocol Ecosystem
    deriving stock (Eq, Show)

-- | Requirements do not depend on available drivers or advertised capabilities.
requiredLoadLaws :: LoadScope -> Ecosystem -> [LoadLaw]
requiredLoadLaws scope ecosystem =
    filter
        applicable
        [ ColdMetadata
        , ShippedAdvisoryMetadata
        , AllAdvisoryMetadata
        , SlowReaders
        , PrivateCopyQuarter
        , PrivateCopyWhole
        , AssembledResponseHit
        , RevalidateMetadata
        , RevalidateAdvisories
        , CacheFits
        , CacheEvicts
        , PrivateArtifact
        , PublicArtifactOnboarding
        , PatternHotSet
        , PatternColdInstall
        , PatternCiFleet
        , PatternHeterogeneous
        , PatternZipf
        , PatternScan
        , PatternDefaultBodyCap
        , PrivateArtifactCeiling
        , AdmissionHerd
        , HerdUnderPrivateCopy
        , WarmUnderCold
        , AdmissionRamp
        ]
        <> case scope of
            ReadLoadFoundation -> []
            FullLoad -> [MirrorWorker]
        <> case ecosystem of
            PyPI -> [ManagedMirrorBaseline, ManagedMirrorWarm, ManagedMirrorRevision]
            RubyGems -> [RepeatedDependencies, OneProjectDependencies, ManyProjectDependencies, BatchMetadata, QuickSpecification, VerifiedKnownLengthArtifact, VerifiedSlowArtifact]
            _ -> []
  where
    applicable law = ecosystem /= RubyGems || law `notElem` [AssembledResponseHit, CacheFits, CacheEvicts]

-- | Extra variants remain available through explicit scenario selection.
onRequestLoadLaws :: [LoadLaw]
onRequestLoadLaws = [PrivateCopyFivePercent, PatternRestart]

-- | Resolve native names without changing measured work or accepting an unknown protocol.
loadScenarioName :: Ecosystem -> LoadLaw -> Maybe Text
loadScenarioName RubyGems law | law `elem` [AssembledResponseHit, CacheFits, CacheEvicts] = Nothing
loadScenarioName ecosystem law | ecosystem /= RubyGems, law `elem` [RepeatedDependencies, OneProjectDependencies, ManyProjectDependencies] = Nothing
loadScenarioName ecosystem law = case ecosystem of
    Npm -> Just (name "merge-cold" "tarball")
    PyPI -> Just (name "index-cold" "wheel")
    Cargo -> Just (name "index-cold" "crate")
    RubyGems -> Just (name "dependencies-cold" "gem")
  where
    name cold artifact = case law of
        ColdMetadata -> cold
        ShippedAdvisoryMetadata -> cold <> "-advisories"
        AllAdvisoryMetadata -> cold <> "-all-advisory-rules"
        SlowReaders -> "slow-readers"
        PrivateCopyFivePercent -> "heavy-private-5pct"
        PrivateCopyQuarter -> "heavy-private-25pct"
        PrivateCopyWhole -> "heavy-private"
        AssembledResponseHit -> "assembled-response-hit"
        RevalidateMetadata -> "revalidate-not-modified"
        RevalidateAdvisories -> "revalidate-not-modified-advisories"
        CacheFits -> "cache-fits-large"
        CacheEvicts -> "cache-evicts-large"
        PrivateArtifact -> artifact <> "-hot-path"
        PublicArtifactOnboarding -> artifact <> "-onboarding"
        PatternHotSet -> "pattern-hot-set-control"
        PatternColdInstall -> "pattern-cold-install"
        PatternCiFleet -> "pattern-ci-fleet"
        PatternHeterogeneous -> "pattern-heterogeneous"
        PatternZipf -> "pattern-zipf"
        PatternRestart -> "pattern-restart"
        PatternScan -> "pattern-scan"
        PatternDefaultBodyCap -> "pattern-cold-install-default-body-cap"
        PrivateArtifactCeiling -> artifact <> "-ceiling"
        AdmissionHerd -> "herd"
        HerdUnderPrivateCopy -> "herd-under-heavy-private"
        WarmUnderCold -> "warm-under-cold"
        AdmissionRamp -> "ramp"
        MirrorWorker -> "worker-mirroring"
        ManagedMirrorBaseline -> "mirror-native-baseline"
        ManagedMirrorWarm -> "mirror-managed-warm"
        ManagedMirrorRevision -> "mirror-managed-revision"
        BatchMetadata -> "batch-dependencies"
        QuickSpecification -> "quick-gemspec"
        VerifiedKnownLengthArtifact -> "gem-verified-known-length"
        VerifiedSlowArtifact -> "gem-verified-slow-readers"
        RepeatedDependencies -> "dependencies-repeated-corpus"
        OneProjectDependencies -> "dependencies-working-set-one"
        ManyProjectDependencies -> "dependencies-working-set-many"

-- | Report every missing law while retaining unrelated registered scenarios.
loadCoverageGaps :: LoadScope -> Ecosystem -> [Text] -> [LoadGap]
loadCoverageGaps scope ecosystem actual =
    [NoLoadProtocol ecosystem | any (isNothing . loadScenarioName ecosystem) required]
        <> [MissingLoadScenario law name | law <- required, name <- maybeToList (loadScenarioName ecosystem law), Map.notMember name counts]
        <> [DuplicateLoadScenario name | (name, count) <- Map.toList counts, count > 1]
  where
    required = requiredLoadLaws scope ecosystem
    counts = Map.fromListWith (+) [(name, 1 :: Int) | name <- actual]

-- | Independent pass obligations preserve the worker's process limit and non-duration scenarios.
requiredLoadPasses :: LoadScope -> Ecosystem -> PodShape -> Either [LoadGap] [(LoadLaw, FloorKey)]
requiredLoadPasses scope ecosystem shape
    | any (isNothing . loadScenarioName ecosystem) laws = Left [NoLoadProtocol ecosystem]
    | otherwise =
        Right
            [ (law, (scenarioKey ecosystem name, whichPass))
            | law <- laws
            , shape == Unlimited || law /= MirrorWorker
            , name <- maybeToList (loadScenarioName ecosystem law)
            , whichPass <- Loaded : [ConcurrencyOne | hasServiceTime law]
            ]
  where
    laws = requiredLoadLaws scope ecosystem

hasServiceTime :: LoadLaw -> Bool
hasServiceTime = \case
    PatternHotSet -> False
    PatternColdInstall -> False
    PatternCiFleet -> False
    PatternHeterogeneous -> False
    PatternZipf -> False
    PatternRestart -> False
    PatternScan -> False
    PatternDefaultBodyCap -> False
    AdmissionHerd -> False
    HerdUnderPrivateCopy -> False
    WarmUnderCold -> False
    AdmissionRamp -> False
    SlowReaders -> False
    ColdMetadata -> True
    ShippedAdvisoryMetadata -> True
    AllAdvisoryMetadata -> True
    PrivateCopyFivePercent -> True
    PrivateCopyQuarter -> True
    PrivateCopyWhole -> True
    AssembledResponseHit -> True
    RevalidateMetadata -> True
    RevalidateAdvisories -> True
    CacheFits -> True
    CacheEvicts -> True
    PrivateArtifact -> True
    PublicArtifactOnboarding -> True
    PrivateArtifactCeiling -> True
    MirrorWorker -> True
    ManagedMirrorBaseline -> True
    ManagedMirrorWarm -> True
    ManagedMirrorRevision -> True
    BatchMetadata -> True
    QuickSpecification -> True
    VerifiedKnownLengthArtifact -> True
    VerifiedSlowArtifact -> False
    RepeatedDependencies -> True
    OneProjectDependencies -> True
    ManyProjectDependencies -> True
