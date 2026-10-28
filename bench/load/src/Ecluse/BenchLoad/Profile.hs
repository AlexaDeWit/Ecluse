-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Named diagnostic selections keep routine load runs smaller than the full calibration grid.
module Ecluse.BenchLoad.Profile (
    LoadProfile (..),
    parseLoadProfile,
    renderLoadProfile,
    selectProfile,
) where

import Ecluse.BenchLoad.Contract (LoadLaw (..), loadScenarioName)
import Ecluse.BenchLoad.Selection (scenarioKey)
import Ecluse.Core.Ecosystem (Ecosystem (PyPI, RubyGems))
import Ecluse.Test.EcosystemBench (benchmarkEcosystems)

-- | Full runs retain calibration coverage. Named selections remain diagnostic.
data LoadProfile = Full | ReadPaths | MemoryAdmission | Artifacts
    deriving stock (Eq, Show)

-- | Accept only the workflow's named profiles.
parseLoadProfile :: Text -> Either Text LoadProfile
parseLoadProfile = \case
    "full" -> Right Full
    "read-paths" -> Right ReadPaths
    "memory-admission" -> Right MemoryAdmission
    "artifacts" -> Right Artifacts
    name -> Left ("unknown load profile: " <> name)

-- | The profile name recorded in native reports.
renderLoadProfile :: LoadProfile -> Text
renderLoadProfile = \case
    Full -> "full"
    ReadPaths -> "read-paths"
    MemoryAdmission -> "memory-admission"
    Artifacts -> "artifacts"

-- | Explicit scenario keys override the profile and retain their custom selection identity.
selectProfile :: LoadProfile -> Maybe [Text] -> (Text, Maybe [Text])
selectProfile _ (Just keys) = ("custom", Just keys)
selectProfile profile Nothing = (renderLoadProfile profile, selected)
  where
    selected = case profile of
        Full -> Nothing
        _ -> Just [scenarioKey ecosystem name | ecosystem <- benchmarkEcosystems, law <- laws ecosystem, name <- maybeToList (loadScenarioName ecosystem law)]
    laws ecosystem = case profile of
        Full -> []
        ReadPaths ->
            [ ColdMetadata
            , ShippedAdvisoryMetadata
            , PrivateCopyWhole
            ]
                <> (if ecosystem == RubyGems then [RepeatedDependencies, RevalidateMetadata, OneProjectDependencies, ManyProjectDependencies] else [AssembledResponseHit, RevalidateMetadata, CacheFits, CacheEvicts])
                <> [law | ecosystem == PyPI, law <- [ManagedMirrorBaseline, ManagedMirrorWarm, ManagedMirrorRevision]]
                <> [law | ecosystem == RubyGems, law <- [BatchMetadata, QuickSpecification]]
        MemoryAdmission -> [PrivateCopyWhole, SlowReaders, AdmissionHerd, HerdUnderPrivateCopy, WarmUnderCold, AdmissionRamp]
        Artifacts -> [PrivateArtifact, PublicArtifactOnboarding, PrivateArtifactCeiling] <> [law | ecosystem == RubyGems, law <- [VerifiedKnownLengthArtifact, VerifiedSlowArtifact]]
