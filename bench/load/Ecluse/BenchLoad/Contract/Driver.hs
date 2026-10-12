-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | One load composition keeps native operations and their measurement brackets intact.
module Ecluse.BenchLoad.Contract.Driver (LoadReadDriver (..), readLoadContract) where

import Network.Wai (Application)

import Ecluse.BenchLoad.Advisories (allRulesAdvisories, shippedAdvisories)
import Ecluse.BenchLoad.Harness (LoadKnobs, Scenario, UpstreamFixture (..))
import Ecluse.BenchLoad.PatternScenario (patternScenarios)
import Ecluse.BenchLoad.PrivateCopy (PrivateCopy (..), privateCopyScenarios)

-- | Drivers supply native operations, while the contract owns the shared scenario sequence.
data LoadReadDriver = LoadReadDriver
    { lrdPrivateCopy :: PrivateCopy
    , lrdColdMetadata :: Scenario
    , lrdAssembledHit :: Scenario
    , lrdRevalidate :: Scenario
    , lrdCacheFits :: Scenario
    , lrdCacheEvicts :: Scenario
    , lrdPrivateArtifact :: Scenario
    , lrdPublicArtifact :: Scenario
    , lrdArtifactCeiling :: Scenario
    , lrdHerd :: Scenario
    , lrdWarmUnderCold :: Scenario
    , lrdRamp :: Scenario
    , lrdPatternPrivate :: LoadKnobs -> Application
    , lrdPatternPublic :: LoadKnobs -> Map Text LByteString -> Map ByteString LByteString -> IO Application
    }

-- | Extensions retain their original position before replay patterns, outside the read foundation.
readLoadContract :: LoadReadDriver -> [Scenario] -> UpstreamFixture
readLoadContract driver extensions =
    UpstreamFixture
        { fixtureEcosystem = ecosystem
        , fixtureScenarios =
            [ cold
            , shippedAdvisories ecosystem cold
            , allRulesAdvisories ecosystem cold
            ]
                <> privateCopyScenarios copy
                <> [ lrdAssembledHit driver
                   , revalidate
                   , shippedAdvisories ecosystem revalidate
                   , lrdCacheFits driver
                   , lrdCacheEvicts driver
                   , lrdPrivateArtifact driver
                   , lrdPublicArtifact driver
                   , lrdArtifactCeiling driver
                   , lrdHerd driver
                   , lrdWarmUnderCold driver
                   , lrdRamp driver
                   ]
                <> extensions
                <> patternScenarios ecosystem (pcPackages copy) (lrdPatternPrivate driver) (lrdPatternPublic driver) (pcUrl copy)
        }
  where
    copy = lrdPrivateCopy driver
    ecosystem = pcEcosystem copy
    cold = lrdColdMetadata driver
    revalidate = lrdRevalidate driver
