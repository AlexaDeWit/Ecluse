-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Canonical ecosystem laws use typed operations and retain each family's resource lifetime.
module Ecluse.Test.Contract.Ecosystem (
    MissingCapability (..),
    FamilyDriver (..),
    FamilyFixture (..),
    ServingEvidence (..),
    EcosystemDriver (..),
    fullEcosystemContract,
    readOnlyEcosystemFoundation,
    ecosystemCompletion,
    missingCapabilityProfiles,
    missingCapabilityObligations,
) where

import Test.Hspec (aroundAll, expectationFailure, it)

import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Completion
import Ecluse.Test.Contract.Install (InstallationDriver, installationContract)
import Ecluse.Test.Contract.Install.Advisory (AdvisoryScenarioDriver, advisoryContract)
import Ecluse.Test.Contract.Mirror (MirrorDriver, MirrorFixture, mirrorContract)
import Ecluse.Test.Contract.Phase (PhaseRecorder)
import Ecluse.Test.Contract.Publication (PublicationFixture, publicationContract)
import Ecluse.Test.Contract.Runner (DeclaredGroup (..))
import Ecluse.Test.Contract.Sweep.Composition (SweepDriver, sweepContract)
import Ecluse.Test.Contract.Telemetry (MirrorTelemetryFixture, ReadTelemetryDriver, mirrorTelemetryContract, readTelemetryContract)
import Ecluse.Test.Contract.Types

-- | An unavailable production operation fails its required laws without acquiring a fixture.
newtype MissingCapability = MissingCapability Text
    deriving stock (Eq, Show)

-- | Availability changes execution, never the full contract's required laws.
data FamilyDriver a = Available a | Unavailable MissingCapability

-- | The provider owns acquisition and scheduling metadata, but never the canonical scenario body.
data FamilyFixture a = FamilyFixture
    { familyGroup :: Group
    , familyAcquire :: PhaseRecorder -> (a -> IO ()) -> IO ()
    }

-- | Serving evidence always names canonical integration cases for this ecosystem.
newtype ServingEvidence = ServingEvidence {servingEvidenceGroup :: Text}

-- | Common scenarios receive native operations and expectations. Native extensions remain explicit.
data EcosystemDriver graph value plane store project = EcosystemDriver
    { ecosystemName :: Text
    , ecosystemProxying :: FamilyDriver ServingEvidence
    , ecosystemInstallation :: FamilyDriver (FamilyFixture InstallationDriver)
    , ecosystemPolicy :: FamilyDriver ServingEvidence
    , ecosystemAdvisories :: FamilyDriver (FamilyFixture AdvisoryScenarioDriver)
    , ecosystemMirroring :: FamilyDriver (MirrorFixture graph value, FamilyFixture (MirrorDriver graph value))
    , ecosystemPublication :: FamilyDriver (FamilyFixture PublicationFixture)
    , ecosystemDredger :: FamilyDriver (Group, PhaseRecorder -> SweepDriver plane store project)
    , ecosystemTelemetry :: FamilyDriver (FamilyFixture ReadTelemetryDriver, FamilyDriver (FamilyFixture MirrorTelemetryFixture))
    , ecosystemNativeExtensions :: [DeclaredGroup]
    }

-- | Compose every E2E family. 'ecosystemCompletion' also requires the canonical lower-tier evidence.
fullEcosystemContract :: (Eq graph, Show graph, Eq value, Show value) => EcosystemDriver graph value plane store project -> [DeclaredGroup]
fullEcosystemContract = composeEcosystem FullEcosystem

-- | This explicit foundation certifies read behaviour only, regardless of advertised capabilities.
readOnlyEcosystemFoundation :: (Eq graph, Show graph, Eq value, Show value) => EcosystemDriver graph value plane store project -> [DeclaredGroup]
readOnlyEcosystemFoundation = composeEcosystem ReadOnlyEcosystemFoundation

composeEcosystem :: (Eq graph, Show graph, Eq value, Show value) => CompletionScope -> EcosystemDriver graph value plane store project -> [DeclaredGroup]
composeEcosystem scope driver =
    external "proxying" (ecosystemProxying driver)
        <> acquired "installation" "installation" installationContract (ecosystemInstallation driver)
        <> external "policy" (ecosystemPolicy driver)
        <> acquired "advisories" "advisories" advisoryContract (ecosystemAdvisories driver)
        <> writes
        <> telemetry
        <> ecosystemNativeExtensions driver
  where
    subject = ecosystemName driver
    laws family = fromMaybe [] $ lookup family (requiredFamilyLaws scope)
    prefix suffix = subject <> "/" <> suffix
    external family = \case
        Available _ -> []
        Unavailable reason -> [missingGroup subject family (prefix family) (laws family) reason]
    acquired family suffix body availability =
        [ case availability of
            Available fixture ->
                let metadata = canonicalCases family (prefix suffix) (familyGroup fixture) (laws family)
                 in DeclaredGroup metadata $ \recorder -> aroundAll (familyAcquire fixture recorder) (body $ prefix suffix)
            Unavailable reason -> missingGroup subject family (prefix suffix) (laws family) reason
        ]
    writes
        | scope /= FullEcosystem = []
        | otherwise =
            [ case ecosystemMirroring driver of
                Available (fixture, provider) ->
                    let metadata = canonicalCases "mirroring" (prefix "mirror") (familyGroup provider) (laws "mirroring")
                     in DeclaredGroup metadata $ \recorder -> aroundAll (familyAcquire provider recorder) (mirrorContract (prefix "mirror") fixture)
                Unavailable reason -> missingGroup subject "mirroring" (prefix "mirror") (laws "mirroring") reason
            ]
                <> acquired "publication" "publish" publicationContract (ecosystemPublication driver)
                <> [ case ecosystemDredger driver of
                        Available (metadata, provider) -> DeclaredGroup (canonicalCases "dredger" (prefix "dredger") metadata (laws "dredger")) (sweepContract . provider)
                        Unavailable reason -> missingGroup subject "dredger" (prefix "dredger") (laws "dredger") reason
                   ]
    telemetry = case ecosystemTelemetry driver of
        Unavailable reason -> [missingGroup subject "telemetry" (prefix "telemetry") (laws "telemetry") reason]
        Available (readFixture, writeDriver) ->
            [ let metadata = canonicalCases "telemetry" (prefix "telemetry") (familyGroup readFixture) readTelemetryLaws
               in DeclaredGroup metadata $ \recorder -> aroundAll (familyAcquire readFixture recorder) (readTelemetryContract $ prefix "telemetry")
            ]
                <> [ case writeDriver of
                        Available fixture ->
                            let metadata = canonicalCases "telemetry" (prefix "telemetry") (familyGroup fixture) writeTelemetryLaws
                             in DeclaredGroup metadata $ \recorder -> aroundAll (familyAcquire fixture recorder) (mirrorTelemetryContract $ prefix "telemetry")
                        Unavailable reason -> missingGroup subject "telemetry-write" (prefix "telemetry") writeTelemetryLaws reason
                   | scope == FullEcosystem
                   ]

readTelemetryLaws :: [Text]
readTelemetryLaws = ["signals", "disabled", "export-failure", "datadog"]

writeTelemetryLaws :: [Text]
writeTelemetryLaws = ["domain-spans", "private-fetch"]

canonicalCases :: Text -> Text -> Group -> [Text] -> Group
canonicalCases family prefix metadata laws =
    metadata{groupCases = [Case (lawCase family prefix law) [groupContractId metadata] | law <- laws]}

lawCase :: Text -> Text -> Text -> Text
lawCase "publication" prefix "authorisation-refusal" = prefix <> "/scope-refusal"
lawCase _ prefix law = prefix <> "/" <> law

missingGroup :: Text -> Text -> Text -> [Text] -> MissingCapability -> DeclaredGroup
missingGroup subject family prefix laws (MissingCapability reason) = DeclaredGroup metadata $
    const $
        forM_ laws $ \law ->
            it (caseLabel (lawCase family prefix law) ("requires " <> family <> "/" <> law)) $
                expectationFailure $
                    toString $
                        "missing production capability: " <> subject <> ": " <> family <> ": " <> reason
  where
    identifier = subject <> "/missing/" <> family
    metadata = Group identifier "completion" (subject <> "/completion") E2E subject [Case (lawCase family prefix law) [identifier] | law <- laws] [] [] [] []

-- | Reference exact canonical cases across tiers without importing concrete providers into contracts.
ecosystemCompletion :: (Eq graph, Show graph, Eq value, Show value) => CompletionScope -> EcosystemDriver graph value plane store project -> Completion
ecosystemCompletion scope driver = Completion subject scope $ concatMap familyEvidence (requiredFamilyLaws scope)
  where
    subject = ecosystemName driver
    groups = map declaredGroup $ composeEcosystem scope driver
    familyEvidence (family, laws) = case (family, external family) of
        (_, Just (ServingEvidence identifier)) -> [LawEvidence family law Integration identifier ("integration.serving." <> subject <> "." <> law) | law <- laws]
        _ -> map (localEvidence family) laws
    external "proxying" = available (ecosystemProxying driver)
    external "policy" = available (ecosystemPolicy driver)
    external _ = Nothing
    available (Available value) = Just value
    available (Unavailable _) = Nothing
    localEvidence family law =
        let identifier = lawCase family (subject <> "/" <> familyPrefix family) law
            matching = find (any ((== identifier) . caseId) . groupCases) groups
         in LawEvidence family law E2E (maybe (subject <> "/missing/" <> family) groupId matching) identifier
    familyPrefix "mirroring" = "mirror"
    familyPrefix "publication" = "publish"
    familyPrefix family = family

-- | These profiles describe failing test outcomes, not advertised product capabilities.
missingCapabilityProfiles :: Text -> [DeclaredGroup] -> [Profile]
missingCapabilityProfiles subject declarations =
    [Profile (subject <> "/completion") [subject] [] [] | any ((== subject <> "/completion") . groupProfileId . declaredGroup) declarations]

-- | Missing operation cases remain supported test obligations and therefore must fail execution.
missingCapabilityObligations :: [DeclaredGroup] -> [Obligation]
missingCapabilityObligations declarations =
    [ Obligation (groupId metadata) (groupProfileId metadata) "completion" Supported [] (map caseId $ groupCases metadata)
    | DeclaredGroup metadata _ <- declarations
    , groupContractId metadata == "completion"
    ]
