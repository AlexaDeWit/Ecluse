-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Canonical ecosystem laws use typed operations and retain each family's resource lifetime.
module Ecluse.Test.Contract.Ecosystem (
    MissingCapability (..),
    FamilyDriver (..),
    FamilyMetadata (..),
    FamilyFixture (..),
    ServingEvidence (..),
    EcosystemDriver (..),
    fullEcosystemContract,
    ecosystemCompletion,
    missingCapabilityProfiles,
    missingCapabilityObligations,
) where

import Data.List (lookup)
import Data.Text qualified as T
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

-- | An unavailable required operation fails its required laws without acquiring a fixture.
newtype MissingCapability = MissingCapability Text
    deriving stock (Eq, Show)

-- | Availability changes execution, never the full contract's required laws.
data FamilyDriver a = Available a | Unavailable MissingCapability

-- | Canonical fixtures supply scheduling data. Composition owns their cases and E2E partition.
data FamilyMetadata = FamilyMetadata
    { familyId :: Text
    , familyProfile :: Text
    , familyCapabilities :: [Text]
    , familyPrerequisites :: [Text]
    , familyResources :: [Text]
    , familyIsolation :: [Text]
    }

-- | The provider owns acquisition and scheduling metadata, but never the canonical scenario body.
data FamilyFixture a = FamilyFixture
    { familyMetadata :: FamilyMetadata
    , familyAcquire :: PhaseRecorder -> (a -> IO ()) -> IO ()
    }

-- | Serving evidence always names canonical integration cases for this ecosystem.
newtype ServingEvidence = ServingEvidence {servingEvidenceGroup :: Text}

-- | Common scenarios receive native operations and expectations. Native extensions remain explicit.
data EcosystemDriver observation graph value plane store project = EcosystemDriver
    { ecosystemName :: Text
    , ecosystemProxying :: FamilyDriver ServingEvidence
    , ecosystemInstallation :: FamilyDriver (FamilyFixture (InstallationDriver observation))
    , ecosystemPolicy :: FamilyDriver ServingEvidence
    , ecosystemAdvisories :: FamilyDriver (FamilyFixture AdvisoryScenarioDriver)
    , ecosystemMirroring :: FamilyDriver (MirrorFixture graph value, FamilyFixture (MirrorDriver graph value))
    , ecosystemPublication :: FamilyDriver (FamilyFixture PublicationFixture)
    , ecosystemDredger :: FamilyDriver (FamilyMetadata, PhaseRecorder -> SweepDriver plane store project)
    , ecosystemTelemetry :: FamilyDriver (FamilyFixture ReadTelemetryDriver, FamilyDriver (FamilyFixture MirrorTelemetryFixture))
    , ecosystemNativeExtensions :: [DeclaredGroup]
    }

-- | Compose every E2E family. 'ecosystemCompletion' also requires the canonical lower-tier evidence.
fullEcosystemContract :: (Eq observation, Show observation, Eq graph, Show graph, Eq value, Show value) => EcosystemDriver observation graph value plane store project -> [DeclaredGroup]
fullEcosystemContract driver =
    external "proxying" (ecosystemProxying driver)
        <> acquired "installation" "installation" installationContract (ecosystemInstallation driver)
        <> external "policy" (ecosystemPolicy driver)
        <> acquired "advisories" "advisories" advisoryContract (ecosystemAdvisories driver)
        <> writes
        <> telemetry
        <> ecosystemNativeExtensions driver
  where
    subject = ecosystemName driver
    laws family = filter (/= "last-good-generation") $ fromMaybe [] $ lookup family (requiredFamilyLawsFor subject FullEcosystem)
    prefix suffix = subject <> "/" <> suffix
    external family = \case
        Available _ -> []
        Unavailable reason -> [missingGroup subject family (prefix family) (laws family) reason]
    acquired family suffix body availability =
        [ case availability of
            Available fixture ->
                let metadata = canonicalGroup subject family (prefix suffix) (familyMetadata fixture) (laws family)
                 in DeclaredGroup metadata $ \recorder -> aroundAll (familyAcquire fixture recorder) (body $ prefix suffix)
            Unavailable reason -> missingGroup subject family (prefix suffix) (laws family) reason
        ]
    writes =
        [ case ecosystemMirroring driver of
            Available (fixture, provider) ->
                let metadata = canonicalGroup subject "mirroring" (prefix "mirror") (familyMetadata provider) (laws "mirroring")
                 in DeclaredGroup metadata $ \recorder -> aroundAll (familyAcquire provider recorder) (mirrorContract (prefix "mirror") fixture)
            Unavailable reason -> missingGroup subject "mirroring" (prefix "mirror") (laws "mirroring") reason
        ]
            <> acquired "publication" "publish" publicationContract (ecosystemPublication driver)
            <> [ case ecosystemDredger driver of
                    Available (metadata, provider) -> DeclaredGroup (canonicalGroup subject "dredger" (prefix "dredger") metadata (laws "dredger")) (sweepContract . provider)
                    Unavailable reason -> missingGroup subject "dredger" (prefix "dredger") (laws "dredger") reason
               ]
    telemetry = case ecosystemTelemetry driver of
        Unavailable reason -> [missingGroup subject "telemetry" (prefix "telemetry") (laws "telemetry") reason]
        Available (readFixture, writeDriver) ->
            [ let metadata = canonicalGroup subject "telemetry" (prefix "telemetry") (familyMetadata readFixture) readTelemetryLaws
               in DeclaredGroup metadata $ \recorder -> aroundAll (familyAcquire readFixture recorder) (readTelemetryContract $ prefix "telemetry")
            ]
                <> [ case writeDriver of
                        Available fixture ->
                            let metadata = canonicalGroup subject "telemetry" (prefix "telemetry") (familyMetadata fixture) writeTelemetryLaws
                             in DeclaredGroup metadata $ \recorder -> aroundAll (familyAcquire fixture recorder) (mirrorTelemetryContract $ prefix "telemetry")
                        Unavailable reason -> missingGroup subject "telemetry-write" (prefix "telemetry") writeTelemetryLaws reason
                   ]

readTelemetryLaws :: [Text]
readTelemetryLaws = ["signals", "disabled", "export-failure", "datadog"]

writeTelemetryLaws :: [Text]
writeTelemetryLaws = ["domain-spans", "private-fetch"]

canonicalGroup :: Text -> Text -> Text -> FamilyMetadata -> [Text] -> Group
canonicalGroup subject family prefix metadata laws =
    Group
        { groupId = familyId metadata
        , groupContractId = familyId metadata
        , groupProfileId = familyProfile metadata
        , groupTier = E2E
        , groupPartition = subject
        , groupCases = [Case (lawCase family prefix law) [familyId metadata] | law <- laws]
        , groupCapabilities = familyCapabilities metadata
        , groupPrerequisites = familyPrerequisites metadata
        , groupResources = familyResources metadata
        , groupIsolation = familyIsolation metadata
        }

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
                        "missing required implementation: " <> subject <> ": " <> family <> ": " <> reason
  where
    identifier = subject <> "/missing/" <> family
    metadata = Group identifier "completion" (subject <> "/completion") E2E subject [Case (lawCase family prefix law) [identifier] | law <- laws] [] [] [] []

-- | Reference exact canonical cases across tiers without importing concrete providers into contracts.
ecosystemCompletion :: CompletionScope -> EcosystemDriver observation graph value plane store project -> Completion
ecosystemCompletion scope driver = Completion subject scope $ concatMap familyEvidence (requiredFamilyLawsFor subject scope)
  where
    subject = ecosystemName driver
    familyEvidence (family, laws) = case (family, external family) of
        (_, Just (ServingEvidence identifier)) -> map (servingEvidence identifier family) laws
        _ -> map (localEvidence family) laws
    servingEvidence identifier family law
        | Just suffix <- T.stripPrefix "metadata-fetch/" law = LawEvidence family law Unit "core/metadata/fetch" ("core.metadata.fetch." <> subject <> "." <> suffix)
        | Just suffix <- T.stripPrefix "metadata-projection/" law = LawEvidence family law Unit ("core/metadata/projection/" <> subject) ("core.metadata.projection." <> subject <> "." <> suffix)
        | otherwise = LawEvidence family law Integration identifier ("integration.serving." <> subject <> "." <> law)
    external "proxying" = available (ecosystemProxying driver)
    external "policy" = available (ecosystemPolicy driver)
    external _ = Nothing
    available (Available value) = Just value
    available (Unavailable _) = Nothing
    localEvidence "advisories" "last-good-generation" = LawEvidence "advisories" "last-good-generation" Unit "runtime/advisory/local" "advisory/local/last-good"
    localEvidence family law =
        let identifier = lawCase family (subject <> "/" <> familyPrefix family) law
         in LawEvidence family law E2E (owner family law) identifier
    owner "installation" _ = familyOwner "installation" (ecosystemInstallation driver)
    owner "advisories" _ = familyOwner "advisories" (ecosystemAdvisories driver)
    owner "mirroring" _ = case ecosystemMirroring driver of
        Available (_, fixture) -> familyId (familyMetadata fixture)
        Unavailable _ -> missing "mirroring"
    owner "publication" _ = familyOwner "publication" (ecosystemPublication driver)
    owner "dredger" _ = case ecosystemDredger driver of
        Available (metadata, _) -> familyId metadata
        Unavailable _ -> missing "dredger"
    owner "telemetry" law = case ecosystemTelemetry driver of
        Available (readFixture, writeDriver)
            | law `elem` readTelemetryLaws -> familyId (familyMetadata readFixture)
            | otherwise -> familyOwner "telemetry-write" writeDriver
        Unavailable _ -> missing "telemetry"
    owner family _ = missing family
    familyOwner family = \case
        Available fixture -> familyId (familyMetadata fixture)
        Unavailable _ -> missing family
    missing family = subject <> "/missing/" <> family
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
