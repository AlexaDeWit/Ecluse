-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native advisory fixtures and policy acquisition for the shared real-client scenarios.
module Ecluse.E2E.AdvisoriesE2ESpec (
    npmAdvisoryFamily,
    pypiAdvisoryFamily,
    withPublishedAdvisories,
    withPolicy,
    pypiReleaseDriver,
    refusesPyPIRelease,
    scenarioFamily,
    awaitDenials,
) where

import Data.Aeson (encode, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Text qualified as T
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import Test.Hspec (Expectation, expectationFailure, shouldSatisfy)

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI), ecosystemName)
import Ecluse.E2E.Fixtures.Advisories (cvssControlExportBase, epssControlExportBase, epssProofExportBase, maliciousAdvisoryId, maliciousExportBase, npmAdvisoryExportBase, npmAdvisoryId, partialFixAdvisoryId, partialFixExportBase, partialFixRemainingAdvisoryId, pypiAdvisoryExportBase, pypiAdvisoryId)
import Ecluse.E2E.Fixtures.Contracts (ConcreteFixture (..))
import Ecluse.E2E.Fixtures.Npm (corpusRevokedPkg, psName, psVersion)
import Ecluse.E2E.Fixtures.PyPI (pypiDistInfoFor, pypiWheelFor)
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.Lifecycle (timedFixtureWith)
import Ecluse.Test.Contract.Ecosystem (FamilyFixture (..), FamilyMetadata)
import Ecluse.Test.Contract.Install.Advisory
import Ecluse.Test.Contract.Phase (timedFixture)
import Ecluse.Test.Json (asObject, objectAt, textAt)
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Poll (pollUntil)

-- | Publish a native OSV generation through Pilot before starting the serving role.
withPublishedAdvisories :: Ecosystem -> Text -> (GlobalDataPlane -> IO ()) -> GlobalDataPlane -> IO ()
withPublishedAdvisories ecosystem exportBase action plane = do
    createAdvisoryBucket plane
    run <- publishAdvisoryExport plane ecosystem exportBase
    (roleExit run, roleOutput run) `shouldSatisfy` ((== ExitSuccess) . fst)
    action plane

-- | npm compiles its independent native export before exposing advisory operations.
npmAdvisoryFamily :: ConcreteFixture FamilyMetadata -> FamilyFixture AdvisoryScenarioDriver
npmAdvisoryFamily resources = scenarioFamily resources Npm dredgerTopology npmAdvisoryExportBase fixture session
  where
    fixture policy = case policy of
        KnownMaliciousDefault -> npmCase (psName corpusRevokedPkg) "1.0.0" (psVersion corpusRevokedPkg) maliciousAdvisoryId True
        PartialFix -> npmCase "e2e-advisory" "1.18.0" "1.19.0" partialFixAdvisoryId False
        PartialFixAbstaining -> npmCase "e2e-advisory" "1.18.0" "1.19.0" partialFixRemainingAdvisoryId False
        _ -> npmCase "e2e-advisory" "1.18.0" "1.19.0" npmAdvisoryId False
    npmCase name affected fixed advisory mirror =
        AdvisoryCase
            { advisoryPackage = name
            , advisoryAffectedVersion = affected
            , advisoryFixedVersion = fixed
            , advisoryExpectedId = advisory
            , advisoryRefusalMessage = (("No matching version found for " <> name <> "@") <>)
            , advisoryRequiresMirror = mirror
            }
    session policy e2e =
        let name = advisoryPackage (fixture policy)
         in AdvisorySession
                { advisoryListedVersions = sort . map Key.toText . KeyMap.keys <$> npmAdvisoryVersions e2e name
                , advisoryRelease = npmRelease policy e2e name
                , advisoryMirrorContains = if policy == KnownMaliciousDefault then Just (verdaccioMirroredWithinWindow e2e name) else Nothing
                }

npmAdvisoryVersions :: E2E -> Text -> IO Aeson.Object
npmAdvisoryVersions e2e name = do
    (_, body) <- proxyGet e2e ("/npm/" <> name)
    value <- maybe (fail "npm advisory metadata was not JSON") pure (Aeson.decode body)
    pure (objectAt "versions" (asObject value))

npmRelease :: AdvisoryPolicy -> E2E -> Text -> Text -> IO AdvisoryDriver
npmRelease policy e2e name version = do
    versions <- npmAdvisoryVersions e2e name
    let dist = objectAt "dist" (objectAt (Key.fromText version) versions)
        artifacts = maybeToList ((,) <$> textAt "tarball" dist <*> textAt "integrity" dist)
    pure $
        AdvisoryDriver
            { advisoryExpectedArtifact = e2eBaseUrl e2e <> npmTarballPath name version
            , advisoryAdvertisedArtifacts = artifacts
            , advisoryInstall = withNpmProject e2e $ \project -> do
                result <- npmInstallIn project (name <> "@" <> version)
                installed <- installedVersion project name
                pure (result, if policy == KnownMaliciousDefault then isJust installed else installed == Just version)
            , advisoryArtifactStatus = proxyStatus e2e (npmTarballPath name version)
            , advisoryAwaitDenials = awaitDenials e2e
            }

-- | PyPI uses the same scenarios after Pilot compiles its native OSV export.
pypiAdvisoryFamily :: ConcreteFixture FamilyMetadata -> FamilyFixture AdvisoryScenarioDriver
pypiAdvisoryFamily resources = scenarioFamily resources PyPI pypiAdvisoryTopology pypiAdvisoryExportBase fixture session
  where
    fixture policy = AdvisoryCase "flask" "1.18" "1.19" (advisoryFor policy) ("No matching distribution found for flask==" <>) False
    advisoryFor = \case
        KnownMaliciousDefault -> maliciousAdvisoryId
        PartialFix -> partialFixAdvisoryId
        PartialFixAbstaining -> partialFixRemainingAdvisoryId
        _ -> pypiAdvisoryId
    session _ e2e = AdvisorySession (map (wheelVersion . fst) <$> advertisedFiles e2e "flask") (pypiReleaseDriver e2e "flask") Nothing
    wheelVersion filename = fromMaybe filename (T.stripPrefix "flask-" filename >>= T.stripSuffix "-py3-none-any.whl")

-- Independent lifetimes prevent source replacement and the fixed Docker subnet from crossing policies.
scenarioFamily :: ConcreteFixture FamilyMetadata -> Ecosystem -> Topology -> Text -> (AdvisoryPolicy -> AdvisoryCase) -> (AdvisoryPolicy -> E2E -> AdvisorySession) -> FamilyFixture AdvisoryScenarioDriver
scenarioFamily resources ecosystem maliciousTopology scoredSource fixture session =
    FamilyFixture
        { familyMetadata = fixtureMetadata resources
        , familyAcquire = \recorder action ->
            action
                AdvisoryScenarioDriver
                    { advisoryWithPolicy = \policy useSession ->
                        let malicious = policy == KnownMaliciousDefault
                            acquire = if malicious then withDataPlane maliciousTopology else withFixture resources
                            source
                                | malicious = maliciousExportBase
                                | policy `elem` [PartialFix, PartialFixAbstaining] = partialFixExportBase
                                | policy == CvssBelowThreshold = cvssControlExportBase
                                | policy == DenyEpss = epssProofExportBase
                                | policy == EpssBelowThreshold = epssControlExportBase
                                | otherwise = scoredSource
                         in timedFixture recorder acquire $
                                timedFixtureWith recorder (withPublishedAdvisories ecosystem source) $ \plane ->
                                    withPolicy ecosystem (policyFields policy) plane (useSession . session policy)
                    , advisoryCaseForPolicy = fixture
                    }
        }

policyFields :: AdvisoryPolicy -> [Pair]
policyFields = \case
    DenyCve -> cveDeny
    CvssBelowThreshold -> cveDeny
    DenyEpss -> epssDeny
    EpssBelowThreshold -> epssDeny
    KnownMaliciousDefault -> ["min-age" .= object ["ageSeconds" .= (0 :: Int)]]
    Remediation -> []
    RemediationDisabled -> ["remediation-fast-track" .= object ["enabled" .= False]]
    PartialFix -> []
    PartialFixAbstaining -> ["remediation-fast-track" .= object ["remainingAdvisories" .= ("abstain" :: Text)]]
  where
    cveDeny = ["advisory-deny" .= object ["type" .= ("DenyIfCve" :: Text), "minCvss" .= (7.0 :: Double)]]
    epssDeny = ["advisory-deny" .= object ["type" .= ("DenyIfEpss" :: Text), "minEpss" .= (0.5 :: Double)]]

-- | Await the actual advisory swap before exposing a policy-configured proxy to clients.
withPolicy :: Ecosystem -> [Pair] -> GlobalDataPlane -> (E2E -> IO ()) -> IO ()
withPolicy ecosystem extra plane action =
    -- The shared contract inspects per-request denial details.
    withClientProxyWith defaultE2EConfig{ecExtraEnv = advisoryStoreEnv <> [("ECLUSE_OBSERVABILITY__LOG_LEVEL", "debug"), ("ECLUSE_MOUNTS__" <> T.toUpper (ecosystemName ecosystem) <> "__RULES", policy)]} ready plane
  where
    policy = decodeUtf8 . LBS.toStrict . encode . object $ ["min-age" .= object ["ageSeconds" .= (604800 :: Int)]] <> extra
    ready e2e = do
        synced <- awaitProxyLog e2e (T.isInfixOf ("cve-sync[" <> show ecosystem <> "]: advisory database swapped in")) 120
        unless synced $ do
            logs <- proxyContainerLogs e2e
            expectationFailure (toString ("the proxy never synced its advisory generation. Last proxy log lines:\n" <> logTail logTailLines logs))
        action e2e

-- | Use the advertised wheel digest, with a fixture fallback for policy-filtered refusal probes.
pypiReleaseDriver :: E2E -> Text -> Text -> IO AdvisoryDriver
pypiReleaseDriver e2e name version = do
    let wheelPath = gdpWorkDir (e2ePlane e2e) </> "pypi" </> "simple" </> toString name </> toString (pypiWheelFor name version)
    files <- advertisedFiles e2e name
    digest <- maybe (hexSha256Of <$> BS.readFile wheelPath) pure (lookup (pypiWheelFor name version) files)
    pure
        AdvisoryDriver
            { advisoryExpectedArtifact = pypiWheelFor name version
            , advisoryAdvertisedArtifacts = files
            , advisoryInstall = withPipProject e2e name version digest $ \project ->
                (,) <$> pipInstallIn project <*> pipInstalled project (pypiDistInfoFor name version)
            , advisoryArtifactStatus = proxyStatus e2e ("/pypi/simple/" <> name <> "/" <> pypiWheelFor name version)
            , advisoryAwaitDenials = awaitDenials e2e
            }

-- | Preserve PyPI's native refusal diagnostic and full structured denial evidence.
refusesPyPIRelease :: E2E -> Text -> Text -> Text -> Maybe Text -> Expectation
refusesPyPIRelease e2e name version rule advisory = do
    driver <- pypiReleaseDriver e2e name version
    advisoryRefuses ("No matching distribution found for " <> name <> "==" <> version) (Denial name version rule advisory) driver

awaitDenials :: E2E -> Denial -> IO [Denial]
awaitDenials e2e expected =
    pollUntil 120 250000 (elem expected) (mapMaybe parseDenial . T.lines <$> proxyContainerLogs e2e)
