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
) where

import Data.Aeson (Value, encode, object, (.=))
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
import Ecluse.E2E.Fixtures.Advisories (npmAdvisoryExportBase, npmAdvisoryId, pypiAdvisoryExportBase, pypiAdvisoryId)
import Ecluse.E2E.Fixtures.Contracts (ConcreteFixture, concreteFamily)
import Ecluse.E2E.Fixtures.PyPI (pypiDistInfoFor, pypiWheelFor)
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.Lifecycle (timedFixtureWith)
import Ecluse.Test.Contract.Ecosystem (FamilyFixture)
import Ecluse.Test.Contract.Install.Advisory
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
npmAdvisoryFamily :: ConcreteFixture -> FamilyFixture AdvisoryScenarioDriver
npmAdvisoryFamily fixture = concreteFamily fixture $ \recorder action ->
    timedFixtureWith recorder (withPublishedAdvisories Npm npmAdvisoryExportBase) (action . npmAdvisoryDriver)

-- | PyPI uses the same scenarios after Pilot compiles its native OSV export.
pypiAdvisoryFamily :: ConcreteFixture -> FamilyFixture AdvisoryScenarioDriver
pypiAdvisoryFamily fixture = concreteFamily fixture $ \recorder action ->
    timedFixtureWith recorder (withPublishedAdvisories PyPI pypiAdvisoryExportBase) (action . pypiAdvisoryDriver)

npmAdvisoryDriver :: GlobalDataPlane -> AdvisoryScenarioDriver
npmAdvisoryDriver =
    scenarioDriver
        Npm
        "e2e-advisory"
        "1.18.0"
        "1.19.0"
        npmAdvisoryId
        ("No matching version found for e2e-advisory@" <>)
        npmVersions
        npmRelease
  where
    npmVersions e2e = do
        (_, body) <- proxyGet e2e "/npm/e2e-advisory"
        value <- maybe (fail "npm advisory metadata was not JSON") pure (Aeson.decode body)
        pure (sort (map Key.toText (KeyMap.keys (objectAt "versions" (asObject value)))))
    npmRelease e2e version =
        pure $
            AdvisoryDriver
                { advisoryInstall = withNpmProject e2e $ \project -> do
                    result <- npmInstallIn project ("e2e-advisory@" <> version)
                    installed <- installedVersion project "e2e-advisory"
                    pure (result, installed == Just version)
                , advisoryArtifactStatus = proxyStatus e2e (npmTarballPath "e2e-advisory" version)
                , advisoryAwaitDenials = awaitDenials e2e
                }

pypiAdvisoryDriver :: GlobalDataPlane -> AdvisoryScenarioDriver
pypiAdvisoryDriver =
    scenarioDriver
        PyPI
        "flask"
        "1.18"
        "1.19"
        pypiAdvisoryId
        ("No matching distribution found for flask==" <>)
        (\e2e -> map (wheelVersion . fst) <$> advertisedFiles e2e "flask")
        (`pypiReleaseDriver` "flask")
  where
    wheelVersion filename = fromMaybe filename (T.stripPrefix "flask-" filename >>= T.stripSuffix "-py3-none-any.whl")

scenarioDriver :: Ecosystem -> Text -> Text -> Text -> Text -> (Text -> Text) -> (E2E -> IO [Text]) -> (E2E -> Text -> IO AdvisoryDriver) -> GlobalDataPlane -> AdvisoryScenarioDriver
scenarioDriver ecosystem name affected fixed advisory message listed release plane =
    AdvisoryScenarioDriver
        { advisoryWithPolicy = \policy action ->
            withPolicy ecosystem (policyFields policy) plane $ \e2e ->
                action (AdvisorySession (listed e2e) (release e2e))
        , advisoryPackage = name
        , advisoryAffectedVersion = affected
        , advisoryFixedVersion = fixed
        , advisoryExpectedId = advisory
        , advisoryRefusalMessage = message
        }

policyFields :: AdvisoryPolicy -> [Pair]
policyFields = \case
    DenyCve -> ["advisory-deny" .= object ["type" .= ("DenyIfCve" :: Text), "minCvss" .= (7.0 :: Double)]]
    DenyEpss -> ["advisory-deny" .= object ["type" .= ("DenyIfEpss" :: Text), "minEpss" .= (0.5 :: Double)]]
    Remediation -> []
    RemediationDisabled -> ["remediation-fast-track" .= object ["enabled" .= False]]

-- | Await the actual advisory swap before exposing a policy-configured proxy to clients.
withPolicy :: Ecosystem -> [Pair] -> GlobalDataPlane -> (E2E -> IO ()) -> IO ()
withPolicy ecosystem extra plane action =
    withE2EWith defaultE2EConfig{ecExtraEnv = advisoryStoreEnv <> [("ECLUSE_MOUNTS__" <> T.toUpper (ecosystemName ecosystem) <> "__RULES", policy)]} ready plane
  where
    policy = decodeUtf8 . LBS.toStrict . encode . object $ ["min-age" .= object ["ageSeconds" .= (604800 :: Int)]] <> extra
    ready e2e = do
        synced <- awaitProxyLog e2e (T.isInfixOf ("cve-sync[" <> show ecosystem <> "]: advisory database swapped in")) 120
        unless synced $ do
            logs <- proxyContainerLogs e2e
            expectationFailure (toString ("the proxy never synced its advisory generation. Last proxy log lines:\n" <> logTail logTailLines logs))
        action e2e

-- | Read the seeded wheel digest independently of a policy-filtered index.
pypiReleaseDriver :: E2E -> Text -> Text -> IO AdvisoryDriver
pypiReleaseDriver e2e name version = do
    let wheelPath = gdpWorkDir (e2ePlane e2e) </> "pypi" </> "simple" </> toString name </> toString (pypiWheelFor name version)
    files <- advertisedFiles e2e name
    digest <- maybe (hexSha256Of <$> BS.readFile wheelPath) pure (lookup (pypiWheelFor name version) files)
    pure
        AdvisoryDriver
            { advisoryInstall = withPipProject e2e name version digest $ \project ->
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

parseDenial :: Text -> Maybe Denial
parseDenial line = do
    value <- Aeson.decodeStrict' (encodeUtf8 line) :: Maybe Value
    let top = asObject value
        fields = objectAt "data" top
    guard (textAt "message" top == Just "denied")
    Denial <$> textAt "package" fields <*> textAt "version" fields <*> textAt "rule" fields <*> pure (textAt "cve" fields)
