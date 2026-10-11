-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Whole-image PyPI advisory compilation, sync, denial, and quarantine overrides through pip.
module Ecluse.E2E.PyPI.AdvisoriesE2ESpec (specWith) where

import Data.Aeson (Value, encode, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Pair)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Text qualified as T
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Contract.Phase (PhaseRecorder, timedFixture)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.E2E.Fixtures.Advisories (pypiAdvisoryExportBase, pypiAdvisoryId)
import Ecluse.E2E.Fixtures.PyPI (pypiDistInfoFor, pypiWheelFor)
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.Lifecycle (timedFixtureWith)
import Ecluse.Test.Contract.Install.Advisory
import Ecluse.Test.Json (asObject, objectAt, textAt)
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Poll (pollUntil)

-- | Drive the product image and its compiled PyPI artifact with isolated pip clients.
specWith :: PhaseRecorder -> Spec
specWith recorder = aroundAll (timedFixture recorder (withDataPlane pypiAdvisoryTopology)) (aroundAllWith (timedFixtureWith recorder withPublishedAdvisories) scenarios)

withPublishedAdvisories :: (GlobalDataPlane -> IO ()) -> GlobalDataPlane -> IO ()
withPublishedAdvisories action plane = do
    createAdvisoryBucket plane
    run <- publishAdvisoryExport plane PyPI pypiAdvisoryExportBase
    (roleExit run, roleOutput run) `shouldSatisfy` ((== ExitSuccess) . fst)
    action plane

scenarios :: SpecWith GlobalDataPlane
scenarios = describe "PyPI advisory policy through pip" $ do
    for_ advisoryDenies $ \(label, rule, threshold) ->
        it (caseLabel ("pypi/advisories/deny-" <> T.toLower label) ("denies the old affected Flask release by " <> label)) $ \plane ->
            withPolicy ["advisory-deny" .= object ["type" .= rule, threshold]] plane $ \e2e -> do
                files <- advertisedFiles e2e "flask"
                map fst files `shouldBe` [pypiWheelFor "flask" "1.19"]
                refusesRelease e2e "flask" "1.18" rule (Just pypiAdvisoryId)

    it (caseLabel "pypi/advisories/exact-fix" "admits the recent exact fix while the remediation-disabled control quarantines it") $ \plane -> do
        withPolicy [] plane $ \e2e -> do
            files <- advertisedFiles e2e "flask"
            case lookup (pypiWheelFor "flask" "1.19") files of
                Nothing -> expectationFailure "the matching advisory fix was absent from the served index"
                Just digest ->
                    advisoryAdmits (advisoryDriver e2e "flask" "1.19" digest)
        withPolicy ["remediation-fast-track" .= object ["enabled" .= False]] plane $ \e2e -> do
            files <- advertisedFiles e2e "flask"
            map fst files `shouldBe` [pypiWheelFor "flask" "1.18"]
            refusesRelease e2e "flask" "1.19" "BlockedByDefault" Nothing

    it (caseLabel "pypi/advisories/canonical-fix" "admits a recent release with a canonicalised advisory fix and preserves refusal controls") $ \plane -> do
        withPolicy ["advisory-deny" .= object ["type" .= ("DenyIfCve" :: Text), "minCvss" .= (7.0 :: Double)]] plane $ \e2e -> do
            refusesRelease e2e "e2e-raw-fix" "1.18" "DenyIfCve" (Just "GHSA-e2e-raw-fix")
        withPolicy [] plane $ \e2e -> do
            files <- advertisedFiles e2e "e2e-raw-fix"
            case lookup (pypiWheelFor "e2e-raw-fix" "1.19") files of
                Nothing -> expectationFailure "the canonicalised advisory fix was absent from the served index"
                Just digest ->
                    advisoryAdmits (advisoryDriver e2e "e2e-raw-fix" "1.19" digest)
        withPolicy ["remediation-fast-track" .= object ["enabled" .= False]] plane $ \e2e -> do
            files <- advertisedFiles e2e "e2e-raw-fix"
            map fst files `shouldBe` [pypiWheelFor "e2e-raw-fix" "1.18"]
            refusesRelease e2e "e2e-raw-fix" "1.19" "BlockedByDefault" Nothing

advisoryDenies :: [(Text, Text, Pair)]
advisoryDenies = [("CVE", "DenyIfCve", "minCvss" .= (7.0 :: Double)), ("EPSS", "DenyIfEpss", "minEpss" .= (0.5 :: Double))]

quarantineSeconds :: Int
quarantineSeconds = 604800

pypiSyncMessage :: Text
pypiSyncMessage = "cve-sync[" <> show PyPI <> "]: advisory database swapped in"

withPolicy :: [Pair] -> GlobalDataPlane -> (E2E -> IO ()) -> IO ()
withPolicy extra plane action =
    withE2EWith defaultE2EConfig{ecExtraEnv = advisoryStoreEnv <> [("ECLUSE_MOUNTS__PYPI__RULES", policy)]} ready plane
  where
    policy = decodeUtf8 . LBS.toStrict . encode . object $ ["min-age" .= object ["ageSeconds" .= quarantineSeconds]] <> extra
    ready e2e = do
        synced <- awaitProxyLog e2e (T.isInfixOf pypiSyncMessage) 120
        unless synced $ do
            logs <- proxyContainerLogs e2e
            expectationFailure (toString ("the proxy never synced its PyPI advisory generation. Last proxy log lines:\n" <> logTail logTailLines logs))
        action e2e

refusesRelease :: E2E -> Text -> Text -> Text -> Maybe Text -> Expectation
refusesRelease e2e name version rule advisory = do
    let wheelPath = gdpWorkDir (e2ePlane e2e) </> "pypi" </> "simple" </> toString name </> toString (pypiWheelFor name version)
    digest <- hexSha256Of <$> BS.readFile wheelPath
    advisoryRefuses
        ("No matching distribution found for " <> name <> "==" <> version)
        (Denial name version rule advisory)
        (advisoryDriver e2e name version digest)

advisoryDriver :: E2E -> Text -> Text -> Text -> AdvisoryDriver
advisoryDriver e2e name version digest =
    AdvisoryDriver
        { advisoryInstall = withPipProject e2e name version digest $ \project ->
            (,) <$> pipInstallIn project <*> pipInstalled project (pypiDistInfoFor name version)
        , advisoryArtifactStatus = proxyStatus e2e ("/pypi/simple/" <> name <> "/" <> pypiWheelFor name version)
        , advisoryAwaitDenials = \expected ->
            pollUntil
                120
                250000
                (elem expected)
                (mapMaybe parseDenial . T.lines <$> proxyContainerLogs e2e)
        }

parseDenial :: Text -> Maybe Denial
parseDenial line = do
    value <- Aeson.decodeStrict' (encodeUtf8 line) :: Maybe Value
    let top = asObject value
        fields = objectAt "data" top
    guard (textAt "message" top == Just "denied")
    Denial <$> textAt "package" fields <*> textAt "version" fields <*> textAt "rule" fields <*> pure (textAt "cve" fields)
