-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Whole-image PyPI advisory compilation, sync, denial, and quarantine overrides through pip.
module Ecluse.E2E.PyPI.AdvisoriesE2ESpec (spec) where

import Data.Aeson (Value, encode, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Pair)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Text qualified as T
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.E2E.Fixtures.Advisories (pypiAdvisoryExportBase, pypiAdvisoryId)
import Ecluse.E2E.Fixtures.PyPI (pypiDistInfoFor, pypiWheelFor)
import Ecluse.E2E.Harness
import Ecluse.Test.Json (asObject, objectAt, textAt)
import Ecluse.Test.Package (hexSha256Of)

-- | Drive the product image and its compiled PyPI artifact with isolated pip clients.
spec :: Spec
spec = whenE2EAvailable (aroundAll withGlobalDataPlane (aroundAllWith withPublishedAdvisories scenarios))

withPublishedAdvisories :: (GlobalDataPlane -> IO ()) -> GlobalDataPlane -> IO ()
withPublishedAdvisories action plane = do
    createAdvisoryBucket plane
    run <- publishAdvisoryExport plane PyPI pypiAdvisoryExportBase
    (roleExit run, roleOutput run) `shouldSatisfy` ((== ExitSuccess) . fst)
    action plane

scenarios :: SpecWith GlobalDataPlane
scenarios = describe "PyPI advisory policy through pip" $ do
    for_ advisoryDenies $ \(label, rule, threshold) ->
        it ("denies the old affected Flask release by " <> toString label) $ \plane ->
            withPolicy ["advisory-deny" .= object ["type" .= rule, threshold]] plane $ \e2e -> do
                files <- advertisedFiles e2e "flask"
                map fst files `shouldBe` [pypiWheelFor "flask" "1.19"]
                assertPipRefused e2e "flask" "1.18"
                assertDenial e2e "flask" "1.18" rule (Just pypiAdvisoryId)

    it "admits the recent exact fix while the remediation-disabled control quarantines it" $ \plane -> do
        withPolicy [] plane $ \e2e -> do
            files <- advertisedFiles e2e "flask"
            case lookup (pypiWheelFor "flask" "1.19") files of
                Nothing -> expectationFailure "the matching advisory fix was absent from the served index"
                Just digest ->
                    withPipProject e2e "flask" "1.19" digest $ \project -> do
                        void $ pipInstallIn project >>= shouldSucceedThroughProxy e2e
                        pipInstalled project (pypiDistInfoFor "flask" "1.19") `shouldReturn` True
        withPolicy ["remediation-fast-track" .= object ["enabled" .= False]] plane $ \e2e -> do
            files <- advertisedFiles e2e "flask"
            map fst files `shouldBe` [pypiWheelFor "flask" "1.18"]
            assertPipRefused e2e "flask" "1.19"
            assertDenial e2e "flask" "1.19" "BlockedByDefault" Nothing

    it "admits a recent release with a canonicalised advisory fix and preserves refusal controls" $ \plane -> do
        withPolicy ["advisory-deny" .= object ["type" .= ("DenyIfCve" :: Text), "minCvss" .= (7.0 :: Double)]] plane $ \e2e -> do
            assertPipRefused e2e "e2e-raw-fix" "1.18"
            assertDenial e2e "e2e-raw-fix" "1.18" "DenyIfCve" (Just "GHSA-e2e-raw-fix")
        withPolicy [] plane $ \e2e -> do
            files <- advertisedFiles e2e "e2e-raw-fix"
            case lookup (pypiWheelFor "e2e-raw-fix" "1.19") files of
                Nothing -> expectationFailure "the canonicalised advisory fix was absent from the served index"
                Just digest ->
                    withPipProject e2e "e2e-raw-fix" "1.19" digest $ \project -> do
                        void $ pipInstallIn project >>= shouldSucceedThroughProxy e2e
                        pipInstalled project (pypiDistInfoFor "e2e-raw-fix" "1.19") `shouldReturn` True
        withPolicy ["remediation-fast-track" .= object ["enabled" .= False]] plane $ \e2e -> do
            files <- advertisedFiles e2e "e2e-raw-fix"
            map fst files `shouldBe` [pypiWheelFor "e2e-raw-fix" "1.18"]
            assertPipRefused e2e "e2e-raw-fix" "1.19"
            assertDenial e2e "e2e-raw-fix" "1.19" "BlockedByDefault" Nothing

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

assertPipRefused :: E2E -> Text -> Text -> Expectation
assertPipRefused e2e name version = do
    let wheelPath = gdpWorkDir (e2ePlane e2e) </> "pypi" </> "simple" </> toString name </> toString (pypiWheelFor name version)
    digest <- hexSha256Of <$> BS.readFile wheelPath
    withPipProject e2e name version digest $ \project -> do
        result <- pipInstallIn project >>= shouldFail
        (crStdout result <> crStderr result) `shouldSatisfy` T.isInfixOf ("No matching distribution found for " <> name <> "==" <> version)
        pipInstalled project (pypiDistInfoFor name version) `shouldReturn` False

assertDenial :: E2E -> Text -> Text -> Text -> Maybe Text -> Expectation
assertDenial e2e name version rule advisory = do
    proxyStatus e2e ("/pypi/simple/" <> name <> "/" <> pypiWheelFor name version) `shouldReturn` 403
    logged <- awaitProxyLog e2e (any matches . T.lines) 120
    unless logged $ do
        logs <- proxyContainerLogs e2e
        expectationFailure (toString ("the refused wheel never produced its named denial. Last proxy log lines:\n" <> logTail logTailLines logs))
  where
    matches line = case Aeson.decodeStrict' (encodeUtf8 line) :: Maybe Value of
        Nothing -> False
        Just value ->
            let top = asObject value
                fields = objectAt "data" top
             in textAt "message" top == Just "denied"
                    && textAt "package" fields == Just name
                    && textAt "version" fields == Just version
                    && textAt "rule" fields == Just rule
                    && textAt "cve" fields == advisory
