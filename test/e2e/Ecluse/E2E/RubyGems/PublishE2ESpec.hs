-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native RubyGems publication retains exact bytes and installs back through the first-party view.
module Ecluse.E2E.RubyGems.PublishE2ESpec (
    rubyPublicationFamily,
    rubyPublicationConfig,
    rubyPublishRelease,
) where

import Data.ByteString qualified as BS
import Data.List (lookup)
import Data.Text qualified as T
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import Test.Hspec (shouldBe, shouldReturn)

import Ecluse.E2E.Fixtures.Contracts (concreteFamily)
import Ecluse.E2E.Fixtures.RubyGems
import Ecluse.E2E.Harness
import Ecluse.E2E.Harness.RubyGems
import Ecluse.E2E.Harness.RubyGemsGateway
import Ecluse.E2E.Harness.RubyGemsRegistry
import Ecluse.Test.Contract.Ecosystem (FamilyFixture)
import Ecluse.Test.Contract.Phase (timedFixture)
import Ecluse.Test.Contract.Publication
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Registry.RubyGems.Fixtures

-- | Caller authorisation and target API-key presentation remain independently configured.
rubyPublicationConfig :: E2EConfig
rubyPublicationConfig =
    defaultE2EConfig
        { ecExtraEnv =
            [ ("ECLUSE_MOUNTS__RUBYGEMS__PRIVATE_UPSTREAM__REGISTRY__URL", stubUrl RubyGemsPrivate)
            , ("ECLUSE_MOUNTS__RUBYGEMS__FIRST_PARTY", "ecluse-fixture-firstparty")
            , ("ECLUSE_MOUNTS__RUBYGEMS__PUBLICATION_TARGET__REGISTRY__URL", stubUrl RubyGemsPrivate)
            , ("ECLUSE_MOUNTS__RUBYGEMS__PUBLICATION_TARGET__REGISTRY__UPLOAD_URL", T.dropWhileEnd (== '/') (stubUrl RubyGemsPrivate))
            , ("ECLUSE_MOUNTS__RUBYGEMS__PUBLICATION_TARGET__REGISTRY__TOKEN", rubyRegistryToken)
            , ("ECLUSE_SERVER__AUTH_TOKEN", rubyRegistryToken)
            ]
        }

-- | The native command, fresh Bundler install and physical target inventory supply every shared law.
rubyPublicationFamily :: FamilyFixture PublicationFixture
rubyPublicationFamily = concreteFamily rubyPublicationFixture $ \recorder action plane ->
    timedFixture recorder withRubyRegistry $ \registry ->
        withStubFault plane (RubyGemsBackend (rubyRegistryPort registry) False) $
            withObservedRubyProxy rubyPublicationConfig plane $ \observed -> do
                releases <- loadRubyReleases
                accepted <- rubyRelease releases "ecluse-fixture-firstparty" "1.0.0" "ruby"
                refused <- rubyRelease releases "ecluse-fixture-sentinel" "1.0.0" "ruby"
                retained <- rubyRelease releases "ecluse-fixture-firstparty" "2.0.0" "ruby"
                action (PublicationFixture "1.0.0" (driver observed registry accepted) (driver observed registry refused) (yankDriver observed registry retained))

driver :: ObservedRubyProxy -> RubyRegistry -> RubyRelease -> PublicationDriver
driver observed registry release =
    PublicationDriver
        { publicationRun = publishObserved observed registry release
        , publicationInstall = withRubyProject currentBundler proxy (e2eBaseUrl proxy <> "/rubygems/") (Just ("ruby:" <> rubyRegistryToken)) $ \project -> do
            rubyDependency project (releaseName release) ("= " <> releaseVersion release)
            result <- rubyInstall project False
            when (crExit result == ExitSuccess) $ do
                value <- rubyEvaluate project "require 'ecluse_fixture_firstparty'; puts EcluseFixtureFirstparty::VALUE" >>= shouldSucceed
                T.strip (crStdout value) `shouldBe` "ecluse-fixture-firstparty-1.0.0-ruby"
            pure result
        , publicationVersions = versions
        , publicationAwait = versions
        , publicationSettle = pass
        }
  where
    proxy = rubyProxy observed
    versions = rubyRegistryVersions registry "rubygems-private" (releaseName release)

yankDriver :: ObservedRubyProxy -> RubyRegistry -> RubyRelease -> NonUploadDriver
yankDriver observed registry release =
    NonUploadDriver
        { nonUploadRelease = publishObserved observed registry release
        , nonUploadWrite = withRubyProject currentBundler proxy (e2eBaseUrl proxy <> "/rubygems/") Nothing $ \project ->
            rubyGemCommand project rubyRegistryToken ["yank", toString (releaseName release), "--version", toString (releaseVersion release)]
        , nonUploadRefusal = ["404"]
        , nonUploadDestination = do
            stored <- rubyRegistryFiles registry "rubygems-private"
            uploads <- readTVarIO (rubyRegistryUploads registry)
            pure ([releaseFilename item <> " " <> hexSha256Of (releaseArchive item) | item <- stored] <> ["uploads received " <> show (length uploads)])
        , nonUploadSettle = pass
        }
  where
    proxy = rubyProxy observed

publishObserved :: ObservedRubyProxy -> RubyRegistry -> RubyRelease -> IO ClientResult
publishObserved observed registry release = do
    before <- rubyTraffic observed
    headersBefore <- length <$> readTVarIO (rubyRegistryHeaders registry)
    uploadsBefore <- length <$> readTVarIO (rubyRegistryUploads registry)
    result <- rubyPublishRelease (rubyProxy observed) currentBundler release
    when (crExit result == ExitSuccess) $ do
        traffic <- rubyTrafficSince before <$> rubyTraffic observed
        [(rubyRequestMethod request, rubyRequestPath request) | request <- rubyClientRequests traffic]
            `shouldBe` [("POST", "/rubygems/api/v1/gems")]
        target <- drop headersBefore <$> readTVarIO (rubyRegistryHeaders registry)
        [(host, method, path, lookup "Authorization" headers, lookup "OTP" headers) | (host, method, path, headers) <- target]
            `shouldBe` [("rubygems-private", "POST", "/api/v1/gems", Just (encodeUtf8 rubyRegistryToken), Nothing)]
        drop uploadsBefore
            <$> readTVarIO (rubyRegistryUploads registry)
            `shouldReturn` [("rubygems-private", releaseArchive release)]
    pure result

-- | The RubyGems client uploads the exact independent archive without rebuilding its metadata.
rubyPublishRelease :: E2E -> BundlerClient -> RubyRelease -> IO ClientResult
rubyPublishRelease proxy client release = withRubyProject client proxy (e2eBaseUrl proxy <> "/rubygems/") Nothing $ \project -> do
    let archive = rubyDirectory project </> toString (releaseFilename release)
    BS.writeFile archive (releaseArchive release)
    rubyGemCommand project rubyRegistryToken ["push", archive]
