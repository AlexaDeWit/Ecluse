-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Real isolated Bundler installs and exact native publications drive common maintenance scenarios.
module Ecluse.E2E.Harness.RubyGemsCodeArtifact.Client (rubySweepClientOps) where

import Data.Text qualified as T
import System.Exit (ExitCode (ExitFailure, ExitSuccess))

import Ecluse.Core.Server.Path (encodeComponent)
import Ecluse.E2E.Harness.NativeEdge (observedProxy)
import Ecluse.E2E.Harness.Proxy (proxyContainerLogs)
import Ecluse.E2E.Harness.RubyGems
import Ecluse.E2E.Harness.RubyGemsCodeArtifact
import Ecluse.E2E.Harness.RubyGemsRegistry (rubyRegistryToken)
import Ecluse.E2E.Harness.Types (ClientResult (..))
import Ecluse.Test.Contract.Sweep.Types (ClientOps (..))
import Ecluse.Test.Registry.RubyGems.Fixtures

-- | Native process outcomes and installed Gem specifications remain independent of production parsing.
rubySweepClientOps :: ClientOps RubySweepStore RubyProject
rubySweepClientOps =
    ClientOps
        { clientInstall = \store name version -> withProject store (\project -> installIn project name version)
        , clientWithProject = withProject
        , clientInstallIn = installIn
        , clientRefusedIn = installIn
        , clientInstalledVersion = installedVersion
        , clientPublish = publish
        , clientArtifact = \store name version -> do
            release <- rubyRelease (rspReleases (rssPlane store)) name version "ruby"
            response@(status, body) <- storeRequest store ("gems/" <> encodeComponent (releaseFilename release)) "GET" Nothing
            when (status == 503) $ do
                logs <- proxyContainerLogs (observedProxy (rssObserved store))
                putTextLn ("unexpected RubyGems maintenance artifact response for " <> name <> "@" <> version <> ": " <> show status <> "\nBody:\n" <> decodeUtf8 body <> "\nProxy logs:\n" <> logs)
            pure response
        , clientMetadataStatus = \store name -> fst <$> storeRequest store ("info/" <> encodeComponent name) "GET" Nothing
        }

withProject :: RubySweepStore -> (RubyProject -> IO a) -> IO a
withProject store = withRubyProject currentBundler (observedProxy (rssObserved store)) (rssReadUrl store) (Just ("aws:" <> rubyRegistryToken))

installIn :: RubyProject -> Text -> Maybe Text -> IO ClientResult
installIn project name version = do
    rubyDependency project name (maybe ">= 0.a" ("= " <>) version)
    rubyInstall project False

installedVersion :: RubyProject -> Text -> IO (Maybe Text)
installedVersion project name = do
    result <- rubyEvaluate project ("spec = Gem.loaded_specs[" <> show name <> "]; print(spec.version.to_s) if spec")
    unless (crExit result == ExitSuccess) (fail (toString (crStderr result)))
    pure (if T.null (T.strip (crStdout result)) then Nothing else Just (T.strip (crStdout result)))

publish :: RubySweepStore -> Text -> Text -> IO ClientResult
publish store name version = do
    let releases = filter (\release -> releaseName release == name && releaseVersion release == version) (rspReleases (rssPlane store))
    when (null releases) (fail "the native publication coordinate is absent from the genuine Ruby corpus")
    outcomes <- traverse (storeRequest store "api/v1/gems" "POST" . Just . releaseArchive) releases
    let complete = all (\(status, _) -> status >= 200 && status < 300) outcomes
    pure
        ClientResult
            { crCommand = "native RubyGems archive POST"
            , crExit = if complete then ExitSuccess else ExitFailure 1
            , crStdout = T.unlines [show status <> " " <> decodeUtf8 body | (status, body) <- outcomes]
            , crStderr = ""
            }
