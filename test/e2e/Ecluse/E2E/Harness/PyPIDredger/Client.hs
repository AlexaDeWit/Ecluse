-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Fresh pip clients and native multipart publication for the shared maintenance scenarios.
module Ecluse.E2E.Harness.PyPIDredger.Client (pypiSweepClientOps) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Client qualified as Http
import Network.HTTP.Types (statusCode)
import System.Directory (doesFileExist, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.E2E.Fixtures.PyPI (pypiWheelFor)
import Ecluse.E2E.Fixtures.PyPI.Mirror (Distribution (..), distributionUploadBody, nativeUploadContentType)
import Ecluse.E2E.Harness.Client (runClient)
import Ecluse.E2E.Harness.Pip (pipCredentialUrl, pipSingleAttempt, withPipProject)
import Ecluse.E2E.Harness.PyPIDredger
import Ecluse.E2E.Harness.Types
import Ecluse.Test.Contract.Sweep.Types (ClientOps (..))

-- | Every install uses the product's native index and records pip's actual exit and installed version.
pypiSweepClientOps :: ClientOps PyPISweepStore PipProject
pypiSweepClientOps =
    ClientOps
        { clientInstall = \store name version -> withProject store (\project -> installIn [] project name version)
        , clientWithProject = withProject
        , clientInstallIn = installIn []
        , clientRefusedIn = installIn pipSingleAttempt
        , clientInstalledVersion = installedVersion
        , clientPublish = publish
        , clientArtifact = \store name version -> fetchStorePath store ("simple/" <> name <> "/" <> pypiWheelFor name version)
        , clientMetadataStatus = \store name -> fst <$> fetchStorePath store ("simple/" <> name <> "/")
        }

withProject :: PyPISweepStore -> (PipProject -> IO a) -> IO a
withProject store = withPipProject proxy "unused-fixture" "0" "unused"
  where
    proxy = (pssProxy store){e2eClientEndpoints = Map.singleton PyPI (pipCredentialUrl "aws" "fixture-token" (pssReadUrl store <> "simple/"))}

installIn :: [String] -> PipProject -> Text -> Maybe Text -> IO ClientResult
installIn attempts project name version =
    runClient
        (ppDir project)
        (ppEnv project)
        "python3"
        ( ["-m", "pip", "--isolated", "install"]
            <> attempts
            <> [ "--no-cache-dir"
               , "--disable-pip-version-check"
               , "--no-input"
               , "--only-binary=:all:"
               , "--index-url"
               , toString (ppIndex project)
               , "--target"
               , ppDir project </> "site"
               , toString (name <> maybe "" ("==" <>) version)
               ]
        )

installedVersion :: PipProject -> Text -> IO (Maybe Text)
installedVersion project name = do
    entries <- listDirectory (ppDir project </> "site")
    let candidates = [ppDir project </> "site" </> entry </> "METADATA" | entry <- entries, (T.replace "-" "_" name <> "-") `T.isPrefixOf` toText entry, ".dist-info" `T.isSuffixOf` toText entry]
    files <- filterM doesFileExist candidates
    case files of
        [path] -> listToMaybe . mapMaybe (T.stripPrefix "Version: ") . T.lines . decodeUtf8 <$> readFileBS path
        [] -> pure Nothing
        _ -> fail "pip left more than one installed version for the fixture project"

fetchStorePath :: PyPISweepStore -> Text -> IO (Int, LByteString)
fetchStorePath store path = do
    base <- Http.parseRequest (toString (pssReadUrl store <> path))
    response <- Http.httpLbs (Http.applyBasicAuth "aws" "fixture-token" base) (e2eManager (pssProxy store))
    pure (statusCode (Http.responseStatus response), Http.responseBody response)

publish :: PyPISweepStore -> Text -> Text -> IO ClientResult
publish store name version = do
    distributions <- case filter (\d -> distributionProject d == name && distributionVersion d == version) (pssUploads store) of
        [] -> fail "the independent fixture corpus has no requested publication"
        values -> pure values
    results <- traverse (publishDistribution store) distributions
    pure
        ClientResult
            { crCommand = "native multipart POST"
            , crExit = if all ((== 200) . fst) results then ExitSuccess else ExitFailure 1
            , crStdout = T.unlines [show status <> " " <> decodeUtf8 bytes | (status, bytes) <- results]
            , crStderr = ""
            }

publishDistribution :: PyPISweepStore -> Distribution -> IO (Int, LByteString)
publishDistribution store distribution = do
    base <- Http.parseRequest (toString (pssPublishUrl store))
    let request =
            Http.applyBasicAuth
                "aws"
                "fixture-token"
                base
                    { Http.method = "POST"
                    , Http.requestHeaders = [("Content-Type", nativeUploadContentType)]
                    , Http.requestBody = Http.RequestBodyBS (distributionUploadBody distribution [])
                    }
    response <- Http.httpLbs request (e2eManager (pssProxy store))
    pure (statusCode (Http.responseStatus response), Http.responseBody response)
