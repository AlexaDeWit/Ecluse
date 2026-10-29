-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Fresh Cargo builds and exact native upload frames for common maintenance scenarios.
module Ecluse.E2E.Harness.CargoDredger.Client (cargoSweepClientOps) where

import Network.HTTP.Client qualified as Http
import Network.HTTP.Types (statusCode)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))

import Ecluse.E2E.Harness.Cargo
import Ecluse.E2E.Harness.CargoDredger
import Ecluse.E2E.Harness.NativeEdge (observedProxy)
import Ecluse.E2E.Harness.CargoRegistry (cargoRegistryToken)
import Ecluse.E2E.Harness.Types
import Ecluse.Test.Contract.Sweep.Types (ClientOps (..))
import Ecluse.Test.Registry.Cargo.Fixtures
import Ecluse.Test.Registry.Cargo.Reference (CargoLocked (..))

-- | Install evidence comes from Cargo's exit status and independently decoded lockfile.
cargoSweepClientOps :: ClientOps CargoSweepStore CargoProject
cargoSweepClientOps =
    ClientOps
        { clientInstall = \store name version -> withProject store (\project -> installIn project name version)
        , clientWithProject = withProject
        , clientInstallIn = installIn
        , clientRefusedIn = installIn
        , clientInstalledVersion = installedVersion
        , clientPublish = publish
        , clientArtifact = \store name version -> fetchStorePath store ("crates/" <> name <> "/" <> version <> "/" <> name <> "-" <> version <> ".crate")
        , clientMetadataStatus = \store name -> fst <$> fetchStorePath store (cargoShard name)
        }

withProject :: CargoSweepStore -> (CargoProject -> IO a) -> IO a
withProject store = withCargoProject currentCargo CargoReplacement (observedProxy (cssObserved store)) ("sparse+" <> cssReadUrl store) (Just cargoRegistryToken)

installIn :: CargoProject -> Text -> Maybe Text -> IO ClientResult
installIn project name version = do
    cargoSelectRequirement CargoReplacement name (maybe "*" ("=" <>) version) project
    runCargo project ["build"]

installedVersion :: CargoProject -> Text -> IO (Maybe Text)
installedVersion project name = do
    locked <- cargoLock project >>= either (fail . toString) pure
    case [cargoLockedVersion package | package <- locked, cargoLockedName package == name] of
        [] -> pure Nothing
        [version] -> pure (Just version)
        _ -> fail "Cargo retained multiple selected versions of the native leaf fixture"

fetchStorePath :: CargoSweepStore -> Text -> IO (Int, LByteString)
fetchStorePath store path = fetchPath store (cssReadUrl store <> path) "GET" Nothing

fetchPath :: CargoSweepStore -> Text -> ByteString -> Maybe ByteString -> IO (Int, LByteString)
fetchPath store path method body = do
    base <- Http.parseRequest (toString path)
    let request =
            base
                { Http.method = method
                , Http.requestHeaders = [("Authorization", encodeUtf8 cargoRegistryToken), ("Content-Type", "application/octet-stream")]
                , Http.requestBody = maybe (Http.RequestBodyBS "") Http.RequestBodyBS body
                , Http.redirectCount = 0
                }
    response <- Http.httpLbs request (e2eManager (observedProxy (cssObserved store)))
    pure (statusCode (Http.responseStatus response), Http.responseBody response)

publish :: CargoSweepStore -> Text -> Text -> IO ClientResult
publish store name version = do
    release <-
        maybe (fail "the native publication coordinate is absent from the owned corpus") pure $
            find (\candidate -> crateName (releaseCrate candidate) == name && crateVersion (releaseCrate candidate) == version) (cspReleases (cssPlane store))
    bytes <- either (fail . toString) pure (cargoUpload release)
    (status, body) <- fetchPath store (cssPublishUrl store <> "api/v1/crates/new") "PUT" (Just bytes)
    pure
        ClientResult
            { crCommand = "native Cargo framed PUT"
            , crExit = if status == 200 then ExitSuccess else ExitFailure 1
            , crStdout = show status <> " " <> decodeUtf8 body
            , crStderr = ""
            }
