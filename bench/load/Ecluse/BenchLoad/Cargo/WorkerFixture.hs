-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native Cargo archives, index rows and independent upload observations for worker load.
module Ecluse.BenchLoad.Cargo.WorkerFixture (
    WorkerFixture (..),
    newWorkerFixture,
    resetWorkerFixture,
    sourceApp,
    targetApp,
    stateApp,
    fixturePackage,
    fixtureVersion,
    fixtureTrigger,
) where

import Codec.Compression.GZip qualified as GZip
import Data.Aeson (Value (Object), object)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Types (hContentType, status200, status400, status404)
import Network.Wai (Application, pathInfo, requestMethod, responseLBS, strictRequestBody)

import Ecluse.BenchLoad.Fixture (artifactBytes, delayUpstream)
import Ecluse.BenchLoad.WorkerState (WorkerTarget (..), acceptUpload, countRequest, newWorkerTarget, resetWorkerTarget, targetStateApp)
import Ecluse.Core.Ecosystem (Ecosystem (Cargo))
import Ecluse.Core.Package (PackageName, mkPackageName)
import Ecluse.Core.Registry.Cargo.Discovery (renderCargoDiscovery)
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Registry.Archive (tarBytes)
import Ecluse.Test.Registry.Cargo.Fixtures (CargoCrate (..), CargoRelease (..), cargoIndex, cargoShard, cargoUploadMetadata)
import Ecluse.Test.Registry.Cargo.Reference (UploadFrame (..), referenceUpload)
import Ecluse.Test.Wai (selfBaseUrl)

-- | Each run owns its complete target copy and conditional durable object state.
data WorkerFixture = WorkerFixture
    { wfFiles :: Map Text ByteString
    , wfRelease :: CargoRelease
    , wfState :: WorkerTarget
    }

-- | Package owned source bytes without executing the crate or a toolchain.
newWorkerFixture :: Int -> IO WorkerFixture
newWorkerFixture payloadBytes = do
    archive <-
        LBS.toStrict . GZip.compress
            <$> tarBytes
                [ ("example-1.0.0/Cargo.toml", encodeUtf8 manifest)
                , ("example-1.0.0/README.md", readme)
                , ("example-1.0.0/src/lib.rs", "pub fn marker() {}\n")
                , ("example-1.0.0/data", LBS.toStrict (artifactBytes payloadBytes))
                ]
    let spec = CargoCrate "example" "1.0.0" "unused" [] (object []) (object []) []
        release = CargoRelease spec archive (hexSha256Of archive) (decodeUtf8 readme)
    WorkerFixture (Map.singleton fixtureTrigger archive) release <$> newWorkerTarget
  where
    readme = "Owned Cargo worker load fixture\n"
    manifest =
        T.unlines
            [ "[package]"
            , "name = \"example\""
            , "version = \"1.0.0\""
            , "edition = \"2021\""
            , "rust-version = \"1.74\""
            , "authors = [\"Ecluse contributors\"]"
            , "description = \"Owned Cargo registry contract fixture\""
            , "license = \"MIT\""
            , "readme = \"README.md\""
            ]

-- | Remove the archive and state together before each measured complete copy.
resetWorkerFixture :: WorkerFixture -> IO ()
resetWorkerFixture = resetWorkerTarget . wfState

-- | The single native archive identifies the version trigger.
fixtureTrigger :: Text
fixtureTrigger = "example-1.0.0.crate"

-- | The archive and index use one exact native identity.
fixturePackage :: PackageName
fixturePackage = mkPackageName Cargo Nothing "example"

-- | The raw native version agrees with the packaged manifest.
fixtureVersion :: Version
fixtureVersion = mkVersion Cargo "1.0.0"

-- | Discovery and complete index requests remain separate from archive reads.
sourceApp :: Int -> WorkerFixture -> Application
sourceApp latency fixture request respond = do
    delayUpstream latency
    case filter (not . T.null) (pathInfo request) of
        ["config.json"] -> do
            countRequest (wfState fixture) "source-discovery"
            respond (responseLBS status200 [(hContentType, "application/json")] (fromStrict (renderCargoDiscovery (selfBaseUrl request <> "/crates/{crate}/{crate}-{version}.crate") Nothing False)))
        segments | segments == T.splitOn "/" (cargoShard "example") -> do
            countRequest (wfState fixture) "source-index"
            respond (responseLBS status200 [(hContentType, "text/plain")] (fromStrict (cargoIndex [wfRelease fixture])))
        ["crates", "example", filename] | Just bytes <- Map.lookup filename (wfFiles fixture) -> do
            countRequest (wfState fixture) "source-file"
            respond (responseLBS status200 [(hContentType, "application/octet-stream")] (fromStrict bytes))
        _ -> respond (responseLBS status404 [] "missing")

-- | Accept only native frames whose complete metadata and archive match the independent fixture.
targetApp :: Int -> WorkerFixture -> Application
targetApp latency fixture request respond = do
    delayUpstream latency
    case (requestMethod request, filter (not . T.null) (pathInfo request)) of
        ("GET", ["config.json"]) -> do
            countRequest (wfState fixture) "target-discovery"
            respond (responseLBS status200 [(hContentType, "application/json")] (fromStrict (renderCargoDiscovery (selfBaseUrl request <> "/crates/{crate}/{crate}-{version}.crate") (Just (selfBaseUrl request)) False)))
        ("GET", segments) | segments == T.splitOn "/" (cargoShard "example") -> do
            countRequest (wfState fixture) "target-index"
            files <- readIORef (wtFiles (wfState fixture))
            respond (responseLBS status200 [(hContentType, "text/plain")] (fromStrict (cargoIndex [wfRelease fixture | files == wfFiles fixture])))
        ("GET", ["crates", "example", filename]) -> do
            files <- readIORef (wtFiles (wfState fixture))
            respond $ maybe (responseLBS status404 [] "missing") (responseLBS status200 [] . fromStrict) (Map.lookup filename files)
        ("PUT", ["api", "v1", "crates", "new"]) -> do
            bytes <- LBS.toStrict <$> strictRequestBody request
            case referenceUpload bytes of
                Right uploaded
                    | Object expected <- cargoUploadMetadata (wfRelease fixture)
                    , uploadMetadata uploaded == expected
                    , uploadArchive uploaded == releaseArchive (wfRelease fixture) ->
                        acceptUpload (wfState fixture) fixtureTrigger (uploadArchive uploaded) (responseLBS status200 [(hContentType, "application/json")] "{\"ok\":true}") >>= respond
                _ -> respond (responseLBS status400 [] "Cargo upload fields or archive differ from the independent native fixture")
        _ -> respond (responseLBS status404 [] "missing")

-- | Serve the run's durable objects through the shared conditional object fixture.
stateApp :: Int -> WorkerFixture -> Application
stateApp latency = targetStateApp latency . wfState
