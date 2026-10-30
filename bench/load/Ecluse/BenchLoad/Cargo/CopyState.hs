-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Durable state for Cargo's private-copy rows, retaining only checked source rows present in the actual target index.
module Ecluse.BenchLoad.Cargo.CopyState (withCopyState, copyStateSettings, copyStateApplication) where

import Control.Concurrent (threadDelay)
import Control.Monad (foldM)
import Data.Aeson (Object, Value (Object, String))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Client (defaultManagerSettings, newManager, parseRequest)
import Network.Wai (Application, rawPathInfo)
import Network.Wai.Handler.Warp (testWithApplication)

import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Fixture (ProxyConfigure)
import Ecluse.BenchLoad.Harness (LoadKnobs (lkUpstreamLatencyMicros))
import Ecluse.BenchLoad.ObjectStore (conditionalObjectRead, objectRevision)
import Ecluse.BenchLoad.ProxyProcess (ProxySettings (..), mirrorStateBucket)
import Ecluse.Core.Ecosystem (Ecosystem (Cargo))
import Ecluse.Core.Mirror.State (encodeProjectRecord, extendProjectRecord, stateTarget)
import Ecluse.Core.Mirror.State.Internal (projectKey, retainVerifiedIdentity)
import Ecluse.Core.Package (Artifact (artUrl), PackageInfo (infoVersions), renderPackageName)
import Ecluse.Core.Registry (RegistryResponse (..))
import Ecluse.Core.Registry.CachedDocument (cargoCached)
import Ecluse.Core.Registry.Cargo.Document (cargoRecords)
import Ecluse.Core.Registry.Cargo.Metadata (selectCargoVersionDoc)
import Ecluse.Core.Registry.Cargo.Request (cargoArtifactHosts)
import Ecluse.Core.Registry.Metadata (Manifest (..), VersionDoc (VersionDoc))
import Ecluse.Core.Registry.MirrorUpload (MirrorReadData (CargoMirrorRead), checkedMirrorInventory, expectedArtifact, inventoryFiles, verifyMirrorBytes)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit, MirrorArtifactBodyLimit), Limits (maxMetadataBytes, maxMirrorArtifactBytes, progressFloor), defaultLimits, ecosystemArtifactAuthorities)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Corpus (CaptureUpstream (CaptureUpstream), CorpusPackage (cpPackage), cargoCorpusPackages)
import Ecluse.Test.Deadline (withExchangeClient)
import Ecluse.Test.Registry.Cargo.Fixtures (cargoShard)
import Ecluse.Test.Registry.Cargo.Metadata (cargoRead, parseCargoDiscovery)
import Ecluse.Test.Registry.Cargo.Reference (IndexLine (indexValue), referenceIndex)
import Ecluse.Test.Registry.Metadata.Fetch (captureManifest, fetchTestBody)

-- | Serve state that the configure callback fills from both stubs before boot. Unmatched private rows never acquire managed status.
withCopyState :: LoadKnobs -> (ProxyConfigure -> IO a) -> IO a
withCopyState knobs use = do
    records <- newIORef Map.empty
    let application request respond = readIORef records >>= \current -> copyStateApplication (lkUpstreamLatencyMicros knobs) current request respond
    testWithApplication (pure application) $ \statePort ->
        use $ \privatePort publicPort -> do
            writeIORef records =<< prepareRecords privatePort publicPort
            pure (copyStateSettings statePort)

-- | Bind the proxy to a state endpoint on this port, under the hosts its prepared records name.
copyStateSettings :: Int -> ProxySettings -> ProxySettings
copyStateSettings statePort settings =
    settings{psMirrorStatePort = Just statePort, psPublicHost = Just copyStatePublicHost, psPrivateHost = Just copyStatePrivateHost}

-- | The public upstream host a native private-copy state fixture reads its sources from.
copyStatePublicHost :: Text
copyStatePublicHost = "localhost"

-- | The private upstream host a native private-copy state fixture keys its records by.
copyStatePrivateHost :: Text
copyStatePrivateHost = "127.0.0.1"

prepareRecords :: Int -> Int -> IO (Map ByteString ByteString)
prepareRecords privatePort publicPort =
    withExchangeClient (newManager defaultManagerSettings) $ \client -> do
        config <- fetch client (MetadataBodyLimit 65536) (publicBase <> "/config.json")
        discovery <- either (benchFail . show) pure (parseCargoDiscovery (ecosystemArtifactAuthorities cargoArtifactHosts) publicBase (responseBody config))
        fmap (Map.fromList . catMaybes) $ forM cargoCorpusPackages $ \package -> do
            let name = cpPackage package
                path = "/" <> cargoShard (renderPackageName name)
            private <- fetch client (MetadataBodyLimit (maxMetadataBytes defaultLimits)) (privateBase <> path)
            public <- fetch client (MetadataBodyLimit (maxMetadataBytes defaultLimits)) (publicBase <> path)
            case (responseStatusCode private, responseStatusCode public) of
                (404, _) -> pure Nothing
                (_, 404) -> pure Nothing
                (200, 200) -> do
                    targetRows <- either benchFail pure (nativeRows (responseBody private))
                    source <- captureManifest (cargoRead discovery) (CaptureUpstream publicBase (ecosystemArtifactAuthorities cargoArtifactHosts)) name [responseBody public] >>= either (benchFail . show) pure
                    record <- foldM (retain client name targetRows source) Nothing (Map.toList (infoVersions (manifestInfo source)))
                    traverse
                        ( \value -> do
                            body <- either (benchFail . show) pure (encodeProjectRecord defaultLimits value)
                            let key = encodeUtf8 ("/" <> mirrorStateBucket <> "/" <> projectKey target name <> "/project.json")
                            pure (key, body)
                        )
                        record
                statuses -> benchFail ("Cargo copy-state preparation received HTTP " <> show statuses)
  where
    publicBase = "http://" <> copyStatePublicHost <> ":" <> show publicPort
    privateBase = "http://" <> copyStatePrivateHost <> ":" <> show privatePort
    target = stateTarget ("https://" <> copyStatePrivateHost <> ":" <> show privatePort)
    fetch client bound url = do
        request <- parseRequest (toString url)
        fetchTestBody client (progressFloor defaultLimits) bound request >>= either (benchFail . show) pure
    retain client name targetRows source previous (rawVersion, details) = do
        let version = mkVersion Cargo rawVersion
        selected <- maybe (benchFail "Cargo source has no selected native row") pure (selectCargoVersionDoc version (manifestRaw source))
        document <- maybe (benchFail "Cargo source selection lost native coordinates") pure (snd cargoCached selected)
        case cargoRecords document of
            [(_, Object fields)] | Map.lookup rawVersion targetRows == Just [fields] -> do
                inventory <- either (benchFail . show) pure (checkedMirrorInventory name version (VersionDoc details (Just selected)) (CargoMirrorRead document))
                verified <- forM (toList (inventoryFiles inventory)) $ \file -> do
                    path <- maybe (benchFail "Cargo relay source archive escaped its owned origin") pure (T.stripPrefix publicBase (artUrl (expectedArtifact file)))
                    bytes <- fetch client (MirrorArtifactBodyLimit (maxMirrorArtifactBytes defaultLimits)) (privateBase <> path)
                    unless (responseStatusCode bytes == 200) (benchFail "Cargo copy-state target archive is absent")
                    either (benchFail . show) pure (verifyMirrorBytes file (responseBody bytes))
                record <- either (benchFail . show) pure (extendProjectRecord defaultLimits target inventory previous)
                Just <$> foldM (\current proof -> either (benchFail . show) pure (retainVerifiedIdentity version proof current)) record verified
            _ | Map.member rawVersion targetRows -> benchFail "Cargo private-copy row differs from its owned source"
            _ -> pure previous

nativeRows :: ByteString -> Either Text (Map Text [Object])
nativeRows bytes = Map.fromListWith (<>) <$> traverse row (referenceIndex bytes)
  where
    row line = do
        fields <- indexValue line
        version <- case KeyMap.lookup "vers" fields of
            Just (String value) -> Right value
            _ -> Left "Cargo private index has no raw version"
        pure (version, [fields])

-- | Serve native record bytes with opaque revisions and conditional reads.
copyStateApplication :: Int -> Map ByteString ByteString -> Application
copyStateApplication latency records request respond = do
    when (latency > 0) (threadDelay latency)
    respond (conditionalObjectRead request ((\body -> (objectRevision body, body)) <$> Map.lookup (rawPathInfo request) records))
