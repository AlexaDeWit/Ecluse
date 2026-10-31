-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Focused directory replay uses an immutable public capture and an empty authorised private view.
module Ecluse.BenchLoad.RubyGems.Directory (rubyGemsDirectoryScenario, directoryEntries, directoryStub) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (hContentType, status200, status404)
import Network.Wai (Application, pathInfo, responseLBS)
import System.Directory (getFileSize)

import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Fixture (delayUpstream, fetchChecked, httpTarget, withProxyOverStubs)
import Ecluse.BenchLoad.Harness (LoadKnobs (..), Scenario (..), scenario)
import Ecluse.Core.Ecosystem (Ecosystem (RubyGems))
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Wai (localhost)

-- | Explicit selection requires the pinned external body, at most four clients and fifteen seconds.
rubyGemsDirectoryScenario :: Scenario
rubyGemsDirectoryScenario =
    (scenario "versions-directory" description boot)
        { scenarioServiceTime = False
        , scenarioPublicLegUnavailableReason = Just "one injected latency per request does not model a directory transfer"
        }
  where
    description =
        "Diagnostic only. Replay the public https://rubygems.org/versions capture dated 2026-10-28T21:14:57Z. Source bytes: "
            <> show captureBytes
            <> ". Source distinct names: 198153. Source SHA-256: "
            <> captureDigest
            <> ". The private directory is empty. Preflight compares independently reconciled version sets and requires blank hints. Oha reports actual transferred body bytes across all statuses. This row has no calibrated floor."
    boot knobs use = do
        when (lkConcurrency knobs > 4 || lkDurationSeconds knobs > 15) (benchFail "versions-directory requires at most four clients and fifteen seconds")
        path <- lookupEnv "BENCH_RUBYGEMS_DIRECTORY_BODY" >>= maybe (benchFail "versions-directory requires BENCH_RUBYGEMS_DIRECTORY_BODY") pure
        size <- getFileSize path
        unless (size == captureBytes) (benchFail "versions-directory capture byte count differs from its pin")
        bytes <- readFileBS path
        unless (BS.length bytes == fromInteger captureBytes && hexSha256Of bytes == captureDigest) (benchFail "versions-directory capture differs from its pinned SHA-256")
        expected <- either benchFail pure (directoryEntries bytes)
        let latency = lkUpstreamLatencyMicros knobs
            targets port = [localhost port <> "/rubygems/versions"]
        withProxyOverStubs RubyGems knobs 60 Nothing (directoryStub latency "---\n") (directoryStub latency bytes) targets $ \proxy urls -> do
            for_ urls $ \url -> do
                response <- fetchChecked status200 [] url
                let body = toStrict (HTTP.responseBody response)
                observed <- either benchFail pure (directoryEntries body)
                unless (observed == expected && not (Map.null observed)) (benchFail "versions-directory preflight lost or added names or versions")
                rows <- either benchFail pure (directoryRows body)
                unless (all (T.null . third) rows && length rows == Map.size observed) (benchFail "versions-directory preflight requires one row per name with a blank hint")
            httpTarget use proxy urls
    third (_, _, hint) = hint

captureBytes :: Integer
captureBytes = 24528874

captureDigest :: Text
captureDigest = "e5d7e406eec7ed3d15f043041474ff3a182f6023f1e3b9baf44b6fa24c4838e1"

-- | This independent reader reconciles journal additions and removals without the production parser.
directoryEntries :: ByteString -> Either Text (Map Text (Set Text))
directoryEntries bytes = do
    rows <- directoryRows bytes
    let apply names (name, versions, _) = Map.alter (\current -> Just (foldl' update (fromMaybe Set.empty current) versions)) name names
          where
            update current version = case T.stripPrefix "-" version of
                Just removed -> Set.delete removed current
                Nothing -> Set.insert version current
        histories = foldl' apply Map.empty [(name, T.splitOn "," versions, hint) | (name, versions, hint) <- rows]
    pure (Map.filter (not . Set.null) histories)

directoryRows :: ByteString -> Either Text [(Text, Text, Text)]
directoryRows bytes = do
    body <- first show (Text.decodeUtf8' bytes)
    rows <- case dropWhile (/= "---") (T.lines body) of
        [] -> Left "directory header delimiter is absent"
        _ : rest -> Right rest
    traverse row (filter (not . T.null) rows)
  where
    row raw = case T.splitOn " " raw of
        [name, versions, hint] | not (T.null name || T.null versions) && all validVersion (T.splitOn "," versions) -> Right (name, versions, hint)
        _ -> Left "directory row is incomplete"
    validVersion version = not (T.null version) && version /= "-"

-- | A native directory source serves exact supplied bytes and refuses unrelated paths.
directoryStub :: Int -> ByteString -> Application
directoryStub latency bytes request respond = do
    delayUpstream latency
    case pathInfo request of
        ["versions"] -> respond (responseLBS status200 [(hContentType, "text/plain")] (fromStrict bytes))
        _ -> respond (responseLBS status404 [] "missing")
