-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Genuine native archives and quick specifications bind every measured platform upload.
module Ecluse.BenchLoad.RubyGems.WorkerFixture (
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

import Data.Aeson (withObject, (.:))
import Data.Aeson.Types (parseEither)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Types (hContentType, status200, status400, status404, status503)
import Network.Wai (Application, pathInfo, requestHeaders, requestMethod, responseLBS, strictRequestBody)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.IO.Temp (withTempDirectory)
import System.Process (readProcessWithExitCode)

import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Fixture (delayUpstream)
import Ecluse.BenchLoad.WorkerState (stateAppFor)
import Ecluse.Core.Ecosystem (Ecosystem (RubyGems))
import Ecluse.Core.Package (PackageName, mkPackageName)
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Test.Registry.RubyGems.Fixtures (RubyRelease (..), loadRubyReleasesFrom)
import Ecluse.Test.Registry.RubyGems.Reference (RubyInfoRow (..), referenceInfo, runRubyReference)

-- | Complete target bytes and conditional object state belong to one measured fixture.
data WorkerFixture = WorkerFixture
    { wfFiles :: Map Text ByteString
    , wfSpecs :: Map Text ByteString
    , wfInfo :: ByteString
    , wfRows :: Map Text ByteString
    , wfTarget :: IORef (Map Text ByteString)
    , wfObjects :: IORef (Map ByteString (ByteString, ByteString))
    , wfCounts :: IORef (Map Text Int)
    , wfReject :: IORef Bool
    }

-- | Ruby builds payload-sized archives before timing. The manifest authenticates every retained body.
newWorkerFixture :: Int -> IO WorkerFixture
newWorkerFixture payloadBytes = do
    createDirectoryIfMissing True "scratchpad"
    withTempDirectory "scratchpad" "rubygems-worker" $ \directory -> do
        ruby <- fromMaybe "ruby" <$> lookupEnv "ECLTEST_RUBY"
        (status, _, diagnostic) <- readProcessWithExitCode ruby ["test/fixtures/rubygems/generate.rb", "--output", directory, "--payload-bytes", show payloadBytes] ""
        unless (status == ExitSuccess) (benchFail ("RubyGems worker fixture generation failed: " <> toText diagnostic))
        releases <- filter ((== "ecluse-fixture-platform") . releaseName) <$> loadRubyReleasesFrom directory
        unless (sort (map releasePlatform releases) == ["arm64-darwin", "java", "ruby", "x86_64-linux-musl"] && all ((== "1.0.0") . releaseVersion) releases) (benchFail "RubyGems worker fixture lost an independent platform coordinate")
        info <- BS.readFile (directory </> "info/ecluse-fixture-platform")
        rows <- either benchFail pure (referenceInfo info)
        let physical = Map.fromList (zip [0 ..] (BS.split 10 info))
        native <- forM releases $ \release -> do
            observed <- case filter (\row -> rowVersion row == releaseVersion release && rowPlatform row == releasePlatform release) rows of
                [row] -> pure row
                _ -> benchFail "RubyGems worker source row is absent or ambiguous"
            unless (lookup "checksum" (rowFields observed) == Just (releaseChecksum release)) (benchFail "RubyGems worker source checksum differs from the native archive")
            source <- maybe (benchFail "RubyGems worker physical source row is absent") pure (Map.lookup (rowPosition observed) physical)
            archive <- runRubyReference "archive" (releaseArchive release)
            observedBytes <- either (benchFail . toText) pure (parseEither (withObject "native archive" (\value -> value .: "file_sizes" >>= (.: "lib/payload.bin"))) archive)
            unless (observedBytes == payloadBytes) (benchFail "RubyGems worker archive payload size differs from the configured workload")
            void (runRubyReference "compact" (releaseCompact release))
            pure (releaseFilename release, source)
        let files = Map.fromList [(releaseFilename release, releaseArchive release) | release <- releases]
            specs = Map.fromList [(toText (releaseCompactPath release), releaseCompact release) | release <- releases]
        WorkerFixture files specs info (Map.fromList native) <$> newIORef mempty <*> newIORef mempty <*> newIORef mempty <*> newIORef False

-- | Target archives and durable objects reset together outside each measured job interval.
resetWorkerFixture :: WorkerFixture -> IO ()
resetWorkerFixture fixture = do
    writeIORef (wfTarget fixture) mempty
    writeIORef (wfObjects fixture) mempty
    writeIORef (wfCounts fixture) mempty

-- | A generic artifact triggers the complete four-platform version.
fixtureTrigger :: Text
fixtureTrigger = "ecluse-fixture-platform-1.0.0.gem"

-- | Every fixture artifact belongs to this unscoped package.
fixturePackage :: PackageName
fixturePackage = mkPackageName RubyGems Nothing "ecluse-fixture-platform"

-- | All platform archives use the same exact raw version.
fixtureVersion :: Version
fixtureVersion = mkVersion RubyGems "1.0.0"

-- | Metadata, original quick specifications and archives remain independently observable source reads.
sourceApp :: Int -> WorkerFixture -> Application
sourceApp latency fixture request respond = do
    delayUpstream latency
    case pathInfo request of
        ["info", "ecluse-fixture-platform"] -> count fixture "source-info" >> respond (responseLBS status200 [(hContentType, "text/plain")] (fromStrict (wfInfo fixture)))
        ["gems", filename] | Just bytes <- Map.lookup filename (wfFiles fixture) -> count fixture "source-file" >> respond (responseLBS status200 [] (fromStrict bytes))
        segments | Just bytes <- Map.lookup (T.intercalate "/" segments) (wfSpecs fixture) -> count fixture "source-quick" >> respond (responseLBS status200 [] (fromStrict bytes))
        _ -> respond (responseLBS status404 [] "missing")

-- | Publication succeeds only for exact independently built bytes after durable inventory exists.
targetApp :: Int -> WorkerFixture -> Application
targetApp latency fixture request respond = do
    delayUpstream latency
    case (requestMethod request, pathInfo request) of
        ("GET", ["info", "ecluse-fixture-platform"]) -> do
            count fixture "target-info"
            files <- readIORef (wfTarget fixture)
            let rows = [row | (filename, row) <- Map.toList (wfRows fixture), Map.member filename files]
            respond (responseLBS status200 [(hContentType, "text/plain")] (fromStrict ("---\n" <> foldMap (<> "\n") rows)))
        ("GET", ["gems", filename]) -> do
            files <- readIORef (wfTarget fixture)
            respond (maybe (responseLBS status404 [] "missing") (responseLBS status200 [] . fromStrict) (Map.lookup filename files))
        ("GET", segments) | Just bytes <- Map.lookup (T.intercalate "/" segments) (wfSpecs fixture) -> do
            files <- readIORef (wfTarget fixture)
            respond (if Map.size files == Map.size (wfFiles fixture) then responseLBS status200 [] (fromStrict bytes) else responseLBS status404 [] "missing")
        ("POST", ["api", "v1", "gems"]) -> do
            bytes <- LBS.toStrict <$> strictRequestBody request
            if lookup hContentType (requestHeaders request) /= Just "application/octet-stream"
                then respond (responseLBS status400 [] "RubyGems upload has the wrong content type")
                else case [(filename, expected) | (filename, expected) <- Map.toList (wfFiles fixture), expected == bytes] of
                    [(filename, _)] -> do
                        count fixture "upload-attempt"
                        reject <- readIORef (wfReject fixture)
                        objects <- readIORef (wfObjects fixture)
                        if reject
                            then respond (responseLBS status503 [] "injected upload rejection")
                            else
                                if Map.null objects
                                    then respond (responseLBS status400 [] "upload preceded durable inventory")
                                    else do
                                        modifyIORef' (wfTarget fixture) (Map.insert filename bytes)
                                        count fixture "upload"
                                        respond (responseLBS status200 [] "accepted")
                    _ -> respond (responseLBS status400 [] "RubyGems upload differs from every independent native archive")
        _ -> respond (responseLBS status404 [] "missing")

-- | The same conditional object fixture supplies durable source state for every ecosystem.
stateApp :: Int -> WorkerFixture -> Application
stateApp latency fixture = stateAppFor (wfObjects fixture) (wfCounts fixture) latency

count :: WorkerFixture -> Text -> IO ()
count fixture label = atomicModifyIORef' (wfCounts fixture) (\counts -> (Map.insertWith (+) label 1 counts, ()))
