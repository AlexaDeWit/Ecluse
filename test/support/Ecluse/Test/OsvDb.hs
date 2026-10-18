-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Compile temporary advisory artifacts through Pilot's compiler, read back what they hold, and
serve one through a slot as a synced mount does. Local HTTP stubs serve the chosen OSV archive and
the shared EPSS feed slice.
-}
module Ecluse.Test.OsvDb (
    epssFixtureFile,
    epssFixtureWith,
    withFixtureOsvDb,
    withOsvZipDb,
    compileOsvToSqlite,
    compileOsvZipDbTo,
    compileOsvZipDbWithFeedTo,

    -- * Candidates a publication decides on
    SourceServer,
    withSourceServer,
    withServedCandidate,
    withCompiledCandidate,

    -- * Reading an artifact back
    openCveDb,
    scoresOf,
    metaOf,
    withServedArtifact,

    -- * Pilot configuration over the stubs
    stubSourceEnv,
    withSourceStubs,
    denyIfEpssRules,
    epssFreeRules,
) where

import Codec.Compression.GZip qualified as GZip
import Conduit (MonadResource)
import Control.Monad.Catch (MonadMask)
import Data.Map.Strict qualified as Map
import Database.SQLite.Simple (Only (fromOnly), query_, withConnection)
import Network.HTTP.Types (Header)
import Network.HTTP.Types.Header (hLastModified)
import Network.HTTP.Types.Status (Status, status200)
import System.IO.Temp (withSystemTempDirectory)
import UnliftIO.Exception (bracket)

import Ecluse.Core.Cve (CveDb (cveDbClose), CveDbRejected)
import Ecluse.Core.Cve.Internal (transferCveDb)
import Ecluse.Core.Cve.Slot (newCveSlot, swapIn)
import Ecluse.Core.Cve.Types (DbEtag (DbEtag))
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Osv.Compile (CompileSources, OsvCandidate, mkCompileSources, newFeedManager, promoteCandidate, withOsvCandidate)
import Ecluse.Core.Osv.Ecosystem (OsvEcosystem, osvEcosystemFor)
import Ecluse.Core.Osv.Provenance (QuietTime (..), SourcePathRecording (FingerprintSourcePath))
import Ecluse.Core.Osv.Schema (EpssRequirement (EpssRequired))
import Ecluse.Core.Rules (RuleDeps)
import Ecluse.Core.Telemetry.Record (AdvisoryCompileMetricsPort)
import Ecluse.Test.Osv (CorpusVersion, osvCorpusZip, runOsvTestM)
import Ecluse.Test.Port (noopAdvisoryCompileMetricsPort)
import Ecluse.Test.Rules (slotRuleDeps)
import Ecluse.Test.Stub (Stub, stubBaseUrl, withRoutedStubIO, withStub)
import Ecluse.Test.Support (expectRight)
import Katip (KatipContext)
import OpenTelemetry.Trace.Core (TracerProvider)
import UnliftIO (MonadUnliftIO)

-- | Reject incompatible artifacts as values. The caller must protect the returned database's ownership.
openCveDb :: Ecosystem -> EpssRequirement -> FilePath -> IO (Either CveDbRejected CveDb)
openCveDb eco epssRequirement dbFile = transferCveDb eco epssRequirement dbFile pure

-- | The shared EPSS feed slice omits some corpus aliases to cover missing scores.
epssFixtureFile :: FilePath
epssFixtureFile = "test/unit/fixtures/epss/sample-epss.csv.gz"

-- | The fixture feed with these CVE aliases and scores appended, for a case that needs a score it lacks.
epssFixtureWith :: [(Text, Text)] -> IO LByteString
epssFixtureWith rows = do
    fixture <- GZip.decompress <$> readFileLBS epssFixtureFile
    pure (GZip.compress (fixture <> foldMap (\(cve, score) -> encodeUtf8 (cve <> "," <> score <> ",0.10000\n")) rows))

-- | Compile a committed corpus version into a temporary artifact through local HTTP stubs.
withFixtureOsvDb :: CorpusVersion -> (FilePath -> IO a) -> IO a
withFixtureOsvDb v use = do
    zipBytes <- osvCorpusZip v
    withOsvZipDb Npm zipBytes use

-- | Compile an archive and the shared EPSS slice into a temporary ecosystem artifact over local HTTP.
withOsvZipDb :: Ecosystem -> LByteString -> (FilePath -> IO a) -> IO a
withOsvZipDb eco zipBytes use =
    withSystemTempDirectory "ecluse-osv-fixture" (compileOsvZipDbTo eco zipBytes >=> use)

-- | Compile one ecosystem into @outDir@ through Pilot's feed manager and promote it at once, as a compile with no publication does.
compileOsvToSqlite :: (MonadResource m, MonadMask m, MonadUnliftIO m, KatipContext m) => AdvisoryCompileMetricsPort -> Maybe TracerProvider -> FilePath -> OsvEcosystem -> EpssRequirement -> CompileSources -> QuietTime -> m FilePath
compileOsvToSqlite metrics mTracerProvider outDir eco requirement sources quietTime = do
    feeds <- liftIO newFeedManager
    withOsvCandidate metrics mTracerProvider feeds outDir eco requirement sources quietTime promoteCandidate

-- | Compile into the supplied directory so tests can exercise artifact replacement.
compileOsvZipDbTo :: Ecosystem -> LByteString -> FilePath -> IO FilePath
compileOsvZipDbTo eco zipBytes dir = do
    epssBytes <- readFileLBS epssFixtureFile
    compileOsvZipDbWithFeedTo eco EpssRequired (status200, epssBytes) zipBytes dir

-- | 'compileOsvZipDbTo' against a chosen feed answer, under a chosen EPSS requirement.
compileOsvZipDbWithFeedTo :: Ecosystem -> EpssRequirement -> (Status, LByteString) -> LByteString -> FilePath -> IO FilePath
compileOsvZipDbWithFeedTo eco requirement (feedStatus, epssBytes) zipBytes dir =
    withStub status200 zipBytes $ \osvStub ->
        withStub feedStatus epssBytes $ \epssStub ->
            runOsvTestM
                ( compileOsvToSqlite
                    noopAdvisoryCompileMetricsPort
                    Nothing
                    dir
                    (osvEcosystemFor eco)
                    requirement
                    (stubSources osvStub epssStub)
                    fixtureQuietTime
                )

-- | Stable OSV and EPSS URLs, so successive compiles share each source's identity, with an OSV answer a case changes between them.
data SourceServer = SourceServer
    { ssAnswer :: IORef (LByteString, [Header])
    , ssSources :: CompileSources
    }

-- | Serve the shared EPSS slice and, until a case sets one, an empty OSV answer.
withSourceServer :: (SourceServer -> IO a) -> IO a
withSourceServer use = do
    epssBytes <- readFileLBS epssFixtureFile
    answer <- newIORef (mempty, [])
    withRoutedStubIO (\_ -> (\(body, headers) -> (status200, headers, body)) <$> readIORef answer) $ \osvStub ->
        withStub status200 epssBytes $ \epssStub ->
            use
                SourceServer
                    { ssAnswer = answer
                    , ssSources = stubSources osvStub epssStub
                    }

-- The shipped default recording, at the paths the public feeds serve.
stubSources :: Stub -> Stub -> CompileSources
stubSources osvStub epssStub =
    mkCompileSources
        FingerprintSourcePath
        (toString (stubBaseUrl osvStub) <> "/all.zip")
        (toString (stubBaseUrl epssStub) <> "/epss_scores-current.csv.gz")

-- | Serve this archive under this HTTP @Last-Modified@, and compile it into an unpromoted candidate under @dir@.
withServedCandidate :: SourceServer -> Ecosystem -> LByteString -> Maybe Text -> FilePath -> (OsvCandidate -> IO a) -> IO a
withServedCandidate server eco zipBytes lastModified dir use = do
    writeIORef (ssAnswer server) (zipBytes, [(hLastModified, encodeUtf8 stamp) | stamp <- maybeToList lastModified])
    feeds <- newFeedManager
    runOsvTestM (withOsvCandidate noopAdvisoryCompileMetricsPort Nothing feeds dir (osvEcosystemFor eco) EpssRequired (ssSources server) fixtureQuietTime (liftIO . use))

-- | Compile an archive and the shared EPSS slice into an unpromoted candidate under @dir@, for a publication to decide on.
withCompiledCandidate :: Ecosystem -> LByteString -> FilePath -> (OsvCandidate -> IO a) -> IO a
withCompiledCandidate eco zipBytes dir use =
    withSourceServer $ \server -> withServedCandidate server eco zipBytes Nothing dir use

-- A century, so a committed fixture's own dates never raise the quiet-time alarm in a suite
-- that is about something else.
fixtureQuietTime :: QuietTime
fixtureQuietTime = QuietTime{qtOsv = century, qtEpss = century}
  where
    century = 100 * 365 * 86400

-- | Every range's EPSS score, in table order.
scoresOf :: FilePath -> IO [Maybe Double]
scoresOf dbFile = withConnection dbFile $ \conn ->
    map fromOnly <$> (query_ conn "SELECT epss_score FROM package_vulnerability_ranges" :: IO [Only (Maybe Double)])

-- | The artifact's @meta@ table.
metaOf :: FilePath -> IO (Map Text Text)
metaOf dbFile = withConnection dbFile $ \conn ->
    Map.fromList <$> (query_ conn "SELECT key, value FROM meta" :: IO [(Text, Text)])

-- | Serve the artifact from a fresh slot, as a synced mount reads it, and close it once the action returns.
withServedArtifact :: Ecosystem -> FilePath -> (RuleDeps -> CveDb -> IO a) -> IO a
withServedArtifact eco path use =
    bracket (openCveDb eco EpssRequired path >>= expectRight) cveDbClose $ \db -> do
        slot <- newCveSlot
        swapIn slot (DbEtag (toText path)) Nothing db
        use (slotRuleDeps slot) db

-- | Configuration that points both advisory sources at stubs.
stubSourceEnv :: Stub -> Stub -> [(String, String)]
stubSourceEnv osvStub epssStub =
    [ ("ECLUSE_ADVISORIES__OSV_EXPORT_BASE_URL", toString (stubBaseUrl osvStub))
    , ("ECLUSE_ADVISORIES__EPSS_FEED_URL", toString (stubBaseUrl epssStub) <> "/epss.csv.gz")
    ]

-- | Serve this OSV archive and an EPSS feed of this status and body from stubs, handing the case their 'stubSourceEnv'.
withSourceStubs :: LByteString -> (Status, LByteString) -> ([(String, String)] -> Stub -> IO a) -> IO a
withSourceStubs zipBytes (feedStatus, feedBytes) use =
    withStub status200 zipBytes $ \osvStub ->
        withStub feedStatus feedBytes $ \epssStub ->
            use (stubSourceEnv osvStub epssStub) epssStub

-- | A rule set with one @DenyIfEpss@, at the threshold and alignment that still require the feed.
denyIfEpssRules :: String
denyIfEpssRules = "{\"risk\":{\"type\":\"DenyIfEpss\",\"minEpss\":1,\"onUnavailable\":\"skip\"}}"

-- | A patch under which no rule reads EPSS: the shipped remediation rule abstains on partial fixes.
epssFreeRules :: String
epssFreeRules = "{\"remediation-fast-track\":{\"remainingAdvisories\":\"abstain\"}}"
