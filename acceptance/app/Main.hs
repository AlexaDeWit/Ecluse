-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Performance acceptance over the benchmark catalogue.
@captures@ measures each leg over the committed captures and holds its allocation to a budget.
@live@ measures the same legs over freshly fetched registry documents and reports them.
-}
module Main (main, fullAllocationContract) where

-- relude's prelude exports a Bounded/Enum-based `universe`. The Generic-derived one is used here.
import Prelude hiding (universe)

import Control.Exception qualified as Exception
import Data.Aeson (Value, encode)
import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Time (getCurrentTime)
import Data.Universe.Class (universe)
import GHC.Clock (getMonotonicTime)
import GHC.Conc (getAllocationCounter)
import Network.HTTP.Client (Request, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.IO.Temp (withSystemTempDirectory)

import Ecluse.Acceptance (
    Fetched (Fetched, Refused, Unreachable),
    Leg (FullAllAdvisoryRules, FullDocument, FullShippedAdvisories, SingleVersion),
    Measurement (Measurement),
    OperatingPoint (OperatingPoint),
    PackageOutcome (Failed, Measured, Unavailable),
    Sample (Sample),
    assessCaptures,
    capturesAnnotations,
    capturesExitCode,
    classifyFetch,
    legKey,
    liveAnnotations,
    liveExitCode,
    loadCriteria,
    renderCapturesReport,
    renderLiveReport,
 )
import Ecluse.Acceptance.Evidence (capturesEvidence, liveEvidence)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.Adapter.Capability (AdapterMetadata (metadataRead))
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient)
import Ecluse.Core.Registry.Metadata (Manifest (manifestDigest, manifestInfo, manifestRaw))
import Ecluse.Core.Registry.Metadata.Fetch.Types (EcosystemRead (erRequest))
import Ecluse.Core.Rules (RuleDeps)
import Ecluse.Core.Rules.Types (EvalContext (EvalContext), PrecededRule)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), Limits (progressFloor), defaultLimits, maxMetadataBytes)
import Ecluse.Core.Snapshot (Snapshot (Snapshot))
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Rts (RtsPosture (rpAllocAreaBytes, rpCapabilities), currentRtsPosture)
import Ecluse.Test.Corpus (CaptureRecord (crCapturedAt), CorpusPackage (cpPackage), cpName, permissiveAgeRules, readCaptureRecords)
import Ecluse.Test.Corpus.Advisories (allAdvisoryRules, checkCapturesServed, compileCorpusAdvisories, configuredShippedPolicy)
import Ecluse.Test.Deadline (withExchangeClient)
import Ecluse.Test.EcosystemBench (EcosystemBench (..), LoadedEntry (..), ecosystemBenches)
import Ecluse.Test.OsvDb (withServedArtifact)
import Ecluse.Test.Registry.Metadata.Fetch (captureChunks, fetchTestBody)
import Ecluse.Test.Rules (inertRuleDeps)
import Ecluse.Test.Server.Transform (SelectedDepth (Depth), detailsDepth, serveDocumentSizeUnder)

main :: IO ()
main =
    getArgs >>= \case
        ["captures"] -> captures
        ["live"] -> live
        _ -> die "usage: perf-acceptance (captures | live)"

-- | Hold each leg over the committed captures to its allocation budget, and exit 1 on any problem.
captures :: IO ()
captures = do
    criteria <- loadCriteria
    benches <- ecosystemBenches
    runs <- forM benches $ \bench -> withCorpusAdvisories bench $ \advisories -> do
        records <- readCaptureRecords (ebEcosystem bench) >>= either fail pure
        outcomes <- forM (ebCorpus bench) $ \LoadedEntry{lePackage = package, leBytes = raw} ->
            case crCapturedAt <$> Map.lookup (cpName package) records of
                Nothing -> pure (Failed (cpName package) "bench/corpus/pins.json records no capture time")
                Just clock -> outcomeFrom (cpName package) Nothing <$> fullAllocationContract bench advisories (EvalContext clock Nothing) (cpPackage package) raw
        pure (ebEcosystem bench, outcomes)
    let report = assessCaptures criteria runs
    op <- operatingPoint
    writeObservations (capturesEvidence op report)
    publish (renderCapturesReport op report)
    traverse_ putTextLn (capturesAnnotations report)
    exitWith (capturesExitCode report)

-- | Report each leg over live registry documents, and exit 1 when the proxy refused a document.
live :: IO ()
live = do
    benches <- ecosystemBenches
    withExchangeClient (newManager tlsManagerSettings) $ \client -> do
        now <- getCurrentTime
        runs <- forM benches $ \bench -> withCorpusAdvisories bench $ \advisories ->
            (ebEcosystem bench,) <$> traverse (\LoadedEntry{lePackage = package} -> measureLive client advisories (EvalContext now Nothing) bench package) (ebCorpus bench)
        op <- operatingPoint
        writeObservations (liveEvidence op runs)
        publish (renderLiveReport op runs)
        traverse_ putTextLn (liveAnnotations runs)
        exitWith (liveExitCode runs)

-- Serve the ecosystem's corpus advisories to the advisory legs while the action runs, once every covered capture reaches its rows.
withCorpusAdvisories :: EcosystemBench -> (RuleDeps -> IO a) -> IO a
withCorpusAdvisories bench use =
    withSystemTempDirectory "ecluse-acceptance-advisories" $ \dir -> do
        artifact <- compileCorpusAdvisories eco dir
        withServedArtifact eco artifact $ \deps _ -> do
            checkCapturesServed deps [cpPackage package | LoadedEntry{lePackage = package} <- ebCorpus bench] >>= either (fail . toString) pure
            use deps
  where
    eco = ebEcosystem bench

measureLive :: ExchangeClient -> RuleDeps -> EvalContext -> EcosystemBench -> CorpusPackage -> IO PackageOutcome
measureLive client advisories ctx bench package = do
    t0 <- getMonotonicTime
    fetched <- fetchDocument client bench pkg
    t1 <- getMonotonicTime
    case fetched of
        Unreachable reason -> pure (Unavailable name reason)
        Refused reason -> pure (Failed name reason)
        Fetched raw -> outcomeFrom name (Just ((t1 - t0) * 1000)) <$> fullAllocationContract bench advisories ctx pkg raw
  where
    pkg = cpPackage package
    name = cpName package

fetchDocument :: ExchangeClient -> EcosystemBench -> PackageName -> IO Fetched
fetchDocument client ecosystem pkg = case liveRequest ecosystem pkg of
    Left reason -> pure (Refused reason)
    Right request ->
        classifyFetch
            <$> fetchTestBody client (progressFloor defaultLimits) (MetadataBodyLimit (maxMetadataBytes defaultLimits)) request

liveRequest :: EcosystemBench -> PackageName -> Either Text Request
liveRequest ecosystem = first show . erRequest (metadataRead (ebMetadata ecosystem)) (ebOrigin ecosystem) Nothing

outcomeFrom :: Text -> Maybe Double -> Either Text (Int, [(Leg, Measurement)]) -> PackageOutcome
outcomeFrom name upstreamMs = \case
    Left reason -> Failed name reason
    Right (versions, legs) -> Measured (Sample name versions upstreamMs legs)

-- | Compose all four measured operations over one document with the served corpus advisories.
fullAllocationContract :: EcosystemBench -> RuleDeps -> EvalContext -> PackageName -> ByteString -> IO (Either Text (Int, [(Leg, Measurement)]))
fullAllocationContract bench advisories ctx pkg raw =
    case ebDecode bench pkg raw >>= maybe (Left "the document lists no versions") Right . nonEmpty of
        Left reason -> pure (Left reason)
        Right versions -> do
            target <- Exception.evaluate (mkVersion (ebEcosystem bench) (NE.last versions))
            legs <- forM universe $ \leg -> (leg,) <$> measurePasses (operation target leg) raw
            pure $ case [leg | (leg, Nothing) <- legs] of
                [] -> Right (length versions, [(leg, measurement) | (leg, Just measurement) <- legs])
                failed -> Left ("these legs did not decode or project: " <> unwords (map legKey failed))
  where
    operation target = \case
        FullDocument -> full inertRuleDeps permissiveAgeRules
        SingleVersion -> runSelective bench pkg target
        FullShippedAdvisories -> full advisories configuredShippedPolicy
        FullAllAdvisoryRules -> full advisories allAdvisoryRules
    full deps policy = runFull deps policy ctx bench pkg

-- Each pass reads its own copy, allocated and cut into chunks before the pass, so no pass reuses another's evaluated input.
measurePasses :: ([ByteString] -> IO Bool) -> ByteString -> IO (Maybe Measurement)
measurePasses operation raw = do
    copies <- BS.useAsCStringLen raw (replicateM passCount . BS.packCStringLen)
    passes <- traverse (measurePass operation <=< chunked) copies
    pure (summarise <$> (nonEmpty =<< sequence passes))
  where
    chunked copy = let chunks = captureChunks copy in chunks <$ Exception.evaluate (length chunks)
    summarise measured =
        let bytes = NE.sort (fmap fst measured)
         in Measurement (median bytes) (NE.head bytes) (NE.last bytes) (median (fmap snd measured))

-- The allocation counter counts down, and covers only the calling thread.
measurePass :: ([ByteString] -> IO Bool) -> [ByteString] -> IO (Maybe (Int64, Double))
measurePass operation chunks = do
    before <- getAllocationCounter
    t0 <- getMonotonicTime
    done <- operation chunks
    t1 <- getMonotonicTime
    after <- getAllocationCounter
    pure (if done then Just (before - after, (t1 - t0) * 1000) else Nothing)

runFull :: RuleDeps -> [PrecededRule] -> EvalContext -> EcosystemBench -> PackageName -> [ByteString] -> IO Bool
runFull deps policy ctx bench pkg chunks =
    ebRead bench pkg chunks >>= \case
        Left _ -> pure False
        Right manifest -> do
            size <- serveDocumentSizeUnder deps policy (ebMetadata bench) ctx (Snapshot (manifestDigest manifest) (manifestRaw manifest), manifestInfo manifest)
            Exception.evaluate (size > 0)

runSelective :: EcosystemBench -> PackageName -> Version -> [ByteString] -> IO Bool
runSelective bench pkg version chunks =
    ebSelective bench pkg version chunks >>= \selected -> Exception.evaluate $ case detailsDepth <$> selected of
        Right (Depth depth) -> depth `seq` True
        _ -> False

operatingPoint :: IO OperatingPoint
operatingPoint = do
    posture <- currentRtsPosture
    pure (OperatingPoint passCount (rpCapabilities posture) (rpAllocAreaBytes posture))

publish :: Text -> IO ()
publish rendered = do
    putText rendered
    lookupEnv "GITHUB_STEP_SUMMARY" >>= traverse_ (`appendFileText` rendered)

passCount :: Int
passCount = 5

median :: (Ord a) => NonEmpty a -> a
median xs = fromMaybe (NE.head sorted) (toList sorted !!? (length xs `div` 2))
  where
    sorted = NE.sort xs

writeObservations :: Value -> IO ()
writeObservations value = lookupEnv "ECLUSE_PERFORMANCE_OBSERVATIONS" >>= traverse_ (`writeFileLBS` (encode value <> "\n"))
