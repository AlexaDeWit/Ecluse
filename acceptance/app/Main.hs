-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Live performance acceptance using the registered benchmark catalogue.
The shared catalogue selects packages and adapters, while measurements use freshly fetched bytes.
-}
module Main (main) where

import Control.Exception qualified as Exception
import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime, getCurrentTime)
import GHC.Clock (getMonotonicTime)
import Network.HTTP.Client (Manager, Request, newManager, responseTimeout, responseTimeoutMicro)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Ecluse.Acceptance (CriteriaCatalogue (catalogueCriteria), OperatingPoint (OperatingPoint), Sample (..), evaluate, loadCriteria, renderReport, reportExitCode)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems), ecosystemName)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (RegistryResponse (..), isSuccessStatus)
import Ecluse.Core.Registry.Exchange (boundedFetch)
import Ecluse.Core.Registry.Npm.Request qualified as Npm
import Ecluse.Core.Registry.PyPI.Request qualified as PyPI
import Ecluse.Core.Registry.Request (noValidators)
import Ecluse.Core.Rules.Types (EvalContext (EvalContext))
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Snapshot (ContentDigest, Snapshot (Snapshot), digestOf)
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Test.Corpus (CorpusPackage (cpPackage), cpName)
import Ecluse.Test.EcosystemBench (EcosystemBench (..), ecosystemBenches)
import Ecluse.Test.Server.Transform (SelectedDepth (Depth), detailsDepth, serveDocumentSize)

-- | Report both ecosystems and forward their combined verdict as the process exit status.
main :: IO ()
main = do
    criteria <- loadCriteria
    benches <- ecosystemBenches
    configured <- forM benches $ \bench ->
        case Map.lookup (ebEcosystem bench) (catalogueCriteria criteria) of
            Nothing -> fail ("missing acceptance criteria for " <> toString (ecosystemName (ebEcosystem bench)))
            Just budgets -> pure (bench, budgets)
    manager <- newManager tlsManagerSettings
    now <- getCurrentTime
    reports <- forM configured $ \(bench, budgets) -> do
        inputs <- traverse (measurePackage manager now bench . corpusPackage) (ebCorpus bench)
        pure (evaluate (ebEcosystem bench) budgets inputs)
    let count = sum (map (length . ebCorpus) benches)
        rendered = renderReport (OperatingPoint sampleCount count) reports
    putText rendered
    lookupEnv "GITHUB_STEP_SUMMARY" >>= traverse_ (`appendFileText` rendered)
    exitWith (reportExitCode reports)
  where
    corpusPackage (package, _, _, _) = package

measurePackage :: Manager -> UTCTime -> EcosystemBench -> CorpusPackage -> IO (Either (Text, Text) Sample)
measurePackage manager now bench package = do
    t0 <- getMonotonicTime
    fetched <- fetchDocument manager (ebEcosystem bench) pkg
    t1 <- getMonotonicTime
    case fetched of
        Left reason -> pure (Left (name, reason))
        Right raw -> case ebDecode bench pkg raw >>= maybe (Left "document exposed no versions") Right . nonEmpty of
            Left reason -> pure (Left (name, reason))
            Right versions -> do
                digest <- Exception.evaluate (digestOf raw)
                target <- Exception.evaluate (mkVersion (ebEcosystem bench) (NE.last versions))
                versionCount <- Exception.evaluate (length versions)
                full <- measurePasses (runFull now bench pkg digest) raw
                single <- measurePasses (runSelective bench pkg target) raw
                pure $ case (full, single) of
                    (Just fullSecs, Just singleSecs) ->
                        Right
                            Sample
                                { sampleName = name
                                , sampleVersions = versionCount
                                , sampleUpstreamMs = (t1 - t0) * 1000
                                , sampleFullOverheadMs = fullSecs * 1000
                                , sampleSingleVersionOverheadMs = singleSecs * 1000
                                }
                    _ -> Left (name, "document did not decode or project")
  where
    pkg = cpPackage package
    name = cpName package

fetchDocument :: Manager -> Ecosystem -> PackageName -> IO (Either Text ByteString)
fetchDocument manager eco pkg = case liveRequest eco pkg of
    Left reason -> pure (Left reason)
    Right request -> do
        result <- boundedFetch manager defaultLimits request{responseTimeout = responseTimeoutMicro (30 * 1000 * 1000)}
        pure $ case result of
            Left fault -> Left (show fault)
            Right response
                | isSuccessStatus (responseStatusCode response) -> Right (responseBody response)
                | otherwise -> Left ("registry HTTP " <> show (responseStatusCode response))

liveRequest :: Ecosystem -> PackageName -> Either Text Request
liveRequest eco pkg = case eco of
    Npm -> first show (Npm.metadataRequest "https://registry.npmjs.org" Nothing Npm.Full noValidators pkg)
    PyPI -> first show (PyPI.simpleIndexRequest "https://pypi.org" Nothing noValidators pkg)
    RubyGems -> Left "no registered performance adapter for rubygems"

-- Allocate each copy in IO before timing. Evaluating one pure copy thunk would share it across passes.
measurePasses :: (ByteString -> IO Bool) -> ByteString -> IO (Maybe Double)
measurePasses operation raw = do
    copies <- BS.useAsCStringLen raw (replicateM sampleCount . BS.packCStringLen)
    passes <- forM copies $ \copy -> do
        t0 <- getMonotonicTime
        done <- operation copy
        t1 <- getMonotonicTime
        pure (if done then Just (t1 - t0) else Nothing)
    pure (median <$> sequence passes)

runFull :: UTCTime -> EcosystemBench -> PackageName -> ContentDigest -> ByteString -> IO Bool
runFull now bench pkg digest raw = case ebProject bench pkg raw of
    Left _ -> pure False
    Right (info, document) -> do
        size <- serveDocumentSize (ebMetadata bench) (EvalContext now Nothing) (Snapshot digest document, info)
        Exception.evaluate (size > 0)

runSelective :: EcosystemBench -> PackageName -> Version -> ByteString -> IO Bool
runSelective bench pkg version raw = Exception.evaluate $
    case detailsDepth <$> ebSelective bench pkg version raw of
        Right (Depth depth) -> depth `seq` True
        _ -> False

sampleCount :: Int
sampleCount = 5

median :: [Double] -> Double
median xs = fromMaybe 0 (sort xs !!? (length xs `div` 2))
