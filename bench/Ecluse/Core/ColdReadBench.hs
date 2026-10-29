-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Cold production metadata reads against unchanged captures over loopback HTTP.
Each iteration includes the bounded response read, digest, projection, and result comparison.
-}
module Ecluse.Core.ColdReadBench (withBenchmarks) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Ecluse.Report.BenchLabels (ecosystemPrefix)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (status200, status404)
import Network.Wai (Application, rawPathInfo, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnfIO)
import Test.Tasty.HUnit (assertBool, assertFailure, (@?=))

import Ecluse.Bench.Corpus (LoadedEntry (..), entryName)
import Ecluse.Core.Deadline (newDeadlines)
import Ecluse.Core.Ecosystem (Ecosystem (Cargo), ecosystemName)
import Ecluse.Core.Fault.Http (typedBodyReads)
import Ecluse.Core.Package (infoVersions)
import Ecluse.Core.Registry (FetchFault (FetchBoundExceeded))
import Ecluse.Core.Registry.CachedDocument (CachedDoc, cargoCached, cargoPacked, npmCached, npmPacked, pypiPacked, pypiSimpleCached, rubyGemsCached)
import Ecluse.Core.Registry.Metadata (Manifest (..), MetadataClient (..), MetadataError (MetadataFetch), VersionDoc (..), VersionRead (..))
import Ecluse.Core.Registry.Metadata.Fetch.Types (EcosystemRead (erRequest))
import Ecluse.Core.Registry.Origin (perCallerOrigin)
import Ecluse.Core.Registry.Request (mkBaseRequest)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (BodyTooLarge), Limits (maxMetadataBytes), defaultLimits)
import Ecluse.Core.Security.Egress (mkRegistryUrl)
import Ecluse.Core.Server.Metadata (ecosystemMetadataReads, privateMetadataClient)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Corpus (cpPackage)
import Ecluse.Test.Corpus.Cargo (cargoCaptureDiscoveryBytes)
import Ecluse.Test.Deadline (untracedClient)
import Ecluse.Test.EcosystemBench (EcosystemBench (..))
import Ecluse.Test.Port (noopMetricsPort, passthroughTracingPort)
import Ecluse.Test.Snapshot (digestOf)
import Ecluse.Test.Support (expectRight)

-- | Keep the replay server alive while the caller runs the benchmark tree.
withBenchmarks :: [EcosystemBench] -> ([Benchmark] -> IO a) -> IO a
withBenchmarks ecosystems action = do
    responses <- fmap Map.fromList (traverse replayResponse [(ecosystem, entry) | ecosystem <- ecosystems, entry <- ebCorpus ecosystem])
    testWithApplication (pure (replayApplication (withDiscovery responses))) $ \port -> do
        manager <- HTTP.newManager (typedBodyReads (HTTP.managerSetProxy HTTP.noProxy HTTP.defaultManagerSettings{HTTP.managerModifyRequest = pure . redirect port}))
        groups <- traverse (ecosystemGroup manager) ecosystems
        action groups
  where
    -- Every request stays on loopback, including requests for an unexpected origin.
    redirect port request = request{HTTP.host = "127.0.0.1", HTTP.port = port, HTTP.secure = False, HTTP.proxy = Nothing}
    withDiscovery responses
        | any ((== Cargo) . ebEcosystem) ecosystems = Map.insert "/config.json" (fromStrict cargoCaptureDiscoveryBytes) responses
        | otherwise = responses

replayResponse :: (EcosystemBench, LoadedEntry) -> IO (ByteString, BL.ByteString)
replayResponse (ecosystem, LoadedEntry{lePackage = package, leBytes = raw}) = do
    base <- expectRight (mkRegistryUrl (ebOrigin ecosystem))
    nativeRead <- expectRight (ebReader ecosystem)
    request <- expectRight (erRequest nativeRead (mkBaseRequest base) Nothing (cpPackage package))
    pure (HTTP.path request, BL.fromStrict raw)

replayApplication :: Map ByteString BL.ByteString -> Application
replayApplication responses request respond =
    respond $ case Map.lookup (rawPathInfo request) responses of
        Just body -> responseLBS status200 [] body
        Nothing -> responseLBS status404 [] "capture not found"

ecosystemGroup :: HTTP.Manager -> EcosystemBench -> IO Benchmark
ecosystemGroup manager ecosystem = do
    entries <- traverse (entryGroup manager ecosystem) (ebCorpus ecosystem)
    pure (bgroup (toString (ecosystemPrefix <> ecosystemName (ebEcosystem ecosystem))) [bgroup "cold production reads (per package)" entries])

entryGroup :: HTTP.Manager -> EcosystemBench -> LoadedEntry -> IO Benchmark
entryGroup manager ecosystem entry@LoadedEntry{leBytes = raw} = do
    defaults <- readGroup manager ecosystem entry "default cap" defaultLimits
    raised <-
        if BS.length raw > maxMetadataBytes defaultLimits
            then (: []) <$> readGroup manager ecosystem entry "capture-sized cap" defaultLimits{maxMetadataBytes = BS.length raw}
            else pure []
    pure (bgroup (entryName entry) (defaults : raised))

readGroup :: HTTP.Manager -> EcosystemBench -> LoadedEntry -> String -> Limits -> IO Benchmark
readGroup manager ecosystem LoadedEntry{lePackage = package, leBytes = raw, leInfo = info} label limits = do
    client <- metadataClient manager ecosystem limits
    (key, _) <- maybe (assertFailure "cold reads: empty capture projection") pure (Map.lookupMax (infoVersions info))
    let name = cpPackage package
        version = mkVersion (ebEcosystem ecosystem) key
        full = fetchFullManifest client name
        selected = fetchVersionMetadata client name version
        cap = maxMetadataBytes limits
        groupName outcome = label <> " " <> show cap <> " bytes: " <> outcome
    if BS.length raw > cap
        then do
            let expected = Left (MetadataFetch (FetchBoundExceeded (BodyTooLarge (MetadataBodyLimit cap))))
                fullRefusal = void <$> full
                selectedRefusal = void <$> selected
            fullRefusal >>= (@?= expected)
            selectedRefusal >>= (@?= expected)
            pure $ bgroup (groupName "body-limit refusal") [checkedBench "full document" expected fullRefusal, checkedBench "selected version" expected selectedRefusal]
        else do
            manifest <- full >>= expectRight
            selectedRead <- selected >>= expectRight
            manifestBodyBytes manifest @?= BS.length raw
            manifestDigest manifest @?= digestOf raw
            vrBodyBytes selectedRead @?= BS.length raw
            assertBool "cold reads: full projection contains versions" (not (Map.null (infoVersions (manifestInfo manifest))))
            assertBool "cold reads: selected version exists" (isJust (vrVersion selectedRead))
            fmap vdDetails (vrVersion selectedRead) @?= Map.lookup key (infoVersions (manifestInfo manifest))
            pure $
                bgroup
                    (groupName "success")
                    [ checkedBench "full document" (Right True) (fmap (sameManifest manifest) <$> full)
                    , checkedBench "selected version" (Right True) (fmap (sameSelected selectedRead) <$> selected)
                    ]

checkedBench :: (Eq result) => String -> result -> IO result -> Benchmark
checkedBench label expected action = bench label (whnfIO (action >>= assertBool "cold read changed its preflight result" . (== expected)))

sameManifest :: Manifest -> Manifest -> Bool
sameManifest expected actual =
    manifestInfo expected == manifestInfo actual
        && sameDocument (manifestRaw expected) (manifestRaw actual)
        && manifestBodyBytes expected == manifestBodyBytes actual
        && manifestDigest expected == manifestDigest actual

sameSelected :: VersionRead -> VersionRead -> Bool
sameSelected expected actual =
    sameVersion (vrVersion expected) (vrVersion actual)
        && vrBodyBytes expected == vrBodyBytes actual
        && vrUpstreamLatest expected == vrUpstreamLatest actual
  where
    sameVersion Nothing Nothing = True
    sameVersion (Just left) (Just right) = vdDetails left == vdDetails right && sameRaw (vdRaw left) (vdRaw right)
    sameVersion _ _ = False
    sameRaw Nothing Nothing = True
    sameRaw (Just left) (Just right) = sameDocument left right
    sameRaw _ _ = False

-- Compare held payloads without forcing cache estimates or decoding packed releases and files during timing.
sameDocument :: CachedDoc -> CachedDoc -> Bool
sameDocument left right
    | Just expected <- snd cargoPacked left, Just actual <- snd cargoPacked right = expected == actual
    | Just expected <- snd cargoCached left, Just actual <- snd cargoCached right = expected == actual
    | Just expected <- snd rubyGemsCached left, Just actual <- snd rubyGemsCached right = expected == actual
    | otherwise = case (snd npmPacked left, snd npmPacked right, snd pypiPacked left, snd pypiPacked right) of
        (Just expected, Just actual, _, _) -> expected == actual
        (_, _, Just expected, Just actual) -> expected == actual
        _ -> case (snd npmCached left, snd npmCached right, snd pypiSimpleCached left, snd pypiSimpleCached right) of
            (Just expected, Just actual, _, _) -> expected == actual
            (_, _, Just expected, Just actual) -> expected == actual
            _ -> False

metadataClient :: HTTP.Manager -> EcosystemBench -> Limits -> IO MetadataClient
metadataClient manager ecosystem limits = do
    base <- expectRight (mkRegistryUrl (ebOrigin ecosystem))
    -- The bench's stub always answers, so nothing sweeps these deadlines.
    client <- untracedClient manager <$> newDeadlines
    pure (privateMetadataClient (ecosystemMetadataReads (ebReadFactory ecosystem) passthroughTracingPort noopMetricsPort (\_ _ -> pass) (\_ _ -> pass) (const pass) (perCallerOrigin limits client (mkBaseRequest base) Nothing)))
