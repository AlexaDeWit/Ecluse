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
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (status200, status404)
import Network.Wai (Application, rawPathInfo, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnfIO)
import Test.Tasty.HUnit (assertBool, assertFailure, (@?=))

import Ecluse.Bench.Corpus (LoadedEntry, entryName)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems), ecosystemName)
import Ecluse.Core.Package (infoVersions)
import Ecluse.Core.Registry (FetchFault (FetchBoundExceeded))
import Ecluse.Core.Registry.CachedDocument (CachedDoc, npmCached, pypiSimpleCached)
import Ecluse.Core.Registry.Metadata (Manifest (..), MetadataClient (..), MetadataError (MetadataFetch), VersionDoc (..), VersionRead (..))
import Ecluse.Core.Registry.Npm.Metadata (newNpmMetadataReads)
import Ecluse.Core.Registry.Npm.Request (MetadataForm (Full), metadataRequest)
import Ecluse.Core.Registry.Origin (perCallerOrigin)
import Ecluse.Core.Registry.PyPI.Metadata (newPyPIMetadataReads)
import Ecluse.Core.Registry.PyPI.Request (simpleIndexRequest)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (BodyTooLarge), Limits (maxMetadataBytes), defaultLimits)
import Ecluse.Core.Security.Egress (mkRegistryUrl)
import Ecluse.Core.Server.Metadata (privateMetadataClient)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Corpus (cpPackage)
import Ecluse.Test.EcosystemBench (EcosystemBench (..))
import Ecluse.Test.Port (noopMetricsPort, passthroughTracingPort)
import Ecluse.Test.Snapshot (digestOf)
import Ecluse.Test.Support (expectRight)

-- | Keep the replay server alive while the caller runs the benchmark tree.
withBenchmarks :: [EcosystemBench] -> ([Benchmark] -> IO a) -> IO a
withBenchmarks ecosystems action = do
    responses <- fmap Map.fromList (traverse replayResponse [(ebEcosystem ecosystem, entry) | ecosystem <- ecosystems, entry <- ebCorpus ecosystem])
    testWithApplication (pure (replayApplication responses)) $ \port -> do
        manager <- HTTP.newManager (HTTP.managerSetProxy HTTP.noProxy HTTP.defaultManagerSettings{HTTP.managerModifyRequest = pure . redirect port})
        groups <- traverse (ecosystemGroup manager) ecosystems
        action groups
  where
    -- Every request stays on loopback, including requests for an unexpected origin.
    redirect port request = request{HTTP.host = "127.0.0.1", HTTP.port = port, HTTP.secure = False, HTTP.proxy = Nothing}

replayResponse :: (Ecosystem, LoadedEntry) -> IO (ByteString, BL.ByteString)
replayResponse (ecosystem, (package, raw, _, _)) = do
    request <- case ecosystem of
        Npm -> expectRight (metadataRequest (originUrl ecosystem) Nothing Full (cpPackage package))
        PyPI -> expectRight (simpleIndexRequest (originUrl ecosystem) Nothing (cpPackage package))
        RubyGems -> assertFailure "cold reads: RubyGems has no metadata reader"
    pure (HTTP.path request, BL.fromStrict raw)

replayApplication :: Map ByteString BL.ByteString -> Application
replayApplication responses request respond =
    respond $ case Map.lookup (rawPathInfo request) responses of
        Just body -> responseLBS status200 [] body
        Nothing -> responseLBS status404 [] "capture not found"

ecosystemGroup :: HTTP.Manager -> EcosystemBench -> IO Benchmark
ecosystemGroup manager ecosystem = do
    entries <- traverse (entryGroup manager (ebEcosystem ecosystem)) (ebCorpus ecosystem)
    pure (bgroup ("ecosystem: " <> toString (ecosystemName (ebEcosystem ecosystem))) [bgroup "cold production reads (per package)" entries])

entryGroup :: HTTP.Manager -> Ecosystem -> LoadedEntry -> IO Benchmark
entryGroup manager ecosystem entry@(_, raw, _, _) = do
    defaults <- readGroup manager ecosystem entry "default cap" defaultLimits
    raised <-
        if BS.length raw > maxMetadataBytes defaultLimits
            then (: []) <$> readGroup manager ecosystem entry "capture-sized cap" defaultLimits{maxMetadataBytes = BS.length raw}
            else pure []
    pure (bgroup (entryName entry) (defaults : raised))

readGroup :: HTTP.Manager -> Ecosystem -> LoadedEntry -> String -> Limits -> IO Benchmark
readGroup manager ecosystem (package, raw, info, _) label limits = do
    client <- metadataClient manager ecosystem limits
    (key, _) <- maybe (assertFailure "cold reads: empty capture projection") pure (Map.lookupMax (infoVersions info))
    let name = cpPackage package
        version = mkVersion ecosystem key
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

-- Comparing the payload avoids forcing the lazy cache-accounting estimate on an uncached read.
sameDocument :: CachedDoc -> CachedDoc -> Bool
sameDocument left right = snd npmCached left == snd npmCached right && snd pypiSimpleCached left == snd pypiSimpleCached right

metadataClient :: HTTP.Manager -> Ecosystem -> Limits -> IO MetadataClient
metadataClient manager ecosystem limits = do
    base <- expectRight (mkRegistryUrl (originUrl ecosystem))
    makeReads <- case ecosystem of
        Npm -> pure newNpmMetadataReads
        PyPI -> pure newPyPIMetadataReads
        RubyGems -> assertFailure "cold reads: RubyGems has no metadata reader"
    pure (privateMetadataClient (makeReads passthroughTracingPort noopMetricsPort (\_ _ -> pass) (\_ _ -> pass) (const pass) (perCallerOrigin limits manager base Nothing)))

originUrl :: Ecosystem -> Text
originUrl = \case
    Npm -> "https://registry.npmjs.org"
    PyPI -> "https://pypi.org"
    RubyGems -> "https://rubygems.org"
