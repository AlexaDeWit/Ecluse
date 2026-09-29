-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Listings whose private upstream holds a copy of the public versions, as a mirror target that
is also the private upstream does. The copy holds the newest share of each capture's versions by
publish time, or the whole capture, and stays fixed for the run. Every listing decodes its own
private copy, because a private read passes the caller's credentials through.
-}
module Ecluse.BenchLoad.PrivateCopy (
    PrivateCopy (..),
    privateCopyScenarios,
    shareStubs,
) where

import Data.Aeson (Value)
import Data.Ratio ((%))
import Network.Wai (Application)

import Ecluse.BenchLoad.Fixture (httpTarget, loadCorpusBodies, loadCorpusCuts, withProxyOverStubs)
import Ecluse.BenchLoad.Harness (LoadKnobs (lkUpstreamLatencyMicros), Scenario, Target, scenario)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Package (PackageName)
import Ecluse.Test.Corpus (CorpusPackage)

-- | One ecosystem's parts of the private-copy scenarios.
data PrivateCopy = PrivateCopy
    { pcEcosystem :: Ecosystem
    , pcListing :: Text
    -- ^ The requests the scenarios send, as their descriptions name them.
    , pcRegistry :: Text
    -- ^ The kind of private registry that holds the whole capture, as the descriptions name it.
    , pcPackages :: [CorpusPackage]
    , pcCut :: Rational -> PackageName -> ByteString -> Either String Value
    -- ^ The newest share of a capture's versions, from "Ecluse.Test.Corpus.Subset".
    , pcStub :: Int -> Map Text LByteString -> IO Application
    -- ^ An upstream serving these bodies after this latency in microseconds.
    , pcMix :: Int -> [Text]
    -- ^ The weighted listing URLs for the proxy's port.
    , pcPreflight :: [Text] -> IO ()
    -- ^ Checks the listings before load, failing the scenario on a wrong response.
    }

-- | The private copies from the smallest share to the whole capture.
privateCopyScenarios :: PrivateCopy -> [Scenario]
privateCopyScenarios copy = map (shareScenario copy) [5, 25] <> [wholeScenario copy]

wholeScenario :: PrivateCopy -> Scenario
wholeScenario copy =
    scenario
        "heavy-private"
        (pcListing copy <> " with public cache TTL 0, while the private upstream returns the complete public capture, as " <> pcRegistry copy <> " does. " <> ownCopy)
        (withStubs copy (copyStubs copy loadCorpusBodies))

shareScenario :: PrivateCopy -> Integer -> Scenario
shareScenario copy percent =
    scenario
        ("heavy-private-" <> show percent <> "pct")
        (pcListing copy <> " with public cache TTL 0, while the private upstream returns the newest " <> show percent <> "% of each capture's versions by publish time, rounded up to a whole version, as a mirror target that has mirrored those versions does. The private copy stays fixed for the run. " <> ownCopy)
        (withStubs copy (shareStubs copy (percent % 100)))

ownCopy :: Text
ownCopy = "Each request decodes its own private copy, which single-flight cannot share across callers."

-- | The private upstream over the newest share of each capture, and the public one over each capture whole.
shareStubs :: PrivateCopy -> Rational -> Int -> IO (Application, Application)
shareStubs copy share = copyStubs copy (loadCorpusCuts (pcCut copy share))

copyStubs :: PrivateCopy -> ([CorpusPackage] -> IO (Map Text LByteString)) -> Int -> IO (Application, Application)
copyStubs copy loadPrivate latency = do
    public <- pcStub copy latency =<< loadCorpusBodies (pcPackages copy)
    private <- pcStub copy latency =<< loadPrivate (pcPackages copy)
    pure (private, public)

withStubs :: PrivateCopy -> (Int -> IO (Application, Application)) -> LoadKnobs -> (Target -> IO a) -> IO a
withStubs copy stubs knobs k = do
    (private, public) <- stubs (lkUpstreamLatencyMicros knobs)
    withProxyOverStubs (pcEcosystem copy) knobs 0 Nothing private public (pcMix copy) $ \proxy urls -> do
        pcPreflight copy urls
        httpTarget k proxy urls
