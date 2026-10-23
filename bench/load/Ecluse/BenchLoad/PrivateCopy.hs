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
    herdUnderWholeCopy,
    CopyStubs (..),
    shareStubs,
) where

import Data.Aeson (Value)
import Data.Ratio ((%))
import Network.Wai (Application)

import Ecluse.BenchLoad.Fixture (httpTarget, loadCorpusBodies, loadCorpusCuts, weightedMix, withProxyOverStubs)
import Ecluse.BenchLoad.Harness (Driver (DriveBurstUnder), LoadKnobs (lkUpstreamLatencyMicros), Scenario (scenarioServiceTime), burstReadingMs, proxied, scenario, urlLoad)
import Ecluse.BenchLoad.ProxyProcess (ProxyProcess, proxyPort)
import Ecluse.BenchLoad.Stress (herdSize)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Package (PackageName)
import Ecluse.Test.Corpus (CorpusPackage (cpWeight))

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
    , pcUrl :: Int -> Text -> Text
    -- ^ A package's listing URL on the proxy's port.
    , pcPreflight :: [Text] -> IO ()
    -- ^ Checks the listings before load, failing the scenario on a wrong response.
    , pcHerdPackage :: Text
    -- ^ The package whose listing the ecosystem's herd requests.
    }

-- | The private copies from the smallest share to the whole capture.
privateCopyScenarios :: PrivateCopy -> [Scenario]
privateCopyScenarios copy = map (shareScenario copy) [5, 25] <> [wholeScenario copy]

wholeScenario :: PrivateCopy -> Scenario
wholeScenario copy =
    scenario
        "heavy-private"
        (wholeListings copy <> " " <> ownCopy)
        (\knobs k -> withStubs copy (wholeStubs copy) knobs (httpTarget k))

{- | The whole copy's listings with the ecosystem's herd sent halfway through the window, so the memory
the herd's large responses hold outside the heap adds to a heap the private decodes keep high.
-}
herdUnderWholeCopy :: PrivateCopy -> Scenario
herdUnderWholeCopy copy =
    ( scenario
        "herd-under-heavy-private"
        (wholeListings copy <> " Halfway through the window, " <> show herdSize <> " more connections each send one request for the " <> pcHerdPackage copy <> " listing, all at once. The harness reads the proxy cgroup's memory every " <> show burstReadingMs <> " ms until that burst ends.")
        ( \knobs k ->
            withStubs copy (wholeStubs copy) knobs $ \proxy urls ->
                k (proxied proxy (DriveBurstUnder herdSize (pcUrl copy (proxyPort proxy) (pcHerdPackage copy)) (urlLoad urls)))
        )
    )
        { scenarioServiceTime = False
        }

shareScenario :: PrivateCopy -> Integer -> Scenario
shareScenario copy percent =
    scenario
        ("heavy-private-" <> show percent <> "pct")
        (pcListing copy <> " with public cache TTL 0, while the private upstream returns the newest " <> show percent <> "% of each capture's versions by publish time, rounded up to a whole version, as a mirror target that has mirrored those versions does. The private copy stays fixed for the run. " <> ownCopy)
        (\knobs k -> withStubs copy (shareStubs copy (percent % 100)) knobs (httpTarget k))

wholeListings :: PrivateCopy -> Text
wholeListings copy = pcListing copy <> " with public cache TTL 0, while the private upstream returns the complete public capture, as " <> pcRegistry copy <> " does."

ownCopy :: Text
ownCopy = "Each request decodes its own private copy, which single-flight cannot share across callers."

-- | The two upstreams of a private-copy scenario.
data CopyStubs = CopyStubs
    { csPrivate :: Application
    , csPublic :: Application
    }

-- | The private upstream over the newest share of each capture, and the public one over each capture whole.
shareStubs :: PrivateCopy -> Rational -> Int -> IO CopyStubs
shareStubs copy share = copyStubs copy (loadCorpusCuts (pcCut copy share))

copyStubs :: PrivateCopy -> ([CorpusPackage] -> IO (Map Text LByteString)) -> Int -> IO CopyStubs
copyStubs copy loadPrivate latency = do
    public <- pcStub copy latency =<< loadCorpusBodies (pcPackages copy)
    private <- pcStub copy latency =<< loadPrivate (pcPackages copy)
    pure CopyStubs{csPrivate = private, csPublic = public}

-- The proxy over the stubs, with the weighted listings preflighted before the body drives them.
withStubs :: PrivateCopy -> (Int -> IO CopyStubs) -> LoadKnobs -> (ProxyProcess -> [Text] -> IO a) -> IO a
withStubs copy makeStubs knobs body = do
    stubs <- makeStubs (lkUpstreamLatencyMicros knobs)
    withProxyOverStubs (pcEcosystem copy) knobs 0 Nothing (csPrivate stubs) (csPublic stubs) (weightedMix cpWeight (pcUrl copy) (pcPackages copy)) $ \proxy urls -> do
        pcPreflight copy urls
        body proxy urls

wholeStubs :: PrivateCopy -> Int -> IO CopyStubs
wholeStubs copy = copyStubs copy loadCorpusBodies
