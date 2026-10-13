-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Shared listing setup preserves native capture scope, preflights, and cache bounds.
module Ecluse.BenchLoad.Listing (
    ListingSetup (..),
    listingLoad,
    revalidateLoad,
    cacheFitsLoad,
    cacheEvictsLoad,
    listingWorkingSet,
    withListingStubs,
) where

import Network.Wai (Application)

import Ecluse.BenchLoad.Error (benchFail)
import Ecluse.BenchLoad.Fixture (httpTarget, loadCorpusBodies, longCacheTtl, primeETag, weightedMix, withProxyOverStubs)
import Ecluse.BenchLoad.Harness (Driver (DriveHttp), Load (Load), LoadKnobs (..), Target, proxied)
import Ecluse.BenchLoad.PrivateCopy (PrivateCopy (..))
import Ecluse.BenchLoad.ProxyProcess (ProxyProcess)
import Ecluse.Test.Corpus (CorpusPackage (cpWeight))

-- | Native policies run before the unchanged measurement callback.
data ListingSetup = ListingSetup
    { lsCopy :: PrivateCopy
    , lsPrivate :: LoadKnobs -> Application
    , lsCapturePackages :: [CorpusPackage] -> [CorpusPackage]
    -- ^ npm loads the entire corpus even when the request mix selects fewer packages.
    , lsPreflight :: [Text] -> IO ()
    , lsEvictionEntries :: LoadKnobs -> Int -> Either Text Int
    , lsRevalidateError :: Text
    }

-- | The weighted corpus retains the fixture's listing preflight.
listingLoad :: ListingSetup -> Int -> LoadKnobs -> (Target -> IO a) -> IO a
listingLoad setup ttl knobs k =
    withListings setup knobs ttl Nothing (pcPackages (lsCopy setup)) cpWeight (httpTarget k)

-- | Prime and validate the first listing before driving conditional requests.
revalidateLoad :: ListingSetup -> LoadKnobs -> (Target -> IO a) -> IO a
revalidateLoad setup knobs k =
    withListings setup knobs longCacheTtl Nothing (take 1 (listingWorkingSet setup knobs)) (const 1) $ \proxy -> \case
        url : _ -> do
            etag <- primeETag url
            k (proxied proxy (DriveHttp (Load [("If-None-Match", etag)] [url])))
        [] -> benchFail (lsRevalidateError setup)

-- | Give each selected project one assembled-response slot.
cacheFitsLoad :: ListingSetup -> LoadKnobs -> (Target -> IO a) -> IO a
cacheFitsLoad setup knobs k =
    let packages = listingWorkingSet setup knobs
     in withListings setup knobs longCacheTtl (Just (length packages)) packages (const 1) (httpTarget k)

-- | Apply the fixture's eviction bound before loading captures or starting a proxy.
cacheEvictsLoad :: ListingSetup -> LoadKnobs -> (Target -> IO a) -> IO a
cacheEvictsLoad setup knobs k = do
    let packages = listingWorkingSet setup knobs
    entries <- either benchFail pure (lsEvictionEntries setup knobs (length packages))
    withListings setup knobs longCacheTtl (Just entries) packages (const 1) (httpTarget k)

-- | A nonpositive requested size still selects the first captured project.
listingWorkingSet :: ListingSetup -> LoadKnobs -> [CorpusPackage]
listingWorkingSet setup knobs = take (max 1 (lkWorkingSet knobs)) (pcPackages (lsCopy setup))

withListings :: ListingSetup -> LoadKnobs -> Int -> Maybe Int -> [CorpusPackage] -> (CorpusPackage -> Int) -> (ProxyProcess -> [Text] -> IO a) -> IO a
withListings setup knobs ttl entries packages weight body =
    withListingStubs setup knobs ttl entries packages (weightedMix weight (pcUrl (lsCopy setup)) packages) $ \proxy urls -> do
        lsPreflight setup urls
        body proxy urls

-- | Unprimed callers retain their own URL mix and add no listing preflight.
withListingStubs :: ListingSetup -> LoadKnobs -> Int -> Maybe Int -> [CorpusPackage] -> (Int -> [Text]) -> (ProxyProcess -> [Text] -> IO a) -> IO a
withListingStubs setup knobs ttl entries packages mix body = do
    captures <- loadCorpusBodies (lsCapturePackages setup packages)
    public <- pcStub copy (lkUpstreamLatencyMicros knobs) captures
    withProxyOverStubs (pcEcosystem copy) knobs ttl entries (lsPrivate setup knobs) public mix body
  where
    copy = lsCopy setup
