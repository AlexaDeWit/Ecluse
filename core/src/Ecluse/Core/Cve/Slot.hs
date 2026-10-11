-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Advisory generations stay pinned for each lookup. Retirement belongs to the last
reader, or to the swapper when no readers remain, even if the swapper is cancelled.
-}
module Ecluse.Core.Cve.Slot (
    CveSlot,
    newCveSlot,
    withSlotGeneration,
    currentAdvisoryEtag,
    AdvisorySource (..),
    currentAdvisorySource,
    observeAdvisoryPublication,
    generationInstalledAt,
    swapIn,
) where

import Data.Time (UTCTime)
import GHC.Clock (getMonotonicTime)
import UnliftIO.Exception (bracket, mask_, uninterruptibleMask_)

import Ecluse.Core.Cve (CveDb (..), CveLookup)
import Ecluse.Core.Cve.Types (DbEtag)
import Ecluse.Core.Osv.Provenance (AdvisoryProvenance)

data Generation = Generation
    { genDb :: CveDb
    , genEtag :: DbEtag
    , genSource :: AdvisorySource
    , genRetired :: TVar Bool
    , genClosed :: TMVar ()
    , genReaders :: TVar Int
    , genInstalledAt :: Double
    }

-- | The store's publication metadata. Republishing identical bytes can advance the timestamp.
data AdvisorySource = AdvisorySource
    { asProvenance :: AdvisoryProvenance
    , asPushedAt :: Maybe UTCTime
    -- ^ The published object's own timestamp, 'Nothing' when the store reported none.
    }
    deriving stock (Eq, Show)

-- | The slot: the currently-active generation, or nothing before the first sync.
newtype CveSlot = CveSlot {slotCell :: TVar (Maybe Generation)}

-- | A fresh, empty slot: readers see 'Nothing' until the first 'swapIn'.
newCveSlot :: IO CveSlot
newCveSlot = CveSlot <$> newTVarIO Nothing

-- | Borrow a generation's lookup, ETag and publication metadata until the action returns or is cancelled.
withSlotGeneration :: CveSlot -> (Maybe (DbEtag, CveLookup, AdvisorySource) -> IO a) -> IO a
withSlotGeneration slot use = bracket acquire release (use . fmap (\g -> (genEtag g, cveDbLookup (genDb g), genSource g)))
  where
    acquire = atomically $ do
        mGen <- readTVar (slotCell slot)
        for_ mGen (\g -> modifyTVar' (genReaders g) (+ 1))
        pure mGen
    release = traverse_ $ \g -> do
        shouldClose <- atomically $ do
            modifyTVar' (genReaders g) (subtract 1)
            remaining <- readTVar (genReaders g)
            retired <- readTVar (genRetired g)
            pure (retired && remaining == 0)
        when shouldClose (closeGeneration g)

-- | Read the active ETag without pinning, or 'Nothing' before the first sync.
currentAdvisoryEtag :: CveSlot -> IO (Maybe DbEtag)
currentAdvisoryEtag slot = fmap genEtag <$> readTVarIO (slotCell slot)

-- | Read the active publication metadata, retained across failed polls, or 'Nothing' before the first sync.
currentAdvisorySource :: CveSlot -> IO (Maybe AdvisorySource)
currentAdvisorySource slot = fmap genSource <$> readTVarIO (slotCell slot)

-- | Advance publication time only for the installed ETag, without replacing or retiring its database.
observeAdvisoryPublication :: CveSlot -> DbEtag -> Maybe UTCTime -> IO ()
observeAdvisoryPublication slot etag pushedAt = atomically $ do
    current <- readTVar (slotCell slot)
    for_ current $ \g ->
        when (genEtag g == etag && pushedAt > asPushedAt (genSource g)) $
            writeTVar (slotCell slot) (Just g{genSource = (genSource g){asPushedAt = pushedAt}})

-- | Read when 'swapIn' installed the active generation, separately from its publication time.
generationInstalledAt :: CveSlot -> IO (Maybe Double)
generationInstalledAt slot = fmap genInstalledAt <$> readTVarIO (slotCell slot)

-- | Take ownership of the verified database, install it, then close the displaced database after its readers drain.
swapIn :: CveSlot -> DbEtag -> Maybe UTCTime -> CveDb -> IO ()
swapIn slot etag pushedAt newDb = mask_ $ do
    readers <- newTVarIO (0 :: Int)
    retired <- newTVarIO False
    closed <- newEmptyTMVarIO
    installedAt <- getMonotonicTime
    let source = AdvisorySource{asProvenance = cveDbProvenance newDb, asPushedAt = pushedAt}
    displaced <- atomically $ do
        old <- readTVar (slotCell slot)
        writeTVar (slotCell slot) (Just (Generation newDb etag source retired closed readers installedAt))
        forM old $ \g -> do
            writeTVar (genRetired g) True
            remaining <- readTVar (genReaders g)
            pure (g, remaining == 0)
    for_ displaced $ \(g, shouldClose) -> do
        when shouldClose (closeGeneration g)
        atomically (readTMVar (genClosed g))

-- The close contract never throws. Protect only close and completion, never the reader drain.
closeGeneration :: Generation -> IO ()
closeGeneration g = uninterruptibleMask_ $ do
    cveDbClose (genDb g)
    atomically (putTMVar (genClosed g) ())
