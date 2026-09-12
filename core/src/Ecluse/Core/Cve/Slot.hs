-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Advisory generations stay pinned for each lookup. Retirement belongs to the last
reader, or to the swapper when no readers remain, even if the swapper is cancelled.
-}
module Ecluse.Core.Cve.Slot (
    CveSlot,
    newCveSlot,
    withSlotLookup,
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

import Ecluse.Core.Cve (CveDb (..), CveLookup, DbEtag)
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

{- | Where the serving artifact came from. The publication time is the store's, not the
artifact's, so republishing the same bytes can advance it.
-}
data AdvisorySource = AdvisorySource
    { asProvenance :: AdvisoryProvenance
    , asPushedAt :: Maybe UTCTime
    -- ^ The published object's own timestamp, 'Nothing' when the store reported none.
    }
    deriving stock (Eq, Show)

{- | The slot: the currently-active generation, or nothing before the first sync, beside the
monotonic time the slot itself was created.
-}
data CveSlot = CveSlot
    { slotCell :: TVar (Maybe Generation)
    , slotCreatedAt :: Double
    }

-- | A fresh, empty slot: readers see 'Nothing' until the first 'swapIn'.
newCveSlot :: IO CveSlot
newCveSlot = CveSlot <$> newTVarIO Nothing <*> getMonotonicTime

{- | Borrow the current generation's lookup for the duration of one action. The bracket
pins the generation, so a concurrent 'swapIn' cannot close it mid-read.
-}
withSlotLookup :: CveSlot -> (Maybe CveLookup -> IO a) -> IO a
withSlotLookup slot use = bracket acquire release (use . fmap (cveDbLookup . genDb))
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

{- | The active generation's artifact 'DbEtag', or 'Nothing' before the first sync. The
read does not pin the generation, so it never delays a 'swapIn'.
-}
currentAdvisoryEtag :: CveSlot -> IO (Maybe DbEtag)
currentAdvisoryEtag slot = fmap genEtag <$> readTVarIO (slotCell slot)

{- | What the serving artifact came from, or 'Nothing' before the first sync. A failed poll
never reaches 'swapIn', so a warm process keeps the last value it read.
-}
currentAdvisorySource :: CveSlot -> IO (Maybe AdvisorySource)
currentAdvisorySource slot = fmap genSource <$> readTVarIO (slotCell slot)

-- | Advance publication time only for the installed ETag, without replacing or retiring its database.
observeAdvisoryPublication :: CveSlot -> DbEtag -> Maybe UTCTime -> IO ()
observeAdvisoryPublication slot etag pushedAt = atomically $ do
    current <- readTVar (slotCell slot)
    for_ current $ \g ->
        when (genEtag g == etag && pushedAt > asPushedAt (genSource g)) $
            writeTVar (slotCell slot) (Just g{genSource = (genSource g){asPushedAt = pushedAt}})

{- | When the serving generation went live, or when the slot was created if no swap has landed.
Only 'swapIn' moves it, so it measures what the slot serves, not the liveness of what fills it.
-}
generationInstalledAt :: CveSlot -> IO Double
generationInstalledAt slot =
    maybe (slotCreatedAt slot) genInstalledAt <$> readTVarIO (slotCell slot)

{- | Install a newly verified generation, drain the displaced one's readers, then close it.
The slot owns @newDb@ from entry, so no caller cleanup may close it.
-}
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
