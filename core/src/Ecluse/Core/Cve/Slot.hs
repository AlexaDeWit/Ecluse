-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The read side of the advisory database's atomic shadow-swap. A slot holds the
currently-active 'CveDb' generation. A reader borrows it through a bracket, so the
swap can tell when no read still needs a superseded generation.

One slot serves one ecosystem's artifact. A rule evaluation borrows the current
generation's 'CveLookup' view through 'withSlotLookup', which the composition root
installs as 'Ecluse.Core.Rules.rdWithCveLookup'. The sync task installs a
newly-verified generation with 'swapIn', which waits for the displaced generation's
readers to drain and then closes it. Closing is also the reclamation. The sync task
already renamed the new artifact over the old one's only file name. The drained close
therefore releases the old inode's last reference, and the kernel frees the storage.
Pruning is a property the OS enforces, never a delete this code could mistime.

Before the first successful sync the slot is empty and hands readers 'Nothing'. The
CVE rule abstains and the ordinary policy governs.

The slot also carries __when__ the serving generation went live, on the monotonic clock.
It is the only place that knows, because the slot outlives the sync task that fills it: a
supervised restart builds a fresh task against the same slot. 'generationInstalledAt' hands
that stamp out, and the advisory-database age gauge reads it at each collection.
-}
module Ecluse.Core.Cve.Slot (
    CveSlot,
    newCveSlot,
    withSlotLookup,
    currentAdvisoryEtag,
    generationInstalledAt,
    swapIn,
) where

import Control.Concurrent.STM (check)
import GHC.Clock (getMonotonicTime)
import UnliftIO.Exception (bracket)

import Ecluse.Core.Cve (CveDb (..), CveLookup, DbEtag)

{- | One installed generation: the owning resource, its artifact ETag, its live-reader
count, and the monotonic time it went live.
-}
data Generation = Generation
    { genDb :: CveDb
    , genEtag :: DbEtag
    , genReaders :: TVar Int
    , genInstalledAt :: Double
    }

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
    release = traverse_ (\g -> atomically (modifyTVar' (genReaders g) (subtract 1)))

{- | The active generation's artifact 'DbEtag', or 'Nothing' before the first sync. The
read does not pin the generation, so it never delays a 'swapIn'.
-}
currentAdvisoryEtag :: CveSlot -> IO (Maybe DbEtag)
currentAdvisoryEtag slot = fmap genEtag <$> readTVarIO (slotCell slot)

{- | When the serving generation went live, on the monotonic clock, or when the slot was
created if no swap has landed yet. Only 'swapIn' moves it, so it measures the age of what
the slot actually serves, not the liveness of whatever fills it.
-}
generationInstalledAt :: CveSlot -> IO Double
generationInstalledAt slot =
    maybe (slotCreatedAt slot) genInstalledAt <$> readTVarIO (slotCell slot)

{- | Install a newly verified generation, drain the displaced one's readers, then close it.
The slot owns @newDb@ from entry and publishes it first, so no caller cleanup may close it.
Cancellation during the drain propagates, leaving the displaced generation unclosed.
-}
swapIn :: CveSlot -> DbEtag -> CveDb -> IO ()
swapIn slot etag newDb = do
    readers <- newTVarIO (0 :: Int)
    installedAt <- getMonotonicTime
    displaced <- atomically $ do
        old <- readTVar (slotCell slot)
        writeTVar (slotCell slot) (Just (Generation newDb etag readers installedAt))
        pure old
    for_ displaced $ \g -> do
        atomically (readTVar (genReaders g) >>= check . (== 0))
        -- 'cveDbClose' never throws (the handle absorbs close faults), so the
        -- swallow the module header describes needs no guard here.
        cveDbClose (genDb g)
