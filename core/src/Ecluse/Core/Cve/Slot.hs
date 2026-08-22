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
-}
module Ecluse.Core.Cve.Slot (
    CveSlot,
    newCveSlot,
    withSlotLookup,
    currentAdvisoryEtag,
    swapIn,
) where

import Control.Concurrent.STM (check)
import UnliftIO.Exception (bracket)

import Ecluse.Core.Cve (CveDb (..), CveLookup, DbEtag)

{- | One installed generation: the owning resource, its artifact ETag, and its
live-reader count.
-}
data Generation = Generation
    { genDb :: CveDb
    , genEtag :: DbEtag
    , genReaders :: TVar Int
    }

-- | The slot: the currently-active generation, or nothing before the first sync.
newtype CveSlot = CveSlot (TVar (Maybe Generation))

-- | A fresh, empty slot: readers see 'Nothing' until the first 'swapIn'.
newCveSlot :: IO CveSlot
newCveSlot = CveSlot <$> newTVarIO Nothing

{- | Borrow the current generation's lookup for the duration of one action. The bracket
pins the generation, so a concurrent 'swapIn' cannot close it mid-read.
-}
withSlotLookup :: CveSlot -> (Maybe CveLookup -> IO a) -> IO a
withSlotLookup (CveSlot cell) use = bracket acquire release (use . fmap (cveDbLookup . genDb))
  where
    acquire = atomically $ do
        mGen <- readTVar cell
        for_ mGen (\g -> modifyTVar' (genReaders g) (+ 1))
        pure mGen
    release = traverse_ (\g -> atomically (modifyTVar' (genReaders g) (subtract 1)))

{- | The active generation's artifact 'DbEtag', or 'Nothing' before the first sync. The
read does not pin the generation, so it never delays a 'swapIn'.
-}
currentAdvisoryEtag :: CveSlot -> IO (Maybe DbEtag)
currentAdvisoryEtag (CveSlot cell) = fmap genEtag <$> readTVarIO cell

{- | Install a newly verified generation, drain the displaced one's readers, then close it.
The slot owns @newDb@ from entry and publishes it first, so no caller cleanup may close it.
Cancellation during the drain propagates, leaving the displaced generation unclosed.
-}
swapIn :: CveSlot -> DbEtag -> CveDb -> IO ()
swapIn (CveSlot cell) etag newDb = do
    readers <- newTVarIO (0 :: Int)
    displaced <- atomically $ do
        old <- readTVar cell
        writeTVar cell (Just (Generation newDb etag readers))
        pure old
    for_ displaced $ \g -> do
        atomically (readTVar (genReaders g) >>= check . (== 0))
        -- 'cveDbClose' never throws (the handle absorbs close faults), so the
        -- swallow the module header describes needs no guard here.
        cveDbClose (genDb g)
