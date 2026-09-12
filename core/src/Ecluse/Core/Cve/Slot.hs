-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The read side of the advisory database's atomic shadow-swap: one slot per ecosystem,
holding the generation serving now. Before the first sync it hands readers 'Nothing', and the
CVE rule abstains.

A rule evaluation borrows the current generation's 'CveLookup' through 'withSlotLookup', which
the composition root installs as 'Ecluse.Core.Rules.rdWithCveLookup'. 'swapIn' installs a
newly-verified generation, waits for the displaced one's readers to drain, then closes it. The
sync task has already renamed the new artifact over the old one's only file name, so that close
releases the old inode's last reference: pruning is a property the OS enforces, never a delete
this code could mistime.

The slot also carries what the serving artifact records about its sources, when its object was
published ('currentAdvisorySource'), and the monotonic time the generation went live
('generationInstalledAt', which the advisory-database age gauge reads). It is the only place
that knows, because it outlives the sync task that fills it: a supervised restart builds a
fresh task against the same slot.
-}
module Ecluse.Core.Cve.Slot (
    CveSlot,
    newCveSlot,
    withSlotLookup,
    currentAdvisoryEtag,
    AdvisorySource (..),
    currentAdvisorySource,
    generationInstalledAt,
    swapIn,
) where

import Control.Concurrent.STM (check)
import Data.Time (UTCTime)
import GHC.Clock (getMonotonicTime)
import UnliftIO.Exception (bracket)

import Ecluse.Core.Cve (CveDb (..), CveLookup, DbEtag)
import Ecluse.Core.Osv.Provenance (AdvisoryProvenance)

{- | One installed generation: the owning resource, its artifact ETag, what the artifact says
about its sources, its live-reader count, and the monotonic time it went live.
-}
data Generation = Generation
    { genDb :: CveDb
    , genEtag :: DbEtag
    , genSource :: AdvisorySource
    , genReaders :: TVar Int
    , genInstalledAt :: Double
    }

{- | Where the serving artifact came from: what it records about its own sources, and when the
object carrying it was published. The publication time is the store's, not the artifact's, so a
recompile of unchanged bytes still moves it.
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
    release = traverse_ (\g -> atomically (modifyTVar' (genReaders g) (subtract 1)))

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
@pushedAt@ is the published object's own timestamp, from the sync that fetched it.
-}
swapIn :: CveSlot -> DbEtag -> Maybe UTCTime -> CveDb -> IO ()
swapIn slot etag pushedAt newDb = do
    readers <- newTVarIO (0 :: Int)
    installedAt <- getMonotonicTime
    let source = AdvisorySource{asProvenance = cveDbProvenance newDb, asPushedAt = pushedAt}
    displaced <- atomically $ do
        old <- readTVar (slotCell slot)
        writeTVar (slotCell slot) (Just (Generation newDb etag source readers installedAt))
        pure old
    for_ displaced $ \g -> do
        atomically (readTVar (genReaders g) >>= check . (== 0))
        -- 'cveDbClose' never throws (the handle absorbs close faults), so the
        -- swallow the module header describes needs no guard here.
        cveDbClose (genDb g)
