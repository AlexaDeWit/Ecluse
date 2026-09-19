-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE DeriveFunctor #-}

{- | Bounded name-prefix walks with resumable bucket cursors.
Listings have no stable order. Oversized buckets split until the depth bound.
-}
module Ecluse.Core.Registry.Sweep.Walk (
    bucketNameBudget,
    bucketDepthLimit,
    walkBuckets,
    resumeAfter,
    BucketNames (..),
    collectBucketWith,
    insertInventory,
) where

import Control.Monad (foldM)
import Data.Conduit (ConduitT, await, fuseBothMaybe, runConduit)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.Maintenance (
    NameAlphabet,
    NamePrefix,
    extendBucket,
    initialBuckets,
    renderNamePrefix,
 )

-- | Maximum distinct package names held in one bucket across its target inventories.
bucketNameBudget :: Int
bucketNameBudget = 10000

{- | How far a bucket may be narrowed. Past it the names share a prefix this long and narrowing
has stopped dividing them, so the walk reports rather than descending without end.
-}
bucketDepthLimit :: Int
bucketDepthLimit = 4

-- | The buckets a walk covers, in the order it covers them.
walkBuckets :: NameAlphabet -> [NamePrefix]
walkBuckets = toList . initialBuckets

{- | The buckets still to cover, given the one last completed. A bucket the record falls inside is
kept, because the walk stopped part way through that bucket's own split.
-}
resumeAfter :: Maybe NamePrefix -> [NamePrefix] -> [NamePrefix]
resumeAfter = maybe id (filter . stillToDo)

{- A bucket is done when it sorts at or before the record without containing it. Containing it
means the record is a narrower bucket inside this one, so this one is only part done. -}
stillToDo :: NamePrefix -> NamePrefix -> Bool
stillToDo done bucket = bucket > done || properlyCovers bucket done

properlyCovers :: NamePrefix -> NamePrefix -> Bool
properlyCovers bucket done = raw /= renderNamePrefix done && raw `T.isPrefixOf` renderNamePrefix done
  where
    raw = renderNamePrefix bucket

-- | What reading one bucket's listing produced.
data BucketNames fault a
    = -- | The bucket was read whole, its names sorted.
      BucketRead [a]
    | -- | The bucket outgrew the budget, so these narrower ones cover it instead.
      BucketOverflowed (NonEmpty NamePrefix)
    | -- | The bucket outgrew the budget and nothing narrows it further.
      BucketUnsplittable
    | -- | The listing stopped on a fault, and nothing was read.
      BucketFaulted fault
    deriving stock (Functor)

-- | Merge one bucket's listing pages under the distinct-name bound, keeping each name's evidence.
collectBucketWith ::
    NameAlphabet ->
    NamePrefix ->
    (a -> a -> a) ->
    ConduitT () [(PackageName, a)] IO (Maybe fault) ->
    IO (BucketNames fault (PackageName, a))
collectBucketWith alphabet prefix merge source = outcome <$> runConduit (fuseBothMaybe source (takeToBudget merge))
  where
    outcome = \case
        (_, Nothing) -> maybe BucketUnsplittable BucketOverflowed (nonEmpty (narrowerBuckets alphabet prefix))
        (Just (Just fault), _) -> BucketFaulted fault
        (_, Just names) -> BucketRead names

{- The buckets covering this one, none where it cannot be narrowed. An alphabet with no characters
divides nothing, and past the depth bound a further character has stopped dividing the names. -}
narrowerBuckets :: NameAlphabet -> NamePrefix -> [NamePrefix]
narrowerBuckets alphabet prefix
    | T.compareLength (renderNamePrefix prefix) bucketDepthLimit /= LT = []
    | otherwise = extendBucket alphabet prefix

{- Fold the pages until the bucket is read or the budget is crossed. 'Nothing' means the budget
went first, which abandons the stream where it stands. -}
takeToBudget :: (a -> a -> a) -> ConduitT [(PackageName, a)] o IO (Maybe [(PackageName, a)])
takeToBudget merge = go Map.empty
  where
    go held =
        await >>= \case
            Nothing -> pure (Just (Map.toAscList held))
            Just page -> maybe (pure Nothing) go (foldM (insertInventory bucketNameBudget merge) held page)

-- | Refuse a new inventory identity before insertion would cross the bound. Existing ones merge.
insertInventory :: (Ord key) => Int -> (value -> value -> value) -> Map key value -> (key, value) -> Maybe (Map key value)
insertInventory limit merge held (key, value)
    | Map.notMember key held && Map.size held >= max 0 limit = Nothing
    | otherwise = Just (Map.insertWith merge key value held)
