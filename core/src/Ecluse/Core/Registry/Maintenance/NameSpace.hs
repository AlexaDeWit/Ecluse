-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The buckets a store's name space is walked in.

A bucket is a leading-character prefix of a package's base name, so the buckets of one alphabet
are disjoint and cover every permitted name. A backend that cannot filter its listing takes the
empty alphabet, whose single bucket is the whole store.
-}
module Ecluse.Core.Registry.Maintenance.NameSpace (
    NameAlphabet,
    mkNameAlphabet,
    noNameAlphabet,
    NamePrefix,
    wholeNameSpace,
    renderNamePrefix,
    parseNamePrefix,
    initialBuckets,
    extendBucket,
    inBucket,
) where

import Data.Text qualified as T

import Ecluse.Core.Package (PackageName, unscopedName)

-- | Permitted leading characters of ecosystem package names.
newtype NameAlphabet = NameAlphabet [Char]
    deriving stock (Eq, Show)

-- | Build an alphabet, dropping repeats and keeping the order given.
mkNameAlphabet :: [Char] -> NameAlphabet
mkNameAlphabet = NameAlphabet . ordNub

-- | Use a single whole-store bucket when the backend cannot filter its listing.
noNameAlphabet :: NameAlphabet
noNameAlphabet = NameAlphabet []

-- | A bucket prefix addresses the package's base name, excluding its namespace.
newtype NamePrefix = NamePrefix Text
    deriving stock (Eq, Ord, Show)

-- | The unfiltered whole-store bucket.
wholeNameSpace :: NamePrefix
wholeNameSpace = NamePrefix ""

-- | The prefix as a store filter and a walk cursor spell it. Empty stands for no filter at all.
renderNamePrefix :: NamePrefix -> Text
renderNamePrefix (NamePrefix raw) = raw

-- | Reject prefixes outside the current alphabet so an incompatible cursor restarts the walk.
parseNamePrefix :: NameAlphabet -> Text -> Maybe NamePrefix
parseNamePrefix (NameAlphabet chars) raw
    | T.all (`elem` chars) raw = Just (NamePrefix raw)
    | otherwise = Nothing

-- | Partition the store into disjoint buckets that cover every permitted name.
initialBuckets :: NameAlphabet -> NonEmpty NamePrefix
initialBuckets (NameAlphabet chars) =
    maybe (wholeNameSpace :| []) (fmap (NamePrefix . T.singleton)) (nonEmpty chars)

-- | Subdivide an oversized bucket. An empty alphabet permits no subdivision.
extendBucket :: NameAlphabet -> NamePrefix -> [NamePrefix]
extendBucket (NameAlphabet chars) (NamePrefix raw) =
    [NamePrefix (raw <> T.singleton ch) | ch <- chars]

-- | Whether a name falls in a bucket, for a store whose listing has no prefix filter of its own.
inBucket :: NamePrefix -> PackageName -> Bool
inBucket (NamePrefix raw) name = raw `T.isPrefixOf` unscopedName name
