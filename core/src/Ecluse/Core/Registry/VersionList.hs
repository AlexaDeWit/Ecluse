-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Bounded inventory accumulation counts source entries before unusable releases and duplicates drop.
module Ecluse.Core.Registry.VersionList (
    VersionListItem (..),
    VersionListState,
    emptyVersionList,
    collectVersionList,
    finishVersionList,
) where

import Data.Map.Strict qualified as Map

import Ecluse.Core.Registry (ParseError (ParseError))
import Ecluse.Core.Security (LimitError, Limits, checkVersionCountOf)
import Ecluse.Core.Version (Version, renderVersion)

-- | Container markers distinguish an empty inventory from a document of the wrong shape.
data VersionListItem = VersionListObject | VersionListEntry (Maybe Version)

-- | Count every observed entry, retaining only usable identifiers in source-key order.
data VersionListState = VersionListState Bool Int (Map Text Version)

-- | Start without assuming that the response contains an inventory object.
emptyVersionList :: VersionListState
emptyVersionList = VersionListState False 0 mempty

-- | Apply the ceiling before inserting a usable identifier or discarding an unusable entry.
collectVersionList :: Limits -> VersionListState -> VersionListItem -> Either LimitError VersionListState
collectVersionList limits (VersionListState objectSeen count versions) = \case
    VersionListObject -> Right (VersionListState True count versions)
    VersionListEntry candidate -> do
        let count' = count + 1
        checkVersionCountOf limits count'
        pure (VersionListState objectSeen count' (maybe versions (\version -> Map.insert (renderVersion version) version versions) candidate))

-- | Refuse a non-object response instead of treating it as an empty store.
finishVersionList :: VersionListState -> Either ParseError [Version]
finishVersionList (VersionListState objectSeen _ versions)
    | objectSeen = Right (Map.elems versions)
    | otherwise = Left (ParseError "the version list is not a JSON object")
