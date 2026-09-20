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
data VersionListItem = VersionListObject | VersionListContainer | VersionListInvalidContainer | VersionListEntry (Maybe Version)

-- | Count every observed entry, retaining only usable identifiers in source-key order.
data VersionListState = VersionListState
    { inventoryObjectSeen :: Bool
    , inventoryContainerSeen :: Bool
    , inventoryAcceptEntries :: Bool
    , inventoryInvalidContainer :: Bool
    , inventoryCount :: Int
    , inventoryVersions :: Map Text Version
    }

-- | Start without assuming that the response contains an inventory object.
emptyVersionList :: VersionListState
emptyVersionList = VersionListState False False False False 0 mempty

-- | Apply the ceiling before inserting a usable identifier or discarding an unusable entry.
collectVersionList :: Limits -> VersionListState -> VersionListItem -> Either LimitError VersionListState
collectVersionList limits inventory = \case
    VersionListObject -> Right inventory{inventoryObjectSeen = True}
    VersionListContainer ->
        Right
            inventory
                { inventoryContainerSeen = True
                , inventoryAcceptEntries = not (inventoryContainerSeen inventory)
                }
    VersionListInvalidContainer ->
        Right
            inventory
                { inventoryContainerSeen = True
                , inventoryAcceptEntries = False
                , inventoryInvalidContainer = inventoryInvalidContainer inventory || not (inventoryContainerSeen inventory)
                }
    VersionListEntry _ | not (inventoryAcceptEntries inventory) -> Right inventory
    VersionListEntry candidate -> do
        let count = inventoryCount inventory + 1
        checkVersionCountOf limits count
        pure
            inventory
                { inventoryCount = count
                , inventoryVersions = maybe (inventoryVersions inventory) (\version -> Map.insert (renderVersion version) version (inventoryVersions inventory)) candidate
                }

-- | Refuse invalid response and inventory containers instead of certifying an empty store.
finishVersionList :: VersionListState -> Either ParseError [Version]
finishVersionList inventory
    | not (inventoryObjectSeen inventory) = Left (ParseError "the version list is not a JSON object")
    | inventoryInvalidContainer inventory = Left (ParseError "the versions field is not an object or null")
    | otherwise = Right (Map.elems (inventoryVersions inventory))
