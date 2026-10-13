-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Package-name enumeration for npm stores, retaining names without their metadata.
module Ecluse.Core.Registry.Npm.Maintenance.Listing (packageListingWalk) where

import Data.JsonStream.Lexer (Cursor, Element (ObjectBegin))
import Data.Map.Strict qualified as Map

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.Json.Intern (nameText)
import Ecluse.Core.Registry.Json.Walk (Step, Walk (failWith, finish), eachMember, skipFrom, withElement)
import Ecluse.Core.Registry.Npm.Project (projectName)

-- | Keep valid names in raw-key order, collapse duplicates, and skip values without decoding them.
packageListingWalk :: Cursor -> Step [PackageName]
packageListingWalk tokens = withElement tokens $ \element rest -> case element of
    ObjectBegin -> eachMember member (\names _ -> finish (Map.elems names)) Map.empty rest
    _ -> skipFrom element rest (const (failWith "the store's package listing is not a JSON object"))
  where
    member names key after next = withElement after $ \value afterValue ->
        skipFrom value afterValue (next collected)
      where
        raw = nameText key
        collected = case projectName raw of
            Right name | raw /= "_updated" -> Map.insert raw name names
            _ -> names
