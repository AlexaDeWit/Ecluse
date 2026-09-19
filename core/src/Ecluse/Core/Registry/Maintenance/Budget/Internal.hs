-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The vocabulary a maintenance budget is written in: the pools a backend meters under, the
requests a cycle makes, and the rate one scope runs at. "Ecluse.Core.Registry.Maintenance.Budget"
curates what a caller outside needs of it.

Importing this module opts out of the public surface's stability promises. It exists so a spec can
pin each arm against the name a configuration key spells it with.
-}
module Ecluse.Core.Registry.Maintenance.Budget.Internal (
    QuotaDimension (..),
    quotaDimensions,
    quotaDimensionName,
    RequestKind (..),
    requestKinds,
    requestKindName,
    CyclePace (..),
    freePace,
    paceOf,
    paceSeconds,
) where

import Data.Map.Strict qualified as Map
import Data.Time (NominalDiffTime)

{- | One pool a backend meters its callers by. The arms are pool shapes rather than one vendor's
API names, so each backend maps its own calls onto them.
-}
data QuotaDimension
    = -- | Calls that list a store's package names.
      NameListing
    | -- | Calls that list one package's versions.
      VersionListing
    | -- | Requests the backend counts as reads of the account holding the store.
      AccountReads
    | -- | Requests the backend counts as writes to that account.
      AccountWrites
    | -- | Requests sharing the ceiling of one authentication token.
      TokenReads
    | -- | The single undivided request capacity of a backend that publishes no other pool.
      StoreRequests
    deriving stock (Eq, Ord, Show)

{- | Every pool this build meters under. 'quotaDimensionName' carries no wildcard and the spec
pins this list, so a new arm fails both until it is named in each.
-}
quotaDimensions :: [QuotaDimension]
quotaDimensions = [NameListing, VersionListing, AccountReads, AccountWrites, TokenReads, StoreRequests]

-- | The dimension as a configuration key spells it.
quotaDimensionName :: QuotaDimension -> Text
quotaDimensionName = \case
    NameListing -> "nameListing"
    VersionListing -> "versionListing"
    AccountReads -> "accountReads"
    AccountWrites -> "accountWrites"
    TokenReads -> "tokenReads"
    StoreRequests -> "storeRequests"

-- | One request a cycle makes against the store being dredged.
data RequestKind
    = -- | One page of the store's package-name listing.
      ListingPage
    | -- | One enumeration of a package's versions.
      VersionPage
    | -- | One read of a package's metadata back from the store.
      ManifestRead
    | -- | One destructive call, whatever number of versions the backend takes in it.
      DeleteBatch
    | -- | One read of a standing permission, including a reassessment before a delete.
      PermissionRead
    | -- | One read of the walk's resumption marker.
      CursorRead
    | -- | One write or clearing of that marker.
      CursorWrite
    deriving stock (Eq, Ord, Show)

-- | Every request a cycle can make, held to 'requestKindName' the way 'quotaDimensions' is.
requestKinds :: [RequestKind]
requestKinds = [ListingPage, VersionPage, ManifestRead, DeleteBatch, PermissionRead, CursorRead, CursorWrite]

-- | The kind as a configuration weight spells it.
requestKindName :: RequestKind -> Text
requestKindName = \case
    ListingPage -> "listingPage"
    VersionPage -> "versionPage"
    ManifestRead -> "manifestRead"
    DeleteBatch -> "deleteBatch"
    PermissionRead -> "permissionRead"
    CursorRead -> "cursorRead"
    CursorWrite -> "cursorWrite"

-- | What one request of each kind costs its scope in seconds, at the rate a cycle runs.
newtype CyclePace = CyclePace (Map RequestKind Rational)
    deriving stock (Eq, Show)

-- | The pace that imposes no wait, which a scope with no declared capacity runs at.
freePace :: CyclePace
freePace = CyclePace Map.empty

-- | Build a pace from the seconds each kind is held to.
paceOf :: Map RequestKind Rational -> CyclePace
paceOf = CyclePace

-- | The wait one request of this kind takes, which is none where the pace names no cost.
paceSeconds :: CyclePace -> RequestKind -> NominalDiffTime
paceSeconds (CyclePace costs) kind = maybe 0 fromRational (Map.lookup kind costs)
