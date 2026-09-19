-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The verdict behind @\/readyz@, and the per-mount advisory state it was decided from.

One configured ecosystem awaiting its advisory database does not take the whole listener out of
rotation, so a router keeps sending the healthy mounts their traffic. Only a mount whose rules
deny on the database waits for one. Readiness routes traffic and gates no request: a mount with
no advisory database refuses what needs one through its own rule policy. The constructors are
exported for matching, and 'mountReadiness' is the only builder.
-}
module Ecluse.Core.Server.Readiness (
    -- * One mount's advisory state
    DatabaseRequirement (..),
    MountReadiness (..),
    mountStateFor,

    -- * The verdict
    Readiness (..),
    mountReadiness,
    alwaysReady,

    -- * Reading the verdict
    routable,
    allMountsReady,
) where

import Data.Map.Strict qualified as Map

import Ecluse.Core.Ecosystem (Ecosystem)

-- | Whether a mount's own rules deny on the advisory database, so it cannot decide without one.
data DatabaseRequirement
    = DatabaseRequired
    | DatabaseOptional
    deriving stock (Eq, Show)

-- | One mount's advisory state. The flip is one-way, so a mount never falls back to awaiting.
data MountReadiness
    = MountReady
    | MountAwaitingFirstSync
    deriving stock (Eq, Show)

{- | One mount's state from what its rules need and whether its first sync has landed. A mount
that only reads the database, and never denies on it, serves before any artifact is published.
-}
mountStateFor :: DatabaseRequirement -> Bool -> MountReadiness
mountStateFor requirement synced = case requirement of
    DatabaseOptional -> MountReady
    DatabaseRequired -> bool MountAwaitingFirstSync MountReady synced

-- | The readiness verdict, carrying the mounts it was decided from.
data Readiness
    = -- | At least one configured mount is ready, or no mount is configured.
      Routable (Map.Map Ecosystem MountReadiness)
    | -- | Mounts are configured and none has its advisory database yet.
      AwaitingMounts (Map.Map Ecosystem MountReadiness)
    | -- | Readiness closed for good, whatever the mounts hold (the Dredger's halt latch).
      Latched
    deriving stock (Eq, Show)

-- | Decide the verdict from the mounts. No configured mount is routable: nothing gates routing.
mountReadiness :: Map.Map Ecosystem MountReadiness -> Readiness
mountReadiness mounts
    | Map.null mounts || MountReady `elem` Map.elems mounts = Routable mounts
    | otherwise = AwaitingMounts mounts

-- | The verdict of a role with no advisory mount to wait for.
alwaysReady :: Readiness
alwaysReady = mountReadiness Map.empty

-- | Whether @\/readyz@ answers @200@. Derived from the verdict, never stored beside it.
routable :: Readiness -> Bool
routable = \case
    Routable _ -> True
    AwaitingMounts _ -> False
    Latched -> False

{- | Whether every configured mount is ready, which for one that denies on the advisory database
means it holds one. A wait condition, not the routing verdict: the Dredger holds its first sweep.
-}
allMountsReady :: Readiness -> Bool
allMountsReady = \case
    Routable mounts -> all (== MountReady) (Map.elems mounts)
    AwaitingMounts _ -> False
    Latched -> False
