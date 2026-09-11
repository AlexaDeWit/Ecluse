-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The verdict behind @\/readyz@, and the per-mount advisory state it was decided from.
One configured ecosystem awaiting its advisory database does not take the whole listener out
of rotation, so a router keeps sending the healthy mounts their traffic. The constructors are
exported for matching and 'mountReadiness' is the sanctioned builder, so a verdict a producer
makes agrees with its own map. Readiness routes traffic. It gates no request: a mount with no
advisory database refuses what needs one through its own rule policy.
-}
module Ecluse.Core.Server.Readiness (
    MountReadiness (..),
    Readiness (..),
    mountReadiness,
    alwaysReady,
    routable,
    allMountsReady,
) where

import Data.Map.Strict qualified as Map

import Ecluse.Core.Ecosystem (Ecosystem)

-- | One mount's advisory state. The flip is one-way, so a mount never falls back to awaiting.
data MountReadiness
    = MountReady
    | MountAwaitingFirstSync
    deriving stock (Eq, Show)

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

{- | Whether every configured mount holds its advisory database. This is a wait condition and
not the routing verdict: the Dredger holds its first sweep for it.
-}
allMountsReady :: Readiness -> Bool
allMountsReady = \case
    Routable mounts -> all (== MountReady) (Map.elems mounts)
    AwaitingMounts _ -> False
    Latched -> False
