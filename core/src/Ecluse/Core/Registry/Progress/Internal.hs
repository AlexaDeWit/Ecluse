-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The window "Ecluse.Core.Registry.Progress" keeps for each transfer, and its pure transitions.
Importing this module opts out of the public surface's stability promises. It exists so a spec can
hold the transitions to a reference.
-}
module Ecluse.Core.Registry.Progress.Internal (
    Window (..),
    freshWindow,
    beginAt,
    endAt,
    waitedBy,
) where

import Ecluse.Core.Security (ProgressFloor, floorMinBytes)

{- | The nanoseconds waited and the bytes moved in the current window, each constructor's last two
fields. 'Waiting' first holds the monotonic time its open wait on the upstream began.
-}
data Window
    = Waiting {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Int
    | Idle {-# UNPACK #-} !Word64 {-# UNPACK #-} !Int

-- | No wait open, nothing waited, and nothing moved: a transfer's start, and the window after the floor's bytes.
freshWindow :: Window
freshWindow = Idle 0 0

-- | Open a wait at a monotonic time in nanoseconds.
beginAt :: Word64 -> Window -> Window
beginAt now = \case
    Waiting _ waited moved -> Waiting now waited moved
    Idle waited moved -> Waiting now waited moved

-- | Close the open wait with the bytes it moved. Reaching the floor's bytes starts a new window.
endAt :: ProgressFloor -> Word64 -> Int -> Window -> Window
endAt progress now bytes current
    | total >= floorMinBytes progress = freshWindow
    | otherwise = Idle (waitedBy now current) total
  where
    total = movedIn current + bytes

{- | The nanoseconds waited by a monotonic time. The sweeper can read its clock just before the
transfer stamps a later wait, so the open wait's gap floors at zero.
-}
waitedBy :: Word64 -> Window -> Word64
waitedBy now = \case
    Waiting began waited _ -> waited + if now > began then now - began else 0
    Idle waited _ -> waited

movedIn :: Window -> Int
movedIn = \case
    Waiting _ _ moved -> moved
    Idle _ moved -> moved
