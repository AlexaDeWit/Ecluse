-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Async-safe release for a claimed in-flight slot.

The metadata cache ("Ecluse.Core.Server.Cache.Store") and the credential refresher
("Ecluse.Core.Credential.Refresh.Internal") each collapse duplicate work onto one
execution by claiming a slot. An asynchronous exception taken between the claim and the
run that would free it wedges the slot: a follower parks forever and every caller behind
it stalls until the process restarts. Both face that hazard, so the release discipline
lives here once.
-}
module Ecluse.Core.InFlight (
    guardInFlight,
) where

import UnliftIO.Exception (finally, withException)

{- | Run a leader's @body@ and release its already-claimed in-flight slot on every exit. Call it
inside the mask that committed the claim, pass that mask's @restore@, and leave nothing interruptible between.
-}
guardInFlight ::
    -- | The enclosing mask's @restore@, applied to the body so it stays interruptible.
    (IO a -> IO a) ->
    {- | Runs with the orphaning failure just before the release, to hand it to a waiting
    follower. A consumer whose waiters re-decide against the freed slot passes a no-op.
    -}
    (SomeException -> IO ()) ->
    -- | Free the claimed slot. Runs on every exit, asynchronous exceptions included.
    IO () ->
    -- | The leader's run, executed under @restore@.
    IO a ->
    IO a
guardInFlight restore onOrphan releaseSlot body =
    (restore body `withException` onOrphan) `finally` releaseSlot
