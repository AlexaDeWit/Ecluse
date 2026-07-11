-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Async-safe release for a claimed __in-flight slot__.

Several places in the proxy collapse duplicate concurrent work onto a single
execution: the metadata cache fronts one upstream fetch per @(source, package)@
("Ecluse.Core.Server.Cache"), and the credential refresher mints at most one token
at a time ("Ecluse.Core.Credential.Refresh"). Each does it by atomically __claiming a
slot__ -- installing an in-flight marker, or setting a flag -- so a second caller finds
the claim and waits (or serves a still-valid value) rather than launching its own run.

A claimed slot carries a sharp obligation: once claimed it must be __released on
every exit__, or the slot wedges. A naive @claim; run; free@ leaks the slot if the
claiming thread is hit by an asynchronous exception -- a request timeout, a killed
handler thread -- in the window between the claim and the run that frees it: a follower
waiting on the slot parks forever, and a later caller blocked behind it never proceeds,
until the process restarts. That is one shared hazard, found and fixed independently in
both consumers, which is why the release discipline lives here once.

'guardInFlight' is that discipline. The caller claims its slot in a single masked 'STM'
transaction and then, with __no interruptible step in between__, hands the leader's run
to 'guardInFlight'. It runs the body and guarantees the slot is released on __every
exit__ -- normal completion, a synchronous failure, or an asynchronous exception anywhere
from the claim onward, including the claim → runner handoff -- and that any follower
waiting on the slot's result is handed the orphaning error rather than left to park. The
body runs under the caller's @restore@ so it stays cancellable; the release and the
waiter hand-off run masked, so the tail cannot itself be interrupted.

What a slot /is/, who waits on it, and how a follower receives a result stay with each
consumer: the cache awaits a result promise; the refresher re-decides against the freed
flag. Only this claim-release discipline is shared.
-}
module Ecluse.Core.InFlight (
    guardInFlight,
) where

import UnliftIO.Exception (finally, withException)

{- | Run a leader's @body@ with the guarantee that its already-claimed in-flight slot is
released on every exit, closing the orphan window.

Call it from inside the same 'UnliftIO.Exception.mask' that committed the claim, with no
interruptible action between the claim and this call, passing that mask's @restore@. The
body runs under @restore@ so it stays cancellable; on any exit the slot is released, and
on a failure the orphaning exception is first handed to any waiting follower -- both run
masked, so the release cannot be orphaned in turn.
-}
guardInFlight ::
    -- | The enclosing mask's @restore@, applied to the body so it stays interruptible.
    (IO a -> IO a) ->
    {- | Run with the orphaning failure before the slot is released, to hand it to a
    follower waiting on the slot's result (the cache fills its result promise so the
    follower unblocks with the error). A consumer whose waiters instead re-decide
    against the freed slot passes a no-op.
    -}
    (SomeException -> IO ()) ->
    {- | Free the claimed slot. Runs on every exit: a normal return, a synchronous
    failure, or an asynchronous exception.
    -}
    IO () ->
    -- | The leader's run, executed under @restore@.
    IO a ->
    IO a
guardInFlight restore onOrphan releaseSlot body =
    (restore body `withException` onOrphan) `finally` releaseSlot
