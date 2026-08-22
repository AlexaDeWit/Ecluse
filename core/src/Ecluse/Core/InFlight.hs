-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Async-safe release for a claimed in-flight slot.

Several places in the proxy collapse duplicate concurrent work onto a single
execution. The metadata cache fronts one upstream fetch per @(source, package)@
("Ecluse.Core.Server.Cache"), and the credential refresher mints at most one token at
a time ("Ecluse.Core.Credential.Refresh"). Each does it by claiming a slot atomically:
it installs an in-flight marker, or it sets a flag. A second caller then finds the
claim and waits (or serves a still-valid value) rather than launching its own run.

A claimed slot carries a sharp obligation. The claimant must release it on every exit,
or the slot wedges. A naive @claim@ then @run@ then @free@ leaks the slot when the
claiming thread takes an asynchronous exception: a request timeout, a killed handler
thread. That window is between the claim and the run that frees it. A follower waiting
on the slot then parks forever, and a later caller blocked behind it never proceeds,
until the process restarts. Both consumers face that one hazard, so the release
discipline lives here once.

'guardInFlight' is that discipline. The caller claims its slot in a single masked 'STM'
transaction and then, with no interruptible step in between, hands the leader's run to
'guardInFlight'. It runs the body and releases the slot on every exit: normal
completion, a synchronous failure, or an asynchronous exception. That covers any
exception from the claim onward, including the claim → runner handoff. It also hands the
orphaning error to any follower waiting on the slot's result rather than leaving it to
park. The body runs under the caller's @restore@ so it stays cancellable. The release
and the waiter hand-off run masked, so the tail cannot itself be interrupted.

What a slot /is/, who waits on it, and how a follower receives a result stay with each
consumer. The cache awaits a result promise, and the refresher re-decides against the
freed flag. Only this claim-release discipline is shared.
-}
module Ecluse.Core.InFlight (
    guardInFlight,
) where

import UnliftIO.Exception (finally, withException)

{- | Run a leader's @body@ and release its already-claimed in-flight slot on every exit,
closing the orphan window.

Call it from inside the same 'UnliftIO.Exception.mask' that committed the claim, pass that
mask's @restore@, and leave no interruptible action between the claim and this call.
-}
guardInFlight ::
    -- | The enclosing mask's @restore@, applied to the body so it stays interruptible.
    (IO a -> IO a) ->
    {- | Runs with the orphaning failure just before the release, to hand it to a
    follower waiting on the slot's result. The cache, for one, fills its result promise
    so the follower unblocks with the error. A consumer whose waiters instead re-decide
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
