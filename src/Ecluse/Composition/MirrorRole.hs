-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | What each half of the demand-driven mirror pipeline needs of the process running it, and the
boot refusal a role earns against the resolved mirror runtime.

Mirroring has a producer half (the serve path enqueues a job for every artifact it admits) and
a consumer half (the worker drains the queue and publishes). One process runs both, or a
split deployment runs each in its own fleet so the front door and the worker scale apart.
The split only works over a durable queue, which is what 'mirrorRoleRefusal' decides.
-}
module Ecluse.Composition.MirrorRole (
    runsWorker,
    spawnsWorker,
    enqueuesJobs,
    roleInvocation,
    mirrorRoleRefusal,
) where

import Ecluse.Composition.BootError (BootError (MirrorRoleWithoutMirroring, SplitRoleNeedsDurableQueue))
import Ecluse.Composition.MirrorQueue (
    MirrorQueuePlan (MemoryBackend, SqsBackend),
    MirrorRuntimePlan (MirrorWith, NoMirroring),
 )
import Ecluse.Composition.Types (MirrorRole (MirrorOnly, ServeAndMirror, ServeOnly))

-- | Whether this role would run the mirror worker, given a runtime that has one to run.
runsWorker :: MirrorRole -> Bool
runsWorker = \case
    ServeAndMirror -> True
    ServeOnly -> False
    MirrorOnly -> True

{- | Whether this process runs the mirror worker: the role wants one and a mount declares a
mirror target. Under 'NoMirroring' a spawned loop would poll an inert queue with nothing to pace it.
-}
spawnsWorker :: MirrorRole -> MirrorRuntimePlan -> Bool
spawnsWorker role = \case
    NoMirroring -> False
    MirrorWith _ -> runsWorker role

{- | Whether this role serves requests, and so enqueues a mirror job for each version it
admits. The composition root reads it to decide whether to build the enqueue buffer.
-}
enqueuesJobs :: MirrorRole -> Bool
enqueuesJobs = \case
    ServeAndMirror -> True
    ServeOnly -> True
    MirrorOnly -> False

-- | How an operator spells this role on the command line, for the boot refusal's message.
roleInvocation :: MirrorRole -> Text
roleInvocation = \case
    ServeAndMirror -> "ecluse proxy"
    ServeOnly -> "ecluse proxy --no-worker"
    MirrorOnly -> "ecluse mirror"

{- | Refuse a role the resolved mirror runtime cannot serve. A split role over the bounded
in-memory queue would strand every job, because that queue lives inside one process.
-}
mirrorRoleRefusal :: MirrorRole -> MirrorRuntimePlan -> Either [BootError] ()
mirrorRoleRefusal role plan = case (role, plan) of
    (ServeAndMirror, _) -> Right ()
    (ServeOnly, NoMirroring) -> Right ()
    (ServeOnly, MirrorWith SqsBackend{}) -> Right ()
    (ServeOnly, MirrorWith MemoryBackend) -> Left [SplitRoleNeedsDurableQueue (roleInvocation role)]
    (MirrorOnly, MirrorWith SqsBackend{}) -> Right ()
    (MirrorOnly, MirrorWith MemoryBackend) -> Left [SplitRoleNeedsDurableQueue (roleInvocation role)]
    (MirrorOnly, NoMirroring) -> Left [MirrorRoleWithoutMirroring]
