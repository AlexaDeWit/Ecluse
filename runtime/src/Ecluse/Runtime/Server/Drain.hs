-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The graceful-shutdown drain vocabulary: the one-way 'DrainSignal' the front
door observes on every request, and the bound on how long a drain may run.
"Ecluse.Runtime.Server"'s 'runWarp' raises the signal from the OS shutdown
handler; the readiness probe and the going-away middleware
("Ecluse.Runtime.Server.Middleware") read it.
-}
module Ecluse.Runtime.Server.Drain (
    DrainSignal,
    newDrainSignal,
    neverDraining,
    beginDrain,
    isDraining,
    ShutdownDrainTimeout (..),
    defaultShutdownDrainTimeout,
) where

{- | The shared shutdown-drain flag the front door observes during a graceful
rollover, as a small handle (a reader plus a one-way raise) rather than a bare
'TVar' -- so the same field can hold either a live, flip-once signal ('newDrainSignal')
or the inert 'neverDraining' constant the socket-free tests assemble against, and
nothing downstream can lower it back. It is raised once, on a shutdown signal, and
read on every request by the readiness probe and the going-away middleware.
-}
data DrainSignal = DrainSignal
    { drainState :: STM Bool
    -- ^ Whether the instance is draining: 'False' while serving, 'True' once raised.
    , drainRaise :: STM ()
    -- ^ Raise the flag. Idempotent -- a second raise is a no-op.
    }

{- | Allocate a live, lowered shutdown-drain signal backed by a 'TVar'. @runWarp@
allocates one per launch and flips it from the signal handler; the @application@ it
builds reads the very same signal, so the readiness probe and the going-away
middleware see the drain the instant the handler raises it.
-}
newDrainSignal :: IO DrainSignal
newDrainSignal = do
    tvar <- newTVarIO False
    pure
        DrainSignal
            { drainState = readTVar tvar
            , drainRaise = writeTVar tvar True
            }

{- | The inert drain signal: permanently lowered, raising it is a no-op. The
@mkServerConfig@ default, so an @application@ assembled for a socket-free test (and
one driven without ever entering shutdown) reports ready and adds no going-away
header. A real launch overrides it with 'newDrainSignal' in @runWarp@.
-}
neverDraining :: DrainSignal
neverDraining =
    DrainSignal
        { drainState = pure False
        , drainRaise = pure ()
        }

-- | Raise a drain signal -- the one-way transition into draining. Idempotent.
beginDrain :: DrainSignal -> IO ()
beginDrain = atomically . drainRaise

-- | Read whether a drain signal is raised.
isDraining :: DrainSignal -> IO Bool
isDraining = atomically . drainState

{- | The bound on the graceful drain: how many seconds the server waits for
in-flight requests and in-progress artifact streams to finish after it stops
accepting new connections, before the process exits regardless. A @newtype@ so a
raw seconds count is not mistaken for some other 'Int', and so a non-positive value
cannot be passed where a positive timeout is meant (see @runWarp@).
-}
newtype ShutdownDrainTimeout = ShutdownDrainTimeout Int
    deriving stock (Eq, Show)

{- | The default graceful-drain bound: 30 seconds. Long enough for an in-flight
metadata fetch or a moderate artifact stream to complete during a rolling deploy,
short enough that a stuck request cannot pin the old instance indefinitely.
-}
defaultShutdownDrainTimeout :: ShutdownDrainTimeout
defaultShutdownDrainTimeout = ShutdownDrainTimeout 30
