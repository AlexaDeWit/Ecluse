-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The graceful-shutdown drain vocabulary: the one-way 'DrainSignal' the front
door observes on every request, and the bound on how long a drain may run.
"Ecluse.Runtime.Server"'s 'runWarp' raises the signal from the OS shutdown handler.
The readiness probe and the going-away middleware
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
rollover. It is a small handle, a reader plus a one-way raise, rather than a bare
'TVar'. The same field then holds either a live, flip-once signal ('newDrainSignal') or
the inert 'neverDraining' constant the socket-free tests assemble against. Nothing
downstream can lower it back. A shutdown signal raises it once, and the readiness probe
and the going-away middleware read it on every request.
-}
data DrainSignal = DrainSignal
    { drainState :: STM Bool
    -- ^ Whether the instance is draining: 'False' while serving, 'True' once raised.
    , drainRaise :: STM ()
    -- ^ Raise the flag. Idempotent: a second raise is a no-op.
    }

{- | Allocate a live, lowered shutdown-drain signal backed by a 'TVar'. The @runWarp@
launcher allocates one per launch, hands it to the @application@ builder through the
'ServerConfig' it passes, then flips it from the signal handler. The readiness probe
and the going-away middleware then read exactly that signal, the instant the handler
raises it.
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

-- | Raise a drain signal: the one-way transition into draining. Idempotent.
beginDrain :: DrainSignal -> IO ()
beginDrain = atomically . drainRaise

-- | Read whether a drain signal is raised.
isDraining :: DrainSignal -> IO Bool
isDraining = atomically . drainState

{- | The bound on the graceful drain. The server stops accepting new connections, then
waits this many seconds for in-flight requests and in-progress artifact streams to
finish. The process then exits regardless. A @newtype@, so a raw seconds count is
not mistaken for some other 'Int'. It also keeps a non-positive value out of a place
that means a positive timeout (see @runWarp@).
-}
newtype ShutdownDrainTimeout = ShutdownDrainTimeout Int
    deriving stock (Eq, Show)

{- | The default graceful-drain bound: 30 seconds. That is long enough for an
in-flight metadata fetch or a moderate artifact stream to finish during a rolling
deploy. It is short enough that a stuck request cannot pin the old instance
indefinitely.
-}
defaultShutdownDrainTimeout :: ShutdownDrainTimeout
defaultShutdownDrainTimeout = ShutdownDrainTimeout 30
