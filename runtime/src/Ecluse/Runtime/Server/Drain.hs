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

{- | The shutdown-drain flag the front door observes during a graceful rollover. Nothing
lowers it again. The readiness probe and the going-away middleware read it per request.
-}
data DrainSignal = DrainSignal
    { drainState :: STM Bool
    -- ^ Whether the instance is draining: 'False' while serving, 'True' once raised.
    , drainRaise :: STM ()
    -- ^ Raise the flag. Idempotent: a second raise is a no-op.
    }

{- | Allocate a live drain signal, lowered. @runWarp@ allocates one per launch and raises
it from the shutdown signal handler.
-}
newDrainSignal :: IO DrainSignal
newDrainSignal = do
    tvar <- newTVarIO False
    pure
        DrainSignal
            { drainState = readTVar tvar
            , drainRaise = writeTVar tvar True
            }

{- | The inert drain signal: permanently lowered, and raising it does nothing. It is the
@mkServerConfig@ default, so a socket-free test reports ready and stamps no going-away header.
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

{- | The bound on the graceful drain, in seconds. The server stops accepting connections,
waits this long for in-flight requests and artifact streams, then exits regardless.
-}
newtype ShutdownDrainTimeout = ShutdownDrainTimeout Int
    deriving stock (Eq, Show)

{- | The default graceful-drain bound: 30 seconds. It covers an in-flight metadata fetch or
a moderate artifact stream, and stops a stuck request pinning the old instance.
-}
defaultShutdownDrainTimeout :: ShutdownDrainTimeout
defaultShutdownDrainTimeout = ShutdownDrainTimeout 30
