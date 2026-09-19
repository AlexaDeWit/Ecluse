-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The HTTP front door: the raw @wai@ 'Application', its dispatch, the middleware stack, and
'runWarp'. It is a raw 'Application' rather than a framework because matching on @pathInfo@
keeps the encoded-slash handling and the streaming control the proxy depends on
(@docs\/architecture\/web-layer.md@). Dispatch matches a request's leading segments to a
configured 'MountBinding', strips the prefix, and asks that mount's router what the remainder
names, so this module holds no path grammar and no status of its own. A path under no mount is
the neutral @404@, and @\/livez@ and @\/readyz@ are answered above the mounts. "Ecluse.Runtime.Server.Internal" implements it.
-}
module Ecluse.Runtime.Server (
    -- * The WAI application
    ServerConfig (..),
    mkServerConfig,
    MountBinding (..),
    application,
    tracedApplication,

    -- * Running the server
    runWarp,
    raceServerAgainstLoop,
    probeApplication,
    probeOnlyApplication,

    -- * Graceful shutdown
    DrainSignal,
    newDrainSignal,
    neverDraining,
    beginDrain,
    isDraining,
    ShutdownDrainTimeout (..),
    defaultShutdownDrainTimeout,

    -- * Local-dev immediate halt
    InteractiveHalt (..),
    defaultInteractiveHalt,
    withInteractiveHalt,

    -- * Middleware
    serverMiddleware,
) where

import Ecluse.Runtime.Server.Internal (
    DrainSignal,
    InteractiveHalt (..),
    MountBinding (..),
    ServerConfig (..),
    ShutdownDrainTimeout (..),
    application,
    beginDrain,
    defaultInteractiveHalt,
    defaultShutdownDrainTimeout,
    isDraining,
    mkServerConfig,
    neverDraining,
    newDrainSignal,
    probeApplication,
    probeOnlyApplication,
    raceServerAgainstLoop,
    runWarp,
    serverMiddleware,
    tracedApplication,
    withInteractiveHalt,
 )
