{- | The local-development immediate-halt wiring: an interactive session's
"quit now" key, inert outside a terminal. "Ecluse.Runtime.Server"'s 'runWarp'
wraps the whole run in 'withInteractiveHalt'.
-}
module Ecluse.Runtime.Server.Halt (
    InteractiveHalt (..),
    defaultInteractiveHalt,
    withInteractiveHalt,
) where

import System.Exit (ExitCode (ExitFailure))
import System.IO (hIsTerminalDevice, isEOF)
import System.Posix.Process (exitImmediately)
import UnliftIO.Async (withAsync)

{- | The local-development immediate-halt wiring, as three injection points so its
logic is exercised without a real terminal. It exists only to give an interactive
session a "quit now" key: when the server is attached to a TTY, closing standard
input (Ctrl-D) forces an __immediate__ process exit, aborting any in-progress drain
-- the same hard-stop a second Ctrl-C gives, but on the dev's deliberate signal.

It is __inert outside an interactive terminal__: in production standard input is a
non-TTY or closed, 'haltOnInteractive' returns 'False', and no watcher is installed,
so the signal-driven graceful lifecycle is completely untouched. The TTY guard is
what enforces that zero-production-impact contract (see 'withInteractiveHalt').
-}
data InteractiveHalt = InteractiveHalt
    { haltOnInteractive :: IO Bool
    {- ^ Whether to arm the halt at all -- the production guard. The real wiring is
    "is standard input a terminal?", so a non-interactive process never installs the
    watcher.
    -}
    , awaitHaltSignal :: IO ()
    {- ^ Block until the dev's halt signal. The real wiring reads standard input
    until end-of-input (Ctrl-D); it returns when the watcher should fire.
    -}
    , halt :: IO ()
    {- ^ The halt itself: terminate the process __immediately__, bypassing the drain
    wait. The real wiring is a direct @_exit@ ('exitImmediately'), matching the
    second-Ctrl-C hard stop.
    -}
    }

{- | The real local-dev halt: armed only when standard input is a terminal
('hIsTerminalDevice'), fired by end-of-input on standard input (Ctrl-D), and
halting via 'exitImmediately' -- an immediate @_exit@ that bypasses the graceful
drain, mirroring a second Ctrl-C. The exit status (130) is the conventional
"terminated from the terminal" code.
-}
defaultInteractiveHalt :: InteractiveHalt
defaultInteractiveHalt =
    InteractiveHalt
        { haltOnInteractive = hIsTerminalDevice stdin
        , awaitHaltSignal = awaitStdinEof
        , halt = exitImmediately (ExitFailure 130)
        }
  where
    -- Read and discard standard input until end-of-input. On an interactive
    -- terminal this blocks until the dev presses Ctrl-D (or the stream otherwise
    -- closes); typed lines in between are consumed and ignored -- the watcher only
    -- cares about the close.
    awaitStdinEof :: IO ()
    awaitStdinEof = go
      where
        go =
            isEOF >>= \case
                True -> pass
                False -> void getLine >> go

{- | Run an action with the local-dev immediate-halt watcher armed __only when
interactive__. If 'haltOnInteractive' is 'True', a watcher runs alongside the action
for exactly its lifetime ('withAsync', so it is torn down when the action returns or
is cancelled -- it never lingers); the watcher blocks on 'awaitHaltSignal' and, when
that returns, runs 'halt'. If 'False' -- the production case -- the action runs alone,
with no watcher and no extra thread, so nothing about the graceful lifecycle changes.
-}
withInteractiveHalt :: InteractiveHalt -> IO a -> IO a
withInteractiveHalt ih action =
    haltOnInteractive ih >>= \case
        False -> action
        True -> withAsync (awaitHaltSignal ih >> halt ih) (const action)
