-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

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

{- | The local-development immediate-halt wiring, as injection points a test drives without
a terminal. Ctrl-D exits the process at once and aborts the drain. It is inert on a non-TTY.
-}
data InteractiveHalt = InteractiveHalt
    { haltOnInteractive :: IO Bool
    -- ^ Whether to arm the halt. A non-interactive process never installs the watcher.
    , awaitHaltSignal :: IO ()
    {- ^ Block until the dev's halt signal. The real wiring reads standard input
    until end-of-input (Ctrl-D). It returns when the watcher should fire.
    -}
    , halt :: IO ()
    {- ^ Stop the process immediately, bypassing the drain wait. The real wiring is a direct
    @_exit@ ('exitImmediately').
    -}
    }

{- | The real local-dev halt: armed only on a terminal and fired by end-of-input, exiting at
once without a drain. Status 130 is the conventional "terminated from the terminal" code.
-}
defaultInteractiveHalt :: InteractiveHalt
defaultInteractiveHalt =
    InteractiveHalt
        { haltOnInteractive = hIsTerminalDevice stdin
        , awaitHaltSignal = awaitStdinEof
        , halt = exitImmediately (ExitFailure 130)
        }
  where
    -- On a terminal, end-of-input arrives when the dev presses Ctrl-D.
    awaitStdinEof :: IO ()
    awaitStdinEof = go
      where
        go =
            isEOF >>= \case
                True -> pass
                False -> void getLine >> go

{- | Run an action with the immediate-halt watcher armed only when 'haltOnInteractive' is
'True'. The watcher lives exactly as long as the action, so it never outlives it.
-}
withInteractiveHalt :: InteractiveHalt -> IO a -> IO a
withInteractiveHalt ih action =
    haltOnInteractive ih >>= \case
        False -> action
        True -> withAsync (awaitHaltSignal ih >> halt ih) (const action)
