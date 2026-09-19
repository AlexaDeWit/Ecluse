-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The typed process perimeter behind 'Ecluse.run': how one service run ends, the status it
exits with, and what it reports on the way out. "Ecluse" keeps only the entry point in its public
contract, so this is the only module exporting the perimeter, for the composition root and for the
specs that classify an ending directly. Importing it opts out of that contract's stability promise,
as @text@ does.
-}
module Ecluse.Internal (
    ProcessOutcome (..),
    superviseProcess,
    exitCodeFor,
    exitReasonFor,
) where

import Control.Exception (AsyncException (ThreadKilled, UserInterrupt), SomeAsyncException)
import Control.Exception qualified as Exception
import System.Exit (ExitCode (ExitFailure, ExitSuccess))

import Ecluse.Boot (BootAborted (BootAborted))
import Ecluse.Core.Text (displayExceptionT)

{- | How one whole service run ended. Each constructor owns one exit code ('exitCodeFor'), so
an orchestrator reads the ending from the status alone.
-}
data ProcessOutcome
    = -- | The services drained and returned (a graceful shutdown): exit 0.
      ShutdownRequested
    | -- | A service failed up with the carried rendered fault: exit 1.
      ServiceExited Text
    | -- | The boot aborted ('BootAborted') with the carried rendered refusal: exit 2.
      BootFault Text
    | -- | The run was cancelled from outside (a kill, an interrupt): exit 3.
      RunCancelled
    deriving stock (Eq, Show)

{- | Run the service under the typed process perimeter and classify its ending. The base
'Exception.try' and 'Exception.throwIO' are deliberate: what leaves here async must leave async.
-}
superviseProcess :: IO ProcessOutcome -> IO ProcessOutcome
superviseProcess service =
    Exception.try service >>= \case
        Right outcome -> pure outcome
        Left err
            | Just (BootAborted rendered) <- fromException err -> pure (BootFault rendered)
            | Just (code :: ExitCode) <- fromException err -> Exception.throwIO code
            | Just (killed :: AsyncException) <- fromException err ->
                pure $ case killed of
                    ThreadKilled -> RunCancelled
                    UserInterrupt -> RunCancelled
                    -- StackOverflow / HeapOverflow: resource exhaustion is a
                    -- fault of the run, not a cancellation.
                    other -> ServiceExited (displayExceptionT other)
            | Just (_ :: SomeAsyncException) <- fromException err -> Exception.throwIO err
            | otherwise -> pure (ServiceExited (displayExceptionT err))

{- How a run ends. A failing status is representable only beside the reason it reports, so
'Ecluse.run' cannot exit non-zero in silence. -}
data ProcessExit
    = ExitedCleanly
    | ExitedWith ExitCode Text

-- The status and the report one outcome owns. Both 'exitCodeFor' and 'exitReasonFor' read it.
processExitFor :: ProcessOutcome -> ProcessExit
processExitFor = \case
    ShutdownRequested -> ExitedCleanly
    ServiceExited detail -> ExitedWith (ExitFailure 1) ("ecluse: service exited: " <> detail)
    -- The boot phase rendered the whole aggregated block, which reports here unprefixed.
    BootFault rendered -> ExitedWith (ExitFailure 2) rendered
    RunCancelled -> ExitedWith (ExitFailure 3) "ecluse: run cancelled"

-- | The process exit status each 'ProcessOutcome' owns.
exitCodeFor :: ProcessOutcome -> ExitCode
exitCodeFor outcome = case processExitFor outcome of
    ExitedCleanly -> ExitSuccess
    ExitedWith code _ -> code

-- | What an outcome reports before exiting. A graceful shutdown alone has nothing to say.
exitReasonFor :: ProcessOutcome -> Maybe Text
exitReasonFor outcome = case processExitFor outcome of
    ExitedCleanly -> Nothing
    ExitedWith _ reason -> Just reason
