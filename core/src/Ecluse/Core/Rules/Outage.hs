-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Bounded reporting of an advisory source the rules cannot consult. Each advisory-reading
evaluation reports what it saw, and the state machine turns that stream into three reports: the
outage began, it continues (at most once per period), and it recovered. A request never produces
a report of its own, so a sustained outage costs the log one line per period, whatever the traffic.
-}
module Ecluse.Core.Rules.Outage (
    -- * What one evaluation saw
    SourceHealth (..),
    SourceReporter (..),
    reportSource,
    noSourceReporter,

    -- * The transition machine
    OutageState (..),
    OngoingOutage (..),
    OutageReport (..),
    stepOutage,

    -- * A reporter over shared state
    sourceReporter,
) where

import Data.Map.Strict qualified as Map
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)

import Ecluse.Core.Rules.Types (Reason)

-- | What one rule's evaluation established about the source it reads.
data SourceHealth
    = -- | The named rule consulted its source, whatever it decided.
      SourceAnswered Text
    | -- | The named rule could not consult its source, for the given cause.
      SourceUnavailable Text Reason
    deriving stock (Eq, Show)

-- | The observer every advisory-reading evaluation reports to. The composition root installs the live one.
newtype SourceReporter = SourceReporter (SourceHealth -> IO ())

-- | Report one evaluation's reading.
reportSource :: SourceReporter -> SourceHealth -> IO ()
reportSource (SourceReporter report) = report

-- | The inert reporter, for an ecosystem with no advisory source to observe.
noSourceReporter :: SourceReporter
noSourceReporter = SourceReporter (const pass)

-- | One source's outage state.
data OutageState
    = Healthy
    | Outage OngoingOutage
    deriving stock (Eq, Show)

{- | An outage names every rule still unable to consult the source, so a rule whose breaker is
still open keeps the outage open after a sibling's probe succeeds.
-}
data OngoingOutage = OngoingOutage
    { ooSince :: UTCTime
    , ooReportedAt :: UTCTime
    -- ^ When the last report went out, which paces the reminder.
    , ooRules :: Map Text Reason
    -- ^ Each rule still unable to consult the source, with its latest cause. Never empty.
    }
    deriving stock (Eq, Show)

-- | The three reports an outage produces, each carrying what an operator line needs.
data OutageReport
    = -- | The first rule unable to consult the source, with its cause.
      OutageBegan Text Reason
    | -- | The reminder: when the outage began, and every rule still unable, with its latest cause.
      OutageContinues UTCTime (Map Text Reason)
    | -- | Every rule consults the source again. Carries when the outage began.
      OutageRecovered UTCTime
    deriving stock (Eq, Show)

{- | Fold one reading into the state at @now@. A transition reports at once, and an outage that
continues reports again only once @period@ has passed since its last report.
-}
stepOutage :: NominalDiffTime -> UTCTime -> SourceHealth -> OutageState -> (OutageState, Maybe OutageReport)
stepOutage period now health current = case (current, health) of
    (Healthy, SourceAnswered _) -> (Healthy, Nothing)
    (Healthy, SourceUnavailable rule cause) ->
        (Outage (OngoingOutage now now (Map.singleton rule cause)), Just (OutageBegan rule cause))
    (Outage ongoing, SourceAnswered rule)
        | Map.null rules -> (Healthy, Just (OutageRecovered (ooSince ongoing)))
        | otherwise -> (Outage ongoing{ooRules = rules}, Nothing)
      where
        rules = Map.delete rule (ooRules ongoing)
    (Outage ongoing, SourceUnavailable rule cause)
        | diffUTCTime now (ooReportedAt ongoing) >= period ->
            (Outage ongoing{ooReportedAt = now, ooRules = rules}, Just (OutageContinues (ooSince ongoing) rules))
        | otherwise -> (Outage ongoing{ooRules = rules}, Nothing)
      where
        rules = Map.insert rule cause (ooRules ongoing)

{- | A reporter folding into one source's shared state. Off the steady state it reads no clock and
runs no transaction, so a healthy source costs an evaluation one memory read.
-}
sourceReporter :: NominalDiffTime -> IO UTCTime -> TVar OutageState -> (OutageReport -> IO ()) -> SourceReporter
sourceReporter period clock shared emit = SourceReporter $ \health ->
    readTVarIO shared >>= \case
        Healthy | SourceAnswered _ <- health -> pass
        _ -> do
            now <- clock
            report <- atomically $ do
                current <- readTVar shared
                let (next, report) = stepOutage period now health current
                writeTVar shared next
                pure report
            traverse_ emit report
