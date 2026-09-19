-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Bounded reporting of an advisory source the rules cannot consult. Each advisory-reading
evaluation reports what it saw, and the state machine turns that stream into three reports: the
outage began, it continues (at most once per period), and it recovered. The outage also records
which admissions the gate has logged evidence for, once per identity, so traffic volume cannot
multiply the lines. "Ecluse.Core.Rules.Outage.Internal" holds the fold itself.
-}
module Ecluse.Core.Rules.Outage (
    -- * What one evaluation saw
    SourceHealth (..),
    SourceReporter (..),
    noSourceReporter,

    -- * What the fold produces
    OutageState (Healthy),
    OutageReport (..),
    AdmissionIdentity (..),

    -- * A reporter over shared state
    OutageStore,
    tvarOutageStore,
    sourceReporter,
) where

import Data.Time (NominalDiffTime, UTCTime)

import Ecluse.Core.Rules.Outage.Internal (
    AdmissionIdentity (..),
    OutageReport (..),
    OutageState (Healthy),
    OutageStep (osChanged, osReport, osState),
    OutageStore (OutageStore, commitOutage, readOutage),
    SourceHealth (..),
    admissionLogged,
    loggedAdmissionCap,
    stepOutage,
 )

-- | The observer every advisory-reading evaluation reports to. The composition root installs the live one.
data SourceReporter = SourceReporter
    { reportSource :: SourceHealth -> IO ()
    -- ^ Report one evaluation's reading.
    , noteAdmission :: AdmissionIdentity -> IO Bool
    -- ^ Whether the gate logs this admission's evidence: once per identity for the life of the outage.
    }

-- | The inert reporter, for an ecosystem with no advisory source to observe. It logs every admission.
noSourceReporter :: SourceReporter
noSourceReporter = SourceReporter{reportSource = const pass, noteAdmission = const (pure True)}

-- | The live store: one 'TVar' shared by every mount of an ecosystem.
tvarOutageStore :: TVar OutageState -> OutageStore
tvarOutageStore shared =
    OutageStore
        { readOutage = readTVarIO shared
        , commitOutage = \advance -> atomically $ do
            (next, report) <- advance <$> readTVar shared
            writeTVar shared next
            pure report
        }

{- | A reporter folding into one source's store. Only a transition or a due reminder commits, so
a healthy source costs one read and an unchanged outage one read and one clock reading.
-}
sourceReporter :: NominalDiffTime -> IO UTCTime -> OutageStore -> (OutageReport -> IO ()) -> SourceReporter
sourceReporter period clock store emit = SourceReporter{reportSource = report, noteAdmission = note}
  where
    report health =
        readOutage store >>= \case
            Healthy | SourceAnswered _ <- health -> pass
            current -> do
                now <- clock
                let advance = stepOutage period now health
                -- The commit folds again over the fresh state, so a concurrent change is never lost.
                when (osChanged (advance current)) $
                    commitOutage store (\fresh -> let stepped = advance fresh in (osState stepped, osReport stepped)) >>= traverse_ emit

    -- A repeat inside an outage is decided on the read alone, so only a new identity commits.
    note ident =
        readOutage store >>= \case
            Healthy -> pure True
            current ->
                let advance = admissionLogged loggedAdmissionCap ident
                 in if snd (advance current) then commitOutage store advance else pure False
