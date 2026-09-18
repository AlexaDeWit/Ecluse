-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Bounded reporting of an advisory source the rules cannot consult. Each advisory-reading
evaluation reports what it saw, and the state machine turns that stream into three reports: the
outage began, it continues (at most once per period), and it recovered. The outage also records
which admissions the gate has logged evidence for, once per identity, so a request never produces
a line of its own beyond the first for its identity, whatever the traffic.
-}
module Ecluse.Core.Rules.Outage (
    -- * What one evaluation saw
    SourceHealth (..),
    SourceReporter (..),
    noSourceReporter,

    -- * The transition machine
    OutageState (..),
    OngoingOutage (..),
    OutageReport (..),
    OutageStep (..),
    stepOutage,

    -- * Evidence logged during an outage
    AdmissionIdentity (..),
    LoggedAdmissions,
    noLoggedAdmissions,
    loggedAdmissionCap,
    noteLogged,
    admissionLogged,

    -- * A reporter over shared state
    OutageStore (..),
    tvarOutageStore,
    sourceReporter,
) where

import Data.Map.Strict qualified as Map
import Data.Sequence (Seq ((:<|)), (|>))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
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
data SourceReporter = SourceReporter
    { reportSource :: SourceHealth -> IO ()
    -- ^ Report one evaluation's reading.
    , noteAdmission :: AdmissionIdentity -> IO Bool
    -- ^ Whether the gate logs this admission's evidence: once per identity for the life of the outage.
    }

-- | The inert reporter, for an ecosystem with no advisory source to observe. It logs every admission.
noSourceReporter :: SourceReporter
noSourceReporter = SourceReporter{reportSource = const pass, noteAdmission = const (pure True)}

{- | One source's outage state. It carries no 'Eq': comparing two states walks the logged record,
so a change is decided by the fold that made it, never by comparison.
-}
data OutageState
    = Healthy
    | Outage OngoingOutage
    deriving stock (Show)

{- | An outage names every rule still unable to consult the source, so a rule whose breaker is
still open keeps the outage open after a sibling's probe succeeds.
-}
data OngoingOutage = OngoingOutage
    { ooSince :: UTCTime
    , ooReportedAt :: UTCTime
    -- ^ When the last report went out, which paces the reminder.
    , ooRules :: Map Text Reason
    -- ^ Each rule still unable to consult the source, with its latest cause. Never empty.
    , ooLogged :: LoggedAdmissions
    -- ^ The admissions the gate has logged evidence for during this outage.
    }
    deriving stock (Show)

-- | What identifies one admission's evidence line: the package, the version, and the checks it skipped.
data AdmissionIdentity = AdmissionIdentity
    { aiPackage :: Text
    , aiVersion :: Text
    , aiSkipped :: Set Text
    -- ^ The rules skipped for unavailability. A different set is a different line.
    }
    deriving stock (Eq, Ord, Show)

{- | The admissions logged during one outage, oldest first, so the record can evict in arrival
order once it reaches its cap.
-}
data LoggedAdmissions = LoggedAdmissions
    { laOrder :: Seq AdmissionIdentity
    , laMembers :: Set AdmissionIdentity
    }
    deriving stock (Eq, Show)

-- | An outage that has logged nothing yet.
noLoggedAdmissions :: LoggedAdmissions
noLoggedAdmissions = LoggedAdmissions Seq.empty Set.empty

{- | How many identities one outage remembers: a few megabytes resident per ecosystem at the cap,
past which an outage over a large mirror repeats a line only once its oldest identities age out.
-}
loggedAdmissionCap :: Int
loggedAdmissionCap = 4096

{- | Decide whether the gate logs this admission, and the record after it. A repeat is skipped, a
new identity is recorded, and the oldest identity is evicted once the record holds @cap@.
-}
noteLogged :: Int -> AdmissionIdentity -> LoggedAdmissions -> (LoggedAdmissions, Bool)
noteLogged cap ident logged
    | Set.member ident (laMembers logged) = (logged, False)
    | otherwise = (LoggedAdmissions (order |> ident) (Set.insert ident members), True)
  where
    (order, members) = case laOrder logged of
        oldest :<| rest | Seq.length (laOrder logged) >= cap -> (rest, Set.delete oldest (laMembers logged))
        _ -> (laOrder logged, laMembers logged)

{- | 'noteLogged' over the source's state. A healthy source has no outage to remember the admission
under, so the gate logs it and the state is unchanged.
-}
admissionLogged :: Int -> AdmissionIdentity -> OutageState -> (OutageState, Bool)
admissionLogged cap ident = \case
    Healthy -> (Healthy, True)
    Outage ongoing ->
        let (logged, logIt) = noteLogged cap ident (ooLogged ongoing)
         in (Outage ongoing{ooLogged = logged}, logIt)

-- | The three reports an outage produces, each carrying what an operator line needs.
data OutageReport
    = -- | The first rule unable to consult the source, with its cause.
      OutageBegan Text Reason
    | -- | The reminder: when the outage began, and every rule still unable, with its latest cause.
      OutageContinues UTCTime (Map Text Reason)
    | -- | Every rule consults the source again. Carries when the outage began.
      OutageRecovered UTCTime
    deriving stock (Eq, Show)

-- | What one reading did to the state. The change flag is what decides a commit.
data OutageStep = OutageStep
    { osState :: OutageState
    , osChanged :: Bool
    -- ^ Whether the reading changed anything, so an unchanged reading costs no write.
    , osReport :: Maybe OutageReport
    }

{- | Fold one reading into the state at @now@. A transition reports at once, and an outage that
continues reports again only once @period@ has passed since its last report.
-}
stepOutage :: NominalDiffTime -> UTCTime -> SourceHealth -> OutageState -> OutageStep
stepOutage period now health current = case (current, health) of
    (Healthy, SourceAnswered _) -> unchanged
    (Healthy, SourceUnavailable rule cause) ->
        OutageStep (Outage (OngoingOutage now now (Map.singleton rule cause) noLoggedAdmissions)) True (Just (OutageBegan rule cause))
    (Outage ongoing, SourceAnswered rule)
        | not (Map.member rule (ooRules ongoing)) -> unchanged
        | Map.null rules -> OutageStep Healthy True (Just (OutageRecovered (ooSince ongoing)))
        | otherwise -> OutageStep (Outage ongoing{ooRules = rules}) True Nothing
      where
        rules = Map.delete rule (ooRules ongoing)
    (Outage ongoing, SourceUnavailable rule cause)
        | diffUTCTime now (ooReportedAt ongoing) >= period ->
            OutageStep (Outage ongoing{ooReportedAt = now, ooRules = rules}) True (Just (OutageContinues (ooSince ongoing) rules))
        | Map.lookup rule (ooRules ongoing) == Just cause -> unchanged
        | otherwise -> OutageStep (Outage ongoing{ooRules = rules}) True Nothing
      where
        rules = Map.insert rule cause (ooRules ongoing)
  where
    unchanged = OutageStep current False Nothing

-- | Where one source's outage state lives: a plain read, and a fold committed as one unit.
data OutageStore = OutageStore
    { readOutage :: IO OutageState
    , commitOutage :: forall a. (OutageState -> (OutageState, a)) -> IO a
    -- ^ Fold the state in place and hand back what the fold produced.
    }

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

{- | A reporter folding into one source's store. A healthy source costs an evaluation one read, and
an outage that changes nothing (the same rule, still failing, inside the period) costs one read and
one clock reading, so only a transition or a due reminder commits.
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
