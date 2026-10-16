-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Fixtures the rules-engine specs share: the fixed instant every age and cooldown
calculation is taken against, the evaluation context at it, the package version under test,
the advisory push-age maximum a seven-day quarantine derives, the states an advisory source can
stand in, and a verdict comparison that ignores advisory order.
-}
module Ecluse.Rules.Support (
    now,
    ctx,
    ctxAt,
    pkg,
    sixDayLimit,
    SourceState (..),
    sourceStateDeps,
    normaliseVerdictIds,
) where

import Data.List.NonEmpty qualified as NE
import Data.Time (UTCTime (UTCTime), addUTCTime, fromGregorian, nominalDay)
import UnliftIO.Exception (throwIO)

import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageDetails (pkgPublishedAt), mkPackageName, mkScope)
import Ecluse.Core.Rules (RuleDeps (rdAdvisoryDatabase))
import Ecluse.Core.Rules.Freshness (AdvisoryFreshness (AdvisoryFresh, AdvisoryUndated), AdvisoryPublication (PublishedAt), MaxAdvisoryAge, assessAdvisoryAge, maxAdvisoryAgeFor)
import Ecluse.Core.Rules.Types (
    AdvisoryIds,
    EvalContext (EvalContext),
    LimitShortfall (RemainingUnscored),
    Reason (AffectedBy, FixStillApplies, FixesButStillAffected, Remediates, RemediatesWithinLimits, RemediationBeyondLimits),
    Rule (AllowIfOlderThan),
    RuleEvidence,
    RuleVerdict (Allow, CannotVet, Deny, NoDecision),
    ScoreComparison (scFixed, scRemaining),
    completeEvidence,
    mkAdvisoryIds,
    mkLimitShortfalls,
    unAdvisoryIds,
    unLimitShortfalls,
 )
import Ecluse.Test.Package (sampleDetails, v1_0_0)
import Ecluse.Test.Rules (inertRuleDeps, staticAdvisoryDatabase, withAdvisoryFreshness)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))

-- | A fixed "now", so age and cooldown arithmetic stay deterministic.
now :: UTCTime
now = UTCTime (fromGregorian 2026 6 20) 0

-- | The request snapshot the age rules read at an instant. A breaker takes its own injected clock instead.
ctxAt :: UTCTime -> EvalContext
ctxAt t = EvalContext t Nothing

ctx :: EvalContext
ctx = ctxAt now

-- | @thing\@1.0.0@ under an optional npm scope, published @ageDays@ days before 'now'.
pkg :: Maybe Text -> Integer -> RuleEvidence
pkg mScope ageDays = completeEvidence details
  where
    details =
        (sampleDetails (mkPackageName Npm (mkScope <$> mScope) "thing") v1_0_0)
            { pkgPublishedAt = Just (addUTCTime (negate (fromInteger ageDays * nominalDay)) now)
            }

-- | The six-day maximum a seven-day quarantine derives, so boundary cases read as an operator's would.
sixDayLimit :: MaxAdvisoryAge
sixDayLimit = maxAdvisoryAgeFor Nothing [AllowIfOlderThan (7 * nominalDay)]

-- | Where an advisory source stands when a rule reads it.
data SourceState = NoSource | Unloaded | Faulting | Expired | Undated | Loaded
    deriving stock (Bounded, Enum, Eq, Show)

-- | The capabilities for a source state. The given capabilities serve the rows once a generation loads.
sourceStateDeps :: SourceState -> RuleDeps -> RuleDeps
sourceStateDeps standing loaded = case standing of
    NoSource -> inertRuleDeps
    Unloaded -> inertRuleDeps{rdAdvisoryDatabase = staticAdvisoryDatabase (pure AdvisoryFresh) (\use -> use Nothing)}
    Faulting -> inertRuleDeps{rdAdvisoryDatabase = staticAdvisoryDatabase (pure AdvisoryFresh) (\_ -> throwIO (TestContractEscape "advisory database exploded"))}
    Expired -> withAdvisoryFreshness (pure (assessAdvisoryAge sixDayLimit now (PublishedAt (addUTCTime (negate (9 * nominalDay)) now)))) loaded
    Undated -> withAdvisoryFreshness (pure AdvisoryUndated) loaded
    Loaded -> loaded

-- | Sort every advisory list a verdict names. Row order is no contract, and membership and multiplicity stay.
normaliseVerdictIds :: RuleVerdict -> RuleVerdict
normaliseVerdictIds = \case
    Allow reason -> Allow (normaliseReason reason)
    Deny etag reason -> Deny etag (normaliseReason reason)
    NoDecision reason -> NoDecision (normaliseReason reason)
    verdict@CannotVet{} -> verdict
  where
    normaliseReason = \case
        Remediates ids -> Remediates (ordered ids)
        RemediatesWithinLimits compared -> RemediatesWithinLimits (comparison compared)
        FixesButStillAffected fixed affected -> FixesButStillAffected (ordered fixed) (ordered affected)
        RemediationBeyondLimits compared shortfalls -> RemediationBeyondLimits (comparison compared) (mkLimitShortfalls (fmap shortfall (unLimitShortfalls shortfalls)))
        FixStillApplies ids -> FixStillApplies (ordered ids)
        AffectedBy score threshold ids -> AffectedBy score threshold (ordered ids)
        reason -> reason
    comparison compared = compared{scFixed = ordered (scFixed compared), scRemaining = ordered (scRemaining compared)}
    shortfall = \case
        RemainingUnscored score ids -> RemainingUnscored score (ordered ids)
        other -> other

ordered :: AdvisoryIds -> AdvisoryIds
ordered = mkAdvisoryIds . NE.sort . unAdvisoryIds
