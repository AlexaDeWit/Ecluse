-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The sentences a reader sees for what the rules return. A rule returns facts, and a log line,
a denial body, or an audit line asks here for the text. Each render is total over its typed value,
so a reader needs no handler around it.
-}
module Ecluse.Core.Rules.Render (
    -- * A decision and its parts
    renderDecision,
    renderReason,
    renderInability,
    renderDuration,
) where

import Data.Scientific (FPFormat (Fixed), formatScientific, fromFloatDigits)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, nominalDiffTimeToSeconds)

import Ecluse.Core.Package (renderPackageName, renderScope)
import Ecluse.Core.Rules.Types
import Ecluse.Core.Text.Iso8601 (renderIso8601Utc)
import Ecluse.Core.Version (renderVersion)

-- | A decision as one line, for logs and the denial response body.
renderDecision :: RuleEvidence -> Decision -> Text
renderDecision ev decision =
    let subject = renderPackageName (evName ev) <> "@" <> renderVersion (evVersion ev)
     in case decision of
            Admitted name reason skipped ->
                subject <> " was approved by " <> name <> ": " <> renderReason reason <> renderSkippedChecks skipped
            Blocked name _ reason ->
                subject <> " was denied by " <> name <> ": " <> renderReason reason
            BlockedByDefault reasons ->
                subject
                    <> " was denied (no rule allowed it)"
                    <> if null reasons
                        then ""
                        else ": " <> T.intercalate "; " (map renderReason reasons)
            Undecidable _ reason ->
                subject <> " could not be evaluated: " <> renderReason reason

-- The evidence as a parenthetical, so an admission's line never reads as if every check passed.
renderSkippedChecks :: [SkippedCheck] -> Text
renderSkippedChecks [] = ""
renderSkippedChecks checks = " (" <> T.intercalate "; " (map render checks) <> ")"
  where
    render = \case
        SkippedUnavailable rule cause -> "skipped for unavailability: " <> rule <> " (" <> renderInability cause <> ")"
        Unreached rule -> "not reached: " <> rule

-- | One reason as the sentence the audit trail and a denial body carry.
renderReason :: Reason -> Text
renderReason = \case
    ScopeAllowListed scope -> "scope " <> renderScope scope <> " is allow-listed"
    ScopeNotAllowListed scope -> "scope is not the allow-listed " <> renderScope scope
    PublishedLongEnough age minAge ->
        "published " <> renderDuration age <> " ago (at least " <> renderDuration minAge <> " old)"
    PublishedTooRecently age minAge ->
        "published only " <> renderDuration age <> " ago, minimum age is " <> renderDuration minAge
    PublishTimeUnknown -> "publish time is unknown"
    RunsOnInstall how -> "runs code on install: " <> how
    NothingRunsOnInstall -> "no install-time code execution"
    InstallCodeUndetermined -> "install-time code execution not yet determined"
    VersionYanked -> "version is yanked"
    VersionNotYanked -> "version is not yanked"
    VersionDeprecated -> "version is deprecated"
    VersionNotDeprecated -> "version is not deprecated"
    IdentityRevoked ident -> "identity " <> ident <> " is revoked by operator"
    IdentityNotRevoked ident -> "identity is not the revoked " <> ident
    IdentityAllowListed ident -> "identity " <> ident <> " is allow-listed by operator"
    IdentityNotAllowListed ident -> "identity is not the allow-listed " <> ident
    Remediates ids -> "remediates " <> listed ids
    RemediatesWithinLimits compared ->
        "remediates " <> listed (scFixed compared) <> " with " <> listed (scRemaining compared) <> " remaining inside the limits" <> renderComparison compared
    FixesButStillAffected ids open -> "fixes " <> listed ids <> " with " <> listed open <> " remaining"
    RemediationBeyondLimits compared shortfalls ->
        "fixes "
            <> listed (scFixed compared)
            <> " with "
            <> listed (scRemaining compared)
            <> " remaining outside the limits: "
            <> T.intercalate ", " (map renderShortfall (toList (unLimitShortfalls shortfalls)))
            <> renderComparison compared
    FixStillApplies ids -> "every advisory naming this version as its fix still applies to it: " <> listed ids
    FixesNoAdvisory -> "no advisory names this version as its fix"
    AffectedBy score threshold ids ->
        "affected by " <> listed ids <> " (" <> scoreName score <> " >= " <> show threshold <> ")"
    KnownMalicious ids -> "affected by " <> listed ids <> " (known malicious)"
    NoKnownMaliciousAdvisory -> "no known malicious advisory affects this version"
    NotAffectedAtThreshold score ->
        "no advisory at or above the " <> scoreName score <> " threshold affects this version"
    RuleUnable rule inability -> rule <> ": " <> renderInability inability

-- | Why a rule could not vet, without the rule's name, which its holder prints beside it.
renderInability :: Inability -> Text
renderInability = \case
    PublishTimeUnread -> "the publish time is not available"
    InstallSignalUnread -> "the install-time execution signal is not available"
    AvailabilityUnread -> "the availability flag is not available"
    NoDatabaseLoaded -> "no advisory database loaded"
    -- The age, the maximum it passed, and when the push landed, so an operator can tell an
    -- update outage from a maximum set too short.
    PushPastMaximum observed ->
        "the advisory push is "
            <> renderDuration (advisoryAge observed)
            <> " old, past the maximum of "
            <> renderDuration (advisoryMaxAge observed)
            <> " (pushed at "
            <> renderIso8601Utc (advisoryPushedAt observed)
            <> ")"
    PushUndated -> "the object store reported no publication time for the serving advisory artifact"
    SourceBreakerOpen -> "the rule source circuit breaker is open"
    EvaluationFailed -> "the rule could not be evaluated"
    RuleThrew thrown -> "the rule threw: " <> thrown
    AttemptTimedOut -> "the attempt timed out"

listed :: AdvisoryIds -> Text
listed = T.intercalate ", " . toList . unAdvisoryIds

renderComparison :: ScoreComparison -> Text
renderComparison compared =
    " (" <> T.intercalate ", " (sideFacts Cvss "inclusive" (scCvss compared) (dlMaxRemainingCvss limits) (dlCvssGap limits) <> sideFacts Epss "exclusive" (scEpss compared) (dlMaxRemainingEpss limits) (dlEpssGap limits)) <> ")"
  where
    limits = scLimits compared
    sideFacts score bound sides limit gap =
        [ "remaining " <> scoreName score <> " " <> renderReading (ssHighestRemaining sides)
        , scoreName score <> " ceiling " <> renderScore limit <> " " <> bound
        , "fixed " <> scoreName score <> " " <> renderReading (ssHighestFixed sides)
        ]
            <> [scoreName score <> " gap at least " <> renderScore least | GapOfAtLeast least <- [gap]]

renderShortfall :: LimitShortfall -> Text
renderShortfall = \case
    RemainingAboveCeiling Cvss -> "remaining CVSS above its ceiling"
    RemainingAboveCeiling Epss -> "remaining EPSS at or above its ceiling"
    RemainingUnscored score ids -> "no " <> scoreName score <> " score for " <> listed ids
    FixedUnscored score -> "no fixed " <> scoreName score <> " score"
    GapBelowMinimum score -> scoreName score <> " gap below its minimum"

renderReading :: ScoreReading -> Text
renderReading = \case
    Scored score -> renderScore score
    Unscored -> "unscored"

-- The shortest decimal that reads back as the stored score, never exponent notation.
renderScore :: Double -> Text
renderScore score
    | isNaN score || isInfinite score = show score
    | otherwise = toText (formatScientific Fixed Nothing (fromFloatDigits score))

scoreName :: AdvisoryScore -> Text
scoreName = \case
    Cvss -> "CVSS"
    Epss -> "EPSS"

-- | Keep two non-zero units to distinguish near-threshold durations. Negative values render as zero.
renderDuration :: NominalDiffTime -> Text
renderDuration d = case take 2 (durationComponents secs) of
    [] -> "0 seconds"
    parts -> T.unwords (map renderDurationPart parts)
  where
    secs = max 0 (round (nominalDiffTimeToSeconds d)) :: Integer

durationLadder :: [(Text, Integer)]
durationLadder =
    [ ("day", 86400)
    , ("hour", 3600)
    , ("minute", 60)
    , ("second", 1)
    ]

durationComponents :: Integer -> [(Text, Integer)]
durationComponents = go durationLadder
  where
    go [] _ = []
    go ((unit, size) : rest) r =
        let (q, r') = r `divMod` size
         in [(unit, q) | q > 0] <> go rest r'

-- Render one @(unit, count)@ component, pluralising the unit (@1 minute@, @30 seconds@).
renderDurationPart :: (Text, Integer) -> Text
renderDurationPart (unit, n) = show n <> " " <> unit <> (if n == 1 then "" else "s")
