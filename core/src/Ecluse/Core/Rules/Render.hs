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

    -- * Reading a rendered denial back
    cveIdsInReason,
) where

import Data.Text qualified as T
import Data.Time (NominalDiffTime, nominalDiffTimeToSeconds)

import Ecluse.Core.Package (renderPackageName, renderScope)
import Ecluse.Core.Rules.Types
import Ecluse.Core.Text (renderIso8601Utc)
import Ecluse.Core.Version (renderVersion)

{- | A human-readable summary of a decision, suitable for logs and the denial
response body.
-}
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
    IdentityRevoked ident -> "identity " <> ident <> " is revoked by operator"
    IdentityNotRevoked ident -> "identity is not the revoked " <> ident
    IdentityAllowListed ident -> "identity " <> ident <> " is allow-listed by operator"
    IdentityNotAllowListed ident -> "identity is not the allow-listed " <> ident
    Remediates ids -> "remediates " <> listed ids
    FixesButStillAffected ids open ->
        "fixes " <> listed ids <> " but is still affected by " <> listed open
    FixesNoAdvisory -> "no advisory names this version as its fix"
    NoDatabaseToRemediate -> "no advisory database is loaded"
    AffectedBy score threshold ids ->
        "affected by " <> listed ids <> " (" <> scoreName score <> " >= " <> show threshold <> ")"
    NotAffectedAtThreshold score ->
        "no advisory at or above the " <> scoreName score <> " threshold affects this version"
    RuleUnable rule inability -> rule <> ": " <> renderInability inability

{- | Why a rule could not vet, without the rule's name. A record that names the rule in a field
of its own prints this beside it.
-}
renderInability :: Inability -> Text
renderInability = \case
    PublishTimeUnread -> "the publish time is not available"
    InstallSignalUnread -> "the install-time execution signal is not available"
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

-- | Read the advisory identifiers from a scored denial's rendered message, or return none.
cveIdsInReason :: Text -> [Text]
cveIdsInReason message
    | T.null afterThreshold = []
    | otherwise = filter (not . T.null) (map T.strip (T.splitOn ", " ids))
  where
    -- 'stripPrefix' drops the marker without an O(n) 'Data.Text.length' on it (STAN-0208).
    -- An absent marker leaves the body empty, so the guard yields @[]@.
    (_, afterAffected) = T.breakOn "affected by " message
    body = fromMaybe "" (T.stripPrefix "affected by " afterAffected)
    (ids, afterThreshold) = T.breakOn " (" body
