{- | The policy rules engine.

A rule set is evaluated against a single 'PackageDetails' snapshot to produce a
'Decision'. The model is __deny by default__: a package is allowed only if some
rule explicitly allows it, and the __first decisive rule wins__.

The initial rule set is pure (no IO). Effectful rules (CVE lookups, etc.) are a
later tier layered on top of this one; see @docs\/architecture.md@. The rule
data types live in "Ecluse.Rules.Types".
-}
module Ecluse.Rules (
    ruleName,
    evalRule,
    evalRules,
    renderDecision,
    renderDuration,
) where

import Data.Text qualified as T
import Data.Time (NominalDiffTime, diffUTCTime)
import Ecluse.Package
import Ecluse.Rules.Types

-- | A stable, human-facing name for a rule (for logs and denial messages).
ruleName :: Rule -> Text
ruleName = \case
    AllowScope{} -> "AllowScope"
    AllowIfPublishedBefore{} -> "AllowIfPublishedBefore"

-- | Evaluate a single rule against a single package version. Pure and total.
evalRule :: EvalContext -> Rule -> PackageDetails -> RuleOutcome
evalRule _ (AllowScope scope) pd =
    case packageScope (pkgName pd) of
        Just s
            | s == scope ->
                Allow ("scope " <> renderScope scope <> " is allow-listed")
        _ ->
            Abstain ("scope is not the allow-listed " <> renderScope scope)
evalRule ctx (AllowIfPublishedBefore minAge) pd =
    let age = diffUTCTime (ctxNow ctx) (pkgPublishedAt pd)
     in if age >= minAge
            then
                Allow
                    ( "published "
                        <> renderDuration age
                        <> " ago (at least "
                        <> renderDuration minAge
                        <> " old)"
                    )
            else
                Abstain
                    ( "published only "
                        <> renderDuration age
                        <> " ago, minimum age is "
                        <> renderDuration minAge
                    )

{- | Evaluate a package version against an ordered rule set.

The first rule to produce a decisive outcome ('Allow' or 'Deny') wins. If every
rule abstains the package is denied by default, and all abstain reasons are
collected (in rule order) for the audit trail and denial message.
-}
evalRules :: EvalContext -> [Rule] -> PackageDetails -> Decision
evalRules ctx rules pd = go rules []
  where
    go [] reasons = DeniedByDefault (reverse reasons)
    go (r : rs) reasons =
        case evalRule ctx r pd of
            Allow reason -> Approved r reason
            Deny reason -> Denied r reason
            Abstain reason -> go rs (reason : reasons)

{- | A human-readable summary of a decision, suitable for logs and the
(eventual) denial response body.
-}
renderDecision :: PackageDetails -> Decision -> Text
renderDecision pd decision =
    let subject = renderPackageName (pkgName pd) <> "@" <> renderVersion (pkgVersion pd)
     in case decision of
            Approved rule reason ->
                subject <> " was approved by " <> ruleName rule <> ": " <> reason
            Denied rule reason ->
                subject <> " was denied by " <> ruleName rule <> ": " <> reason
            DeniedByDefault reasons ->
                subject
                    <> " was denied (no rule allowed it)"
                    <> if null reasons
                        then ""
                        else ": " <> T.intercalate "; " reasons

{- | Render a duration as an approximate, human-friendly string for use in
decision messages (e.g. @"7 days"@, @"3 hours"@). Always non-negative.
-}
renderDuration :: NominalDiffTime -> Text
renderDuration d =
    let secs = max 0 (round (realToFrac d :: Double)) :: Integer
     in pick units secs
  where
    units :: [(Text, Integer)]
    units =
        [ ("day", 86400)
        , ("hour", 3600)
        , ("minute", 60)
        ]
    pick [] secs = plural secs "second"
    pick ((unit, size) : rest) secs
        | secs >= size = plural (secs `div` size) unit
        | otherwise = pick rest secs
    plural n unit = show n <> " " <> unit <> (if n == 1 then "" else "s")
