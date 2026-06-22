{- | The policy rules engine.

A rule set is evaluated against a single 'PackageDetails' snapshot to produce a
'Decision'. The model is __deny by default; precedence decides__: each rule
carries an integer precedence ('PrecededRule'), and the highest-precedence rule
that does not abstain wins. At equal precedence a deny beats an allow, and the
built-in deny defaults sit strictly above the allow defaults
('defaultPrecedence'), so "any deny overrides any allow" holds out of the box.
If every rule abstains the package is denied by default. Because precedence —
not list order — decides, the rule set is order-independent except for the
equal-precedence deny tiebreak and the order abstain reasons are gathered.

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

import Data.Foldable (maximumBy)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, diffUTCTime)
import Ecluse.Package
import Ecluse.Rules.Types
import Ecluse.Version (renderVersion)

-- | A stable, human-facing name for a rule (for logs and denial messages).
ruleName :: Rule -> Text
ruleName = \case
    AllowScope{} -> "AllowScope"
    AllowIfPublishedBefore{} -> "AllowIfPublishedBefore"
    DenyHasInstallScripts -> "DenyHasInstallScripts"

{- | Evaluate a single rule against a single package version. Total — a malformed
rule or package yields an outcome, never an exception, so hostile metadata
cannot crash the gate.
-}
evalRule :: EvalContext -> Rule -> PackageDetails -> RuleOutcome
evalRule _ (AllowScope scope) pd =
    case pkgNamespace (pkgName pd) of
        Just s
            | s == scope ->
                Allow ("scope " <> renderScope scope <> " is allow-listed")
        _ ->
            Abstain ("scope is not the allow-listed " <> renderScope scope)
evalRule ctx (AllowIfPublishedBefore minAge) pd =
    case pkgPublishedAt pd of
        Nothing -> Abstain "publish time is unknown"
        Just publishedAt ->
            let age = diffUTCTime (ctxNow ctx) publishedAt
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
evalRule _ DenyHasInstallScripts pd =
    case pkgInstallCode pd of
        RunsCodeOnInstall how -> Deny ("runs code on install: " <> how)
        NoCodeOnInstall -> Abstain "no install-time code execution"
        CodeExecUnknown -> Abstain "install-time code execution not yet determined"

{- | Evaluate a package version against a rule set.

__Precedence decides.__ Every rule that does not abstain is a candidate, and the
__highest-precedence candidate wins__; at equal precedence a 'Deny' beats an
'Allow'. The winner yields 'Approved' or 'Denied'. If no rule takes a position —
every rule abstains, including the empty rule set — the package is
'DeniedByDefault', with every abstain reason collected in list order for the
audit trail and denial message. Only that reason order, and which of two equally
ranked rules is reported, depend on list order; the decision itself does not.
-}
evalRules :: EvalContext -> [PrecededRule] -> PackageDetails -> Decision
evalRules ctx rules pd =
    case nonEmpty candidates of
        Nothing -> DeniedByDefault abstainReasons
        Just cands -> winningDecision (maximumBy (comparing sortKey) cands)
  where
    -- One pass: each rule is either an abstain (its reason, kept in order for
    -- the audit trail) or a candidate that took a position.
    (abstainReasons, candidates) = partitionEithers (map classify rules)

    classify :: PrecededRule -> Either Text Candidate
    classify (PrecededRule prec r) = case evalRule ctx r pd of
        Abstain reason -> Left reason
        Allow reason -> Right (Candidate prec False r reason)
        Deny reason -> Right (Candidate prec True r reason)

    -- Highest precedence wins; at equal precedence a deny (rank 'True') outranks
    -- an allow (rank 'False'), since 'maximumBy' takes the greatest key.
    sortKey :: Candidate -> (Int, Bool)
    sortKey c = (candPrecedence c, candIsDeny c)

    winningDecision :: Candidate -> Decision
    winningDecision c =
        if candIsDeny c
            then Denied (candRule c) (candReason c)
            else Approved (candRule c) (candReason c)

{- | A rule that took a position against the package, carried with the inputs the
winner selection and the resulting 'Decision' need.
-}
data Candidate = Candidate
    { candPrecedence :: Int
    , candIsDeny :: Bool
    , candRule :: Rule
    , candReason :: Text
    }

{- | A human-readable summary of a decision, suitable for logs and the denial
response body.
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
decision messages. Always non-negative.

>>> renderDuration 604800
"7 days"

>>> renderDuration 90
"1 minute"
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
