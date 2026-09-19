-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Test and bench fixtures for driving "Ecluse.Core.Rules", shared by the suites and the
performance harnesses so neither wires the boot-bound capabilities the live composition injects.
-}
module Ecluse.Test.Rules (
    -- * Boot-bound capability fixtures
    inertRuleDeps,

    -- * Precedence pairing
    atDefaultPrecedence,

    -- * Fixed-verdict prepared rules
    constRule,
    admitRule,
    denyRule,
    cannotVetRule,

    -- * Reading back a decision
    admittedBy,
    blockedBy,
    isApproved,
    isUndecidable,
    isBlockedByDefault,

    -- * Reading back a verdict
    isAllow,
    isDeny,
    isNoDecision,
    isCannotVet,
    isUnavailable,

    -- * Shaping a version under test
    withInstallScripts,

    -- * One-call packument evaluation
    filterPlan,
) where

import Ecluse.Core.Breaker (noBreakerReporter)
import Ecluse.Core.Package (
    CodeExecSignal (RunsCodeOnInstall),
    PackageInfo (infoVersions),
 )
import Ecluse.Core.Package.Filter (FilterPlan, filterPlanFromDecisions)
import Ecluse.Core.Rules (
    PreparedRule (PreparedRule, prepAdvisoryGate, prepEval, prepName, prepPrecedence, prepResilience),
    RuleDeps (..),
    evalRules,
    noSourceReporter,
    prepare,
 )
import Ecluse.Core.Rules.Freshness (AdvisoryFreshness (AdvisoryFresh))
import Ecluse.Core.Rules.Types (
    Decision (Admitted, Blocked, BlockedByDefault, Undecidable),
    EvalContext,
    Fact (Known),
    FailureAlignment (FailDeny),
    PrecededRule (PrecededRule),
    Rule,
    RuleEvaluation (Unavailable),
    RuleEvidence (evInstallCode),
    RuleVerdict (Allow, CannotVet, Deny, NoDecision),
    completeEvidence,
    defaultPrecedence,
 )

{- | Rule capabilities with no advisory database and no breaker observer. The CVE rules abstain, so
a suite or bench that does not test the advisory path needs no capability wiring.
-}
inertRuleDeps :: RuleDeps
inertRuleDeps =
    RuleDeps
        { rdWithCveLookup = \use -> use Nothing
        , rdCurrentAdvisoryEtag = pure Nothing
        , rdBreakerReporter = noBreakerReporter
        , rdSourceReporter = noSourceReporter
        , rdAdvisoryFreshness = pure AdvisoryFresh
        }

{- | Pair a rule with its type's 'defaultPrecedence'. The live policy instead assigns each rule its
configured precedence ("Ecluse.Config.Rule").
-}
atDefaultPrecedence :: Rule -> PrecededRule
atDefaultPrecedence r = PrecededRule (defaultPrecedence r) r

{- | A prepared rule returning a fixed verdict, so an evaluation reaches a chosen decision
independent of the version under test.
-}
constRule :: Text -> RuleVerdict -> PreparedRule
constRule ruleName verdict =
    PreparedRule
        { prepName = ruleName
        , prepPrecedence = 0
        , prepResilience = Nothing
        , prepAdvisoryGate = Nothing
        , prepEval = \_ _ -> pure verdict
        }

{- | The three fixed-verdict rules the admission and worker suites reuse. 'cannotVetRule' models
an absent advisory database, so a fail-closed evaluation reaches an undecidable decision.
-}
admitRule, denyRule, cannotVetRule :: PreparedRule
admitRule = constRule "test-admit" (Allow "admitted for test")
denyRule = constRule "test-deny" (Deny Nothing "denied by current policy")
cannotVetRule = constRule "test-cannot-vet" (CannotVet FailDeny "no advisory database is loaded")

-- | The rule name credited for an admission or a block, if any (the engine credits by name).
admittedBy, blockedBy :: Decision -> Maybe Text
admittedBy = \case
    Admitted ruleName _ _ -> Just ruleName
    _ -> Nothing
blockedBy = \case
    Blocked ruleName _ _ -> Just ruleName
    _ -> Nothing

isApproved, isUndecidable :: Decision -> Bool
isApproved = \case
    Admitted{} -> True
    _ -> False
isUndecidable = \case
    Undecidable{} -> True
    _ -> False

-- | Whether the deny-by-default floor decided, rather than any rule.
isBlockedByDefault :: Decision -> Bool
isBlockedByDefault = \case
    BlockedByDefault{} -> True
    _ -> False

-- | Which arm one rule's verdict took, for a case that decides on the arm and not its payload.
isAllow, isDeny, isNoDecision, isCannotVet :: RuleVerdict -> Bool
isAllow = \case
    Allow{} -> True
    _ -> False
isDeny = \case
    Deny{} -> True
    _ -> False
isNoDecision = \case
    NoDecision{} -> True
    _ -> False
isCannotVet = \case
    CannotVet{} -> True
    _ -> False

-- | Whether a resilient evaluation reported its source out rather than reaching a verdict.
isUnavailable :: RuleEvaluation -> Bool
isUnavailable = \case
    Unavailable{} -> True
    _ -> False

-- | Mark the version as running code on install, so the install-script deny fires.
withInstallScripts :: RuleEvidence -> RuleEvidence
withInstallScripts ev = ev{evInstallCode = Known (RunsCodeOnInstall "postinstall hook")}

{- | Decide a single public packument against a rule set in one call. A spec or bench exercises the
real engine and the real survivor resolution without wiring the staged serve path itself.
-}
filterPlan :: RuleDeps -> EvalContext -> [PrecededRule] -> PackageInfo -> IO FilterPlan
filterPlan deps ctx rules info = do
    prepared <- prepare deps rules
    decisions <- traverse (evalRules ctx prepared . completeEvidence) (infoVersions info)
    pure (filterPlanFromDecisions decisions)
