-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Test and bench fixtures for driving "Ecluse.Core.Rules".

This mirrors the module under test, under the @Ecluse.X -> Ecluse.Test.X@
convention this support library follows: the fixtures the suites and the
performance harnesses share to evaluate a rule policy without the boot-bound
capabilities the live composition injects. 'inertRuleDeps' stands in for the
advisory-database and breaker-observer capabilities, 'atDefaultPrecedence'
stands in for the precedence a configured policy assigns explicitly, and
'filterPlan' composes the engine's staged evaluation into the one call a spec
or bench drives.
-}
module Ecluse.Test.Rules (
    -- * Boot-bound capability fixtures
    inertRuleDeps,
    noFaultReporter,

    -- * Precedence pairing
    atDefaultPrecedence,

    -- * One-call packument evaluation
    filterPlan,
) where

import Ecluse.Core.Package (PackageInfo (infoVersions))
import Ecluse.Core.Package.Filter (FilterPlan, filterPlanFromDecisions)
import Ecluse.Core.Rules (FaultReporter (..), RuleDeps (..), evalRules, noBreakerReporter, prepare)
import Ecluse.Core.Rules.Types (
    EvalContext,
    PrecededRule (PrecededRule),
    Rule,
    defaultPrecedence,
 )

{- | Rule capabilities with no advisory database and no breaker observer: the
default for exercising the engine's pure rules. The CVE rules abstain (no
database is ever loaded) and breaker transitions go unobserved, so a suite or
bench that is not testing the advisory path needs no capability wiring.
-}
inertRuleDeps :: RuleDeps
inertRuleDeps =
    RuleDeps
        { rdWithCveLookup = \use -> use Nothing
        , rdCurrentAdvisoryEtag = pure Nothing
        , rdBreakerReporter = noBreakerReporter
        , rdFaultReporter = noFaultReporter
        }

{- | The inert 'FaultReporter': records nothing. Effectful-rule fault reporting is a
production-only observer (the live composition logs through it), so every suite that
builds a 'RuleDeps' or 'Resilience' uses this inert stand-in. Kept here rather than in
the library because no library or executable code path uses it.
-}
noFaultReporter :: FaultReporter
noFaultReporter = FaultReporter (\_ _ -> pass)

{- | Pair a rule with its type's 'defaultPrecedence'. The live policy assigns
each rule its configured precedence ("Ecluse.Config.Rule"); this is the fixture
form for building a policy directly from 'Rule' values.
-}
atDefaultPrecedence :: Rule -> PrecededRule
atDefaultPrecedence r = PrecededRule (defaultPrecedence r) r

{- | Decide a single public packument against a rule set in one call:
'prepare' the policy, decide every version through 'evalRules', and resolve
survivors and @latest@ with 'filterPlanFromDecisions'. This is the same
composition the serve pipeline performs in stages, so a spec or bench
exercises the real engine and the real survivor resolution without wiring the
staged path itself.
-}
filterPlan :: RuleDeps -> EvalContext -> [PrecededRule] -> PackageInfo -> IO FilterPlan
filterPlan deps ctx rules info = do
    prepared <- prepare deps rules
    decisions <- traverse (evalRules ctx prepared) (infoVersions info)
    pure (filterPlanFromDecisions decisions info)
