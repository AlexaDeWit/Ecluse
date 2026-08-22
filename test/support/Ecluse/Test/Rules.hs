-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Test and bench fixtures for driving "Ecluse.Core.Rules".

The module name follows this support library's @Ecluse.X -> Ecluse.Test.X@ convention.
The suites and the performance harnesses share these fixtures to evaluate a rule policy
without the boot-bound capabilities the live composition injects. 'inertRuleDeps' stands
in for the advisory-database and breaker-observer capabilities. 'atDefaultPrecedence'
stands in for the precedence a configured policy assigns explicitly. 'filterPlan'
composes the engine's staged evaluation into the one call a spec or bench drives.
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

{- | Rule capabilities with no advisory database and no breaker observer: the default
for exercising the engine's pure rules. The CVE rules abstain because nothing loads a
database, and breaker transitions go unobserved. A suite or bench that does not test the
advisory path therefore needs no capability wiring.
-}
inertRuleDeps :: RuleDeps
inertRuleDeps =
    RuleDeps
        { rdWithCveLookup = \use -> use Nothing
        , rdCurrentAdvisoryEtag = pure Nothing
        , rdBreakerReporter = noBreakerReporter
        , rdFaultReporter = noFaultReporter
        }

{- | The inert 'FaultReporter': it records nothing. Effectful-rule fault reporting is a
production-only observer, and the live composition logs through it. Every suite that
builds a 'RuleDeps' or 'Resilience' uses this inert stand-in. It lives here rather than
in the library because no library or executable code path uses it.
-}
noFaultReporter :: FaultReporter
noFaultReporter = FaultReporter (\_ _ -> pass)

{- | Pair a rule with its type's 'defaultPrecedence'. The live policy assigns each rule
its configured precedence ("Ecluse.Config.Rule"). This is the fixture form for building
a policy directly from 'Rule' values.
-}
atDefaultPrecedence :: Rule -> PrecededRule
atDefaultPrecedence r = PrecededRule (defaultPrecedence r) r

{- | Decide a single public packument against a rule set in one call. It runs 'prepare'
on the policy, decides every version through 'evalRules', and resolves survivors and
@latest@ with 'filterPlanFromDecisions'. The serve pipeline performs the same
composition in stages. A spec or bench therefore exercises the real engine and the real
survivor resolution without wiring the staged path itself.
-}
filterPlan :: RuleDeps -> EvalContext -> [PrecededRule] -> PackageInfo -> IO FilterPlan
filterPlan deps ctx rules info = do
    prepared <- prepare deps rules
    decisions <- traverse (evalRules ctx prepared) (infoVersions info)
    pure (filterPlanFromDecisions decisions info)
