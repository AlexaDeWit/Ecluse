-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Test and bench fixtures for driving "Ecluse.Core.Rules", shared by the suites and the
performance harnesses so neither wires the boot-bound capabilities the live composition injects.
-}
module Ecluse.Test.Rules (
    -- * Boot-bound capability fixtures
    inertRuleDeps,
    servingRuleDeps,
    slotRuleDeps,

    -- * Precedence pairing
    atDefaultPrecedence,

    -- * One version under one rule
    evalRule,

    -- * Fixed-verdict prepared rules
    constRule,
    admitRule,
    denyRule,
    cannotVetRule,

    -- * Reasons for a fixed verdict
    remediation,
    revocation,
    exposure,

    -- * Package-read prepared rules
    packageRule,
    mapPackageRead,
    mapResilience,

    -- * Reading back a decision
    admittedBy,
    blockedBy,
    isApproved,
    isUndecidable,
    isBlockedByDefault,
    sentences,

    -- * Reading back a verdict
    isAllow,
    isDeny,
    isNoDecision,
    isCannotVet,
    isUnavailable,
    verdictSentence,

    -- * Shaping a version under test
    withInstallScripts,

    -- * One-call packument evaluation
    filterPlan,
) where

import Ecluse.Core.Breaker (noBreakerReporter)
import Ecluse.Core.Cve (CveLookup)
import Ecluse.Core.Cve.Slot (CveSlot, currentAdvisoryEtag, withSlotGeneration)
import Ecluse.Core.Cve.Types (DbEtag)
import Ecluse.Core.Package (
    CodeExecSignal (RunsCodeOnInstall),
    PackageInfo (infoVersions),
 )
import Ecluse.Core.Package.Filter (FilterPlan, filterPlanFromDecisions)
import Ecluse.Core.Rules (
    AdvisoryAlignment (AdvisoryAlignment),
    AdvisoryDatabase (AdvisoryDatabase, NoAdvisoryDatabase),
    PackageRead (..),
    PreparedRule (PreparedRule, prepEval, prepName, prepPrecedence),
    Resilience,
    RuleDeps (..),
    RuleEval (PerPackage, PerVersion),
    VerdictSource (FromAdvisories, FromEvidence),
    newEvaluator,
    noSourceReporter,
    prepare,
    readAdvisories,
    verdictSource,
 )
import Ecluse.Core.Rules.Freshness (AdvisoryFreshness (AdvisoryFresh))
import Ecluse.Core.Rules.Render (renderInability, renderReason)
import Ecluse.Core.Rules.Types (
    AdvisoryScore (Cvss),
    Decision (Admitted, Blocked, BlockedByDefault, Undecidable),
    EvalContext,
    Fact (Known),
    FailureAlignment (FailDeny),
    Inability (NoDatabaseLoaded),
    PrecededRule (PrecededRule),
    Reason (AffectedBy, IdentityRevoked, Remediates),
    Rule,
    RuleEvaluation (Unavailable),
    RuleEvidence (evInstallCode, evName),
    RuleVerdict (Allow, CannotVet, Deny, NoDecision),
    completeEvidence,
    defaultPrecedence,
    mkAdvisoryIds,
 )

{- | Rule capabilities with no advisory database configured and no observers. Each advisory rule
returns its fixed no-database verdict, so a suite that does not test the advisory path wires nothing.
-}
inertRuleDeps :: RuleDeps
inertRuleDeps =
    RuleDeps
        { rdAdvisoryDatabase = NoAdvisoryDatabase
        , rdCurrentAdvisoryEtag = pure Nothing
        , rdBreakerReporter = noBreakerReporter
        , rdSourceReporter = noSourceReporter
        , rdAdvisoryFreshness = pure AdvisoryFresh
        }

-- | 'inertRuleDeps' with a database configured and the given generation serving.
servingRuleDeps :: DbEtag -> CveLookup -> RuleDeps
servingRuleDeps etag cve = inertRuleDeps{rdAdvisoryDatabase = AdvisoryDatabase (\use -> use (Just (etag, cve)))}

-- | 'inertRuleDeps' reading whichever generation the slot serves, as a synced mount does.
slotRuleDeps :: CveSlot -> RuleDeps
slotRuleDeps slot = inertRuleDeps{rdAdvisoryDatabase = AdvisoryDatabase (withSlotGeneration slot), rdCurrentAdvisoryEtag = currentAdvisoryEtag slot}

{- | Pair a rule with its type's 'defaultPrecedence'. The live policy instead assigns each rule its
configured precedence ("Ecluse.Config.Rule").
-}
atDefaultPrecedence :: Rule -> PrecededRule
atDefaultPrecedence r = PrecededRule (defaultPrecedence r) r

{- | One rule's verdict for one version. An advisory rule reads its rows directly, with no gate and no
resilience, so a lookup fault escapes.
-}
evalRule :: RuleDeps -> EvalContext -> Rule -> RuleEvidence -> IO RuleVerdict
evalRule deps ctx rule ev = case verdictSource rule of
    FromEvidence verdict -> pure (verdict ctx ev)
    FromAdvisories _ verdict -> (`verdict` ev) <$> readAdvisories deps (evName ev)

{- | A prepared rule returning a fixed verdict, so an evaluation reaches a chosen decision
independent of the version under test.
-}
constRule :: Text -> RuleVerdict -> PreparedRule
constRule ruleName verdict =
    PreparedRule
        { prepName = ruleName
        , prepPrecedence = 0
        , prepEval = PerVersion (\_ _ -> pure verdict)
        }

{- | An advisory rule on a fresh push whose read runs the given effect and finds no rows. Every
version takes the given verdict, and a faulted read resolves under the given alignment.
-}
packageRule :: Text -> Int -> FailureAlignment -> Maybe Resilience -> IO () -> RuleVerdict -> PreparedRule
packageRule ruleName prec alignment resilience effect verdict =
    PreparedRule
        { prepName = ruleName
        , prepPrecedence = prec
        , prepEval =
            PerPackage
                PackageRead
                    { prFreshness = pure AdvisoryFresh
                    , prResilience = resilience
                    , prRows = \_ -> Nothing <$ effect
                    , prAlignment = AdvisoryAlignment FailDeny alignment
                    , prVerdict = \_ _ -> verdict
                    , prReporter = noSourceReporter
                    }
        }

-- | Adjust a prepared advisory rule's package read. A per-version rule has none.
mapPackageRead :: (PackageRead -> PackageRead) -> PreparedRule -> PreparedRule
mapPackageRead adjust rule = case prepEval rule of
    PerPackage packageRead -> rule{prepEval = PerPackage (adjust packageRead)}
    PerVersion _ -> rule

-- | Adjust the resilience around a prepared rule's package read.
mapResilience :: (Resilience -> Resilience) -> PreparedRule -> PreparedRule
mapResilience adjust = mapPackageRead (\packageRead -> packageRead{prResilience = adjust <$> prResilience packageRead})

{- | The three fixed-verdict rules the admission and worker suites reuse. 'cannotVetRule' models
an absent advisory database, so a fail-closed evaluation reaches an undecidable decision.
-}
admitRule, denyRule, cannotVetRule :: PreparedRule
admitRule = constRule "test-admit" (Allow remediation)
denyRule = constRule "test-deny" (Deny Nothing revocation)
cannotVetRule = constRule "test-cannot-vet" (CannotVet FailDeny NoDatabaseLoaded)

{- | The reasons a fixed-verdict rule gives where a case reads the verdict and not the reason: an
allow's, a deny's that names no advisory generation, and a deny's that names one.
-}
remediation, revocation, exposure :: Reason
remediation = Remediates (mkAdvisoryIds ("GHSA-test-0001" :| []))
revocation = IdentityRevoked "thing"
exposure = AffectedBy Cvss 7.0 (mkAdvisoryIds ("GHSA-test-0001" :| []))

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

-- | A decision's reasons as the sentences a reader sees, for a case that pins the text.
sentences :: Decision -> [Text]
sentences = \case
    Admitted _ reason _ -> [renderReason reason]
    Blocked _ _ reason -> [renderReason reason]
    BlockedByDefault reasons -> map renderReason reasons
    Undecidable _ reason -> [renderReason reason]

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

{- | A verdict's reason as the sentence a reader sees. An inability reads without its rule's name,
which a decision adds.
-}
verdictSentence :: RuleVerdict -> Text
verdictSentence = \case
    Allow reason -> renderReason reason
    Deny _ reason -> renderReason reason
    NoDecision reason -> renderReason reason
    CannotVet _ why -> renderInability why

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
    decide <- prepare deps rules >>= newEvaluator ctx
    decisions <- traverse (decide . completeEvidence) (infoVersions info)
    pure (filterPlanFromDecisions decisions)
