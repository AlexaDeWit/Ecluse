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
    staticAdvisoryDatabase,
    withAdvisoryFreshness,

    -- * Remediation rules
    shippedRemediation,
    differentialRemediation,
    remediationWith,

    -- * Precedence and reach
    atDefaultPrecedence,
    atPrecedence,
    admissionOnly,

    -- * One version under one rule
    evalRule,

    -- * Evaluation
    newEvaluator,
    evalRules,

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
    AdvisoryGeneration (..),
    PackageRead (..),
    PreparedRule (PreparedRule, prepEval, prepName, prepPrecedence),
    Resilience,
    RuleDeps (..),
    RuleEval (PerPackage, PerVersion),
    VerdictSource (FromAdvisories, FromEvidence),
    evalRules,
    newEvaluator,
    noSourceReporter,
    prepare,
    readAdvisories,
    verdictSource,
    withCveLookup,
 )
import Ecluse.Core.Rules.Freshness (AdvisoryFreshness (AdvisoryFresh))
import Ecluse.Core.Rules.Render (renderInability, renderReason)
import Ecluse.Core.Rules.Types (
    AdvisoryScore (Cvss),
    AllowIfRemediatesCveParams (AllowIfRemediatesCveParams),
    Decision (Admitted, Blocked, BlockedByDefault, Undecidable),
    EvalContext,
    Fact (Known),
    FailureAlignment (FailDeny),
    Inability (NoDatabaseLoaded),
    PrecededRule (PrecededRule),
    Reason (AffectedBy, IdentityRevoked, Remediates),
    RemainingAdvisories (Differential),
    Rule (AllowIfRemediatesCve),
    RuleEvaluation (Unavailable),
    RuleEvidence (evInstallCode, evName),
    RuleReach (AdmissionAndRevocation, AdmissionOnly),
    RuleVerdict (Allow, CannotVet, Deny, NoDecision),
    completeEvidence,
    defaultDifferentialLimits,
    defaultPrecedence,
    mkAdvisoryIds,
 )

-- | Rule capabilities without a configured database or observers, returning fixed no-database verdicts.
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
servingRuleDeps etag cve = inertRuleDeps{rdAdvisoryDatabase = staticAdvisoryDatabase (pure AdvisoryFresh) (\use -> use (Just (etag, cve)))}

-- | Read a fixture slot whose publications are always treated as fresh.
slotRuleDeps :: CveSlot -> RuleDeps
slotRuleDeps slot =
    inertRuleDeps
        { rdAdvisoryDatabase = AdvisoryDatabase $ \use ->
            withSlotGeneration slot $ \generation ->
                use
                    AdvisoryGeneration
                        { agFreshness = pure AdvisoryFresh
                        , agLookup = \readLookup -> readLookup ((\(etag, lookup, _) -> (etag, lookup)) <$> generation)
                        }
        , rdCurrentAdvisoryEtag = currentAdvisoryEtag slot
        }

-- | Inject a fixed age with a test lookup that carries no publication metadata.
staticAdvisoryDatabase :: IO AdvisoryFreshness -> (forall a. (Maybe (DbEtag, CveLookup) -> IO a) -> IO a) -> AdvisoryDatabase
staticAdvisoryDatabase freshness lookup = AdvisoryDatabase (\use -> use AdvisoryGeneration{agFreshness = freshness, agLookup = lookup})

-- | Replace both age capabilities for a fixture whose publication metadata stays fixed.
withAdvisoryFreshness :: IO AdvisoryFreshness -> RuleDeps -> RuleDeps
withAdvisoryFreshness freshness deps =
    deps
        { rdAdvisoryFreshness = freshness
        , rdAdvisoryDatabase = case rdAdvisoryDatabase deps of
            NoAdvisoryDatabase -> NoAdvisoryDatabase
            AdvisoryDatabase borrow -> AdvisoryDatabase (\use -> borrow (\generation -> use generation{agFreshness = freshness}))
        }

-- | The remediation rule @config/default.yaml@ ships: deny on unavailable evidence, differential at the shipped limits.
shippedRemediation :: Rule
shippedRemediation = differentialRemediation FailDeny

-- | The remediation rule at the shipped limits, with the given choice for unavailable evidence.
differentialRemediation :: FailureAlignment -> Rule
differentialRemediation alignment = remediationWith alignment (Differential defaultDifferentialLimits)

-- | A remediation rule with the given choices for unavailable evidence and for remaining advisories.
remediationWith :: FailureAlignment -> RemainingAdvisories -> Rule
remediationWith alignment remaining = AllowIfRemediatesCve (AllowIfRemediatesCveParams alignment remaining)

-- | Use the rule type's 'defaultPrecedence'. Configuration supplies precedence for live policies.
atDefaultPrecedence :: Rule -> PrecededRule
atDefaultPrecedence r = atPrecedence (defaultPrecedence r) r

-- | A rule at an explicit precedence, applying at both phases as an entry with no @appliesTo@ does.
atPrecedence :: Int -> Rule -> PrecededRule
atPrecedence prec = PrecededRule prec AdmissionAndRevocation

-- | Limit a rule to admission, as @appliesTo: [admission]@ does.
admissionOnly :: PrecededRule -> PrecededRule
admissionOnly (PrecededRule prec _ rule) = PrecededRule prec AdmissionOnly rule

-- | Decide one version directly. Advisory rules skip age and resilience, and query faults escape.
evalRule :: RuleDeps -> EvalContext -> Rule -> RuleEvidence -> IO RuleVerdict
evalRule deps ctx rule ev = case verdictSource rule of
    FromEvidence verdict -> pure (verdict ctx ev)
    FromAdvisories _ verdict -> (`verdict` ev) <$> withCveLookup deps (readAdvisories (evName ev))

-- | Return a fixed verdict for any version.
constRule :: Text -> RuleVerdict -> PreparedRule
constRule ruleName verdict =
    PreparedRule
        { prepName = ruleName
        , prepPrecedence = 0
        , prepEval = PerVersion (\_ _ -> pure verdict)
        }

-- | Read no rows on a fresh push, run the supplied effect and resolve faults under the given alignment.
packageRule :: Text -> Int -> FailureAlignment -> Maybe Resilience -> IO () -> RuleVerdict -> PreparedRule
packageRule ruleName prec alignment resilience effect verdict =
    PreparedRule
        { prepName = ruleName
        , prepPrecedence = prec
        , prepEval =
            PerPackage
                PackageRead
                    { prRead = \_ use -> use AdvisoryFresh (Nothing <$ effect)
                    , prResilience = resilience
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

-- | Return an undecidable verdict for an absent advisory database.
admitRule, denyRule, cannotVetRule :: PreparedRule
admitRule = constRule "test-admit" (Allow remediation)
denyRule = constRule "test-deny" (Deny Nothing revocation)
cannotVetRule = constRule "test-cannot-vet" (CannotVet FailDeny NoDatabaseLoaded)

-- | Fixed allow, identity-deny and advisory-deny reasons for fixtures that check the verdict.
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

-- | Render a verdict's reason without the rule name that a decision adds.
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

-- | Decide one public packument through the engine and survivor resolution without the staged serve path.
filterPlan :: RuleDeps -> EvalContext -> [PrecededRule] -> PackageInfo -> IO FilterPlan
filterPlan deps ctx rules info = do
    decide <- prepare deps rules >>= newEvaluator ctx
    decisions <- traverse (decide . completeEvidence) (infoVersions info)
    pure (filterPlanFromDecisions decisions)
