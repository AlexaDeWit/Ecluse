-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The closed rule vocabulary, the evidence a rule reads, rule verdicts, and policy decisions.
"Ecluse.Core.Rules" binds these values to capabilities and evaluates them.
Configuration selects built-in rules and cannot supply evaluation closures.
-}
module Ecluse.Core.Rules.Types (
    -- * The built-in rule vocabulary
    Rule (..),
    RuleType (..),
    ruleType,
    DenyIfCveParams (..),
    DenyIfEpssParams (..),
    AllowIfRemediatesCveParams (..),
    RemainingAdvisories (..),
    DifferentialLimits (..),
    ScoreGap (..),
    defaultDifferentialLimits,
    ruleName,
    readsAdvisories,
    deniesOnAdvisories,
    requiresEpss,
    ruleDenies,

    -- * Precedence and reach
    PrecededRule (..),
    RuleReach (..),
    revocationRules,
    defaultPrecedence,
    defaultAllowIfOlderThanPrecedence,
    defaultAllowIfRemediatesCvePrecedence,
    defaultAllowScopePrecedence,
    defaultDenyIfCvePrecedence,
    defaultDenyIfEpssPrecedence,
    defaultAllowByIdentityPrecedence,
    defaultDenyInstallTimeExecutionPrecedence,

    -- * What a rule reads about one version
    Fact (..),
    RuleEvidence (..),
    completeEvidence,
    identityEvidence,

    -- * Evaluation
    EvalContext (..),
    mkEvalContext,
    RuleVerdict (..),
    RuleEvaluation (..),
    FailureAlignment (..),
    Decision (..),
    SkippedCheck (..),
    skippedChecks,

    -- * Why a rule reached its result
    Reason (..),
    reasonAdvisories,
    AdvisoryIds,
    mkAdvisoryIds,
    unAdvisoryIds,
    Inability (..),
    AdvisoryScore (..),
    ScoreComparison (..),
    ScoreSides (..),
    ScoreReading (..),
    LimitShortfall (..),
    LimitShortfalls,
    mkLimitShortfalls,
    unLimitShortfalls,
    AdvisoryAge (..),

    -- * Unavailability
    Transience (..),
    RetryAfter (..),
) where

import Data.Time (NominalDiffTime, UTCTime)
import Data.Universe.Class (Universe (..))
import Data.Universe.Generic (universeGeneric)
import Ecluse.Core.Cve.Types (DbEtag)
import Ecluse.Core.Fault (RetryAfter (..))
import Ecluse.Core.Package (
    Availability,
    CodeExecSignal,
    DependencySources,
    PackageDetails (pkgAvailability, pkgDependencySources, pkgInstallCode, pkgName, pkgPublishedAt, pkgVersion),
    PackageName,
    Scope,
 )
import Ecluse.Core.Strict (strictElements)
import Ecluse.Core.Version (Version)
import Ecluse.Core.Wire (WireVocab (..), renderWire)

-- | The closed rule vocabulary configuration selects. 'Ecluse.Core.Rules.prepare' binds its capabilities.
data Rule
    = -- | Unconditionally allow every package under the given scope.
      AllowScope Scope
    | -- | Delay new versions, so a malicious publish is found and removed before a build installs it.
      AllowIfOlderThan NominalDiffTime
    | -- | Deny npm versions that declare install scripts. Other adapters refuse this rule at boot.
      DenyInstallTimeExecution
    | -- | Deny a version whose installable dependency a client would fetch from outside the registry, such as an npm git or URL spec.
      DenyExternalDependencies
    | -- | Refuse versions carrying the registry yank flag.
      DenyIfYanked
    | -- | Refuse versions carrying the registry deprecation flag, including obsolete release lines.
      DenyIfDeprecated
    | -- | A hard deny for one package or package@version, at top precedence, so it also revokes mirrored copies.
      DenyByIdentity Text
    | -- | Allow one package or package@version, including a fix the exact fix match cannot recognise.
      AllowByIdentity Text
    | -- | Admit an advisory's exact fix ahead of the quarantine, on the terms 'AllowIfRemediatesCveParams' sets.
      AllowIfRemediatesCve AllowIfRemediatesCveParams
    | -- | Deny versions an advisory at or above the CVSS threshold affects, including historical mirror dependencies.
      DenyIfCve DenyIfCveParams
    | -- | Deny versions affected by an active primary MAL advisory, independently of scores.
      DenyIfKnownMalicious FailureAlignment
    | -- | Deny on a known EPSS score at or above the threshold. An individual missing score abstains.
      DenyIfEpss DenyIfEpssParams
    deriving stock (Eq, Show)

-- | 'DenyIfCve''s configured behaviour, a record so its selectors stay total under the sum.
data DenyIfCveParams = DenyIfCveParams
    { dicMinCvss :: Double
    -- ^ CVSS threshold (0 to 10). A missing score satisfies every threshold, so unscored malware stays denied.
    , dicOnUnavailable :: FailureAlignment
    -- ^ How the rule resolves when the advisory database cannot answer: 'FailNoDecision' skips and records why.
    }
    deriving stock (Eq, Show)

-- | 'DenyIfEpss''s configured behaviour, the EPSS twin of 'DenyIfCveParams'.
data DenyIfEpssParams = DenyIfEpssParams
    { dieMinEpss :: Double
    -- ^ The EPSS probability (0 to 1) at or above which an affecting advisory denies. A missing score supplies no denial.
    , dieOnUnavailable :: FailureAlignment
    -- ^ How the rule resolves when the advisory database cannot answer, as 'dicOnUnavailable'.
    }
    deriving stock (Eq, Show)

-- | 'AllowIfRemediatesCve''s configured behaviour. Configuration states both choices, and nothing defaults them.
data AllowIfRemediatesCveParams = AllowIfRemediatesCveParams
    { aircOnUnavailable :: FailureAlignment
    -- ^ How the rule resolves when the advisory database cannot answer, an expired or undated push included.
    , aircRemaining :: RemainingAdvisories
    -- ^ What the rule does with an exact fix that another advisory still affects.
    }
    deriving stock (Eq, Show)

-- | What the fast lane does with a partial fix: an exact fix that another advisory still affects.
data RemainingAdvisories
    = -- | Abstain, so the partial fix waits out the quarantine.
      AbstainOnRemaining
    | -- | Admit when the remaining advisories' stored scores sit inside the limits.
      Differential DifferentialLimits
    deriving stock (Eq, Show)

-- | The differential's limits. They compare stored scores, which proves no reduced exposure.
data DifferentialLimits = DifferentialLimits
    { dlMaxRemainingCvss :: Double
    -- ^ The CVSS (0 to 10) every remaining advisory must stay at or below.
    , dlMaxRemainingEpss :: Double
    -- ^ The EPSS probability (0 to 1) every remaining advisory must stay strictly below.
    , dlCvssGap :: ScoreGap
    -- ^ How far the highest fixed CVSS must exceed the highest remaining one.
    , dlEpssGap :: ScoreGap
    -- ^ How far the highest fixed EPSS must exceed the highest remaining one, in probability points.
    }
    deriving stock (Eq, Show)

-- | A minimum difference between the highest fixed and the highest remaining score.
data ScoreGap
    = -- | No gap condition, so no fixed score is needed.
      NoGapRequired
    | -- | The highest known fixed score must exceed the highest remaining one by at least this.
      GapOfAtLeast Double
    deriving stock (Eq, Show)

-- | The shipped limits, which fill omitted keys in differential mode only. They are product defaults, not safety boundaries.
defaultDifferentialLimits :: DifferentialLimits
defaultDifferentialLimits =
    DifferentialLimits
        { dlMaxRemainingCvss = 6.9
        , dlMaxRemainingEpss = 0.01
        , dlCvssGap = GapOfAtLeast 1.0
        , dlEpssGap = NoGapRequired
        }

-- | Parameter-free rule identity for configuration and ecosystem blocks.
data RuleType
    = AllowScopeType
    | AllowIfOlderThanType
    | AllowByIdentityType
    | AllowIfRemediatesCveType
    | DenyIfCveType
    | DenyIfKnownMaliciousType
    | DenyIfEpssType
    | DenyIfYankedType
    | DenyIfDeprecatedType
    | DenyInstallTimeExecutionType
    | DenyExternalDependenciesType
    | DenyByIdentityType
    deriving stock (Eq, Generic, Ord, Show)

instance Universe RuleType where universe = universeGeneric

instance WireVocab RuleType where
    wireKind = "rule type"
    wireTable =
        (AllowScopeType, "AllowScope")
            :| [ (AllowIfOlderThanType, "AllowIfOlderThan")
               , (AllowByIdentityType, "AllowByIdentity")
               , (AllowIfRemediatesCveType, "AllowIfRemediatesCve")
               , (DenyIfCveType, "DenyIfCve")
               , (DenyIfKnownMaliciousType, "DenyIfKnownMalicious")
               , (DenyIfEpssType, "DenyIfEpss")
               , (DenyIfYankedType, "DenyIfYanked")
               , (DenyIfDeprecatedType, "DenyIfDeprecated")
               , (DenyInstallTimeExecutionType, "DenyInstallTimeExecution")
               , (DenyExternalDependenciesType, "DenyExternalDependencies")
               , (DenyByIdentityType, "DenyByIdentity")
               ]

-- | Discard a rule's parameters while retaining its canonical type.
ruleType :: Rule -> RuleType
ruleType = \case
    AllowScope{} -> AllowScopeType
    AllowIfOlderThan{} -> AllowIfOlderThanType
    DenyIfYanked -> DenyIfYankedType
    DenyIfDeprecated -> DenyIfDeprecatedType
    DenyInstallTimeExecution -> DenyInstallTimeExecutionType
    DenyExternalDependencies -> DenyExternalDependenciesType
    DenyByIdentity{} -> DenyByIdentityType
    AllowByIdentity{} -> AllowByIdentityType
    AllowIfRemediatesCve{} -> AllowIfRemediatesCveType
    DenyIfCve{} -> DenyIfCveType
    DenyIfKnownMalicious{} -> DenyIfKnownMaliciousType
    DenyIfEpss{} -> DenyIfEpssType

-- | Canonical rule name used for boot-order ties, logs, and denial messages.
ruleName :: Rule -> Text
ruleName = renderWire . ruleType

-- | Whether a rule reads the advisory database, so a rule set holding one waits a bounded while for the first sync.
readsAdvisories :: Rule -> Bool
readsAdvisories = \case
    AllowIfRemediatesCve{} -> True
    DenyIfCve{} -> True
    DenyIfKnownMalicious{} -> True
    DenyIfEpss{} -> True
    AllowScope{} -> False
    AllowIfOlderThan{} -> False
    DenyIfYanked -> False
    DenyIfDeprecated -> False
    DenyInstallTimeExecution -> False
    DenyExternalDependencies -> False
    DenyByIdentity{} -> False
    AllowByIdentity{} -> False

-- | Whether a rule refuses when the advisory database cannot answer, which makes an advisory store mandatory.
deniesOnAdvisories :: Rule -> Bool
deniesOnAdvisories = \case
    DenyIfCve{} -> True
    DenyIfKnownMalicious{} -> True
    DenyIfEpss{} -> True
    AllowIfRemediatesCve params -> aircOnUnavailable params == FailDeny
    AllowScope{} -> False
    AllowIfOlderThan{} -> False
    DenyIfYanked -> False
    DenyIfDeprecated -> False
    DenyInstallTimeExecution -> False
    DenyExternalDependencies -> False
    DenyByIdentity{} -> False
    AllowByIdentity{} -> False

-- | Whether a rule reads EPSS scores, which makes Pilot's enrichment and a consumer's check of it mandatory.
requiresEpss :: Rule -> Bool
requiresEpss = \case
    DenyIfEpss{} -> True
    AllowIfRemediatesCve params -> case aircRemaining params of
        Differential{} -> True
        AbstainOnRemaining -> False
    DenyIfCve{} -> False
    DenyIfKnownMalicious{} -> False
    AllowScope{} -> False
    AllowIfOlderThan{} -> False
    DenyIfYanked -> False
    DenyIfDeprecated -> False
    DenyInstallTimeExecution -> False
    DenyExternalDependencies -> False
    DenyByIdentity{} -> False
    AllowByIdentity{} -> False

-- | Whether a rule can only refuse or abstain, so removing it cannot refuse a version the whole policy admits.
ruleDenies :: Rule -> Bool
ruleDenies = \case
    DenyIfYanked -> True
    DenyIfDeprecated -> True
    DenyInstallTimeExecution -> True
    DenyExternalDependencies -> True
    DenyByIdentity{} -> True
    DenyIfCve{} -> True
    DenyIfKnownMalicious{} -> True
    DenyIfEpss{} -> True
    AllowScope{} -> False
    AllowIfOlderThan{} -> False
    AllowByIdentity{} -> False
    AllowIfRemediatesCve{} -> False

-- | A rule with explicit precedence. 'Ecluse.Core.Rules.bootOrder' orders rules, never a derived 'Ord'.
data PrecededRule = PrecededRule
    { rulePrecedence :: Int
    -- ^ The precedence at which this rule competes. Higher wins.
    , ruleReach :: RuleReach
    -- ^ The phases the rule applies at.
    , prRule :: Rule
    -- ^ The rule itself.
    }
    deriving stock (Eq, Show)

-- | The phases a rule applies at. Every rule applies at admission, so a deny limited to it never removes a stored version.
data RuleReach
    = -- | The gate, the mirror worker's ingest, and the Dredger's revocation all read the rule.
      AdmissionAndRevocation
    | -- | The gate and the mirror worker's ingest read the rule. The Dredger does not.
      AdmissionOnly
    deriving stock (Eq, Show)

-- | The rules the Dredger evaluates. Only a deny may be 'AdmissionOnly', so this never blocks what the policy admits.
revocationRules :: [PrecededRule] -> [PrecededRule]
revocationRules = filter (reachesRevocation . ruleReach)
  where
    reachesRevocation = \case
        AdmissionAndRevocation -> True
        AdmissionOnly -> False

-- | The rule type's precedence when configuration omits one. 'Ecluse.Core.Rules.bootOrder' breaks ties.
defaultPrecedence :: Rule -> Int
defaultPrecedence = \case
    AllowIfOlderThan{} -> defaultAllowIfOlderThanPrecedence
    AllowIfRemediatesCve{} -> defaultAllowIfRemediatesCvePrecedence
    AllowScope{} -> defaultAllowScopePrecedence
    DenyIfCve{} -> defaultDenyIfCvePrecedence
    DenyIfKnownMalicious{} -> defaultDenyInstallTimeExecutionPrecedence
    DenyIfEpss{} -> defaultDenyIfEpssPrecedence
    AllowByIdentity{} -> defaultAllowByIdentityPrecedence
    DenyIfYanked -> defaultDenyInstallTimeExecutionPrecedence
    DenyIfDeprecated -> defaultDenyInstallTimeExecutionPrecedence
    DenyInstallTimeExecution -> defaultDenyInstallTimeExecutionPrecedence
    DenyExternalDependencies -> defaultDenyInstallTimeExecutionPrecedence
    DenyByIdentity{} -> defaultDenyByIdentityPrecedence

-- | Default precedence of 'AllowIfOlderThan': the lowest band, a quarantine that yields to allow-lists and every deny.
defaultAllowIfOlderThanPrecedence :: Int
defaultAllowIfOlderThanPrecedence = 100

-- | Admit security fixes ahead of the quarantine, yielding to 'AllowScope' and to every deny.
defaultAllowIfRemediatesCvePrecedence :: Int
defaultAllowIfRemediatesCvePrecedence = 150

-- | Default precedence of 'AllowScope': above the quarantine, because an allow-list is a stronger statement.
defaultAllowScopePrecedence :: Int
defaultAllowScopePrecedence = 200

-- | Outrank the quarantine, remediation, and scope allows, and yield to an 'AllowByIdentity' override.
defaultDenyIfCvePrecedence :: Int
defaultDenyIfCvePrecedence = 225

-- | Default precedence of 'DenyIfEpss': the rung of 'DenyIfCve', which reads the same database. A tie resolves by name.
defaultDenyIfEpssPrecedence :: Int
defaultDenyIfEpssPrecedence = defaultDenyIfCvePrecedence

-- | An identity pin overrides scored advisory denies, and yields to MAL, availability, install-code, dependency, and identity denies.
defaultAllowByIdentityPrecedence :: Int
defaultAllowByIdentityPrecedence = 250

-- | Default precedence of 'DenyInstallTimeExecution': the deny band, above every allow default.
defaultDenyInstallTimeExecutionPrecedence :: Int
defaultDenyInstallTimeExecutionPrecedence = 300

-- The top precedence, above every other rule, so 'DenyByIdentity' serves as a hard revocation.
defaultDenyByIdentityPrecedence :: Int
defaultDenyByIdentityPrecedence = 400

-- | One fact of the evidence. A determined absence ('Known' 'Nothing') stays distinct from 'Unread', no reading at all.
data Fact a
    = -- | The fact was read, and is whatever it says.
      Known a
    | -- | Nothing read this fact, so a rule that needs it cannot decide.
      Unread
    deriving stock (Eq, Show)

-- | What the engine reads about one version. Identity is unconditional, because a store listing establishes it.
data RuleEvidence = RuleEvidence
    { evName :: PackageName
    -- ^ The package identity, which every rule reads.
    , evVersion :: Version
    -- ^ The version under evaluation.
    , evPublishedAt :: Fact (Maybe UTCTime)
    -- ^ The publish time, absent from some cheap metadata views even when the manifest was read.
    , evAvailability :: Fact Availability
    -- ^ The typed registry flag, without upstream notice text.
    , evInstallCode :: Fact CodeExecSignal
    -- ^ Whether installing the version executes code.
    , evDependencySources :: Fact DependencySources
    -- ^ Whether installing the version fetches a dependency from outside the registry.
    }
    deriving stock (Eq, Show)

-- | Every fact present, the shape the serve, admission, and mirror paths always hold.
completeEvidence :: PackageDetails -> RuleEvidence
completeEvidence pd =
    RuleEvidence
        { evName = pkgName pd
        , evVersion = pkgVersion pd
        , evPublishedAt = Known (pkgPublishedAt pd)
        , evAvailability = Known (pkgAvailability pd)
        , evInstallCode = Known (pkgInstallCode pd)
        , evDependencySources = Known (pkgDependencySources pd)
        }

-- | Identity alone, as an authenticated store listing establishes it. A rule reading any other fact cannot decide.
identityEvidence :: PackageName -> Version -> RuleEvidence
identityEvidence name version =
    RuleEvidence
        { evName = name
        , evVersion = version
        , evPublishedAt = Unread
        , evAvailability = Unread
        , evInstallCode = Unread
        , evDependencySources = Unread
        }

-- | Ambient information a rule may need that is not part of the package itself.
data EvalContext = EvalContext
    { ctxNow :: UTCTime
    -- ^ The wall-clock "now" for age-based rules.
    , ctxAdvisoryEtag :: Maybe DbEtag
    -- ^ The advisory generation active at the audit line. A shadow swap can make it differ from the one evaluated.
    }
    deriving stock (Eq, Show)

-- | Build the context from the mount's injected clock. The advisory ETag is audit-only and cannot affect the decision.
mkEvalContext :: IO UTCTime -> IO (Maybe DbEtag) -> IO EvalContext
mkEvalContext now advisoryEtag = EvalContext <$> now <*> advisoryEtag

-- | A deterministic verdict the harness never retries. 'Allow', 'Deny', and fail-closed 'CannotVet' are decisive.
data RuleVerdict
    = -- | This rule admits the package. Decisive.
      Allow Reason
    | -- | A decisive denial, with its acquired advisory ETag or none for a non-advisory rule.
      Deny (Maybe DbEtag) Reason
    | -- | This rule has no opinion. The reason stays for the audit trail. A no-op.
      NoDecision Reason
    | -- | Deterministic inability to vet, never retried. 'FailDeny' yields 'Undecidable', and 'FailNoDecision' abstains.
      CannotVet FailureAlignment Inability
    deriving stock (Eq, Show)

-- | One rule's evaluation. Only the harness creates 'Unavailable', from faults. Decisive outcomes decide the version, and the rest join the audit trail.
data RuleEvaluation
    = -- | The rule returned a verdict, and the harness takes it at face value.
      Decided RuleVerdict
    | -- | A fault, timeout, or open breaker, with retry advice. 'FailDeny' yields 'Undecidable', and 'FailNoDecision' abstains.
      Unavailable Transience FailureAlignment Inability
    deriving stock (Eq, Show)

-- | Refusal or abstention when a rule cannot vet or its evaluation faults. No alignment admits unvetted bytes.
data FailureAlignment
    = -- | __Fail closed.__ An uncomputable result is decisive: the version is not admitted.
      FailDeny
    | -- | __Fail open.__ An uncomputable result is a no-op: the rule simply does not fire.
      FailNoDecision
    deriving stock (Eq, Show)

-- | The decision for a version against a whole rule set. It credits the deciding rule by name ('ruleName').
data Decision
    = -- | Admitted by the named rule, with the configured checks the admission did not benefit from.
      Admitted Text Reason [SkippedCheck]
    | -- | Blocked by the named rule, with the advisory ETag that supplied its evidence and its reason.
      Blocked Text (Maybe DbEtag) Reason
    | -- | No rule was decisive, so the version is denied. It carries every non-decisive reason, in boot order.
      BlockedByDefault [Reason]
    | -- | A fail-closed rule won without vetting. Packuments omit the version, and artifact requests fail by 'Transience'.
      Undecidable Transience Reason
    deriving stock (Eq, Show)

-- | A configured check the winning allow did not benefit from. A check the engine never ran never reads as passed.
data SkippedCheck
    = -- | The check ran, could not vet the version, and its fail-open alignment let the fold move on.
      SkippedUnavailable Text Inability
    | -- | An earlier allow in the boot order decided, so the engine never ran the check.
      Unreached Text
    deriving stock (Eq, Show)

-- | The skipped-check evidence a decision carries: an admission's, and none for any other outcome.
skippedChecks :: Decision -> [SkippedCheck]
skippedChecks = \case
    Admitted _ _ skipped -> skipped
    Blocked{} -> []
    BlockedByDefault{} -> []
    Undecidable{} -> []

-- | Why a rule reached its result, as facts. 'Ecluse.Core.Rules.Render.renderReason' writes the sentence.
data Reason
    = -- | 'AllowScope' admits: the package is under the scope it allows.
      ScopeAllowListed Scope
    | -- | 'AllowScope' abstains: the package is not under the scope it allows.
      ScopeNotAllowListed Scope
    | -- | 'AllowIfOlderThan' admits: the version's age, then the minimum it met.
      PublishedLongEnough NominalDiffTime NominalDiffTime
    | -- | 'AllowIfOlderThan' abstains: the version's age, then the minimum it has not met.
      PublishedTooRecently NominalDiffTime NominalDiffTime
    | -- | 'AllowIfOlderThan' abstains: the metadata was read and carries no publish time.
      PublishTimeUnknown
    | -- | 'DenyInstallTimeExecution' denies: how installing the version runs code.
      RunsOnInstall Text
    | -- | 'DenyInstallTimeExecution' abstains: installing the version runs no code.
      NothingRunsOnInstall
    | -- | 'DenyInstallTimeExecution' abstains: whether installing runs code is not yet determined.
      InstallCodeUndetermined
    | -- | 'DenyExternalDependencies' denies: the dependency a client would fetch from outside the registry.
      FetchesOutside Text
    | -- | 'DenyExternalDependencies' abstains: every installable dependency resolves through the registry.
      DependenciesThroughRegistry
    | -- | The version carries the yank flag.
      VersionYanked
    | -- | The version carries no yank flag.
      VersionNotYanked
    | -- | The version carries the deprecation flag.
      VersionDeprecated
    | -- | The version carries no deprecation flag.
      VersionNotDeprecated
    | -- | 'DenyByIdentity' denies: the identity the operator revoked.
      IdentityRevoked Text
    | -- | 'DenyByIdentity' abstains: the version is not the identity it revokes.
      IdentityNotRevoked Text
    | -- | 'AllowByIdentity' admits: the identity the operator allow-listed.
      IdentityAllowListed Text
    | -- | 'AllowByIdentity' abstains: the version is not the identity it allows.
      IdentityNotAllowListed Text
    | -- | 'AllowIfRemediatesCve' admits: the advisories this version is the exact fix for, with none still affecting it.
      Remediates AdvisoryIds
    | -- | 'AllowIfRemediatesCve' admits a partial fix whose remaining advisories sit inside the limits.
      RemediatesWithinLimits ScoreComparison
    | -- | 'AllowIfRemediatesCve' abstains on a partial fix: the advisories it genuinely fixes, then those still affecting it.
      FixesButStillAffected AdvisoryIds AdvisoryIds
    | -- | 'AllowIfRemediatesCve' abstains on a partial fix outside the limits, with every condition it failed.
      RemediationBeyondLimits ScoreComparison LimitShortfalls
    | -- | 'AllowIfRemediatesCve' abstains: every advisory naming this version as its fix still affects it through another range.
      FixStillApplies AdvisoryIds
    | -- | 'AllowIfRemediatesCve' abstains: no advisory names this version as its fix.
      FixesNoAdvisory
    | -- | An advisory deny fires: the score it gates on, its threshold, and the affecting advisories.
      AffectedBy AdvisoryScore Double AdvisoryIds
    | -- | Active primary MAL advisories affect the version, independently of scores.
      KnownMalicious AdvisoryIds
    | -- | No active primary MAL advisory affects the version.
      NoKnownMaliciousAdvisory
    | -- | An advisory deny abstains: no advisory at or above its threshold affects the version.
      NotAffectedAtThreshold AdvisoryScore
    | -- | The named rule could not vet the version. The engine's fold builds this, never a rule.
      RuleUnable Text Inability
    deriving stock (Eq, Show)

-- | The advisories still affecting the version, never one it fixes. Every constructor is matched, so a new reason cannot drop out.
reasonAdvisories :: Reason -> [Text]
reasonAdvisories = \case
    Remediates _ -> []
    RemediatesWithinLimits compared -> remaining compared
    FixesButStillAffected _ open -> named open
    RemediationBeyondLimits compared _ -> remaining compared
    FixStillApplies ids -> named ids
    AffectedBy _ _ ids -> named ids
    KnownMalicious ids -> named ids
    ScopeAllowListed{} -> []
    ScopeNotAllowListed{} -> []
    PublishedLongEnough{} -> []
    PublishedTooRecently{} -> []
    PublishTimeUnknown -> []
    RunsOnInstall{} -> []
    NothingRunsOnInstall -> []
    InstallCodeUndetermined -> []
    FetchesOutside{} -> []
    DependenciesThroughRegistry -> []
    VersionYanked -> []
    VersionNotYanked -> []
    VersionDeprecated -> []
    VersionNotDeprecated -> []
    IdentityRevoked{} -> []
    IdentityNotRevoked{} -> []
    IdentityAllowListed{} -> []
    IdentityNotAllowListed{} -> []
    FixesNoAdvisory -> []
    NoKnownMaliciousAdvisory -> []
    NotAffectedAtThreshold{} -> []
    RuleUnable{} -> []
  where
    named = toList . unAdvisoryIds
    remaining = named . scRemaining

-- | The advisories a reason names, at least one. 'mkAdvisoryIds' evaluates every identifier, so no matching is deferred.
newtype AdvisoryIds = AdvisoryIds (NonEmpty Text)
    deriving stock (Eq, Show)

-- | Name the advisories. Evaluating the result evaluates every identifier.
mkAdvisoryIds :: NonEmpty Text -> AdvisoryIds
mkAdvisoryIds = AdvisoryIds . strictElements

-- | The identifiers, in the order the rule found them.
unAdvisoryIds :: AdvisoryIds -> NonEmpty Text
unAdvisoryIds (AdvisoryIds ids) = ids

-- | The score an advisory rule compares.
data AdvisoryScore
    = Cvss
    | Epss
    deriving stock (Eq, Show)

-- | What the differential compared for one partial fix: the advisories on each side, their highest scores, and the limits.
data ScoreComparison = ScoreComparison
    { scFixed :: AdvisoryIds
    -- ^ The advisories the version genuinely fixes.
    , scRemaining :: AdvisoryIds
    -- ^ The advisories that still affect the version.
    , scCvss :: ScoreSides
    , scEpss :: ScoreSides
    , scLimits :: DifferentialLimits
    }
    deriving stock (Eq, Show)

-- | The highest stored score among the genuinely fixed advisories, and among the remaining ones.
data ScoreSides = ScoreSides
    { ssHighestFixed :: ScoreReading
    , ssHighestRemaining :: ScoreReading
    }
    deriving stock (Eq, Show)

-- | A score the advisory artifact stores. 'Unscored' never reads as zero.
data ScoreReading
    = Scored Double
    | Unscored
    deriving stock (Eq, Show)

-- | One differential condition a partial fix failed.
data LimitShortfall
    = -- | The highest remaining score is past its ceiling: above it for CVSS, at or above it for EPSS.
      RemainingAboveCeiling AdvisoryScore
    | -- | These remaining advisories store no score of this kind.
      RemainingUnscored AdvisoryScore AdvisoryIds
    | -- | No genuinely fixed advisory stores a score of this kind, so an enabled gap cannot hold.
      FixedUnscored AdvisoryScore
    | -- | The highest fixed score exceeds the highest remaining one by less than the minimum gap.
      GapBelowMinimum AdvisoryScore
    deriving stock (Eq, Show)

-- | The conditions a partial fix failed, at least one. 'mkLimitShortfalls' evaluates every one.
newtype LimitShortfalls = LimitShortfalls (NonEmpty LimitShortfall)
    deriving stock (Eq, Show)

-- | Name the failed conditions. Evaluating the result evaluates every one.
mkLimitShortfalls :: NonEmpty LimitShortfall -> LimitShortfalls
mkLimitShortfalls = LimitShortfalls . strictElements

-- | The failed conditions, in the order the rule checked them.
unLimitShortfalls :: LimitShortfalls -> NonEmpty LimitShortfall
unLimitShortfalls (LimitShortfalls shortfalls) = shortfalls

-- | Why a rule could not vet a version. Every holder names the rule in a field of its own.
data Inability
    = -- | Nothing read the publish time the rule decides on.
      PublishTimeUnread
    | -- | Nothing read the install-time execution signal the rule decides on.
      InstallSignalUnread
    | -- | Nothing read the availability flag.
      AvailabilityUnread
    | -- | Nothing read where the installable dependencies come from.
      DependencySourcesUnread
    | -- | No advisory database is loaded.
      NoDatabaseLoaded
    | -- | The serving advisory push is older than its maximum.
      PushPastMaximum AdvisoryAge
    | -- | The store gave no publication time for the serving advisory push.
      PushUndated
    | -- | The rule's circuit breaker is open, so the read was not attempted.
      SourceBreakerOpen
    | -- | The read was given up. A client reads this in place of the fault's detail.
      EvaluationFailed
    | -- | The rule or its read threw: the exception as text.
      RuleThrew Text
    | -- | One attempt of the read ran past its timeout.
      AttemptTimedOut
    deriving stock (Eq, Show)

-- | One reading of a push's age against its maximum. An audit line and an alarm render the same reading.
data AdvisoryAge = AdvisoryAge
    { advisoryPushedAt :: UTCTime
    , advisoryAge :: NominalDiffTime
    , advisoryMaxAge :: NominalDiffTime
    }
    deriving stock (Eq, Show)

-- | Whether a retry can help: a transient fault serves @503@, and an internal or parse fault @500@.
data Transience
    = -- | A retry may succeed after an outage, timeout, or open breaker. 'RetryAfter' suggests a client delay.
      WillResolve (Maybe RetryAfter)
    | -- | Not expected to self-heal, so the request is a @500@, never a @503@.
      WontResolve
    deriving stock (Eq, Show)
