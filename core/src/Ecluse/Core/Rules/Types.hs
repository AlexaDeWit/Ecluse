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
    DenyIfCveParams (..),
    DenyIfEpssParams (..),
    ruleName,
    readsAdvisories,

    -- * Precedence
    PrecededRule (..),
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
    Reason,
    RuleVerdict (..),
    RuleEvaluation (..),
    FailureAlignment (..),
    Decision (..),

    -- * Unavailability
    Transience (..),
    RetryAfter (..),
) where

import Data.Time (NominalDiffTime, UTCTime)
import Ecluse.Core.Cve (DbEtag)
import Ecluse.Core.Fault (RetryAfter (..))
import Ecluse.Core.Package (
    CodeExecSignal,
    PackageDetails (pkgInstallCode, pkgName, pkgPublishedAt, pkgVersion),
    PackageName,
    Scope,
 )
import Ecluse.Core.Version (Version)

{- | The closed built-in rule vocabulary accepted from configuration.
'Ecluse.Core.Rules.prepare' binds capabilities without accepting arbitrary evaluation closures.
-}
data Rule
    = -- | Unconditionally allow every package under the given scope.
      AllowScope Scope
    | {- | Delay new versions to give malicious publishes time to be detected and removed.
      Allow only after the configured publish age.
      -}
      AllowIfOlderThan NominalDiffTime
    | {- | Deny install-time code execution through npm scripts, RubyGems native builds, or PyPI sdist build backends.
      Abstain when the package carries no install-time execution signal.
      -}
      DenyInstallTimeExecution
    | {- | A hard deny for a specific package or package@version. Evaluated at top
      precedence (above AllowScope) as a post-mirror revocation mechanism.
      -}
      DenyByIdentity Text
    | {- | Allow an exact package identity, optionally with a version, including fixes the remediation probe cannot match.
      Default precedence overrides advisory denies but yields to install-code and identity denies.
      -}
      AllowByIdentity Text
    | {- | Admit an exact advisory fix without quarantine when no advisory still affects the version.
      Consult the local database and abstain when it is absent or either condition fails.
      -}
      AllowIfRemediatesCve
    | {- | Opt-in denial for affected versions meeting the severity threshold, including historical mirror dependencies.
      'DenyIfCveParams' governs missing scores and unavailable lookups.
      -}
      DenyIfCve DenyIfCveParams
    | {- | Deny on a known EPSS score at or above the threshold. Individual missing scores
      abstain, including malware without CVE aliases. 'DenyIfCve' governs severity independently.
      -}
      DenyIfEpss DenyIfEpssParams
    deriving stock (Eq, Show)

{- | 'DenyIfCve''s configured behaviour: a separate record rather than fields on
the constructor, so its selectors stay total under the sum (@-Wpartial-fields@).
-}
data DenyIfCveParams = DenyIfCveParams
    { dicMinCvss :: Double
    {- ^ CVSS threshold (0 to 10). Qualitative labels use their band's ceiling.
    Missing scores satisfy every threshold, so unscored malware remains denied.
    -}
    , dicOnUnavailable :: FailureAlignment
    {- ^ Resolve an unavailable advisory lookup: 'FailDeny' refuses by default.
    'FailNoDecision' skips the rule and records the reason in the decision's audit trail.
    -}
    }
    deriving stock (Eq, Show)

-- | 'DenyIfEpss''s configured behaviour, the EPSS twin of 'DenyIfCveParams'.
data DenyIfEpssParams = DenyIfEpssParams
    { dieMinEpss :: Double
    {- ^ The EPSS probability (0 to 1) at or above which an affecting advisory denies.
    An individual missing score supplies no denial.
    -}
    , dieOnUnavailable :: FailureAlignment
    -- ^ How the rule resolves when the advisory database cannot answer, as 'dicOnUnavailable'.
    }
    deriving stock (Eq, Show)

{- | A stable, human-facing name for a rule: its identity, derived from the data. It is
the boot-order tiebreak and the credited identity in logs and denial messages.
-}
ruleName :: Rule -> Text
ruleName = \case
    AllowScope{} -> "AllowScope"
    AllowIfOlderThan{} -> "AllowIfOlderThan"
    DenyInstallTimeExecution -> "DenyInstallTimeExecution"
    DenyByIdentity{} -> "DenyByIdentity"
    AllowByIdentity{} -> "AllowByIdentity"
    AllowIfRemediatesCve -> "AllowIfRemediatesCve"
    DenyIfCve{} -> "DenyIfCve"
    DenyIfEpss{} -> "DenyIfEpss"

{- | Whether a rule reads the advisory database. A rule set with none never needs one, and a set
with one is worth waiting a bounded while for the first sync before deciding anything.
-}
readsAdvisories :: Rule -> Bool
readsAdvisories = \case
    AllowIfRemediatesCve -> True
    DenyIfCve{} -> True
    DenyIfEpss{} -> True
    AllowScope{} -> False
    AllowIfOlderThan{} -> False
    DenyInstallTimeExecution -> False
    DenyByIdentity{} -> False
    AllowByIdentity{} -> False

{- | A rule with explicit precedence, ordered highest first and then by name through 'Ecluse.Core.Rules.bootOrder'.
No derived 'Ord' defines policy order.
-}
data PrecededRule = PrecededRule
    { rulePrecedence :: Int
    -- ^ The precedence at which this rule competes. Higher wins.
    , prRule :: Rule
    -- ^ The rule itself.
    }
    deriving stock (Eq, Show)

{- | Use the rule type's default when configuration omits precedence. See 'Ecluse.Core.Rules.bootOrder' for tie-breaking.
Identity allows override both advisory denies, while install-code and identity denies outrank every allow.
-}
defaultPrecedence :: Rule -> Int
defaultPrecedence = \case
    AllowIfOlderThan{} -> defaultAllowIfOlderThanPrecedence
    AllowIfRemediatesCve -> defaultAllowIfRemediatesCvePrecedence
    AllowScope{} -> defaultAllowScopePrecedence
    DenyIfCve{} -> defaultDenyIfCvePrecedence
    DenyIfEpss{} -> defaultDenyIfEpssPrecedence
    AllowByIdentity{} -> defaultAllowByIdentityPrecedence
    DenyInstallTimeExecution -> defaultDenyInstallTimeExecutionPrecedence
    DenyByIdentity{} -> defaultDenyByIdentityPrecedence

{- | Default precedence of 'AllowIfOlderThan': the lowest band, a passive
quarantine that yields to an explicit allow-list and to every deny.
-}
defaultAllowIfOlderThanPrecedence :: Int
defaultAllowIfOlderThanPrecedence = 100

{- | Admit security fixes ahead of quarantine.
Yield to 'AllowScope', whose trusted packages need no advisory probe.
-}
defaultAllowIfRemediatesCvePrecedence :: Int
defaultAllowIfRemediatesCvePrecedence = 150

{- | Default precedence of 'AllowScope': above the passive age quarantine, because an
explicit allow-list is a stronger statement than the time gate. Still below every deny.
-}
defaultAllowScopePrecedence :: Int
defaultAllowScopePrecedence = 200

{- | Outrank quarantine, remediation, and scope allows.
Yield to 'AllowByIdentity' so an operator can override an advisory denial.
-}
defaultDenyIfCvePrecedence :: Int
defaultDenyIfCvePrecedence = 225

{- | Default precedence of 'DenyIfEpss': the same rung as 'DenyIfCve', which reads the
same database and answers to the same identity-pin override. A tie resolves by name.
-}
defaultDenyIfEpssPrecedence :: Int
defaultDenyIfEpssPrecedence = defaultDenyIfCvePrecedence

{- | An identity pin overrides both advisory denies.
'DenyInstallTimeExecution' and 'DenyByIdentity' retain higher default precedence.
-}
defaultAllowByIdentityPrecedence :: Int
defaultAllowByIdentityPrecedence = 250

{- | Default precedence of 'DenyInstallTimeExecution': the deny band, strictly above
every allow default, so a matching deny overrides any allow out of the box.
-}
defaultDenyInstallTimeExecutionPrecedence :: Int
defaultDenyInstallTimeExecutionPrecedence = 300

{- Default precedence of 'DenyByIdentity': the top precedence, strictly above
every other rule (including explicit allow-lists), to serve as a hard revocation.
-}
defaultDenyByIdentityPrecedence :: Int
defaultDenyByIdentityPrecedence = 400

{- | Whether the evidence set carries one fact. 'Known' wraps the fact's own vocabulary, so a
determined absence ('Known' 'Nothing') stays distinct from 'Unread', which is no reading at all.
-}
data Fact a
    = -- | The fact was read, and is whatever it says.
      Known a
    | -- | Nothing read this fact, so a rule that needs it cannot decide.
      Unread
    deriving stock (Eq, Show)

{- | What the engine reads about one version, one entry per fact the rule vocabulary consults.
Identity is unconditional, because a store listing establishes it without any metadata read.
-}
data RuleEvidence = RuleEvidence
    { evName :: PackageName
    -- ^ The package identity, which every rule reads.
    , evVersion :: Version
    -- ^ The version under evaluation.
    , evPublishedAt :: Fact (Maybe UTCTime)
    -- ^ The publish time, absent from some cheap metadata views even when the manifest was read.
    , evInstallCode :: Fact CodeExecSignal
    -- ^ Whether installing the version executes code.
    }
    deriving stock (Eq, Show)

-- | Every fact present, the shape the serve, admission, and mirror paths always hold.
completeEvidence :: PackageDetails -> RuleEvidence
completeEvidence pd =
    RuleEvidence
        { evName = pkgName pd
        , evVersion = pkgVersion pd
        , evPublishedAt = Known (pkgPublishedAt pd)
        , evInstallCode = Known (pkgInstallCode pd)
        }

{- | Identity alone, which an authenticated store listing establishes with no manifest. A rule
reading any further fact cannot decide over it.
-}
identityEvidence :: PackageName -> Version -> RuleEvidence
identityEvidence name version =
    RuleEvidence
        { evName = name
        , evVersion = version
        , evPublishedAt = Unread
        , evInstallCode = Unread
        }

-- | Ambient information a rule may need that is not part of the package itself.
data EvalContext = EvalContext
    { ctxNow :: UTCTime
    -- ^ The wall-clock "now" for age-based rules.
    , ctxAdvisoryEtag :: Maybe DbEtag
    {- ^ The advisory generation active when the audit line emits, or 'Nothing' when none is loaded.
    A shadow swap means this need not identify the generation used for evaluation.
    -}
    }
    deriving stock (Eq, Show)

{- | Build the shared context from the mount's injected clock, never an ad-hoc wall clock.
The advisory ETag is audit-only and cannot affect the decision.
-}
mkEvalContext :: IO UTCTime -> IO (Maybe DbEtag) -> IO EvalContext
mkEvalContext now advisoryEtag = EvalContext <$> now <*> advisoryEtag

-- | A human-facing reason a rule attaches to its result, kept for the audit trail.
type Reason = Text

{- | A deterministic verdict that the harness never retries. 'Allow', 'Deny', and fail-closed 'CannotVet' are decisive.
Other verdict reasons enter the deny-by-default audit trail in boot order.
-}
data RuleVerdict
    = -- | This rule admits the package (with a human reason). Decisive.
      Allow Reason
    | -- | A decisive denial, with its acquired advisory ETag or none for a non-advisory rule.
      Deny (Maybe DbEtag) Reason
    | -- | This rule has no opinion. The reason stays for the audit trail. A no-op.
      NoDecision Reason
    | {- | Deterministic inability to vet: an absent database, or a fact nothing read. Never enters
      retry or breaker handling. 'FailDeny' yields 'Undecidable'. 'FailNoDecision' abstains.
      -}
      CannotVet FailureAlignment Reason
    deriving stock (Eq, Show)

{- | The harness alone creates 'Unavailable' from faults. Decisive verdicts and fail-closed faults determine the decision.
Other outcomes contribute their reasons to the audit trail.
-}
data RuleEvaluation
    = -- | The rule returned a verdict, and the harness takes it at face value.
      Decided RuleVerdict
    | {- | A harness-observed IO fault, timeout, or open breaker, with retry advice in 'Transience'.
      'FailDeny' yields 'Undecidable'. 'FailNoDecision' abstains.
      -}
      Unavailable Transience FailureAlignment Reason
    deriving stock (Eq, Show)

{- | Choose refusal or abstention when a rule cannot vet or its evaluation faults.
There is no failure alignment that admits unvetted bytes.
-}
data FailureAlignment
    = -- | __Fail closed.__ An uncomputable result is decisive: the version is not admitted.
      FailDeny
    | -- | __Fail open.__ An uncomputable result is a no-op: the rule simply does not fire.
      FailNoDecision
    deriving stock (Eq, Show)

{- | The overall decision for a package version against a whole rule set. It credits the
deciding rule by __name__ (see 'ruleName'), independent of how the engine evaluates it.
-}
data Decision
    = -- | Admitted by the named rule, with its reason.
      Admitted Text Reason
    | -- | Blocked by the named rule, with the advisory ETag that supplied its evidence and its reason.
      Blocked Text (Maybe DbEtag) Reason
    | {- | No rule was decisive. Deny-by-default; carries every non-decisive reason,
      in boot order, so the denial response can explain what was considered.
      -}
      BlockedByDefault [Reason]
    | {- | A fail-closed rule won without vetting the version. Packuments omit it, and artifact requests return an error.
      'Transience' selects the error status and retry advice. The reason remains available for audit.
      -}
      Undecidable Transience Reason
    deriving stock (Eq, Show)

{- | Serve transient outages, rate limits, timeouts, and open breakers as @503@.
Serve internal or parse faults as @500@. 'WillResolve' and 'WontResolve' encode that distinction.
-}
data Transience
    = {- | A retry may succeed after an outage, timeout, or open breaker.
      The optional 'RetryAfter' suggests a client delay.
      -}
      WillResolve (Maybe RetryAfter)
    | {- | Not expected to self-heal (an internal or parse error). Retrying cannot
      help, so the request is a @500@, never a @503@.
      -}
      WontResolve
    deriving stock (Eq, Show)
