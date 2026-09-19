-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Integrity admission, metric projections, and the denial audit trail that the packument
and tarball handlers share and their specs reach directly. Importing this module opts out of
the stability promise of the public hub, "Ecluse.Core.Server.Pipeline".

The @module@ field on every line emitted here is 'pipelineInternalModule', a fixed operator
filter key rather than the source module path.
-}
module Ecluse.Core.Server.Pipeline.Internal (
    -- * The operator log-filter key
    pipelineInternalModule,

    -- * Integrity-floor admission (pure)
    admitByIntegrity,

    -- * Metric-label projections (pure)
    packumentServeDecision,
    statusServeDecision,
    serveDecisionClass,
    denialLabels,
    evalTier,
    transienceCause,

    -- * Metric emits (off a serve outcome)
    recordDenials,
    recordEffectfulFailures,

    -- * Denial audit trail (structured log)
    VersionVerdict (..),
    Metadata (..),
    DenialAudit (..),
    denialAuditPayload,
    logDenials,
    logSkippedChecks,
    logSkippedChecksOnce,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Katip (KatipContext, Severity (WarningS), SimpleLogPayload, katipAddContext, logFM, ls, sl)

import Ecluse.Core.Cve (DbEtag (..))
import Ecluse.Core.Package (
    PackageDetails (pkgArtifacts),
    PackageInfo (infoDistTags, infoVersions),
    PackageName,
    renderPackageName,
 )
import Ecluse.Core.Package.Integrity (
    IntegrityFloor,
    VersionIntegrity (BelowFloor, MeetsFloor, NoIntegrity),
    partitionByFloor,
 )
import Ecluse.Core.Rules (PreparedRule (prepResilience), cveIdsInReason)
import Ecluse.Core.Rules.Outage (AdmissionIdentity (AdmissionIdentity))
import Ecluse.Core.Rules.Types (
    Decision (Admitted, Blocked, BlockedByDefault, Undecidable),
    SkippedCheck (SkippedUnavailable, Unreached),
 )
import Ecluse.Core.Server.Response (
    PackumentStatus (PackumentBadGateway, PackumentForbidden, PackumentOk, PackumentServerError, PackumentUnavailable),
    RejectReason (BelowIntegrityFloor, ByPolicy, MissingIntegrity, Unavailable, UpstreamInvalid),
    Rejection (Rejection),
    RuleName (RuleName),
    ServeDecision (Admit, Reject),
    Transience (WillResolve, WontResolve),
    packumentStatus,
 )
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort, mpRuleDenial, mpRuleEffectfulFailure)
import Ecluse.Core.Version (renderVersion)

{- | The @module@ field every line in this family carries. It is held stable as this value
rather than the source module path, so an operator's saved filter keeps matching.
-}
pipelineInternalModule :: Text
pipelineInternalModule = "Ecluse.Server.Pipeline.Internal"

{- | Keep each version's artifacts whose strongest digest meets the integrity floor, per artifact,
so a version drops only when no file of it survives and the listing matches the download gate.
-}
admitByIntegrity ::
    (IntegrityFloor floor) =>
    floor ->
    -- The refusal projected for a present-but-too-weak digest ('BelowFloor') …
    ServeDecision ->
    -- … and for a version carrying no digest at all ('NoIntegrity'). The public and
    -- trusted gates pass their own context-worded decisions.
    ServeDecision ->
    PackageInfo ->
    (PackageInfo, [ServeDecision])
admitByIntegrity floorSpec belowFloorRefusal missingRefusal info =
    ( info
        { infoVersions = admissible
        , infoDistTags = Map.filter ((`Map.member` admissible) . renderVersion) (infoDistTags info)
        }
    , refusals
    )
  where
    -- One walk of an up-to-100k-version map yields the surviving versions and both refusal
    -- buckets. The partitioned map is that large too.
    partitioned :: Map Text (Either VersionIntegrity PackageDetails)
    partitioned = Map.map admitArtifacts (infoVersions info)

    admitArtifacts details =
        (\survivors -> details{pkgArtifacts = survivors}) <$> partitionByFloor floorSpec (pkgArtifacts details)

    admissible :: Map Text PackageDetails
    admissible = Map.mapMaybe rightToMaybe partitioned

    -- 'Map.foldr' visits keys in ascending order and each arm prepends, so the below-floor
    -- refusals precede the missing-integrity ones, each in key order.
    refusals :: [ServeDecision]
    refusals = below <> missing
      where
        (below, missing) = Map.foldr bucket ([], []) partitioned
        bucket (Left BelowFloor) (b, m) = (belowFloorRefusal : b, m)
        bucket (Left NoIntegrity) (b, m) = (b, missingRefusal : m)
        -- 'partitionByFloor' never reports 'MeetsFloor' as a refusal, and a 'Right' is a survivor.
        bucket (Left MeetsFloor) acc = acc
        bucket (Right _) acc = acc

{- | Classify a no-survivors packument outcome into the bounded @ecluse.serve.decision@
value: a forbidden set is a denial, any other non-served status a transient unavailability.
-}
packumentServeDecision :: [ServeDecision] -> Metric.Decision
packumentServeDecision = statusServeDecision . packumentStatus

{- | 'packumentServeDecision' over an already-folded status, so the no-survivors path pays for
one traversal of the decision list rather than two.
-}
statusServeDecision :: PackumentStatus -> Metric.Decision
statusServeDecision = \case
    PackumentOk -> Metric.Admit
    PackumentForbidden -> Metric.Deny
    PackumentUnavailable _ -> Metric.Unavailable
    PackumentBadGateway -> Metric.Unavailable
    PackumentServerError -> Metric.Unavailable

-- | Classify a single artifact-path serve decision into the bounded metric decision.
serveDecisionClass :: ServeDecision -> Metric.Decision
serveDecisionClass = \case
    Admit -> Metric.Admit
    Reject (Rejection reason _) -> case reason of
        ByPolicy{} -> Metric.Deny
        MissingIntegrity -> Metric.Deny
        BelowIntegrityFloor -> Metric.Deny
        Unavailable{} -> Metric.Unavailable
        UpstreamInvalid -> Metric.Unavailable

{- | Map a reject reason to the @ecluse.rule.denials@ labels: the deciding rule (only a
policy denial names one) and the bounded reason class.
-}
denialLabels :: RejectReason -> (Maybe Text, Metric.ReasonClass)
denialLabels = \case
    ByPolicy (RuleName name) -> (Just name, Metric.ReasonPolicy)
    MissingIntegrity -> (Nothing, Metric.ReasonMissingIntegrity)
    BelowIntegrityFloor -> (Nothing, Metric.ReasonMissingIntegrity)
    Unavailable _ -> (Nothing, Metric.ReasonUnavailable)
    UpstreamInvalid -> (Nothing, Metric.ReasonUnavailable)

{- | The rule-evaluation tier a duration is attributed to, from the mount's rule set. The
two tiers are one engine, so a prepared rule's resilience policy is what marks it
effectful, not a separate list.
-}
evalTier :: [PreparedRule] -> Metric.Tier
evalTier rules = if any (isJust . prepResilience) rules then Metric.Effectful else Metric.Structural

{- | Map an undecidable verdict's transience to the bounded @ecluse.rule.effectful.failures@
cause.
-}
transienceCause :: Transience -> Metric.Cause
transienceCause = \case
    WillResolve _ -> Metric.Connection
    WontResolve -> Metric.OtherCause

{- | Record the @ecluse.rule.denials@ counter for each rejected decision, labelled by the
bounded reason class and, for a policy denial, the deciding rule.
-}
recordDenials :: MetricsPort -> [ServeDecision] -> IO ()
recordDenials metrics = traverse_ recordOne
  where
    recordOne :: ServeDecision -> IO ()
    recordOne = \case
        Admit -> pass
        Reject (Rejection reason _) ->
            let (rule, reasonClass) = denialLabels reason
             in mpRuleDenial metrics rule reasonClass

{- | Count each effectful-rule failure among a packument's per-version decisions. An
'Undecidable' is an effectful rule whose source could not be consulted, so it is the
effectful-failure signal.
-}
recordEffectfulFailures :: MetricsPort -> [Decision] -> IO ()
recordEffectfulFailures metrics = traverse_ recordOne
  where
    recordOne :: Decision -> IO ()
    recordOne = \case
        Undecidable transience _ -> mpRuleEffectfulFailure metrics (transienceCause transience)
        Admitted{} -> pass
        Blocked{} -> pass
        BlockedByDefault{} -> pass

{- | A per-version serve outcome that keeps the version alongside its decision, so a denial's
audit line can name the version it refused.
-}
data VersionVerdict = VersionVerdict
    { vvVersion :: Text
    , vvDecision :: ServeDecision
    }
    deriving stock (Eq, Show)

{- | An extensible bag of audit fields folded into a denial line's JSON at emit time. It
lives at the audit boundary and never on the pure 'Ecluse.Core.Rules.Types.Decision', so new
audit data joins here without threading a field through the rule engine.
-}
newtype Metadata = Metadata (Map Text Text)
    deriving stock (Eq, Show)

instance Semigroup Metadata where
    Metadata a <> Metadata b = Metadata (a <> b)

instance Monoid Metadata where
    mempty = Metadata Map.empty

{- | Everything one denial audit line records. The advisory 'DbEtag' is the database active
at emit, not the one the decision was evaluated against, because a shadow swap can land
mid-request.
-}
data DenialAudit = DenialAudit
    { daPackage :: PackageName
    , daVersion :: Text
    , daRule :: Maybe Text
    , daReasonClass :: Metric.ReasonClass
    , daAdvisoryEtag :: Maybe DbEtag
    , daExtra :: Metadata
    }

-- | Render a 'DenialAudit' to the structured payload katip folds into the line's @data@ object.
denialAuditPayload :: DenialAudit -> SimpleLogPayload
denialAuditPayload da =
    versionAuditPayload (daPackage da) (daVersion da) (daAdvisoryEtag da)
        <> maybe mempty (sl "rule") (daRule da)
        <> sl "reason_class" (show (daReasonClass da) :: Text)
        <> metadataPayload (daExtra da)
  where
    metadataPayload (Metadata m) = Map.foldrWithKey (\k v acc -> sl k v <> acc) mempty m

-- The fields every per-version audit line carries, so the denial and the skipped-check lines
-- are queried by the same names.
versionAuditPayload :: PackageName -> Text -> Maybe DbEtag -> SimpleLogPayload
versionAuditPayload pkg version etag =
    sl "module" pipelineInternalModule
        <> sl "package" (renderPackageName pkg)
        <> sl "version" version
        <> maybe mempty (\(DbEtag e) -> sl "active_advisory_db_etag" e) etag

{- | The advisory ids a denial named, recovered from its rendered message into a comma-joined
@cve@ field. Empty for a non-CVE denial, so the field appears only when an advisory drove
the refusal.
-}
cveMetadata :: Text -> Metadata
cveMetadata message = case cveIdsInReason message of
    [] -> mempty
    ids -> Metadata (Map.singleton "cve" (T.intercalate ", " ids))

{- | Emit one audit log line per denied version, __denials only__. 'recordDenials' counts the
same denials as metrics.
-}
logDenials :: (KatipContext m) => PackageName -> Maybe DbEtag -> [VersionVerdict] -> m ()
logDenials pkg etag = traverse_ logOne
  where
    logOne vv = case vvDecision vv of
        Admit -> pass
        Reject (Rejection reason message) ->
            let (rule, reasonClass) = denialLabels reason
                audit = DenialAudit pkg (vvVersion vv) rule reasonClass etag (cveMetadata message)
             in katipAddContext (denialAuditPayload audit) $
                    logFM WarningS (ls ("denied" :: Text))

{- | 'logSkippedChecks' once per admission identity (package, version, skipped rule set) for the
life of the advisory source's outage, so a public serve that admits again repeats no line.
-}
logSkippedChecksOnce :: (KatipContext m) => (AdmissionIdentity -> IO Bool) -> PackageName -> Text -> Maybe DbEtag -> [SkippedCheck] -> m ()
logSkippedChecksOnce note pkg version etag skipped =
    unless (Set.null rules) $ do
        logIt <- liftIO (note (AdmissionIdentity (renderPackageName pkg) version rules))
        when logIt (logSkippedChecks pkg version etag skipped)
  where
    rules = Set.fromList [rule | SkippedUnavailable rule _ <- skipped]

{- | Emit one audit line per check the admission skipped for unavailability. An unreached check
gets no line, and a trusted serve runs no rules, so it never reaches here.
-}
logSkippedChecks :: (KatipContext m) => PackageName -> Text -> Maybe DbEtag -> [SkippedCheck] -> m ()
logSkippedChecks pkg version etag = traverse_ logOne
  where
    logOne = \case
        Unreached _ -> pass
        SkippedUnavailable rule cause ->
            katipAddContext (versionAuditPayload pkg version etag <> sl "rule" rule <> sl "cause" cause) $
                logFM WarningS (ls ("admitted with a check skipped for unavailability" :: Text))
