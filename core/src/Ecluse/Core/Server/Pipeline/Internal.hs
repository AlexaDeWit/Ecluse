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
    -- * The operator log-filter keys
    pipelineModule,
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
    DenialAudit (..),
    denialAuditPayload,
    logDenials,
    logSkippedChecks,
    logSkippedChecksOnce,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Katip (KatipContext, Severity (DebugS), SimpleLogPayload, katipAddContext, logFM, ls, sl)

import Ecluse.Core.Cve.Types (DbEtag (..))
import Ecluse.Core.Package (
    Artifact (artHashes),
    PackageDetails (pkgArtifacts),
    PackageInfo (infoDistTags, infoVersions),
    PackageName,
    renderPackageName,
 )
import Ecluse.Core.Package.Integrity (
    IntegrityFloor,
    VersionIntegrity (BelowFloor, MeetsFloor, NoIntegrity),
    classifyDigests,
    partitionByFloor,
 )
import Ecluse.Core.Rules (PreparedRule, prepResilience)
import Ecluse.Core.Rules.Outage (AdmissionIdentity (AdmissionIdentity), logSkippedCheckEvidence)
import Ecluse.Core.Rules.Types (
    Decision (Admitted, Blocked, BlockedByDefault, Undecidable),
    Reason,
    SkippedCheck (SkippedUnavailable),
    reasonAdvisories,
 )
import Ecluse.Core.Server.Response (
    PackumentStatus (PackumentBadGateway, PackumentDenied, PackumentForbidden, PackumentOk, PackumentServerError, PackumentUnavailable),
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

-- | The @module@ field of the pipeline's own lines, and of any line a request logs whose site names none.
pipelineModule :: Text
pipelineModule = "Ecluse.Server.Pipeline"

-- | The @module@ field every line in this family carries, held stable so an operator's saved filter keeps matching.
pipelineInternalModule :: Text
pipelineInternalModule = "Ecluse.Server.Pipeline.Internal"

{- | Keep each version's files whose strongest digest meets the floor, so a version drops only when
none survives, as at the download gate. Each dropped version keeps its refusal beside its version, so
its audit line can name it.
-}
admitByIntegrity ::
    (IntegrityFloor floor) =>
    floor ->
    -- The refusal for a version whose digest is too weak ('BelowFloor').
    ServeDecision ->
    -- The refusal for a version with no digest ('NoIntegrity'). Each gate passes its own wording.
    ServeDecision ->
    PackageInfo ->
    (PackageInfo, [VersionVerdict])
admitByIntegrity floorSpec belowFloorRefusal missingRefusal info
    -- A listing whose every file clears the floor keeps every version whole, so skip the rebuild.
    | all (all fileMeetsFloor . pkgArtifacts) (infoVersions info) = (withVersions (infoVersions info), [])
    | otherwise = (withVersions admissible, refusals)
  where
    fileMeetsFloor file = classifyDigests floorSpec (artHashes file) == MeetsFloor

    withVersions versions =
        info
            { infoVersions = versions
            , infoDistTags = Map.filter ((`Map.member` versions) . renderVersion) (infoDistTags info)
            }

    partitioned :: Map Text (Either VersionIntegrity PackageDetails)
    partitioned = Map.map admitArtifacts (infoVersions info)

    admitArtifacts details =
        (\survivors -> details{pkgArtifacts = survivors}) <$> partitionByFloor floorSpec artHashes (pkgArtifacts details)

    admissible :: Map Text PackageDetails
    admissible = Map.mapMaybe rightToMaybe partitioned

    -- 'Map.foldrWithKey' visits keys in ascending order and each arm prepends, so the below-floor
    -- refusals precede the missing-integrity ones, each in key order.
    refusals :: [VersionVerdict]
    refusals = below <> missing
      where
        (below, missing) = Map.foldrWithKey bucket ([], []) partitioned
        bucket version (Left BelowFloor) (b, m) = (VersionVerdict version belowFloorRefusal : b, m)
        bucket version (Left NoIntegrity) (b, m) = (b, VersionVerdict version missingRefusal : m)
        -- 'partitionByFloor' never reports 'MeetsFloor' as a refusal, and a 'Right' is a survivor.
        bucket _ (Left MeetsFloor) acc = acc
        bucket _ (Right _) acc = acc

-- | The bounded @ecluse.serve.decision@ value of a no-survivors packument: a refusal is a denial, the rest unavailable.
packumentServeDecision :: [ServeDecision] -> Metric.Decision
packumentServeDecision = statusServeDecision . packumentStatus

-- | 'packumentServeDecision' over a folded status, so the no-survivors path walks the decisions once.
statusServeDecision :: PackumentStatus -> Metric.Decision
statusServeDecision = \case
    PackumentOk -> Metric.Admit
    PackumentDenied -> Metric.Deny
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

-- | The @ecluse.rule.denials@ labels: the deciding rule of a policy denial, and the bounded reason class.
denialLabels :: RejectReason -> (Maybe Text, Metric.ReasonClass)
denialLabels = \case
    ByPolicy (RuleName name) _ -> (Just name, Metric.ReasonPolicy)
    MissingIntegrity -> (Nothing, Metric.ReasonMissingIntegrity)
    BelowIntegrityFloor -> (Nothing, Metric.ReasonMissingIntegrity)
    Unavailable _ -> (Nothing, Metric.ReasonUnavailable)
    UpstreamInvalid -> (Nothing, Metric.ReasonUnavailable)

-- | The rule-evaluation tier of a duration: effectful when any prepared rule reads through a resilience policy.
evalTier :: [PreparedRule] -> Metric.Tier
evalTier rules = if any (isJust . prepResilience) rules then Metric.Effectful else Metric.Structural

-- | Map an undecidable verdict's transience to the bounded @ecluse.rule.effectful.failures@ cause.
transienceCause :: Transience -> Metric.Cause
transienceCause = \case
    WillResolve _ -> Metric.Connection
    WontResolve -> Metric.OtherCause

-- | Count @ecluse.rule.denials@ once per rejected decision, labelled by 'denialLabels'.
recordDenials :: MetricsPort -> [ServeDecision] -> IO ()
recordDenials metrics = traverse_ recordOne
  where
    recordOne :: ServeDecision -> IO ()
    recordOne = \case
        Admit -> pass
        Reject (Rejection reason _) ->
            let (rule, reasonClass) = denialLabels reason
             in mpRuleDenial metrics rule reasonClass

-- | Count each 'Undecidable' decision, the signal that an effectful rule could not consult its source.
recordEffectfulFailures :: MetricsPort -> [Decision] -> IO ()
recordEffectfulFailures metrics = traverse_ recordOne
  where
    recordOne :: Decision -> IO ()
    recordOne = \case
        Undecidable transience _ -> mpRuleEffectfulFailure metrics (transienceCause transience)
        Admitted{} -> pass
        Blocked{} -> pass
        BlockedByDefault{} -> pass

-- | A per-version serve outcome beside its version, so a denial's audit line can name the version it refused.
data VersionVerdict = VersionVerdict
    { vvVersion :: Text
    , vvDecision :: ServeDecision
    }
    deriving stock (Eq, Show)

-- | Everything one denial audit line records. A shadow swap can make the emit-time 'DbEtag' differ from the decision's.
data DenialAudit = DenialAudit
    { daPackage :: PackageName
    , daVersion :: Text
    , daRule :: Maybe Text
    , daReasonClass :: Metric.ReasonClass
    , daAdvisoryEtag :: Maybe DbEtag
    , daReasons :: [Reason]
    -- ^ The reasons a policy denial gave. The @cve@ field joins the advisories still affecting the version.
    }

-- | Render a 'DenialAudit' to the structured payload katip folds into the line's @data@ object.
denialAuditPayload :: DenialAudit -> SimpleLogPayload
denialAuditPayload da =
    versionAuditPayload (daPackage da) (daVersion da) (daAdvisoryEtag da)
        <> maybe mempty (sl "rule") (daRule da)
        <> sl "reason_class" (show (daReasonClass da) :: Text)
        <> case ordNub (concatMap reasonAdvisories (daReasons da)) of
            [] -> mempty
            ids -> sl "cve" (T.intercalate ", " ids)

-- The fields every per-version audit line carries, so the denial and the skipped-check lines
-- are queried by the same names.
versionAuditPayload :: PackageName -> Text -> Maybe DbEtag -> SimpleLogPayload
versionAuditPayload pkg version etag =
    sl "module" pipelineInternalModule
        <> sl "package" (renderPackageName pkg)
        <> sl "version" version
        <> maybe mempty (\(DbEtag e) -> sl "active_advisory_db_etag" e) etag

-- | Emit debug details per denied version. 'recordDenials' counts them at every log level.
logDenials :: (KatipContext m) => PackageName -> Maybe DbEtag -> [VersionVerdict] -> m ()
logDenials pkg etag = traverse_ logOne
  where
    logOne vv = case vvDecision vv of
        Admit -> pass
        Reject (Rejection reason _) ->
            let (rule, reasonClass) = denialLabels reason
                audit = DenialAudit pkg (vvVersion vv) rule reasonClass etag (policyReasons reason)
             in katipAddContext (denialAuditPayload audit) $
                    logFM DebugS (ls ("denied" :: Text))

policyReasons :: RejectReason -> [Reason]
policyReasons = \case
    ByPolicy _ reasons -> reasons
    Unavailable{} -> []
    MissingIntegrity -> []
    BelowIntegrityFloor -> []
    UpstreamInvalid -> []

-- | 'logSkippedChecks' once per package, version and skipped rule set for the outage, so a public serve that admits again repeats no line.
logSkippedChecksOnce :: (KatipContext m) => (AdmissionIdentity -> IO Bool) -> PackageName -> Text -> Maybe DbEtag -> [SkippedCheck] -> m ()
logSkippedChecksOnce note pkg version etag skipped =
    unless (Set.null rules) $ do
        logIt <- liftIO (note (AdmissionIdentity (renderPackageName pkg) version rules))
        when logIt (logSkippedChecks pkg version etag skipped)
  where
    rules = Set.fromList [rule | SkippedUnavailable rule _ <- skipped]

-- | One audit line per check skipped for unavailability. An unreached check gets none. A trusted serve runs no rules, so it never reaches here.
logSkippedChecks :: (KatipContext m) => PackageName -> Text -> Maybe DbEtag -> [SkippedCheck] -> m ()
logSkippedChecks pkg version etag = logSkippedCheckEvidence (versionAuditPayload pkg version etag)
