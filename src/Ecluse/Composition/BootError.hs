-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Aggregated startup refusals, the advisories a boot logs beside them, and their
operator-facing rendering.
-}
module Ecluse.Composition.BootError (
    BootError (..),
    StoreMaintenanceReason (..),
    Advisory (..),
    refuseOnThrow,
    renderBootError,
    renderBootErrors,
    renderAdvisory,
) where

import Data.Text qualified as T
import Data.Time (NominalDiffTime)
import UnliftIO (tryAny)

import Ecluse.Config (
    PolicyError,
    StoreTag,
    renderPolicyError,
    storeTagName,
 )
import Ecluse.Config.Resolve (mountDocRef, mountKeyRef)
import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Ecosystem (Ecosystem (Npm), ecosystemName)
import Ecluse.Core.Registry.Maintenance.Upstream (
    ExternalConnection (externalConnectionText),
    PermissionName (permissionNameText),
    RepositoryName (repositoryNameText),
    UndecidabilityReason (ChainBoundExceeded, NetworkFailure, NoMechanism),
    UnsafeReason (ConfigurationEvidence, InsufficientPermissions),
 )
import Ecluse.Core.Rules.Types (RuleType)
import Ecluse.Core.Security (authorityLabel)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Wire (renderWire)
import Ecluse.Runtime.Aws.Fault (displayAwsFaultT)

-- | A reason the composition root refuses to start. The root aggregates them, so one run reports every problem.
data BootError
    = -- | A rule policy did not resolve (surfaced by 'Ecluse.Config.loadConfig').
      PolicyBootError PolicyError
    | -- | A configured mount's ecosystem has no adapter, so Écluse cannot serve it.
      MissingAdapter Ecosystem
    | -- | A mount has no mirror-write provider. Every mount derives one from its mirror target, so this is a safety net.
      UnresolvedCredential Ecosystem
    | -- | The queue URL names a backend this binary cannot run.
      QueueProviderUnavailable Text
    | -- | @AWS_ENDPOINT_URL_SQS@ is set but @AWS_REGION@ is not, and an emulator or VPC endpoint carries no region.
      QueueRegionMissing
    | -- | @ECLUSE_QUEUE__URL@ names no backend this binary knows, and carries the value. Guessing one would misroute mirror jobs.
      QueueUrlUnrecognised Text
    | -- | @AWS_ENDPOINT_URL_SQS@ is not a parseable URL. It can carry a credential, so the value stays redacted.
      QueueEndpointMalformed Secret
    | -- | @AWS_ENDPOINT_URL@ is not a parseable URL. Refused rather than dropped, so a typo never dials real AWS.
      AwsEndpointMalformed Secret
    | -- | The eager mint threw, carrying every configured consumer key and the rendered exception.
      CodeArtifactMintFailed (NonEmpty Text) Text
    | -- | A mount declares a mirror target this build cannot write for its ecosystem, so it is refused rather than booted half-wired.
      MirrorTargetWithoutPublish Ecosystem
    | -- | A PyPI mirror role cannot retain or validate its expected inventory without durable state.
      MirrorStateRequired Ecosystem
    | -- | An active native writer lacks the credential username its protocol requires. Carries the key path.
      UploadConfigurationRequired Ecosystem Text
    | -- | An active writer on a protocol with a separate upload endpoint has no @uploadUrl@. Carries the key path.
      UploadUrlRequired Ecosystem Text
    | -- | A publication target has no adapter that can write its ecosystem's protocol.
      PublicationTargetWithoutPublish Ecosystem
    | -- | A publication target with no first-party namespaces leaves the anti-shadowing guard nothing to enforce.
      FirstPartyMissing Ecosystem
    | -- | First-party names have no private authority, so every lookup would return 404.
      FirstPartyWithoutPrivateUpstream Ecosystem
    | -- | A static publish credential without @ECLUSE_SERVER__AUTH_TOKEN@ would let anyone publish as Écluse.
      PublishStaticCredentialNeedsEdge Ecosystem StoreTag
    | -- | A publication target shares a host with the named mount's public upstream, so a relayed credential would reach it.
      PublicationTargetOnPublicUpstream Ecosystem Ecosystem Text
    | -- | A publication target is also the named mount's endpoint under the named key, so a publish would reach another role.
      PublicationTargetOnMountEndpoint Ecosystem Ecosystem Text Text
    | -- | A mirror target shares a host with the named mount's public upstream, so the mirror-write credential would reach it.
      MirrorTargetOnPublicUpstream Ecosystem Ecosystem Text
    | -- | A mirror target is also the named mount's endpoint under the named key, so a sweep would delete another role's data.
      MirrorTargetOnMountEndpoint Ecosystem Ecosystem Text Text
    | -- | One repository receives caller credentials and bypasses the public rules through the private leg.
      PrivateUpstreamOnPublicUpstream Ecosystem Text
    | -- | The private upstream can serve public content, which would be trusted as private. Carries what the backend reported.
      PrivateUpstreamUnsafe Ecosystem UnsafeReason
    | -- | Asking the private upstream what it aggregates threw, so nothing was settled about it.
      PrivateUpstreamProbeFailed Ecosystem Text
    | -- | Two endpoints name the carried registry under different tags, so they disagree about what serves that store.
      StoreTagConflict Ecosystem Text Ecosystem Text Text
    | -- | An explicit memory override breaks the plan's invariant even at every tenant's minimum. A computed plan degrades instead.
      MemoryPlanOverrideUnsafe [Text]
    | -- | A split-deployment role selected over the in-memory queue, whose jobs never leave their process.
      SplitRoleNeedsDurableQueue Text
    | -- | The mirror worker launched with no mount declaring a mirror target, so it has nothing to drain.
      MirrorRoleWithoutMirroring
    | -- | Building the mirror-queue backend threw. The rendered exception tells a transient fault from a permanent one.
      MirrorQueueUnavailable Text
    | -- | Preparing the advisory sync threw. The rendered exception tells a transient fault from a permanent one.
      AdvisorySyncUnavailable Text
    | -- | A vetted mirror store has no maintenance backend the Dredger can sweep it with, carrying why.
      StoreMaintenanceUnavailable Ecosystem StoreMaintenanceReason
    | -- | Two @dredger.quotaOverrides@ entries define one capacity pool differently: the pool and both keys.
      DredgerQuotaScopeConflict Text Text Text
    | -- | The pause between sweep chunks is beneath its floor. Only the deleting role reads it, so only that role refuses.
      DredgerChunkPauseBeneathFloor NominalDiffTime NominalDiffTime
    | -- | A mount's rules refuse without advisory evidence and no store is configured. Carries the rule names, in policy order.
      AdvisoryDenyWithoutStore Ecosystem (NonEmpty Text)
    | -- | An active mount carries a rule its adapter blocks, with the declared reason.
      RuleBlockedOnMount Ecosystem RuleType Text
    | -- | An advisory store with no mount leaves @ecluse pilot@ no ecosystem to compile for.
      PilotWithoutEcosystem
    | -- | The progress window is zero or negative. Carries the configured seconds.
      ProgressWindowNotPositive Int
    | -- | The progress window is not below the serve-path cap, so the floor could never fire first. Window and cap, in seconds.
      ProgressWindowNotBelowServeCap Int Int
    | -- | The progress floor's byte count is zero or negative. Carries the configured count.
      MinProgressBytesNotPositive Int
    deriving stock (Eq, Show)

-- | Why a mount's mirror target reached no store maintenance handle.
data StoreMaintenanceReason
    = -- | The mount's store tag names no control plane this build implements.
      NoControlPlane StoreTag
    | -- | The mount's store carries no operator consent to delete from it.
      DeletionNotPermitted StoreTag
    | -- | The store's only control plane is the ecosystem protocol, which spells no listing or version delete.
      NoProtocolMaintenance
    | -- | The declared private cache lacks a supported maintenance or authentication operation.
      PrivateCacheUnavailable Text
    | -- | Building the cleared backend's client against the live environment threw.
      ClientBuildFailed Text
    deriving stock (Eq, Show)

-- | A finding a role boots on and warns about. The deleting role refuses the collapses below instead.
data Advisory
    = -- | A mirror target is also the named mount's private upstream, at the carried registry. Both mounts are carried, as they can differ.
      MirrorTargetOnPrivateUpstream Ecosystem Ecosystem RegistryUrl
    | -- | A mount's mirror target is also its own publication target, at the carried registry.
      MirrorTargetOnOwnPublicationTarget Ecosystem RegistryUrl
    | -- | A @dredger.quotaOverrides@ entry, carried as its key, names a store no mount declares.
      DredgerQuotaOverrideUnmatched Text
    | -- | A mount's remediation rule skips with no advisory store, so it admits no fix and every admission logs the skip.
      RemediationSkipsWithoutStore Ecosystem
    | -- | Whether the private upstream serves public content stayed open, so verifying it stays the operator's. Carries why.
      PrivateUpstreamUndecided Ecosystem UndecidabilityReason
    deriving stock (Eq, Show)

-- | Fold a thrown fault into the named boot error, so a phase that dials a live environment refuses through the aggregate.
refuseOnThrow :: (Text -> BootError) -> IO a -> IO (Either [BootError] a)
refuseOnThrow refusal action = first (pure . refusal . displayAwsFaultT) <$> tryAny action

-- | Render an aggregated refusal as the one block a failed launch reports.
renderBootErrors :: [BootError] -> Text
renderBootErrors = T.unlines . map renderBootError

-- | Render a 'BootError' as a human-facing line for the aggregated failure block.
renderBootError :: BootError -> Text
renderBootError = \case
    PolicyBootError err -> renderPolicyError err
    RuleBlockedOnMount eco ty reason ->
        "mount \""
            <> ecosystemName eco
            <> "\" carries the rule "
            <> renderWire ty
            <> ", which the "
            <> ecosystemName eco
            <> " ecosystem blocks: "
            <> reason
            <> " A rule in shared rules reaches every mount. Move this type from shared rules into "
            <> mountDocRef Npm "rules"
            <> ", or disable its configured entry with enabled: false under "
            <> mountDocRef eco "rules"
            <> " ("
            <> mountKeyRef eco "rules"
            <> ")."
    MissingAdapter eco ->
        "mount " <> ecosystemName eco <> " has no adapter wired in this build"
    UnresolvedCredential eco ->
        "mount "
            <> ecosystemName eco
            <> " has no initialised mirror-write credential in this build"
    QueueProviderUnavailable provider ->
        "mirror queue provider "
            <> provider
            <> " (named by the ECLUSE_QUEUE__URL shape) is not available in this build"
    QueueRegionMissing ->
        "the SQS endpoint override (AWS_ENDPOINT_URL_SQS) is set but AWS_REGION is not: an emulator or VPC endpoint does not carry its region, so AWS_REGION must scope it"
    QueueUrlUnrecognised url ->
        "ECLUSE_QUEUE__URL names no queue backend this build knows: "
            <> url
            <> " (expected an SQS queue URL, https://sqs.{region}.amazonaws.com/{account}/{queue}, or a Pub/Sub topic resource, projects/{project}/topics/{topic}; unset it to run the bounded in-memory queue)"
    -- Both endpoint values can carry a credential, so each reason names its variable,
    -- never the URL.
    QueueEndpointMalformed{} ->
        "the SQS endpoint override (AWS_ENDPOINT_URL_SQS) is not a valid endpoint URL"
    AwsEndpointMalformed{} ->
        "the AWS endpoint override (AWS_ENDPOINT_URL) is not a valid endpoint URL"
    CodeArtifactMintFailed targets detail ->
        "credential provider codeartifact for "
            <> T.intercalate ", " (toList targets)
            <> " failed to mint an initial token at boot: "
            <> detail
            <> " (a transient AWS error may clear on retry. A permanent one, such as a bad domain or region or a missing permission, must be fixed)"
    MirrorTargetWithoutPublish eco ->
        mountKeyRef eco "mirrorTarget"
            <> " is set but this build writes nothing for the "
            <> ecosystemName eco
            <> " protocol: the mirror would drain its queue with no way to publish, so the mount is refused rather than served with a mirror that fails every job."
    MirrorStateRequired eco ->
        "mirrorState.url is required for mirroring on the " <> ecosystemName eco <> " mount"
    UploadConfigurationRequired eco key ->
        mountDocRef eco key <> " is required before this role writes native uploads"
    UploadUrlRequired eco key ->
        mountDocRef eco key
            <> " ("
            <> mountKeyRef eco key
            <> ") is required before this role writes native uploads: url names where Écluse reads and uploadUrl names where it writes. If url holds the upload endpoint, move that address to uploadUrl and set url to the registry's read root."
    PublicationTargetWithoutPublish eco ->
        mountKeyRef eco "publicationTarget"
            <> " is set but this build writes nothing for the "
            <> ecosystemName eco
            <> " protocol: a publish would have no adapter to relay through, so the mount is refused rather than served with a publish route that refuses every attempt."
    FirstPartyMissing eco ->
        mountKeyRef eco "publicationTarget" <> " is set but " <> mountKeyRef eco "firstParty" <> " is not: a publication target needs the namespaces this deployment owns, written in the ecosystem's own shape (npm scopes such as @acme, PyPI distribution names and acme-* prefixes), for the anti-shadowing guard."
    FirstPartyWithoutPrivateUpstream eco ->
        mountKeyRef eco "firstParty"
            <> " is set but "
            <> mountKeyRef eco "privateUpstream"
            <> " is not: first-party names resolve from the private upstream alone. Configure privateUpstream for these names, or remove firstParty."
    PublishStaticCredentialNeedsEdge eco tag ->
        mountKeyRef eco ("publicationTarget." <> storeTagName tag <> ".token")
            <> " is set but ECLUSE_SERVER__AUTH_TOKEN is not: a static publish credential needs a verifiable inbound edge."
    PublicationTargetOnPublicUpstream eco other url ->
        mountKeyRef eco "publicationTarget"
            <> " ("
            <> url
            <> ") shares a host with "
            <> mountKeyRef other "publicUpstream"
            <> ": a publish carries the publisher's own credential, which must never reach a public upstream, so point it at a registry that shares a host with no public upstream"
    PublicationTargetOnMountEndpoint eco other key url ->
        mountKeyRef eco "publicationTarget"
            <> " is also "
            <> mountKeyRef other key
            <> " ("
            <> url
            <> "): point it at a registry that holds no other role, so a publish is never relayed into one"
    MirrorTargetOnPublicUpstream eco other url ->
        mountKeyRef eco "mirrorTarget"
            <> " ("
            <> url
            <> ") shares a host with "
            <> mountKeyRef other "publicUpstream"
            <> ": the mirror write carries this proxy's own credential, which must never reach a public upstream, so point it at a registry that shares a host with no public upstream"
    MirrorTargetOnMountEndpoint eco other key url ->
        mountKeyRef eco "mirrorTarget"
            <> " is also "
            <> mountKeyRef other key
            <> " ("
            <> url
            <> "): the Dredger permanently deletes from the mirror target, so point it at a registry that holds no other role, or run no Dredger against this configuration"
    PrivateUpstreamOnPublicUpstream eco url ->
        mountKeyRef eco "privateUpstream"
            <> " and "
            <> mountKeyRef eco "publicUpstream"
            <> " resolve to the same registry ("
            <> url
            <> "): the private leg forwards caller credentials and admits versions without the public rules. Configure distinct repositories."
    PrivateUpstreamUnsafe eco (ConfigurationEvidence repository connection) ->
        mountKeyRef eco "privateUpstream"
            <> " admits public content: repository "
            <> repositoryNameText repository
            <> " carries the external connection "
            <> externalConnectionText connection
            <> ", so public packages reach clients as trusted private content, past the public rules, the integrity floor and the quarantine. Remove that connection from the repository and its upstream chain, or point privateUpstream at a repository that has none"
    PrivateUpstreamUnsafe eco (InsufficientPermissions permission) ->
        mountKeyRef eco "privateUpstream"
            <> " could not be read: this role's identity is refused "
            <> permissionNameText permission
            <> " on that repository or one in its upstream chain. Or this role resolved no identity at all. An identity that cannot ask cannot clear the repository, so give this role an AWS identity carrying that grant, or point privateUpstream at a repository this role may read"
    PrivateUpstreamProbeFailed eco detail ->
        "the check for a connection to a public registry on "
            <> mountKeyRef eco "privateUpstream"
            <> " threw: "
            <> detail
    StoreTagConflict eco key other otherKey url ->
        mountKeyRef eco key
            <> " and "
            <> mountKeyRef other otherKey
            <> " name the same registry ("
            <> url
            <> ") under two tags: one store has one backend, so declare both endpoints under the same tag"
    MemoryPlanOverrideUnsafe details ->
        "memory plan refused: " <> T.intercalate "; " details
    SplitRoleNeedsDurableQueue invocation ->
        invocation
            <> " splits the mirror worker from the proxy, but ECLUSE_QUEUE__URL is unset, so mirroring runs on the bounded in-memory queue whose jobs never leave the process that enqueued them: point ECLUSE_QUEUE__URL at a durable queue, or run the single-process ecluse proxy"
    MirrorRoleWithoutMirroring ->
        "ecluse mirror runs the mirror worker alone, but no mount declares a mirror target, so it has nothing to mirror: set ECLUSE_MOUNTS__<ECOSYSTEM>__MIRROR_TARGET__<TAG>__URL, or run a role that needs no mirror queue"
    MirrorQueueUnavailable detail ->
        "the mirror queue backend named by ECLUSE_QUEUE__URL could not be built at boot: "
            <> detail
            <> " (a transient AWS or network error may clear on retry. A permanent one, such as unresolvable AWS credentials or a queue URL naming no reachable queue, must be fixed)"
    AdvisorySyncUnavailable detail ->
        "the advisory sync named by ECLUSE_ADVISORIES__URL could not be prepared at boot: "
            <> detail
            <> " (a transient AWS or network error may clear on retry. A permanent one, such as unresolvable AWS credentials or an ECLUSE_ADVISORIES__DATA_DIR this process cannot create, must be fixed)"
    StoreMaintenanceUnavailable eco (PrivateCacheUnavailable detail) ->
        mountKeyRef eco "privateUpstream" <> " has no usable observation backend: " <> detail
    StoreMaintenanceUnavailable eco reason ->
        mountKeyRef eco "mirrorTarget"
            <> " has no usable store maintenance backend: "
            <> renderStoreMaintenanceReason eco reason
            <> " (the Dredger deletes from every mount's mirror target, so it refuses rather than starting against a store it cannot sweep)"
    DredgerQuotaScopeConflict scope oneKey otherKey ->
        "dredger.quotaOverrides: "
            <> authorityLabel oneKey
            <> " and "
            <> authorityLabel otherKey
            <> " both define the capacity pool \""
            <> scopeLabel scope
            <> "\" and define it differently: one pool takes one definition, so give the two entries the same quotas and weights or separate scopes"
    DredgerChunkPauseBeneathFloor configured floorPause ->
        "ECLUSE_DREDGER__CHUNK_PAUSE (dredger.chunkPause) is "
            <> show configured
            <> ", beneath the floor of "
            <> show floorPause
            <> ": the pause between chunks is what leaves time to stop a mistaken sweep. Deletion is permanent, so the pause may be raised and never lowered"
    AdvisoryDenyWithoutStore eco rules ->
        "mount \""
            <> ecosystemName eco
            <> "\" enables rules that refuse without advisory evidence ("
            <> T.intercalate ", " (toList rules)
            <> "), but ECLUSE_ADVISORIES__URL (advisories.url) is unset: those rules have no advisory database to read, so every version they evaluate would refuse. Set the advisory store and run ecluse pilot to publish an artifact for this mount, or remove these rules from its policy"
    PilotWithoutEcosystem ->
        "ECLUSE_ADVISORIES__URL is set but no mount is declared, so ecluse pilot has no ecosystem to compile an advisory artifact for: declare the mounts this deployment serves under ECLUSE_MOUNTS__<ECOSYSTEM>__, or run a role this configuration has work for"
    ProgressWindowNotPositive configured ->
        "ECLUSE_LIMITS__PROGRESS_WINDOW (limits.progressWindow) is "
            <> show configured
            <> ": the window within which an upstream must deliver its minimum bytes must be a positive number of seconds"
    ProgressWindowNotBelowServeCap configured cap ->
        "ECLUSE_LIMITS__PROGRESS_WINDOW (limits.progressWindow) is "
            <> show configured
            <> ", not below the "
            <> show cap
            <> "-second cap on each upstream exchange of a served request: a slow upstream must fail the floor before the cap ends it, so set it below "
            <> show cap
    MinProgressBytesNotPositive configured ->
        "ECLUSE_LIMITS__MIN_PROGRESS_BYTES (limits.minProgressBytes) is "
            <> show configured
            <> ": the bytes an upstream must deliver in each progress window must be a positive count"

renderStoreMaintenanceReason :: Ecosystem -> StoreMaintenanceReason -> Text
renderStoreMaintenanceReason eco = \case
    NoControlPlane tag ->
        "its target is a " <> storeTagName tag <> " store, which carries no store maintenance backend this build can sweep"
    DeletionNotPermitted tag ->
        "its target is a "
            <> storeTagName tag
            <> " store and "
            <> mountKeyRef eco ("mirrorTarget." <> storeTagName tag <> ".permitDeletion")
            <> " is not set: that key is your consent for the Dredger to delete from this store"
    NoProtocolMaintenance ->
        "its store has no control plane, and the "
            <> ecosystemName eco
            <> " protocol carries no package listing or version delete for one"
    PrivateCacheUnavailable detail -> "privateUpstream cannot be previewed: " <> detail
    ClientBuildFailed detail -> "building its client failed: " <> detail

-- | Render an advisory as the warning a boot logs and @ecluse check-config@ prints, so neither words it its own way.
renderAdvisory :: Advisory -> Text
renderAdvisory = \case
    MirrorTargetOnPrivateUpstream eco other url ->
        mirrorCollapseLine eco (endpointRef eco other "privateUpstream") url
    MirrorTargetOnOwnPublicationTarget eco url ->
        mirrorCollapseLine eco "publicationTarget" url
    DredgerQuotaOverrideUnmatched key ->
        "dredger.quotaOverrides: "
            <> authorityLabel key
            <> " names no store this deployment declares, so it paces nothing"
    RemediationSkipsWithoutStore eco ->
        "mount \""
            <> ecosystemName eco
            <> "\" enables AllowIfRemediatesCve with onUnavailable: skip, but ECLUSE_ADVISORIES__URL (advisories.url) is unset: the rule admits no fix, and every admitted artifact logs a skipped remediation check. Set the advisory store, or disable the rule"
    PrivateUpstreamUndecided eco reason ->
        mountKeyRef eco "privateUpstream"
            <> " was not checked for a connection to a public registry: "
            <> renderUndecidability reason
            <> ". A repository that aggregates a public registry serves public packages as trusted private content, so confirming that this one does not stays yours"

-- Why the backend settled nothing, in the words an operator acts on.
renderUndecidability :: UndecidabilityReason -> Text
renderUndecidability = \case
    NoMechanism -> "its backend does not report the repositories and registries it aggregates"
    NetworkFailure -> "its backend did not answer"
    ChainBoundExceeded -> "its upstream chain crossed this walk's bounds before it was read whole"

-- The line both mirror collapses take: the collapsed pair, the registry they share, and the
-- consequence of keeping the configuration.
mirrorCollapseLine :: Ecosystem -> Text -> RegistryUrl -> Text
mirrorCollapseLine eco otherRef url =
    "mount \""
        <> ecosystemName eco
        <> "\": mirrorTarget and "
        <> otherRef
        <> " resolve to the same registry ("
        <> registryUrlText url
        <> "); the Dredger refuses this configuration, so pruning this mirror stays manual"

{- A pool as a line names it: the operator's own label, reduced to its authority where they spelled
a URL, so a credential written into a scope never reaches a log line. -}
scopeLabel :: Text -> Text
scopeLabel raw
    | "://" `T.isInfixOf` raw = authorityLabel raw
    | otherwise = raw

-- A neighbouring mount's endpoint is named by its mount. The subject's own is not.
endpointRef :: Ecosystem -> Ecosystem -> Text -> Text
endpointRef eco other key
    | eco == other = key
    | otherwise = "mount \"" <> ecosystemName other <> "\" " <> key
