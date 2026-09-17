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
import Ecluse.Config.Resolve (mountKeyRef)
import Ecluse.Core.Credential (Secret)
import Ecluse.Core.Ecosystem (Ecosystem, ecosystemName)
import Ecluse.Core.Registry.Maintenance.Upstream (
    ExternalConnection (externalConnectionText),
    PermissionName (permissionNameText),
    RepositoryName (repositoryNameText),
    UndecidabilityReason (ChainBoundExceeded, NetworkFailure, NoMechanism),
    UnsafeReason (ConfigurationEvidence, InsufficientPermissions),
 )
import Ecluse.Core.Security (authorityLabel)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Text (displayExceptionT)

{- | A reason the composition root refuses to start. The root aggregates them, so a
single run reports every problem an operator must fix.
-}
data BootError
    = -- | A rule policy did not resolve (surfaced by 'Ecluse.Config.loadConfig').
      PolicyBootError PolicyError
    | -- | A configured mount's ecosystem has no adapter, so Écluse cannot serve it.
      MissingAdapter Ecosystem
    | {- | A mount has no initialised mirror-write provider. Every active mount derives its
      credential from its mirror target, so this is a safety net, not a reachable state.
      -}
      UnresolvedCredential Ecosystem
    | -- | The queue URL names a backend this binary cannot run.
      QueueProviderUnavailable Text
    | {- | An SQS endpoint override (@AWS_ENDPOINT_URL_SQS@) is set but @AWS_REGION@ is not.
      An emulator or VPC endpoint carries no region in its host, so the ambient one must scope it.
      -}
      QueueRegionMissing
    | {- | @ECLUSE_QUEUE__URL@ is set but its shape names no backend this binary knows. Guessing
      one would send mirror jobs somewhere the operator did not point at. Carries the value.
      -}
      QueueUrlUnrecognised Text
    | {- | The configured SQS endpoint override (@AWS_ENDPOINT_URL_SQS@) is not a parseable
      endpoint URL. It can carry a credential, so the value stays redacted behind the secret.
      -}
      QueueEndpointMalformed Secret
    | {- | The S3 advisory client's endpoint override (@AWS_ENDPOINT_URL@) is not a parseable
      endpoint URL. Refused rather than dropped, so a typo never silently dials real AWS.
      -}
      AwsEndpointMalformed Secret
    | -- | The eager mint threw, carrying every configured consumer key and the rendered exception.
      CodeArtifactMintFailed (NonEmpty Text) Text
    | {- | A mount declares a mirror target, but this build writes nothing for its ecosystem.
      The mirror could never publish, so the mount is refused rather than booted half-wired.
      -}
      MirrorTargetWithoutPublish Ecosystem
    | -- | A publication target has no adapter that can write its ecosystem's protocol.
      PublicationTargetWithoutPublish Ecosystem
    | {- | A publication target is set and the mount declares no first-party namespaces, so the
      anti-shadowing guard has nothing to enforce and any name could be shadowed.
      -}
      FirstPartyMissing Ecosystem
    | -- | First-party names have no private authority, so every lookup would return 404.
      FirstPartyWithoutPrivateUpstream Ecosystem
    | {- | A static publish credential is set without a verifiable inbound edge
      (@ECLUSE_SERVER__AUTH_TOKEN@). An unauthenticated request could otherwise publish as Écluse.
      -}
      PublishStaticCredentialNeedsEdge Ecosystem StoreTag
    | {- | A mount's publication target, at the carried registry, shares a host with the named
      mount's public upstream. The publisher's relayed credential would reach a public registry.
      -}
      PublicationTargetOnPublicUpstream Ecosystem Ecosystem Text
    | {- | A mount's publication target is also the named mount's endpoint under the named key, at
      the carried registry. A publish would be relayed into a role declared for something else.
      -}
      PublicationTargetOnMountEndpoint Ecosystem Ecosystem Text Text
    | {- | A mount's mirror target, at the carried registry, shares a host with the named mount's
      public upstream. Écluse's own mirror-write credential would reach a public registry.
      -}
      MirrorTargetOnPublicUpstream Ecosystem Ecosystem Text
    | {- | A mount's mirror target is also the named mount's endpoint under the named key, at the
      carried registry. A sweep of that store would delete data the other role owns.
      -}
      MirrorTargetOnMountEndpoint Ecosystem Ecosystem Text Text
    | -- | One repository receives caller credentials and bypasses the public rules through the private leg.
      PrivateUpstreamOnPublicUpstream Ecosystem Text
    | {- | The mount's private upstream can serve public content, so every version it holds would
      be trusted as private. Carries what the backend reported.
      -}
      PrivateUpstreamUnsafe Ecosystem UnsafeReason
    | {- | Two endpoints, each carried as its mount and its tagged key path, name the carried
      registry under different tags, so the two declarations disagree about what serves that store.
      -}
      StoreTagConflict Ecosystem Text Ecosystem Text Text
    | {- | An explicit memory override breaks the combined memory-plan invariant even after every
      tenant shed to its minimum. A computed plan degrades and boots, an operator claim does not.
      -}
      MemoryPlanOverrideUnsafe [Text]
    | {- | A split-deployment role (carried as its invocation) was selected over the bounded
      in-memory queue, whose jobs never leave the process that enqueued them.
      -}
      SplitRoleNeedsDurableQueue Text
    | {- | The dedicated mirror worker was launched with no mount declaring a mirror target, so
      it has no queue to drain and nothing to publish.
      -}
      MirrorRoleWithoutMirroring
    | {- | Building the configured mirror-queue backend threw. Carries the rendered exception,
      which tells a transient fault from a permanent one to fix.
      -}
      MirrorQueueUnavailable Text
    | {- | Preparing the configured advisory sync threw. Carries the rendered exception, which
      tells a transient fault from a permanent one to fix.
      -}
      AdvisorySyncUnavailable Text
    | {- | A vetted mirror store has no store maintenance backend the Dredger can sweep it
      with, carrying why.
      -}
      StoreMaintenanceUnavailable Ecosystem StoreMaintenanceReason
    | {- | Two @dredger.quotaOverrides@ entries declare the same capacity pool differently,
      carried as the pool and the two keys that define it.
      -}
      DredgerQuotaScopeConflict Text Text Text
    | {- | The configured pause between sweep chunks is beneath its floor, carried beside it.
      Only the deleting role reads the @dredger@ group, so only that role refuses.
      -}
      DredgerChunkPauseBeneathFloor NominalDiffTime NominalDiffTime
    | {- | A mount's rules deny on the advisory database and no advisory store is configured, so
      those rules could never decide. Carries the mount and the rule names, in policy order.
      -}
      AdvisoryDenyWithoutStore Ecosystem (NonEmpty Text)
    | {- | An advisory store is configured and no mount is, so @ecluse pilot@ has no ecosystem
      to compile an artifact for and would publish nothing.
      -}
      PilotWithoutEcosystem
    deriving stock (Eq, Show)

-- | Why a mount's mirror target reached no store maintenance handle.
data StoreMaintenanceReason
    = -- | The mount's store tag names no control plane this build implements.
      NoControlPlane StoreTag
    | -- | The mount's store carries no operator consent to delete from it.
      DeletionNotPermitted StoreTag
    | {- | The store's only control plane is the ecosystem protocol, which spells no package
      listing or version delete.
      -}
      NoProtocolMaintenance
    | -- | The declared private cache lacks a supported maintenance or authentication operation.
      PrivateCacheUnavailable Text
    | -- | Building the cleared backend's client against the live environment threw.
      ClientBuildFailed Text
    deriving stock (Eq, Show)

{- | A finding a role boots on and warns about. The deleting role refuses the collapses below,
so no advisory naming one reaches it.
-}
data Advisory
    = {- | A mount's mirror target is also the named mount's private upstream, at the carried
      registry. Both mounts are carried, because the two can differ.
      -}
      MirrorTargetOnPrivateUpstream Ecosystem Ecosystem RegistryUrl
    | -- | A mount's mirror target is also its own publication target, at the carried registry.
      MirrorTargetOnOwnPublicationTarget Ecosystem RegistryUrl
    | {- | A @dredger.quotaOverrides@ entry names a store no mount declares, carried as the key it
      was written under.
      -}
      DredgerQuotaOverrideUnmatched Text
    | {- | Whether the mount's private upstream serves public content stayed open, so that
      topology stays the operator's to verify. Carries why it stayed open.
      -}
      PrivateUpstreamUndecided Ecosystem UndecidabilityReason
    deriving stock (Eq, Show)

{- | Fold a thrown fault into the boot error the caller names, so a phase that dials a live
environment refuses through the aggregate rather than escaping the boot as an exception.
-}
refuseOnThrow :: (Text -> BootError) -> IO a -> IO (Either [BootError] a)
refuseOnThrow refusal action = first (pure . refusal . displayExceptionT) <$> tryAny action

{- | Render an aggregated refusal as the one block a failed launch reports, so every problem an
operator must fix appears in a single run.
-}
renderBootErrors :: [BootError] -> Text
renderBootErrors = T.unlines . map renderBootError

-- | Render a 'BootError' as a human-facing line for the aggregated failure block.
renderBootError :: BootError -> Text
renderBootError = \case
    PolicyBootError err -> renderPolicyError err
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
            <> " on that repository or one in its upstream chain, or resolved no identity to ask with. An identity that cannot ask cannot clear the repository, so give this role an AWS identity carrying that grant, or point privateUpstream at a repository this role may read"
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
            <> "\" enables the advisory deny rules "
            <> T.intercalate ", " (toList rules)
            <> ", but ECLUSE_ADVISORIES__URL (advisories.url) is unset: those rules have no advisory database to read, so every version they evaluate would refuse. Set the advisory store and run ecluse pilot to publish an artifact for this mount, or remove these rules from its policy"
    PilotWithoutEcosystem ->
        "ECLUSE_ADVISORIES__URL is set but no mount is declared, so ecluse pilot has no ecosystem to compile an advisory artifact for: declare the mounts this deployment serves under ECLUSE_MOUNTS__<ECOSYSTEM>__, or run a role this configuration has work for"

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

{- | Render an advisory as the warning line a boot logs and @ecluse check-config@ prints. Both
entry points render here, so neither can word a warning its own way.
-}
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
