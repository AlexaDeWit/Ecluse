-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ExistentialQuantification #-}

{- | Per-request mount wiring, runtime capabilities, and the serve handler monad.
Dispatch pairs one 'MountBinding' with 'ServeRuntime' and establishes the ambient
@katip@ context. The composition root supplies the runtime capabilities.
-}
module Ecluse.Core.Server.Context (
    -- * Request runtime
    ServeRuntime (..),

    -- * Packument-serve dependencies
    PackumentDeps (..),
    ReadSourceSelection (..),
    pdPrivateBaseUrl,
    pdPublicBaseUrl,
    pdPrivateBase,
    pdPublicBase,
    pdMirror,
    pdTarballHostGate,
    tarballHostHonoured,

    -- * Publish-serve dependencies
    PublishDeps (..),

    -- * The serve action, and the router an adapter supplies
    RouteAction (..),
    ResponseAction (..),
    MountRouter,

    -- * Mount binding
    MountBinding (..),

    -- * Per-request context
    RequestCtx (..),

    -- * The handler monad
    Handler,
    runHandler,
) where

import Data.IP (IPRange)
import Data.Time (UTCTime)
import Katip (Katip, KatipContext, LogEnv, SimpleLogPayload)
import Katip.Monadic (KatipContextT, runKatipContextT)
import Network.HTTP.Types (Method)
import Network.HTTP.Types.Header (RequestHeaders)
import Network.Wai (ResponseReceived)
import Network.Wai qualified as Wai
import UnliftIO (MonadUnliftIO)

import Ecluse.Core.Credential (ClientCredential, Secret)
import Ecluse.Core.Cve.Types (DbEtag)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Package.Integrity (MinIntegrity, MinTrustedIntegrity)
import Ecluse.Core.Queue (MirrorQueue)
import Ecluse.Core.Registry.Adapter.Capability (AdapterArtifact, AdapterMetadata, AdapterPublish, ProjectName)
import Ecluse.Core.Registry.Exchange.Types (ExchangeClient)
import Ecluse.Core.Registry.Public (PublicControl)
import Ecluse.Core.Registry.Request (BaseRequest, CredentialMapping)
import Ecluse.Core.Rules (PreparedRule)
import Ecluse.Core.Rules.Install (InstallEvidenceProvider)
import Ecluse.Core.Rules.Outage (AdmissionIdentity)
import Ecluse.Core.Security (HostPort, Limits, Origin, TarballHostGate, tarballHostAllowed, thgAllowlist, thgEcosystemHosts)
import Ecluse.Core.Security.Egress (RegistryUrl)
import Ecluse.Core.Server.Admission (ServeAdmission)
import Ecluse.Core.Server.Admission.Bytes (ByteAdmission)
import Ecluse.Core.Server.Admission.Meter (MemoryMeter)
import Ecluse.Core.Server.Cache (MetadataCache)
import Ecluse.Core.Server.Contract (ResponseContract)
import Ecluse.Core.Server.Encoding (MetadataCompression)
import Ecluse.Core.Server.MirrorView (MirrorView)
import Ecluse.Core.Server.Response (HelpMessage)
import Ecluse.Core.Server.Upstream (
    MirrorServePlan,
    MountUpstreams,
    upstreamMirror,
    upstreamPrivateBase,
    upstreamPrivateBaseUrl,
    upstreamPublicBase,
    upstreamPublicBaseUrl,
    upstreamTarballHostGate,
 )
import Ecluse.Core.Telemetry.Record (MetricsPort)
import Ecluse.Core.Telemetry.Span (TracingPort)

-- | Serve capabilities assembled at boot. Both HTTP managers must validate TLS certificates.
data ServeRuntime = ServeRuntime
    { srAdmission :: ServeAdmission
    -- ^ Bounds concurrent metadata work, excluding private artifact reads and artifact relay.
    , srMemoryMeter :: MemoryMeter
    -- ^ The memory budget metadata requests pay into as they read and build.
    , srPublicClient :: ExchangeClient
    -- ^ The validating-TLS pool for the __untrusted__ public metadata fetch and every artifact stream.
    , srPublicControl :: PublicControl
    -- ^ Public request admission and observation shared by all mounts in the process.
    , srPrivateClient :: ExchangeClient
    -- ^ The __trusted__ private upstream's validating-TLS pool, which differs in credential handling only.
    , srMetadataCache :: MetadataCache
    -- ^ The short-TTL, size-bounded metadata cache the serve paths share ("Ecluse.Core.Server.Cache").
    , srQueue :: MirrorQueue
    -- ^ The best-effort hand-off from the serve path to the mirror worker.
    , srMetrics :: MetricsPort
    -- ^ The metric-recording port the serve path emits the @ecluse.*@ catalogue through.
    , srTracing :: TracingPort
    -- ^ The tracing port the serve path opens its hand-added domain spans through.
    }

-- | Which origins may contribute to one served view.
data ReadSourceSelection
    = MergePrivateAndPublic
    | ManagedMirrorAndPublic
    | PrivateOnly
    deriving stock (Eq, Show)

-- | Mount inputs shared by packument and tarball handlers, resolved at boot.
data PackumentDeps = PackumentDeps
    { pdUpstreams :: MountUpstreams
    -- ^ Upstreams bound to their derived host gate, so their authorities cannot diverge.
    , pdReadSourceSelection :: ReadSourceSelection
    -- ^ Restrict origin reads and managed mirror contributions for this view.
    , pdMirrorState :: Maybe MirrorView
    -- ^ Source-only state, validated after each caller authorises the private read.
    , pdFirstParty :: PackageName -> Bool
    -- ^ Whether this deployment owns the name, derived once at the composition root and deny by default. Only the private upstream serves it, and a private miss is @404@.
    , pdMountBaseUrl :: Text
    -- ^ The mount's external base URL, under which served artifact URLs route back through the gate.
    , pdInstallEvidence :: Maybe InstallEvidenceProvider
    -- ^ The native provider resolves unknown facts only at a reached execution rule.
    , pdRules :: [PreparedRule]
    -- ^ Rules prepared at boot and evaluated against every public version.
    , pdAdditionalBlockedRanges :: [IPRange]
    -- ^ Extra blocked IP ranges for untrusted artifact locations. Empty by default.
    , pdLimits :: Limits
    -- ^ Fetch and decode limits. A breach discards the whole upstream contribution.
    , pdInboundToken :: Maybe Secret
    -- ^ The inbound token a client must present (@ECLUSE_SERVER__AUTH_TOKEN@). 'Nothing' leaves the edge open.
    , pdNow :: IO UTCTime
    -- ^ The rules' clock, injected so the quarantine is deterministic under test.
    , pdAdvisoryEtag :: IO (Maybe DbEtag)
    -- ^ Non-pinning read of the active advisory etag. 'Nothing' means no database is loaded.
    , pdNoteAdmission :: AdmissionIdentity -> IO Bool
    -- ^ Whether to log an admission's skipped checks, once per identity per outage ('Ecluse.Core.Rules.Outage.noteAdmission').
    , pdHelp :: Maybe HelpMessage
    -- ^ The operator help message appended to every denial body, if configured.
    , pdMinIntegrity :: MinIntegrity
    -- ^ Public integrity floor, at least SHA-256. Private reads use 'pdMinTrustedIntegrity'.
    , pdMinTrustedIntegrity :: MinTrustedIntegrity
    -- ^ The minimum integrity hash required for a trusted upstream dependency.
    , pdMetadata :: AdapterMetadata
    -- ^ The ecosystem's metadata capability, carried whole, never copied field by field.
    , pdArtifact :: AdapterArtifact
    -- ^ The ecosystem's artifact request formation, carried whole, never copied field by field.
    , pdEgressUrl :: Text -> Either Text RegistryUrl
    -- ^ Validate an artifact URL before enqueueing it. A 'Left' prevents the mirror job.
    , pdMetadataCompression :: MetadataCompression
    -- ^ Whether an admitted listing may be gzipped for a client that accepts it.
    }

-- | The private URL bound to the host gate. 'Nothing' skips the private fetch.
pdPrivateBaseUrl :: PackumentDeps -> Maybe RegistryUrl
pdPrivateBaseUrl = upstreamPrivateBaseUrl . pdUpstreams

-- | The public upstream base URL. Reads are anonymous, with no client credential.
pdPublicBaseUrl :: PackumentDeps -> RegistryUrl
pdPublicBaseUrl = upstreamPublicBaseUrl . pdUpstreams

-- | 'pdPrivateBaseUrl' parsed once per mount, which the private reads build their requests under.
pdPrivateBase :: PackumentDeps -> Maybe BaseRequest
pdPrivateBase = upstreamPrivateBase . pdUpstreams

-- | 'pdPublicBaseUrl' parsed once per mount, which the public reads build their requests under.
pdPublicBase :: PackumentDeps -> BaseRequest
pdPublicBase = upstreamPublicBase . pdUpstreams

-- | The mirror destination for admitted public artifacts, or no write for a serve-only mount.
pdMirror :: PackumentDeps -> MirrorServePlan
pdMirror = upstreamMirror . pdUpstreams

-- | The host gate derived from the mount's upstreams at boot.
pdTarballHostGate :: PackumentDeps -> TarballHostGate
pdTarballHostGate = upstreamTarballHostGate . pdUpstreams

-- | 'tarballHostAllowed' over this mount's inputs. A missing authority refuses, and trusted origins skip only the internal-range block.
tarballHostHonoured :: Origin -> PackumentDeps -> Maybe HostPort -> Maybe HostPort -> Bool
tarballHostHonoured origin deps =
    tarballHostAllowed
        (thgEcosystemHosts (pdTarballHostGate deps))
        origin
        (thgAllowlist (pdTarballHostGate deps))
        (pdAdditionalBlockedRanges deps)

-- | First-party publish inputs. Their presence enables the mount's publish route.
data PublishDeps = PublishDeps
    { pubReadUrl :: RegistryUrl
    -- ^ The origin for credential-scoped native publication discovery.
    , pubTargetUrl :: RegistryUrl
    -- ^ The https-only publication target a publish is relayed to, with the package path appended.
    , pubAllowed :: PackageName -> Bool
    -- ^ Whether this package may publish here, checked before any upstream write. It is 'pdFirstParty'.
    , pubStaticCredential :: Maybe ClientCredential
    -- ^ Replaces the authenticated edge token on publishes. Requires 'pubInboundToken' at boot.
    , pubInboundToken :: Maybe Secret
    -- ^ The read paths' inbound edge token (@ECLUSE_SERVER__AUTH_TOKEN@). 'Nothing' leaves the edge open.
    , pubMaxRequestBytes :: Int
    -- ^ The configured per-mount request cap, covered by the shared aggregate budget.
    , pubLimits :: Limits
    -- ^ Separate client request and registry response bounds for publication.
    , pubBodyBudget :: ByteAdmission
    -- ^ Shared body-byte budget reserved before reading. Exhaustion sheds a @503@.
    , pubHelp :: Maybe HelpMessage
    -- ^ The operator help message appended to a publish denial, if configured.
    , pubProjectName :: ProjectName
    -- ^ The ecosystem's own name parser, which the anti-shadowing guard reads a declared name through.
    , pubAdapter :: AdapterPublish
    -- ^ The ecosystem's publish capability, carried whole, never copied field by field.
    }

-- | A matched request's action, constrained to its route's response contract.
data ResponseAction response
    = -- | A pure value admitted by the route's response contract.
      AnswerLocally response
    | -- | A refusal the route decided, rendered where the mount's help message is known.
      AnswerRefusal (Maybe HelpMessage -> response)
    | {- | A data-plane handler and its pre-commit fallback, both constrained to the route's response type.
      The fallback is lazy: only an escape before commit reads it, so a request that commits never builds it.
      -}
      RunPipeline ~response (Wai.Request -> (response -> IO ResponseReceived) -> Handler ResponseReceived)

-- | A route's response contract and an action that answers only through it, so dispatch needs no ecosystem's response sum.
data RouteAction = forall response. RouteAction (ResponseContract response) (ResponseAction response)

-- | An adapter's routing decision over a method, headers, and the stripped, decoded path segments.
type MountRouter = Method -> RequestHeaders -> [Text] -> RouteAction

-- | Ecosystem wiring under a non-empty prefix, so adding an ecosystem never displaces a root mount.
data MountBinding = MountBinding
    { bindingPrefix :: NonEmpty Text
    -- ^ The leading path segments this mount is served under. Never empty.
    , bindingRouter :: MountRouter
    -- ^ This mount's routing decision, derived from the adapter's own route table.
    , bindingCredential :: CredentialMapping
    -- ^ How the adapter's clients present a credential, and how the proxy carries one upstream.
    , bindingPackumentDeps :: PackumentDeps
    -- ^ A bound mount always serves listings and artifacts. Only /publish/ is opt-in.
    , bindingPublishDeps :: Maybe PublishDeps
    -- ^ Optional first-party publish dependencies. 'Nothing' makes @PUT \/{pkg}@ answer @405@.
    }

-- | The runtime and the matched 'MountBinding' one request is served through, built once per request.
data RequestCtx = RequestCtx
    { ctxRuntime :: ServeRuntime
    -- ^ The data-plane managers, the caches and queue, and the recording ports.
    , ctxMount :: MountBinding
    -- ^ The mount the request matched, carrying its complete ecosystem wiring.
    }

-- | Request context over @katip@'s reader-based logging context, shared across concurrent fetches.
newtype Handler a = Handler
    { unHandler :: ReaderT RequestCtx (KatipContextT IO) a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader RequestCtx
        , MonadUnliftIO
        , Katip
        , KatipContext
        )

-- | Run a handler with the application's log stream and initial trace-correlation payload.
runHandler :: LogEnv -> SimpleLogPayload -> RequestCtx -> Handler a -> IO a
runHandler logEnv initialContext ctx action =
    runKatipContextT logEnv initialContext mempty (runReaderT (unHandler action) ctx)
