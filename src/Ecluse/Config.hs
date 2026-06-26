{- | The configuration boundary: parse the process environment and the structured
config document into one precise, validated value the composition root consumes.

This module is the canonical __parse, don't validate__ edge for configuration.
Untrusted, less-structured input — flat environment variables and an
operator-authored JSON document — is turned into a 'Config' whose types already
encode every invariant, so nothing downstream re-checks a URL, re-resolves a
backend name, or trips over a rule that was misspelled. Two layers feed it:

* The __environment layer__ ('parseEnv') reads the process-level and secret
  values via @envparse@, which __aggregates__ failures: one run reports every
  malformed or missing variable, not just the first.

* The __document layer__ ('decodeDocument') decodes the structured 'ConfigDoc'
  from JSON — a file or the @PROXY_CONFIG@ blob, one schema for both — carrying
  the __mount map__ and the __rule policy__. Its @mounts@ object is __keyed by
  ecosystem name__ (@npm@, @pypi@); the path prefix is derived from that key, not
  declared. Its decoders are __strict__: an unknown key, an unknown ecosystem, or
  an unknown rule @type@ is a loud failure, never a silent skip — the accepted
  keys of each object are enumerated and anything else is rejected.

The two combine in 'loadConfig', which __desugars__ the single-mount environment
variables into a one-entry 'MountMap' (the npm ecosystem, the env-only default)
and __merges__ the document's rule policy over the built-in 'defaultPolicy'. An
env-only launch with no document still runs on that default policy.

The rule-policy merge is a named-map patch sourced from "Ecluse.Rules.Types" —
config selects and refines rules, it does not re-encode their semantics. See
@docs\/architecture\/configuration.md@ and @docs\/architecture\/hosting.md@.

== Secrets

Tokens (the inbound 'cfgAuthToken', outbound registry tokens) arrive __only__
through the environment, never the structured document: the document is
reviewable and diffable, the wrong place for a secret. The document decoder
__rejects__ a token field to keep that boundary enforced rather than merely
documented.
-}
module Ecluse.Config (
    -- * The assembled configuration
    Config (..),
    loadConfig,

    -- * Environment layer
    EnvConfig (..),
    parseEnv,
    parseEnvPure,
    renderEnvErrors,

    -- * Backend selection
    QueueBackend (..),
    parseQueueBackend,
    renderQueueBackend,
    CredentialBackend (..),
    parseCredentialBackend,
    renderCredentialBackend,
    parseMirrorCredentialProvider,
    renderMirrorCredentialProvider,

    -- * Network values
    Url,
    mkUrl,
    unUrl,

    -- * The structured document
    ConfigDoc (..),
    MountDoc (..),
    RulePatch (..),
    RuleEntry (..),
    decodeDocument,

    -- * Mounts
    MountMap,
    Mount (..),
    MountRegistries (..),
    MirrorTarget (..),

    -- * Rule policy
    RulePolicy (..),
    defaultPolicy,
    resolvePolicy,
    PolicyError (..),
    renderPolicyError,
) where

import Data.Aeson (
    FromJSON (parseJSON),
    Value (Array, Bool, Null, Number, Object, String),
    eitherDecodeStrict,
    withObject,
    (.!=),
    (.:),
    (.:?),
 )
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (NominalDiffTime)
import Env qualified
import System.Environment (getEnvironment)
import Validation (eitherToValidation, validationToEither)

import Ecluse.Credential (Secret, mkSecret)
import Ecluse.Ecosystem (Ecosystem (Npm), parseEcosystem)
import Ecluse.Log (LogFormat (..), parseLogFormat)
import Ecluse.Package (mkScope)
import Ecluse.Package.Integrity (MinIntegrity, defaultMinIntegrity, parseMinIntegrity)
import Ecluse.Rules.Types (
    PrecededRule (..),
    Rule (..),
    defaultPrecedence,
 )
import Ecluse.Security (
    Limits (maxBodyBytes, maxNestingDepth, maxVersionCount),
    defaultLimits,
 )
import Ecluse.Telemetry (TelemetrySwitch (..), parseTelemetrySwitch)

-- ── network values ───────────────────────────────────────────────────────────

{- | An absolute upstream\/target URL, stored normalised (surrounding whitespace
trimmed). Opaque so a bare 'Text' that has not been through 'mkUrl' — and so
could be empty — cannot be mistaken for one downstream.
-}
newtype Url = Url Text
    deriving stock (Eq, Ord, Show)

{- | Build a 'Url', rejecting one that is empty after trimming surrounding
whitespace. Returns the reason on the 'Left' so the aggregating layers can name
which value was bad.

>>> mkUrl "https://registry.npmjs.org"
Right (Url "https://registry.npmjs.org")

>>> mkUrl "   "
Left "expected a non-empty URL"
-}
mkUrl :: Text -> Either Text Url
mkUrl raw =
    let trimmed = T.strip raw
     in if T.null trimmed
            then Left "expected a non-empty URL"
            else Right (Url trimmed)

-- | The underlying URL text.
unUrl :: Url -> Text
unUrl (Url u) = u

-- ── backend selection ────────────────────────────────────────────────────────

{- | The mirror-queue backend a mount publishes jobs to. The cloud axis the queue
Handle ("Ecluse.Queue") is constructed for; selected by name in config so the
composition root can build the matching backend.
-}
data QueueBackend
    = -- | AWS SQS (wire name @"sqs"@).
      SqsQueue
    | -- | GCP Cloud Pub\/Sub (wire name @"pubsub"@).
      PubSubQueue
    | {- | A bounded, in-process queue (wire name @"memory"@): no cloud queue, at the
      cost of a non-durable, best-effort mirror. An explicit operator choice for a
      simple \/ single-node \/ air-gapped deployment, never an automatic fallback —
      see "Ecluse.Queue" for why a lost job is correctness-safe (re-enqueued on the
      next demand) and 'Ecluse.Composition.planMirrorQueue' for the boot warning.
      -}
      MemoryQueue
    deriving stock (Eq, Show)

{- | Parse a 'QueueBackend' from its wire name, naming the accepted set on
failure.

>>> parseQueueBackend "sqs"
Right SqsQueue

>>> parseQueueBackend "kafka"
Left "unknown queue provider \"kafka\" (expected one of: sqs, pubsub, memory)"
-}
parseQueueBackend :: Text -> Either Text QueueBackend
parseQueueBackend = \case
    "sqs" -> Right SqsQueue
    "pubsub" -> Right PubSubQueue
    "memory" -> Right MemoryQueue
    other ->
        Left
            ( "unknown queue provider "
                <> quote other
                <> " (expected one of: sqs, pubsub, memory)"
            )

-- | The wire name of a 'QueueBackend' (the inverse of 'parseQueueBackend').
renderQueueBackend :: QueueBackend -> Text
renderQueueBackend = \case
    SqsQueue -> "sqs"
    PubSubQueue -> "pubsub"
    MemoryQueue -> "memory"

{- | How the bearer token that writes to a mount's mirror target is obtained — the
credential axis of the backend matrix (see
@docs\/architecture\/cloud-backends.md@ → "Credential Provider"). The mirror-target
write always needs one; under the default @passthrough@ strategy that is the only
credential a mount holds (reads forward the client's credential to the private
upstream, or are anonymous to the public). The @service@ \/ @delegated-cache@
strategies additionally back the private-upstream read with such a provider (see
@docs\/architecture\/access-model.md@).
-}
data CredentialBackend
    = {- | AWS CodeArtifact: a short-lived token minted via @GetAuthorizationToken@
      (wire name @"codeartifact"@).
      -}
      CodeArtifactCredential
    | -- | A fixed, long-lived token supplied out of band (wire name @"static"@).
      StaticCredential
    | {- | GCP Application Default Credentials: an OAuth2 access token (wire name
      @"adc"@).
      -}
      AdcCredential
    deriving stock (Eq, Ord, Show)

{- | Parse a 'CredentialBackend' from its wire name, naming the accepted set on
failure.

>>> parseCredentialBackend "codeartifact"
Right CodeArtifactCredential

>>> parseCredentialBackend "vault"
Left "unknown credential provider \"vault\" (expected one of: codeartifact, static, adc)"
-}
parseCredentialBackend :: Text -> Either Text CredentialBackend
parseCredentialBackend = \case
    "codeartifact" -> Right CodeArtifactCredential
    "static" -> Right StaticCredential
    "adc" -> Right AdcCredential
    other ->
        Left
            ( "unknown credential provider "
                <> quote other
                <> " (expected one of: codeartifact, static, adc)"
            )

{- | The wire name of a 'CredentialBackend' (the inverse of
'parseCredentialBackend').
-}
renderCredentialBackend :: CredentialBackend -> Text
renderCredentialBackend = \case
    CodeArtifactCredential -> "codeartifact"
    StaticCredential -> "static"
    AdcCredential -> "adc"

{- | Parse the process-level mirror-target write-credential provider selector
('cfgMirrorTargetCredentialProvider', from @MIRROR_TARGET_CREDENTIAL_PROVIDER@) onto
a 'CredentialBackend'. This is the credential-provider axis for the single-mount
mirror-target write — distinct from the per-mount document @credential@ field, so it
has its own operator-facing vocabulary naming the managed registry: @static@,
@codeartifact@, and @gcp-artifact-registry@ (the GCP arm, which maps to the
'AdcCredential' token source and is recognised but not built in this binary).

>>> parseMirrorCredentialProvider "codeartifact"
Right CodeArtifactCredential

>>> parseMirrorCredentialProvider "vault"
Left "unknown mirror-target credential provider \"vault\" (expected one of: static, codeartifact, gcp-artifact-registry)"
-}
parseMirrorCredentialProvider :: Text -> Either Text CredentialBackend
parseMirrorCredentialProvider = \case
    "static" -> Right StaticCredential
    "codeartifact" -> Right CodeArtifactCredential
    "gcp-artifact-registry" -> Right AdcCredential
    other ->
        Left
            ( "unknown mirror-target credential provider "
                <> quote other
                <> " (expected one of: static, codeartifact, gcp-artifact-registry)"
            )

{- | The wire name of a mirror-target credential provider selector (the inverse of
'parseMirrorCredentialProvider'); the GCP arm renders as @gcp-artifact-registry@.
-}
renderMirrorCredentialProvider :: CredentialBackend -> Text
renderMirrorCredentialProvider = \case
    StaticCredential -> "static"
    CodeArtifactCredential -> "codeartifact"
    AdcCredential -> "gcp-artifact-registry"

-- ── environment layer ────────────────────────────────────────────────────────

{- | The flat, process-level configuration read from environment variables. These
are the values too small or too secret for the structured document: process
settings, the three endpoint URLs of the single-mount launch case, the queue
backend, the cloud region\/project, and the inbound auth token.

A single-mount deployment supplies only these; 'loadConfig' desugars them into a
one-entry mount map.
-}
data EnvConfig = EnvConfig
    { cfgPort :: Int
    -- ^ The port the proxy listens on (@PROXY_PORT@, default 4873).
    , cfgPrivateUpstream :: Url
    -- ^ The private upstream registry (@PRIVATE_UPSTREAM_URL@, required).
    , cfgPublicUpstream :: Url
    {- ^ The public upstream registry (@PUBLIC_UPSTREAM_URL@, default
    @https:\/\/registry.npmjs.org@).
    -}
    , cfgMirrorTarget :: Maybe Url
    {- ^ Where approved packages are mirrored to (@MIRROR_TARGET_URL@). __Optional__:
    'Nothing' folds the mirror target onto 'cfgPrivateUpstream' (a single registry
    that is both read as the private upstream and written as the mirror target), so
    the private upstream is the one hard-required endpoint. The write __credential__
    does not fold — it stays the explicit 'cfgMirrorTargetCredentialProvider'.
    -}
    , cfgQueueBackend :: QueueBackend
    -- ^ The mirror-queue backend (@MIRROR_QUEUE_PROVIDER@, default @sqs@).
    , cfgQueueUrl :: Url
    -- ^ The queue identifier for mirror jobs (@MIRROR_QUEUE_URL@, required).
    , cfgQueueMemoryMaxDepth :: Int
    {- ^ The cap on the in-memory mirror queue's depth
    (@MIRROR_QUEUE_MEMORY_MAX_DEPTH@, default 50000), used only when
    @MIRROR_QUEUE_PROVIDER=memory@. A cold-cache @npm ci@ on a large project enqueues
    thousands of mirror jobs at once, so the queue is hard-bounded against an
    out-of-memory burst: a fresh enqueue past the cap is dropped (drop-newest, safe —
    re-enqueued on the next demand). Must be a positive integer. The default
    comfortably covers a large install's burst while bounding worst-case retained
    memory to tens of MiB; raise it to shed fewer jobs under load, lower it to bound
    memory tighter. Ignored by the durable @sqs@ \/ @pubsub@ backends.
    -}
    , cfgAwsRegion :: Maybe Text
    -- ^ The AWS region for SQS\/CodeArtifact (@AWS_REGION@, AWS backends only).
    , cfgAwsEndpointUrlSqs :: Maybe Text
    {- ^ A service-specific SQS endpoint override (@AWS_ENDPOINT_URL_SQS@, the
    AWS-SDK-standard variable). When set, the SQS backend targets this endpoint
    instead of AWS's default resolution — pointing the released image at a local
    emulator (@ministack@) or a VPC endpoint. Takes precedence over
    'cfgAwsEndpointUrl'.
    -}
    , cfgAwsEndpointUrl :: Maybe Text
    {- ^ The generic AWS endpoint override (@AWS_ENDPOINT_URL@, the AWS-SDK-standard
    variable), used for SQS when 'cfgAwsEndpointUrlSqs' is unset.
    -}
    , cfgAwsAccessKeyId :: Maybe Text
    {- ^ The AWS access key id (@AWS_ACCESS_KEY_ID@), read only to sign requests to an
    endpoint override (an emulator is off the ambient credential chain). With no
    override, AWS's own discovery resolves credentials and this is unused.
    -}
    , cfgAwsSecretAccessKey :: Maybe Secret
    {- ^ The AWS secret access key (@AWS_SECRET_ACCESS_KEY@), the counterpart to
    'cfgAwsAccessKeyId' for an endpoint override. Held as a redacted 'Secret' so it
    never reaches the derived 'Show' of this record.
    -}
    , cfgGoogleProject :: Maybe Text
    {- ^ The GCP project for Pub\/Sub\/Artifact Registry (@GOOGLE_CLOUD_PROJECT@,
    GCP backends only).
    -}
    , cfgAuthToken :: Maybe Secret
    {- ^ The inbound client auth token clients must present (@PROXY_AUTH_TOKEN@);
    'Nothing' leaves the proxy open to the network layer. Held as a redacted
    'Secret' so the token text never reaches the derived 'Show' of this record.
    -}
    , cfgRespectUpstreamTarballHost :: Bool
    {- ^ Whether to honour a @dist.tarball@ host that differs from the upstream
    that served the packument (@PROXY_RESPECT_UPSTREAM_TARBALL_HOST@, default
    'False' — the secure default). 'False' fetches a tarball only from the same
    allowlisted upstream that served the metadata; 'True' relaxes to any
    allowlisted host (for a registry that serves artifacts from a separate
    CDN\/files host), never escaping the allowlist or the internal-range block.
    -}
    , cfgMirrorTargetToken :: Maybe Secret
    {- ^ The static bearer token Écluse writes to the mirror target with
    (@MIRROR_TARGET_TOKEN@), when the target is reached with a fixed credential
    rather than a cloud-minted one. It is the token material behind a mount that
    names the @static@ credential backend; 'Nothing' leaves no static provider
    initialized, so a mount that names @static@ then fails the boot-time
    credential-reference check. Held as a redacted 'Secret' so the token text
    never reaches the derived 'Show' of this record.
    -}
    , cfgMirrorTargetCredentialProvider :: CredentialBackend
    {- ^ How the mirror-target write bearer token is obtained
    (@MIRROR_TARGET_CREDENTIAL_PROVIDER@, default @static@). @static@ uses
    'cfgMirrorTargetToken'; @codeartifact@ mints a short-lived token via
    CodeArtifact (its inputs resolved from the @MIRROR_TARGET_CODEARTIFACT_*@ keys
    below, or parsed from the mirror-target URL); @gcp-artifact-registry@ is
    recognised but not built in this binary. This is the credential-provider axis,
    not the per-mount serve strategy (@passthrough@\/@service@), which is separate.
    -}
    , cfgMirrorCodeArtifactDomain :: Maybe Text
    {- ^ The CodeArtifact domain scoping the mirror-target token
    (@MIRROR_TARGET_CODEARTIFACT_DOMAIN@). Optional here: when the mirror-target URL
    is a CodeArtifact endpoint the domain is parsed from its host instead.
    -}
    , cfgMirrorCodeArtifactDomainOwner :: Maybe Text
    {- ^ The 12-digit account number owning the CodeArtifact domain
    (@MIRROR_TARGET_CODEARTIFACT_DOMAIN_OWNER@). Optional: parsed from the
    mirror-target host when absent.
    -}
    , cfgMirrorCodeArtifactRegion :: Maybe Text
    {- ^ The AWS region of the CodeArtifact domain
    (@MIRROR_TARGET_CODEARTIFACT_REGION@). Resolution order: this key, then
    'cfgAwsRegion', then the mirror-target host.
    -}
    , cfgMirrorCodeArtifactTokenDuration :: Maybe Natural
    {- ^ The requested CodeArtifact token lifetime in seconds
    (@MIRROR_TARGET_CODEARTIFACT_TOKEN_DURATION_SECONDS@), capped at 12 hours
    (43200). 'Nothing' lets CodeArtifact default it to the caller's role-credential
    expiry, which the refresh policy adapts to.
    -}
    , cfgHelpMessage :: Maybe Text
    -- ^ A custom string appended to every denial message (@PROXY_HELP_MESSAGE@).
    , cfgCveSyncInterval :: NominalDiffTime
    {- ^ How often the advisory index is refreshed (@CVE_SYNC_INTERVAL_SECONDS@,
    default 3600).
    -}
    , cfgShutdownDrainTimeout :: Int
    {- ^ How many seconds the graceful shutdown drain waits for in-flight requests
    and in-progress artifact streams to finish before the process exits
    (@PROXY_SHUTDOWN_DRAIN_TIMEOUT@, default 30). Threaded to the server as its
    'Ecluse.Server.ShutdownDrainTimeout'.
    -}
    , cfgCacheTtl :: NominalDiffTime
    {- ^ How long a parsed packument stays fresh in the metadata cache
    (@METADATA_CACHE_TTL_SECONDS@, default 60). Short by design — brief staleness
    is benign and conditional-GET revalidates (see "Ecluse.Server.Cache"). A
    non-positive value disables caching: every entry is born already expired, so
    each request re-fetches (a deliberate "off" knob).
    -}
    , cfgCacheMaxEntries :: Int
    {- ^ The metadata cache's bound on the number of distinct packages held before
    it evicts (@METADATA_CACHE_MAX_ENTRIES@, default 1024) — a flood safety valve.
    -}
    , cfgMaxResponseBytes :: Int
    {- ^ The largest upstream metadata body, in bytes, the data plane buffers before
    aborting the fetch (@PROXY_MAX_RESPONSE_BYTES@, default 16 MiB — 'maxBodyBytes'
    of 'Ecluse.Security.defaultLimits'). Bounds memory against a hostile upstream
    returning a multi-gigabyte body; the metadata path streams a bounded read, so a
    body past the cap is refused fail-closed rather than buffered whole (artifacts
    stream and are not subject to this).
    -}
    , cfgMaxVersionCount :: Int
    {- ^ The largest number of versions a parsed packument may carry before it is
    refused (@PROXY_MAX_VERSION_COUNT@, default 100000 — 'maxVersionCount' of
    'Ecluse.Security.defaultLimits'). Bounds per-version rule evaluation against a
    version-flood document.
    -}
    , cfgMaxNestingDepth :: Int
    {- ^ The deepest JSON nesting a decoded upstream document may reach before it is
    refused (@PROXY_MAX_NESTING_DEPTH@, default 64 — 'maxNestingDepth' of
    'Ecluse.Security.defaultLimits'). Bounds stack\/CPU against a pathologically
    nested payload.
    -}
    , cfgLogFormat :: LogFormat
    {- ^ The structured-log output shape (@PROXY_LOG_FORMAT@, default @json@): the
    one-line JSONL stream for a container, or the human-readable console form
    for development (see "Ecluse.Log").
    -}
    , cfgTelemetry :: TelemetrySwitch
    {- ^ The OpenTelemetry master switch (@PROXY_TELEMETRY@, default @off@). With
    it @off@ nothing is wired and no telemetry is emitted; the standard @OTEL_*@
    variables are read by the SDK only once it is @on@ (see "Ecluse.Telemetry").
    -}
    , cfgPublicUrl :: Maybe Url
    {- ^ The proxy's own externally-reachable base URL (@PROXY_PUBLIC_URL@, e.g.
    @https:\/\/registry.example.com@). When set, each served @dist.tarball@ is
    rewritten to an __absolute__ URL under it (@{PROXY_PUBLIC_URL}\/npm\/…@) so a
    client fetches the artifact back through the proxy. 'Nothing' falls back to a
    path-relative rewrite (@\/npm\/…@) — which the @npm@ CLI cannot consume, since
    it reads a leading-slash @dist.tarball@ as a local @file:@ path — so any
    deployment that serves real @npm install@s must set this.
    -}
    , cfgMinPublicIntegrity :: MinIntegrity
    {- ^ The minimum integrity algorithm a __public__ (untrusted) version's digest
    must meet to be admitted (@PROXY_MIN_PUBLIC_INTEGRITY@, default @sha256@). A
    public version whose strongest digest is weaker than this floor (e.g. a SHA-1
    shasum only) is refused, since a collision-broken digest cannot tie its bytes to
    a tamper-evident fingerprint. The floor may be raised (@sha512@, @blake2b@) but
    never set below SHA-256 — a sub-floor value is rejected at load. The trusted
    private upstream is exempt (see "Ecluse.Package.Integrity").
    -}
    }
    deriving stock (Eq, Show)

{- | Read the environment layer from the process environment, __aggregating__
every failure: one run reports all missing or malformed variables rather than
stopping at the first. The 'Left' carries the @envparse@ error list; render it
with 'renderEnvErrors'.
-}
parseEnv :: IO (Either [(String, Env.Error)] EnvConfig)
parseEnv = parseEnvPure <$> getEnvironment

{- | The pure environment parser, over an explicit @(name, value)@ list. The same
parser 'parseEnv' runs against the process environment, exposed so the aggregation
and defaulting behaviour is unit-tested without touching real environment state.
-}
parseEnvPure :: [(String, String)] -> Either [(String, Env.Error)] EnvConfig
parseEnvPure = Env.parsePure envParser

-- The @envparse@ declaration. Its 'Applicative' accumulates errors across every
-- 'Env.var', which is the source of the all-at-once reporting promise. Readers
-- that can fail go through 'Env.eitherReader' so a malformed value (bad URL, a
-- non-integer, an unknown enum name) is reported against its own variable.
envParser :: Env.Parser Env.Error EnvConfig
envParser =
    EnvConfig
        <$> Env.var Env.auto "PROXY_PORT" (Env.def 4873)
        <*> Env.var urlReader "PRIVATE_UPSTREAM_URL" mempty
        <*> Env.var urlReader "PUBLIC_UPSTREAM_URL" (Env.def defaultPublicUpstream)
        -- Optional: an unset mirror target folds onto the private upstream in
        -- 'loadConfig', so only the private upstream is a hard-required endpoint.
        <*> optionalUrl "MIRROR_TARGET_URL"
        <*> Env.var queueBackendReader "MIRROR_QUEUE_PROVIDER" (Env.def SqsQueue)
        <*> Env.var urlReader "MIRROR_QUEUE_URL" mempty
        -- The in-memory queue's cap. Strictly positive: a cap of zero would drop
        -- every job (the queue could never accept a write), a degenerate setting, so
        -- it is rejected loudly rather than silently disabling all mirroring.
        <*> Env.var positiveIntReader "MIRROR_QUEUE_MEMORY_MAX_DEPTH" (Env.def defaultMemoryQueueMaxDepth)
        <*> optionalText "AWS_REGION"
        <*> optionalText "AWS_ENDPOINT_URL_SQS"
        <*> optionalText "AWS_ENDPOINT_URL"
        <*> optionalText "AWS_ACCESS_KEY_ID"
        <*> (fmap mkSecret <$> Env.sensitive (optionalText "AWS_SECRET_ACCESS_KEY"))
        <*> optionalText "GOOGLE_CLOUD_PROJECT"
        <*> (fmap mkSecret <$> Env.sensitive (optionalText "PROXY_AUTH_TOKEN"))
        -- Defaults to the secure value (do not honour a cross-host dist.tarball):
        -- an unset or empty variable is the tightest reading of the allowlist.
        <*> Env.var boolReader "PROXY_RESPECT_UPSTREAM_TARBALL_HOST" (Env.def False)
        <*> (fmap mkSecret <$> Env.sensitive (optionalText "MIRROR_TARGET_TOKEN"))
        <*> Env.var mirrorCredentialProviderReader "MIRROR_TARGET_CREDENTIAL_PROVIDER" (Env.def StaticCredential)
        <*> optionalText "MIRROR_TARGET_CODEARTIFACT_DOMAIN"
        <*> optionalText "MIRROR_TARGET_CODEARTIFACT_DOMAIN_OWNER"
        <*> optionalText "MIRROR_TARGET_CODEARTIFACT_REGION"
        <*> optionalCodeArtifactDuration "MIRROR_TARGET_CODEARTIFACT_TOKEN_DURATION_SECONDS"
        <*> optionalText "PROXY_HELP_MESSAGE"
        <*> Env.var cveIntervalReader "CVE_SYNC_INTERVAL_SECONDS" (Env.def defaultCveSyncInterval)
        -- The graceful-drain bound in seconds: strictly positive (a zero or negative
        -- drain window would defeat the point — no time to finish in-flight work), so
        -- it is rejected loudly rather than coerced.
        <*> Env.var positiveIntReader "PROXY_SHUTDOWN_DRAIN_TIMEOUT" (Env.def defaultShutdownDrainTimeout)
        -- A non-negative seconds count: zero is accepted on purpose, disabling the
        -- metadata cache (every entry expires immediately, so each request
        -- re-fetches). Unlike METADATA_CACHE_MAX_ENTRIES, which must be positive (a
        -- cache holding zero entries is a bug, not a knob), a zero TTL is a coherent
        -- "off" setting; see cfgCacheTtl.
        <*> Env.var secondsReader "METADATA_CACHE_TTL_SECONDS" (Env.def defaultCacheTtl)
        <*> Env.var positiveIntReader "METADATA_CACHE_MAX_ENTRIES" (Env.def defaultCacheMaxEntries)
        -- The response-bound budget (security.md invariant 4), each defaulting to the
        -- corresponding field of 'Ecluse.Security.defaultLimits'. Every one must be a
        -- strictly positive integer: a zero or negative ceiling is a degenerate budget
        -- (a body limit of 0 refuses every body, a version/depth limit of 0 refuses
        -- every document), so it is rejected loudly rather than silently fail-closing
        -- the proxy. The values are generous for real registry documents and tight
        -- enough to fail closed on pathological input.
        <*> Env.var positiveIntReader "PROXY_MAX_RESPONSE_BYTES" (Env.def (maxBodyBytes defaultLimits))
        <*> Env.var positiveIntReader "PROXY_MAX_VERSION_COUNT" (Env.def (maxVersionCount defaultLimits))
        <*> Env.var positiveIntReader "PROXY_MAX_NESTING_DEPTH" (Env.def (maxNestingDepth defaultLimits))
        <*> Env.var logFormatReader "PROXY_LOG_FORMAT" (Env.def JsonLog)
        <*> Env.var telemetrySwitchReader "PROXY_TELEMETRY" (Env.def TelemetryOff)
        <*> optionalUrl "PROXY_PUBLIC_URL"
        -- The public-integrity admission floor (default sha256, the hard minimum). A
        -- value below SHA-256 or an unknown algorithm is rejected loudly here rather
        -- than clamped, so a misconfiguration cannot silently weaken admission.
        <*> Env.var minIntegrityReader "PROXY_MIN_PUBLIC_INTEGRITY" (Env.def defaultMinIntegrity)
  where
    defaultPublicUpstream :: Url
    defaultPublicUpstream = Url "https://registry.npmjs.org"

    defaultCacheTtl :: NominalDiffTime
    defaultCacheTtl = 60

    defaultCacheMaxEntries :: Int
    defaultCacheMaxEntries = 1024

    defaultCveSyncInterval :: NominalDiffTime
    defaultCveSyncInterval = 3600

    defaultShutdownDrainTimeout :: Int
    defaultShutdownDrainTimeout = 30

    -- The in-memory mirror-queue cap default: generous enough to hold a large
    -- cold-cache install's burst of mirror jobs, small enough to keep worst-case
    -- retained memory bounded to tens of MiB. Drops past it are safe (re-enqueued on
    -- the next demand), so this trades a few re-fetches under extreme load for a hard
    -- memory bound; operators raise it to shed fewer jobs or lower it to bound tighter.
    defaultMemoryQueueMaxDepth :: Int
    defaultMemoryQueueMaxDepth = 50000

    -- An optional 'Text' variable: absent yields 'Nothing', present yields the
    -- value. 'def' makes the parser total (it never fails on absence). The reader
    -- maps 'Just' over the parsed value (through both the function and 'Either').
    optionalText :: String -> Env.Parser Env.Error (Maybe Text)
    optionalText name = Env.var ((fmap . fmap) Just Env.str) name (Env.def Nothing)

    -- An optional 'Url' variable: absent yields 'Nothing'. Same shape as
    -- 'optionalText', through 'urlReader'.
    optionalUrl :: String -> Env.Parser Env.Error (Maybe Url)
    optionalUrl name = Env.var ((fmap . fmap) Just urlReader) name (Env.def Nothing)

    -- An optional capped CodeArtifact token-lifetime variable: absent yields
    -- 'Nothing' (CodeArtifact defaults the lifetime). Same shape as 'optionalUrl',
    -- through 'codeArtifactDurationReader'.
    optionalCodeArtifactDuration :: String -> Env.Parser Env.Error (Maybe Natural)
    optionalCodeArtifactDuration name =
        Env.var ((fmap . fmap) Just codeArtifactDurationReader) name (Env.def Nothing)

-- Build a failing 'Env.Reader' from a 'Text'-parsing function, turning its
-- reason into an @envparse@ unread error tagged against the variable. Written
-- directly (rather than via @Env.eitherReader@) so it depends only on the
-- 'Env.unread' constructor common to the @envparse@ versions in use.
textReader :: (Text -> Either Text a) -> Env.Reader Env.Error a
textReader parser s = first (Env.unread . toString) (parser (toText s))

-- An 'Env.Reader' that parses a 'Url', surfacing 'mkUrl's reason.
urlReader :: Env.Reader Env.Error Url
urlReader = textReader mkUrl

-- An 'Env.Reader' for the queue backend enum.
queueBackendReader :: Env.Reader Env.Error QueueBackend
queueBackendReader = textReader parseQueueBackend

-- An 'Env.Reader' for the mirror-target credential-provider selector enum.
mirrorCredentialProviderReader :: Env.Reader Env.Error CredentialBackend
mirrorCredentialProviderReader = textReader parseMirrorCredentialProvider

-- An 'Env.Reader' for a CodeArtifact token lifetime: a positive integer count of
-- seconds capped at 12 hours (43200), CodeArtifact's maximum. A value past the cap
-- is rejected loudly rather than silently clamped, so an operator's intent is never
-- quietly overridden.
codeArtifactDurationReader :: Env.Reader Env.Error Natural
codeArtifactDurationReader = textReader $ \t -> case readMaybe (toString t) :: Maybe Natural of
    Just n | n > 0 && n <= 43200 -> Right n
    _ -> Left ("expected a positive integer count of seconds no greater than 43200 (12h), got " <> quote t)

-- An 'Env.Reader' for the CVE sync interval: a non-negative integer count of
-- seconds, read as a 'NominalDiffTime'.
cveIntervalReader :: Env.Reader Env.Error NominalDiffTime
cveIntervalReader = secondsReader

-- An 'Env.Reader' for a non-negative integer count of seconds, read as a
-- 'NominalDiffTime' (shared by every seconds-valued duration variable).
secondsReader :: Env.Reader Env.Error NominalDiffTime
secondsReader = textReader $ \t -> case readMaybe (toString t) :: Maybe Integer of
    Just n | n >= 0 -> Right (fromInteger n)
    _ -> Left ("expected a non-negative integer count of seconds, got " <> quote t)

-- An 'Env.Reader' for a strictly positive integer (a cache must hold at least one
-- entry, so zero is rejected rather than silently disabling the cache).
positiveIntReader :: Env.Reader Env.Error Int
positiveIntReader = textReader $ \t -> case readMaybe (toString t) :: Maybe Int of
    Just n | n > 0 -> Right n
    _ -> Left ("expected a positive integer, got " <> quote t)

-- An 'Env.Reader' for the log-format enum, surfacing 'parseLogFormat's reason.
logFormatReader :: Env.Reader Env.Error LogFormat
logFormatReader = textReader parseLogFormat

-- An 'Env.Reader' for the telemetry master switch, surfacing
-- 'parseTelemetrySwitch's reason.
telemetrySwitchReader :: Env.Reader Env.Error TelemetrySwitch
telemetrySwitchReader = textReader parseTelemetrySwitch

-- An 'Env.Reader' for the public-integrity floor: an algorithm name, rejected if
-- unknown or weaker than SHA-256 (the hard minimum).
minIntegrityReader :: Env.Reader Env.Error MinIntegrity
minIntegrityReader = textReader parseMinIntegrity

-- An 'Env.Reader' for a boolean flag. Accepts the conventional spellings
-- case-insensitively and rejects anything else loudly (fail-fast, never a silent
-- coercion of a typo to a default), so a security-relevant toggle is never
-- mis-set without the operator seeing it.
boolReader :: Env.Reader Env.Error Bool
boolReader = textReader $ \t -> case T.toLower (T.strip t) of
    "true" -> Right True
    "false" -> Right False
    "1" -> Right True
    "0" -> Right False
    "yes" -> Right True
    "no" -> Right False
    _ -> Left ("expected a boolean (true/false), got " <> quote t)

{- | Render the aggregated environment errors as one human-facing block, one line
per offending variable, so an operator sees every problem from a single failed
launch.
-}
renderEnvErrors :: [(String, Env.Error)] -> Text
renderEnvErrors = T.unlines . map renderOne
  where
    renderOne :: (String, Env.Error) -> Text
    renderOne (name, err) = toText name <> ": " <> renderError err

    renderError :: Env.Error -> Text
    renderError = \case
        Env.UnsetError -> "is required but unset"
        Env.EmptyError -> "is set but empty"
        Env.UnreadError msg -> toText msg

-- ── mounts ───────────────────────────────────────────────────────────────────

{- | The three registry endpoints of a mount: the private upstream and public
upstream it reads from, and the mirror target it writes approved packages to —
the three architectural roles, as a record of named roles rather than a positional
tuple. The credential and queue backends live on the 'MirrorTarget' because that
is the one endpoint Écluse authenticates to with its own identity — reads forward
the client's credential or are anonymous (see
@docs\/architecture\/registry-model.md@ → "Credential flow and authority").
-}
data MountRegistries = MountRegistries
    { regPrivateUpstream :: Url
    {- ^ The authoritative, already-vetted upstream. Reads forward the __client's__
    credential; Écluse holds none for it.
    -}
    , regPublicUpstream :: Url
    -- ^ The public upstream, read anonymously and gated by the rules.
    , regMirrorTarget :: MirrorTarget
    -- ^ Where approved packages are written, with the backends used to do so.
    }
    deriving stock (Eq, Show)

{- | The mirror target of a mount: its URL plus the credential and queue backends
used to write to it. This is the sole endpoint carrying an Écluse-minted
credential.
-}
data MirrorTarget = MirrorTarget
    { mtUrl :: Url
    -- ^ The mirror-target registry endpoint.
    , mtCredential :: CredentialBackend
    -- ^ How the bearer token to publish here is obtained.
    , mtQueue :: QueueBackend
    -- ^ The mirror-queue backend the demand-driven mirror jobs are sent to.
    }
    deriving stock (Eq, Show)

{- | One mount: an ecosystem's registries served under a derived path prefix. It
binds the served 'Ecosystem' (a runtime value, not a type parameter), its three
registry endpoints, and an __already-resolved__ rule policy — the shared policy,
optionally refined for this mount. The path prefix is __not__ stored: it is
derived from 'mountEcosystem' (see @Ecluse.Ecosystem.prefixFor@), so it can
neither collide nor be mistyped. Holding the resolved rules (not the raw patch) is
the parse-don't-validate payoff: the dispatcher evaluates them directly with no
further merge.
-}
data Mount = Mount
    { mountEcosystem :: Ecosystem
    -- ^ The ecosystem this mount serves; the path prefix is derived from it.
    , mountRegistries :: MountRegistries
    -- ^ The private\/public\/mirror endpoints this mount proxies.
    , mountPolicy :: [PrecededRule]
    -- ^ The fully-resolved rule set for this mount.
    }
    deriving stock (Eq, Show)

{- | The mount map: every served mount keyed by its 'Ecosystem'. There is exactly
one mount per ecosystem, and the path prefix dispatch matches on is derived from
the key (see @docs\/architecture\/hosting.md@ → "Mounts"). A single-mount
deployment is the one-entry degenerate case.
-}
type MountMap = Map Ecosystem Mount

-- ── rule policy ──────────────────────────────────────────────────────────────

{- | The resolved rule policy: the named rules in force, each at its competing
precedence. Config has merged the document's patches over 'defaultPolicy' and
turned every entry into a 'PrecededRule'.

It is a named map rather than a bare @['PrecededRule']@ so a mount's per-mount
refinement can patch a rule __by name__ (override or suppress it) over this shared
policy.
-}
newtype RulePolicy = RulePolicy
    { policyRules :: Map Text PrecededRule
    {- ^ The rules in force, keyed by the name they were given in config (or the
    built-in name, for a default).
    -}
    }
    deriving stock (Eq, Show)

{- | The built-in default rule policy, sourced from "Ecluse.Rules.Types" so config
never re-encodes rule semantics — it only selects and refines.

The shipped default is a single rule, @min-age@: 'AllowIfPublishedBefore' a 7-day
quarantine window, at its type's 'defaultPrecedence'. This admits public versions
that have survived the window, the core defence against race-to-publish
typosquatting — a floor to extend, not a wall.
-}
defaultPolicy :: RulePolicy
defaultPolicy =
    RulePolicy (Map.singleton "min-age" (atDefault (AllowIfPublishedBefore sevenDays)))
  where
    sevenDays :: NominalDiffTime
    sevenDays = 7 * 86400

{- | A reason a rule-policy merge could not be resolved. Every case is a
__fail-loud__ rejection: a policy that does not resolve cleanly is a startup
failure, never a silently mis-enforced policy.
-}
data PolicyError
    = {- | A new (non-default) name carried no @type@, so it cannot stand as a
      rule. Carries the offending name.
      -}
      MissingRuleType Text
    | {- | A @type@ was named that is not a known, decodable rule. Carries the name
      and the unknown type. A misspelled deny would otherwise vanish and stop
      blocking; a misspelled allow would over-deny.
      -}
      UnknownRuleType Text Text
    | {- | A field the named rule type does not accept (e.g. an @ageSeconds@ on a
      deny rule), or a required value field missing. Carries the name and a
      reason.
      -}
      MalformedRule Text Text
    | {- | An @"enabled": false@ suppression named a rule the default policy does
      not define, so there is nothing to suppress. Carries the name.
      -}
      SuppressUnknownRule Text
    deriving stock (Eq, Show)

-- | Render a 'PolicyError' as a human-facing line for an aggregated failure block.
renderPolicyError :: PolicyError -> Text
renderPolicyError = \case
    MissingRuleType name ->
        "rule " <> quote name <> " is not a default and is missing its \"type\""
    UnknownRuleType name ty ->
        "rule " <> quote name <> " names unknown type " <> quote ty
    MalformedRule name reason ->
        "rule " <> quote name <> ": " <> reason
    SuppressUnknownRule name ->
        "rule " <> quote name <> " disables a rule that no default defines"

{- | Merge a rule patch over a base policy — the named-map merge at the heart of
the rule-policy model. For each patch entry:

* a name the base __defines__ takes a __partial patch__: an explicit @precedence@
  or rule-value field overrides the default, unspecified fields are kept, and
  @"enabled": false@ __suppresses__ it;
* a __new__ name must carry a @type@ (it __adds__ a rule);
* @"enabled": false@ against a name the base does not define is rejected
  ('SuppressUnknownRule') — you cannot suppress a rule out of existence.

Every reference must resolve: an unknown @type@, a missing @type@ on a new name,
a malformed value, or a suppression of a non-existent rule is a 'PolicyError'.
Errors __aggregate__ across all entries.
-}
resolvePolicy :: RulePolicy -> RulePatch -> Either [PolicyError] RulePolicy
resolvePolicy (RulePolicy base) (RulePatch patch) =
    -- The 'Validation' applicative accumulates each entry's errors through the
    -- '[PolicyError]' 'Semigroup', so one resolution reports every offending
    -- entry rather than stopping at the first; on all-success the updates fold
    -- onto the base in entry order.
    validationToEither $
        RulePolicy . foldl' apply base
            <$> traverse (eitherToValidation . resolveEntry) (Map.toList patch)
  where
    -- Apply one resolved update: a suppression deletes, a rule sets.
    apply :: Map Text PrecededRule -> (Text, Maybe PrecededRule) -> Map Text PrecededRule
    apply acc (name, Nothing) = Map.delete name acc
    apply acc (name, Just pr) = Map.insert name pr acc

    -- Resolve one patch entry to either an error list or a (name, update), where
    -- 'Nothing' is a suppression and 'Just' a rule to set.
    resolveEntry :: (Text, RuleEntry) -> Either [PolicyError] (Text, Maybe PrecededRule)
    resolveEntry (name, entry)
        | entryEnabled entry == Just False =
            if Map.member name base
                then Right (name, Nothing)
                else Left [SuppressUnknownRule name]
        | otherwise =
            case Map.lookup name base of
                Just existing -> (name,) . Just <$> patchExisting name entry existing
                Nothing -> (name,) . Just <$> addNew name entry

    -- Patch a default rule: override its value fields and precedence where given.
    -- A restated @type@ must match a known type.
    patchExisting :: Text -> RuleEntry -> PrecededRule -> Either [PolicyError] PrecededRule
    patchExisting name entry (PrecededRule prec rule) = do
        rule' <- patchRuleValue name entry rule
        pure (PrecededRule (fromMaybe prec (entryPrecedence entry)) rule')

    -- Add a brand-new rule: the @type@ is mandatory and must be known.
    addNew :: Text -> RuleEntry -> Either [PolicyError] PrecededRule
    addNew name entry = case entryType entry of
        Nothing -> Left [MissingRuleType name]
        Just ty -> do
            rule <- buildRule name ty entry
            pure (PrecededRule (fromMaybe (defaultPrecedence rule) (entryPrecedence entry)) rule)

{- Build a fresh rule of the named type from its entry, rejecting an unknown
type and a value field the type cannot use.

The effectful @AllowIfRemediatesCve@ rule type is __not__ a member of the rule
model here, so naming it decodes as a clean 'UnknownRuleType' rather than a crash:
config cannot conjure a rule the engine does not implement.
-}
buildRule :: Text -> Text -> RuleEntry -> Either [PolicyError] Rule
buildRule name ty entry = case ty of
    "AllowIfPublishedBefore" -> case entryAgeSeconds entry of
        Just secs
            | secs >= 0 -> Right (AllowIfPublishedBefore (fromInteger secs))
            | otherwise -> Left [MalformedRule name "\"ageSeconds\" must be non-negative"]
        Nothing -> Left [MalformedRule name "\"AllowIfPublishedBefore\" requires \"ageSeconds\""]
    "AllowScope" -> case entryScope entry of
        Just scope -> Right (AllowScope (mkScope scope))
        Nothing -> Left [MalformedRule name "\"AllowScope\" requires \"scope\""]
    "DenyInstallTimeExecution" -> Right DenyInstallTimeExecution
    _ -> Left [UnknownRuleType name ty]

{- Apply an entry's value fields to an existing default rule, keeping its kind:
only the type's own value field may be overridden, and a restated @type@ must
match the existing rule's type.
-}
patchRuleValue :: Text -> RuleEntry -> Rule -> Either [PolicyError] Rule
patchRuleValue name entry rule = do
    () <- checkRestatedType name entry rule
    case rule of
        AllowIfPublishedBefore d -> case entryAgeSeconds entry of
            Just secs
                | secs >= 0 -> Right (AllowIfPublishedBefore (fromInteger secs))
                | otherwise -> Left [MalformedRule name "\"ageSeconds\" must be non-negative"]
            Nothing -> Right (AllowIfPublishedBefore d)
        AllowScope s -> Right (AllowScope (maybe s mkScope (entryScope entry)))
        DenyInstallTimeExecution -> Right DenyInstallTimeExecution

-- A restated @type@ on a patch must match the existing rule's kind, so a typo
-- there (changing the rule's identity by accident) is caught loudly.
checkRestatedType :: Text -> RuleEntry -> Rule -> Either [PolicyError] ()
checkRestatedType name entry rule = case entryType entry of
    Nothing -> Right ()
    Just ty
        | ty == ruleTypeName rule -> Right ()
        | ty `elem` knownRuleTypes -> Left [MalformedRule name ("\"type\" " <> quote ty <> " does not match the default rule it patches")]
        | otherwise -> Left [UnknownRuleType name ty]

-- The wire @type@ name of a rule constructor.
ruleTypeName :: Rule -> Text
ruleTypeName = \case
    AllowScope{} -> "AllowScope"
    AllowIfPublishedBefore{} -> "AllowIfPublishedBefore"
    DenyInstallTimeExecution -> "DenyInstallTimeExecution"

-- The rule @type@ names config can build. @AllowIfRemediatesCve@ is deliberately
-- absent: it is effectful and not part of this rule model, so it is unknown here.
knownRuleTypes :: [Text]
knownRuleTypes = ["AllowScope", "AllowIfPublishedBefore", "DenyInstallTimeExecution"]

-- ── the structured document ──────────────────────────────────────────────────

{- | The structured config document: the mount map and the top-level rule-policy
patch, decoded from a JSON file or the @PROXY_CONFIG@ env blob (one schema for
both). An env-only deployment supplies no document at all.

The rule policy here is the __raw patch__ over the default; 'resolvePolicy' merges
it. Mounts hold the document's per-mount registry shape; their ecosystem keys the
'MountMap' that 'loadConfig' produces.
-}
data ConfigDoc = ConfigDoc
    { docMounts :: Map Ecosystem MountDoc
    {- ^ The mounts declared in the document, keyed by 'Ecosystem'. Empty when the
    document carries only a rule policy.
    -}
    , docRules :: RulePatch
    -- ^ The top-level rule-policy patch that applies to every mount.
    }
    deriving stock (Eq, Show)

{- | A single mount as written in the document: its registry endpoints and
backends, plus an optional per-mount rule refinement that merges over the shared
policy. The served ecosystem is __not__ written here — it is the @mounts@ key the
mount is declared under, and its path prefix is derived from it.
-}
data MountDoc = MountDoc
    { mdocRegistries :: MountRegistries
    -- ^ The mount's three registry endpoints, as written.
    , mdocRules :: RulePatch
    -- ^ The per-mount rule refinement, empty when omitted.
    }
    deriving stock (Eq, Show)

{- | A rule-policy patch: the named-map merge input. Each entry is one of add \/
override \/ suppress against the base (see 'RuleEntry'); resolution is in
'resolvePolicy'.
-}
newtype RulePatch = RulePatch (Map Text RuleEntry)
    deriving stock (Eq, Show)

{- | One entry of a 'RulePatch'. An entry may name an explicit @type@ (to add a
rule or restate one), a @precedence@ (where it competes), @enabled@ (to suppress
a default), and a type-specific value field (@ageSeconds@, @scope@). Which
combination is legal depends on whether the name is a known default — resolved in
'resolvePolicy'.
-}
data RuleEntry = RuleEntry
    { entryType :: Maybe Text
    -- ^ The rule @type@, if given. Required to __add__ a new (non-default) rule.
    , entryPrecedence :: Maybe Int
    -- ^ An explicit precedence; omitted, the rule type's default is used.
    , entryEnabled :: Maybe Bool
    -- ^ @false@ __suppresses__ a default rule; otherwise the rule is in force.
    , entryAgeSeconds :: Maybe Integer
    -- ^ The quarantine window for an @AllowIfPublishedBefore@ rule, in seconds.
    , entryScope :: Maybe Text
    -- ^ The scope for an @AllowScope@ rule.
    }
    deriving stock (Eq, Show)

{- | Decode a 'ConfigDoc' from JSON bytes (a file's contents or the @PROXY_CONFIG@
blob). __Strict__: an unknown key anywhere in the document is rejected, as is an
unparseable JSON body; the 'Left' carries the decode error.
-}
decodeDocument :: ByteString -> Either Text ConfigDoc
decodeDocument = first toText . eitherDecodeStrict

instance FromJSON ConfigDoc where
    parseJSON = withObject "config document" $ \o -> do
        rejectUnknownKeys "config document" ["mounts", "rules"] o
        -- The @mounts@ object is keyed by ecosystem name (@npm@, @pypi@); each key
        -- is resolved through 'parseEcosystem' and an unknown ecosystem is a loud
        -- failure (strict decoding, never a silent skip) — the path prefix is then
        -- derived from the ecosystem, never declared.
        rawMounts <- o .:? "mounts" .!= mempty
        ConfigDoc
            <$> parseMounts rawMounts
            <*> o .:? "rules" .!= emptyPatch
      where
        parseMounts :: KeyMap MountDoc -> Parser (Map Ecosystem MountDoc)
        parseMounts =
            fmap Map.fromList . traverse parseKeyed . KeyMap.toList

        parseKeyed :: (Key.Key, MountDoc) -> Parser (Ecosystem, MountDoc)
        parseKeyed (k, mdoc) = case parseEcosystem (Key.toText k) of
            Just eco -> pure (eco, mdoc)
            Nothing -> fail ("unknown mount ecosystem " <> show (Key.toText k))

instance FromJSON MountDoc where
    parseJSON = withObject "mount" $ \o -> do
        rejectUnknownKeys
            "mount"
            ["privateUpstream", "publicUpstream", "mirrorTarget", "rules"]
            o
        registries <-
            MountRegistries
                <$> (o .: "privateUpstream" >>= parseUrl)
                <*> (o .: "publicUpstream" >>= parseUrl)
                <*> o .: "mirrorTarget"
        MountDoc registries <$> o .:? "rules" .!= emptyPatch

instance FromJSON MirrorTarget where
    parseJSON = withObject "mirrorTarget" $ \o -> do
        rejectUnknownKeys "mirrorTarget" ["url", "credential", "queue"] o
        MirrorTarget
            <$> (o .: "url" >>= parseUrl)
            <*> (o .: "credential" >>= parseEnum parseCredentialBackend "credential")
            <*> (o .: "queue" >>= parseEnum parseQueueBackend "queue")

instance FromJSON RulePatch where
    parseJSON = withObject "rules" $ \o ->
        RulePatch . Map.fromList <$> traverse decodeEntry (KeyMap.toList o)
      where
        decodeEntry (k, v) = (Key.toText k,) <$> parseJSON v

instance FromJSON RuleEntry where
    parseJSON = withObject "rule" $ \o -> do
        -- Secrets never live in the document: a token field is a hard decode
        -- failure, enforcing the env-only-secrets boundary in the type system
        -- rather than merely documenting it.
        rejectSecretKeys o
        rejectUnknownKeys "rule" ["type", "precedence", "enabled", "ageSeconds", "scope"] o
        RuleEntry
            <$> o .:? "type"
            <*> o .:? "precedence"
            <*> o .:? "enabled"
            <*> o .:? "ageSeconds"
            <*> o .:? "scope"

-- ── decoder helpers ──────────────────────────────────────────────────────────

-- An empty rule patch (the absent-rules default).
emptyPatch :: RulePatch
emptyPatch = RulePatch Map.empty

{- Reject any object key not in the accepted set, naming the offender. This is
the explicit-key-set form of strict decoding: aeson's record decoders silently
ignore extra keys, so the accepted set is enumerated and an unknown key fails the
parse — catching an operator's typo loudly rather than dropping it.
-}
rejectUnknownKeys :: String -> [Key.Key] -> KeyMap Value -> Parser ()
rejectUnknownKeys context accepted o =
    case filter (`notElem` accepted) (KeyMap.keys o) of
        [] -> pure ()
        unknown ->
            fail
                ( "unexpected "
                    <> context
                    <> " key(s): "
                    <> intercalate ", " (map (show . Key.toText) unknown)
                )

{- Reject a known secret-bearing key inside a rule object, so a token can never
be smuggled into the reviewable document. Secrets are environment-only.
-}
rejectSecretKeys :: KeyMap Value -> Parser ()
rejectSecretKeys o =
    case filter (`KeyMap.member` o) secretKeys of
        [] -> pure ()
        present ->
            fail
                ( "secret key(s) are not allowed in the config document (use environment variables): "
                    <> intercalate ", " (map (show . Key.toText) present)
                )
  where
    secretKeys :: [Key.Key]
    secretKeys = ["token", "authToken", "password", "secret", "credentialToken"]

-- Parse a 'Url' value, surfacing 'mkUrl's reason as a decoder failure.
parseUrl :: Value -> Parser Url
parseUrl = withText' $ \t -> either (fail . toString) pure (mkUrl t)

-- Parse a string-valued enum via its 'Text' parser, naming the field on failure.
parseEnum :: (Text -> Either Text a) -> String -> Value -> Parser a
parseEnum parser field =
    withText' $ \t -> either (\e -> fail (field <> ": " <> toString e)) pure (parser t)

-- Run a 'Text'-consuming parser over a JSON string value.
withText' :: (Text -> Parser a) -> Value -> Parser a
withText' f = \case
    String t -> f t
    other -> fail ("expected a string, but encountered " <> valueKind other)

-- A short, human description of a JSON value's kind, for parse-error messages.
valueKind :: Value -> String
valueKind = \case
    Object{} -> "an object"
    Array{} -> "an array"
    Number{} -> "a number"
    Bool{} -> "a boolean"
    Null -> "null"
    String{} -> "a string"

-- ── assembly ─────────────────────────────────────────────────────────────────

{- | The fully assembled, validated configuration the composition root consumes:
the environment layer plus the mount map, every mount carrying its resolved rule
policy. No raw patch is left here — all merging happened during 'loadConfig'.
-}
data Config = Config
    { configEnv :: EnvConfig
    -- ^ The process-level environment layer.
    , configMounts :: MountMap
    -- ^ Every served mount, keyed by 'Ecosystem', each with a resolved policy.
    }
    deriving stock (Eq, Show)

{- | Assemble the final 'Config' from the parsed environment layer and an optional
config document.

* With __no document__, the single-mount environment variables __desugar__ into a
  one-entry mount map for the __npm__ ecosystem (its prefix derived as @\/npm@),
  running on the built-in 'defaultPolicy'.
* With a __document__, the document's top-level rule patch is merged over the
  default to form the shared policy, every declared mount is resolved against it
  (applying its own refinement), and — when the document declares no mounts — the
  env single-mount is desugared onto the shared policy.

Every rule-policy merge is resolved here, so any 'PolicyError' (a bad merge
reference, a missing or unknown @type@) surfaces as a startup failure, aggregated
across all mounts.
-}
loadConfig :: EnvConfig -> Maybe ConfigDoc -> Either [PolicyError] Config
loadConfig env mDoc = case mDoc of
    Nothing -> Right (envOnly defaultPolicy)
    Just doc ->
        resolvePolicy defaultPolicy (docRules doc) >>= \shared ->
            if Map.null (docMounts doc)
                then Right (envOnly shared)
                else Config env <$> resolveMounts shared (docMounts doc)
  where
    -- The env single-mount desugared onto a resolved shared policy. An env-only
    -- launch serves the npm ecosystem, keyed by it and served under the derived
    -- @\/npm@ prefix — never the root @\/@, which the no-root rule forbids.
    envOnly :: RulePolicy -> Config
    envOnly policy = Config env (Map.singleton Npm (envMount (rulesOf policy)))

    envMount :: [PrecededRule] -> Mount
    envMount rules =
        Mount
            { mountEcosystem = Npm
            , mountRegistries =
                MountRegistries
                    { regPrivateUpstream = cfgPrivateUpstream env
                    , regPublicUpstream = cfgPublicUpstream env
                    , regMirrorTarget =
                        MirrorTarget
                            { -- An unset MIRROR_TARGET_URL folds the mirror target onto
                              -- the private upstream (one registry, both read and written).
                              mtUrl = fromMaybe (cfgPrivateUpstream env) (cfgMirrorTarget env)
                            , -- The write credential is selected by the process-level
                              -- provider (static / codeartifact / gcp-artifact-registry);
                              -- it does not fold onto the private upstream's credential.
                              mtCredential = cfgMirrorTargetCredentialProvider env
                            , mtQueue = cfgQueueBackend env
                            }
                    }
            , mountPolicy = rules
            }

-- Resolve every document mount against the shared policy, applying each mount's
-- own refinement and aggregating policy errors across all of them. The map key is
-- the served ecosystem, carried onto each resolved 'Mount'.
resolveMounts :: RulePolicy -> Map Ecosystem MountDoc -> Either [PolicyError] MountMap
resolveMounts shared mounts =
    -- One pass over the mounts, accumulating every mount's policy errors through
    -- the 'Validation' applicative so a failed load names all of them at once.
    validationToEither $
        Map.fromList <$> traverse (eitherToValidation . resolveOne) (Map.toList mounts)
  where
    resolveOne :: (Ecosystem, MountDoc) -> Either [PolicyError] (Ecosystem, Mount)
    resolveOne (eco, mdoc) =
        resolvePolicy shared (mdocRules mdoc) >>= \refined ->
            Right
                ( eco
                , Mount
                    { mountEcosystem = eco
                    , mountRegistries = mdocRegistries mdoc
                    , mountPolicy = rulesOf refined
                    }
                )

-- The rules of a resolved policy as the engine's flat list.
rulesOf :: RulePolicy -> [PrecededRule]
rulesOf = Map.elems . policyRules

-- ── small shared helpers ─────────────────────────────────────────────────────

-- Pair a rule with its type's default precedence (the omitted-precedence case).
atDefault :: Rule -> PrecededRule
atDefault r = PrecededRule (defaultPrecedence r) r

-- Wrap text in double quotes for a human-facing message.
quote :: Text -> Text
quote t = "\"" <> t <> "\""
