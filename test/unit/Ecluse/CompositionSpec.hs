module Ecluse.CompositionSpec (spec) where

import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime (UTCTime), fromGregorian)
import Test.Hspec

import Ecluse (mirrorWriteProvider, mountBindingFor)
import Ecluse.Composition (
    BootError (..),
    CredentialProviders,
    MirrorQueuePlan (..),
    cacheConfigFor,
    composeBindings,
    initCredentialProviders,
    initializedBackends,
    lookupProvider,
    memoryQueueBootWarning,
    mirrorQueuePlanWarning,
    planMirrorCredential,
    planMirrorQueue,
    planMounts,
    renderBootError,
    resolveCodeArtifactConfig,
 )
import Ecluse.Config (
    Config,
    ConfigDoc,
    CredentialBackend (..),
    EnvConfig,
    PolicyError (UnknownRuleType),
    QueueBackend (..),
    decodeDocument,
    loadConfig,
    parseEnvPure,
 )
import Ecluse.Core.Credential (authSecret, currentToken, unSecret)
import Ecluse.Core.Credential.CodeArtifact (CodeArtifactConfig (caDomain, caDomainOwner, caDurationSeconds, caRegion))
import Ecluse.Core.Credential.Refresh (noCredentialReporters)
import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Package (HashAlg (SHA1, SHA512), mkScope)
import Ecluse.Core.Package.Integrity (
    defaultMinIntegrity,
    defaultMinTrustedIntegrity,
    mkMinIntegrity,
    mkMinTrustedIntegrity,
 )
import Ecluse.Core.Queue (defaultMemoryQueueConfig)
import Ecluse.Core.Queue.Sqs (SqsConfig (sqsEndpoint, sqsQueueUrl, sqsRegion), SqsEndpoint (endpointHost, endpointPort, endpointSecure))
import Ecluse.Core.Security (Limits (maxBodyBytes, maxNestingDepth, maxVersionCount), TarballHostPolicy (AnyAllowlistedHost, SameHostAsPackument), defaultLimits)
import Ecluse.Core.Server.Cache (CacheConfig (cacheMaxEntries, cacheTtl))
import Ecluse.Core.Server.Context (
    MountBinding (bindingPackumentDeps, bindingPrefix, bindingPublishDeps),
    PackumentDeps (..),
    PublishDeps (..),
 )
import Ecluse.Core.Server.Response (unHelpMessage)

{- | Tests for the composition root's boot-time wiring. They exercise the two
promises of the slice: a valid configuration produces served mount bindings with
real packument-serve dependencies (so packuments are merged, not @501@ stubs), and
every boot problem — an unresolved rule policy, a configured mount with no adapter,
a credential reference that does not resolve — is a fail-fast, __aggregated__ boot
error. Pure of IO: the clock and the ecosystem-to-adapter resolver are injected, so
nothing here opens a socket.
-}
spec :: Spec
spec = do
    credentialProvidersSpec
    mirrorWriteProviderSpec
    mirrorQueueSpec
    mirrorCredentialSpec
    cacheConfigSpec
    composeBindingsSpec
    bootErrorSpec
    publishWiringSpec
    renderSpec

-- ── fixtures ──────────────────────────────────────────────────────────────────

-- | A fixed clock for the injected 'pdNow'; never advanced (no timing here).
fixedNow :: UTCTime
fixedNow = UTCTime (fromGregorian 2026 6 23) 0

{- | A minimal valid environment with a static mirror-target token, so the env
single-mount's @static@ credential reference resolves.
-}
staticEnvVars :: [(String, String)]
staticEnvVars =
    [ ("PRIVATE_UPSTREAM_URL", "https://private.example.test")
    , ("PUBLIC_UPSTREAM_URL", "https://public.example.test")
    , ("MIRROR_TARGET_URL", "https://mirror.example.test")
    , ("MIRROR_QUEUE_URL", "https://sqs.example.test/q")
    , ("MIRROR_TARGET_TOKEN", "mirror-write-token")
    ]

-- | The same environment without the static write token (no provider initialized).
noTokenEnvVars :: [(String, String)]
noTokenEnvVars = filter ((/= "MIRROR_TARGET_TOKEN") . fst) staticEnvVars

-- Drop any MIRROR_TARGET_URL entry, so a test that supplies its own (a CodeArtifact
-- endpoint to parse) is not shadowed by the base fixture's value.
withoutMirrorTargetUrl :: [(String, String)] -> [(String, String)]
withoutMirrorTargetUrl = filter ((/= "MIRROR_TARGET_URL") . fst)

-- Drop the MIRROR_QUEUE_URL entry, so a test can exercise a backend with no queue URL
-- set (memory needs none; sqs fails loud without one).
withoutQueueUrl :: [(String, String)] -> [(String, String)]
withoutQueueUrl = filter ((/= "MIRROR_QUEUE_URL") . fst)

expectEnv :: [(String, String)] -> IO EnvConfig
expectEnv = either (\errs -> fail ("env parse failed: " <> show errs)) pure . parseEnvPure

-- Build the credential providers, failing the test on a boot error (the static-path
-- examples expect a clean build).
expectProviders :: EnvConfig -> IO CredentialProviders
expectProviders env =
    initCredentialProviders noCredentialReporters env >>= either (\errs -> fail ("provider init failed: " <> show errs)) pure

expectDoc :: ByteString -> IO ConfigDoc
expectDoc = either (\e -> fail ("document decode failed: " <> toString e)) pure . decodeDocument

{- | A document mount keyed by the given ecosystem, naming the given credential
backend — for the no-adapter and unresolved-credential boot-error cases.
-}
mountDoc :: Text -> Text -> ByteString
mountDoc eco credential =
    encodeUtf8
        ( "{\"mounts\":{\""
            <> eco
            <> "\":{\"privateUpstream\":\"https://priv\",\"publicUpstream\":\"https://pub\",\
               \\"mirrorTarget\":{\"url\":\"https://mir\",\"credential\":\""
            <> credential
            <> "\",\"queue\":\"sqs\"}}}}"
        )

-- Build the served bindings from an env + optional document through 'planMounts',
-- with the real adapter resolver, the fixed clock, and the env's static providers.
planFrom :: EnvConfig -> Maybe ConfigDoc -> IO (Either [BootError] [MountBinding])
planFrom env mDoc =
    initCredentialProviders noCredentialReporters env >>= \case
        Left errs -> pure (Left errs)
        Right providers -> planMounts mountBindingFor (pure fixedNow) providers env mDoc

-- ── credential providers ──────────────────────────────────────────────────────

credentialProvidersSpec :: Spec
credentialProvidersSpec = describe "initCredentialProviders" $ do
    it "initializes the static provider from MIRROR_TARGET_TOKEN when set" $ do
        env <- expectEnv staticEnvVars
        providers <- expectProviders env
        initializedBackends providers `shouldBe` fromList [StaticCredential]

    it "yields the configured static token through the initialized provider" $ do
        env <- expectEnv staticEnvVars
        providers <- expectProviders env
        case lookupProvider StaticCredential providers of
            Nothing -> expectationFailure "expected an initialized static provider"
            Just provider -> do
                tok <- currentToken provider
                unSecret (authSecret tok) `shouldBe` "mirror-write-token"

    it "initializes no provider when no static token is supplied" $ do
        env <- expectEnv noTokenEnvVars
        providers <- expectProviders env
        initializedBackends providers `shouldBe` mempty
        -- 'CredentialProvider' has no 'Eq'\/'Show' (it is a record of an IO
        -- function), so resolution is asserted through 'isJust' on a 'Bool'.
        isJust (lookupProvider StaticCredential providers) `shouldBe` False
        -- The cloud-minted backends are not selected here, so they never resolve.
        isJust (lookupProvider CodeArtifactCredential providers) `shouldBe` False
        isJust (lookupProvider AdcCredential providers) `shouldBe` False

    it "refuses to build when the gcp-artifact-registry provider is selected (not built)" $ do
        -- 'CredentialProviders' has no 'Show', so the Left is extracted to compare.
        env <- expectEnv (("MIRROR_TARGET_CREDENTIAL_PROVIDER", "gcp-artifact-registry") : staticEnvVars)
        result <- initCredentialProviders noCredentialReporters env
        leftToMaybe result `shouldBe` Just [MirrorCredentialProviderUnavailable AdcCredential]

    it "refuses to build when codeartifact is selected but its domain cannot be resolved" $ do
        -- A non-CodeArtifact MIRROR_TARGET_URL and no explicit keys: domain and owner
        -- resolve by neither route, so both are named in the aggregated boot failure.
        env <- expectEnv (("MIRROR_TARGET_CREDENTIAL_PROVIDER", "codeartifact") : ("AWS_REGION", "us-east-1") : staticEnvVars)
        result <- initCredentialProviders noCredentialReporters env
        leftToMaybe result
            `shouldBe` Just
                [ CodeArtifactConfigMissing "MIRROR_TARGET_CODEARTIFACT_DOMAIN"
                , CodeArtifactConfigMissing "MIRROR_TARGET_CODEARTIFACT_DOMAIN_OWNER"
                ]

-- ── mirror-write credential selection ─────────────────────────────────────────

mirrorWriteProviderSpec :: Spec
mirrorWriteProviderSpec = describe "mirrorWriteProvider" $ do
    it "selects the initialized static provider as the mirror-write credential" $ do
        env <- expectEnv staticEnvVars
        providers <- expectProviders env
        tok <- currentToken (mirrorWriteProvider StaticCredential providers)
        unSecret (authSecret tok) `shouldBe` "mirror-write-token"

    it "falls back to the empty placeholder when the selected provider is not initialized" $ do
        env <- expectEnv noTokenEnvVars
        providers <- expectProviders env
        tok <- currentToken (mirrorWriteProvider StaticCredential providers)
        unSecret (authSecret tok) `shouldBe` ""

-- ── mirror-queue backend selection ────────────────────────────────────────────

mirrorQueueSpec :: Spec
mirrorQueueSpec = describe "planMirrorQueue" $ do
    it "selects the SQS backend from the configured queue URL and region" $ do
        env <- expectEnv (("AWS_REGION", "us-east-1") : staticEnvVars)
        cfg <- expectSqsBackend env
        sqsQueueUrl cfg `shouldBe` "https://sqs.example.test/q"
        sqsRegion cfg `shouldBe` "us-east-1"

    it "fails fast when the SQS backend has no AWS_REGION" $ do
        -- AWS_REGION is required for the AWS queue; absent, the backend cannot be
        -- region-scoped, so it is a loud boot failure rather than a silent default.
        env <- expectEnv staticEnvVars
        planMirrorQueue env `shouldBe` Left [QueueRegionMissing]

    it "treats a blank AWS_REGION as missing" $ do
        env <- expectEnv (("AWS_REGION", "   ") : staticEnvVars)
        planMirrorQueue env `shouldBe` Left [QueueRegionMissing]

    it "fails fast when the SQS backend has no MIRROR_QUEUE_URL" $ do
        -- MIRROR_QUEUE_URL is optional at the env layer but required for sqs here: the
        -- jobs need a queue to be sent to, so an absent one is a fail-loud boot error.
        env <- expectEnv (("AWS_REGION", "us-east-1") : withoutQueueUrl staticEnvVars)
        planMirrorQueue env `shouldBe` Left [QueueUrlMissing SqsQueue]

    it "aggregates a missing region and a missing queue URL under sqs in one report" $ do
        env <- expectEnv (withoutQueueUrl staticEnvVars)
        planMirrorQueue env `shouldBe` Left [QueueRegionMissing, QueueUrlMissing SqsQueue]

    it "refuses the GCP pubsub backend as not built in this binary (no silent fallback)" $ do
        -- The pubsub arm is recognised by config (S03) but has no backend compiled in;
        -- it must route to a clear "not built" error, never quietly to a different queue.
        env <- expectEnv (("MIRROR_QUEUE_PROVIDER", "pubsub") : ("AWS_REGION", "us-east-1") : staticEnvVars)
        planMirrorQueue env `shouldBe` Left [QueueProviderUnavailable PubSubQueue]

    it "selects the bounded in-memory backend with the configured cap (no AWS_REGION or MIRROR_QUEUE_URL needed)" $ do
        -- The memory backend is an explicit operator choice that needs no cloud queue:
        -- it carries only its depth cap, and neither AWS_REGION nor MIRROR_QUEUE_URL is
        -- consulted, so it resolves cleanly with both absent.
        env <-
            expectEnv
                ( ("MIRROR_QUEUE_PROVIDER", "memory")
                    : ("MIRROR_QUEUE_MEMORY_MAX_DEPTH", "1234")
                    : withoutQueueUrl staticEnvVars
                )
        planMirrorQueue env `shouldBe` Right (MemoryBackend (defaultMemoryQueueConfig 1234))

    it "defaults the in-memory backend's cap when MIRROR_QUEUE_MEMORY_MAX_DEPTH is unset" $ do
        env <- expectEnv (("MIRROR_QUEUE_PROVIDER", "memory") : staticEnvVars)
        planMirrorQueue env `shouldBe` Right (MemoryBackend (defaultMemoryQueueConfig 50000))

    it "warns loudly on selecting the in-memory backend, and not on the durable SQS one" $ do
        -- AC3: selecting memory emits a loud non-durable/best-effort boot warning;
        -- a durable backend warrants none. The composition root logs the Just.
        memEnv <- expectEnv (("MIRROR_QUEUE_PROVIDER", "memory") : staticEnvVars)
        sqsEnv <- expectEnv (("AWS_REGION", "us-east-1") : staticEnvVars)
        (mirrorQueuePlanWarning <$> planMirrorQueue memEnv) `shouldBe` Right (Just memoryQueueBootWarning)
        (mirrorQueuePlanWarning <$> planMirrorQueue sqsEnv) `shouldBe` Right Nothing
        -- The warning names the load-bearing caveats so an operator cannot miss them.
        memoryQueueBootWarning `shouldSatisfy` ("NON-DURABLE" `T.isInfixOf`)
        memoryQueueBootWarning `shouldSatisfy` ("BEST-EFFORT" `T.isInfixOf`)

    it "honours the AWS-standard SQS endpoint override (AWS_ENDPOINT_URL_SQS)" $ do
        env <-
            expectEnv
                ( ("AWS_REGION", "us-east-1")
                    : ("AWS_ENDPOINT_URL_SQS", "http://localhost:4566")
                    : ("AWS_ACCESS_KEY_ID", "test")
                    : ("AWS_SECRET_ACCESS_KEY", "sqs-secret-xyz")
                    : staticEnvVars
                )
        cfg <- expectSqsBackend env
        case sqsEndpoint cfg of
            Just ep -> do
                endpointSecure ep `shouldBe` False
                endpointHost ep `shouldBe` "localhost"
                endpointPort ep `shouldBe` 4566
                -- N2: the endpoint secret key is a redacted Secret — its plaintext
                -- must never reach the derived Show of the config/endpoint.
                let rendered = show cfg :: Text
                rendered `shouldNotSatisfy` ("sqs-secret-xyz" `T.isInfixOf`)
                rendered `shouldSatisfy` ("REDACTED" `T.isInfixOf`)
            Nothing -> expectationFailure "expected the endpoint override to resolve"

    it "falls back to the generic AWS_ENDPOINT_URL when the SQS-specific one is unset" $ do
        env <-
            expectEnv
                ( ("AWS_REGION", "us-east-1")
                    : ("AWS_ENDPOINT_URL", "https://sqs.vpce.example:8443")
                    : staticEnvVars
                )
        cfg <- expectSqsBackend env
        ((endpointSecure &&& endpointPort) <$> sqsEndpoint cfg) `shouldBe` Just (True, 8443)

    it "uses AWS default resolution (no endpoint) when no override is set" $ do
        env <- expectEnv (("AWS_REGION", "us-east-1") : staticEnvVars)
        cfg <- expectSqsBackend env
        sqsEndpoint cfg `shouldBe` Nothing

    it "fails fast on a malformed SQS endpoint override" $ do
        env <- expectEnv (("AWS_REGION", "us-east-1") : ("AWS_ENDPOINT_URL_SQS", "not-a-url") : staticEnvVars)
        planMirrorQueue env `shouldBe` Left [QueueEndpointMalformed "not-a-url"]
  where
    -- Resolve the SQS config from a plan that must select the SQS backend, failing
    -- the example with the actual plan / boot errors otherwise.
    expectSqsBackend :: EnvConfig -> IO SqsConfig
    expectSqsBackend env = case planMirrorQueue env of
        Right (SqsBackend cfg) -> pure cfg
        other -> fail ("expected an SQS mirror-queue plan, got: " <> show other)

-- ── mirror-target credential provider selection ───────────────────────────────

mirrorCredentialSpec :: Spec
mirrorCredentialSpec = describe "planMirrorCredential / resolveCodeArtifactConfig" $ do
    it "selects the static provider (no CodeArtifact config) by default" $ do
        env <- expectEnv staticEnvVars
        planMirrorCredential env `shouldBe` Right Nothing

    it "refuses the gcp-artifact-registry provider as not built" $ do
        env <- expectEnv (("MIRROR_TARGET_CREDENTIAL_PROVIDER", "gcp-artifact-registry") : staticEnvVars)
        planMirrorCredential env `shouldBe` Left [MirrorCredentialProviderUnavailable AdcCredential]

    it "resolves CodeArtifact inputs from the explicit MIRROR_TARGET_CODEARTIFACT_* keys" $ do
        env <-
            expectEnv
                ( ("MIRROR_TARGET_CREDENTIAL_PROVIDER", "codeartifact")
                    : ("MIRROR_TARGET_CODEARTIFACT_DOMAIN", "my-domain")
                    : ("MIRROR_TARGET_CODEARTIFACT_DOMAIN_OWNER", "111122223333")
                    : ("MIRROR_TARGET_CODEARTIFACT_REGION", "eu-west-1")
                    : ("MIRROR_TARGET_CODEARTIFACT_TOKEN_DURATION_SECONDS", "1800")
                    : staticEnvVars
                )
        case resolveCodeArtifactConfig env of
            Right cfg -> do
                caDomain cfg `shouldBe` "my-domain"
                caDomainOwner cfg `shouldBe` Just "111122223333"
                caRegion cfg `shouldBe` "eu-west-1"
                caDurationSeconds cfg `shouldBe` Just 1800
            Left errs -> expectationFailure ("expected a resolved CodeArtifact config, got " <> show errs)

    it "parses the domain, owner, and region from a CodeArtifact MIRROR_TARGET_URL host" $ do
        -- (b) the URL-parse fallback: a real CodeArtifact npm endpoint host, with a
        -- hyphenated domain so the 12-digit owner after the LAST hyphen is recovered.
        env <-
            expectEnv
                ( ("MIRROR_TARGET_CREDENTIAL_PROVIDER", "codeartifact")
                    : ("MIRROR_TARGET_URL", "https://my-domain-111122223333.d.codeartifact.us-west-2.amazonaws.com/npm/my-repo/")
                    : withoutMirrorTargetUrl staticEnvVars
                )
        case resolveCodeArtifactConfig env of
            Right cfg -> do
                caDomain cfg `shouldBe` "my-domain"
                caDomainOwner cfg `shouldBe` Just "111122223333"
                caRegion cfg `shouldBe` "us-west-2"
            Left errs -> expectationFailure ("expected the host to parse, got " <> show errs)

    it "ranks the host-encoded region above AWS_REGION (mints against the domain's region)" $ do
        -- N3: the endpoint host encodes the domain's authoritative region, so a
        -- cross-region deploy (us-west-2 URL, AWS_REGION=us-east-1) mints in us-west-2.
        env <-
            expectEnv
                ( ("MIRROR_TARGET_CREDENTIAL_PROVIDER", "codeartifact")
                    : ("MIRROR_TARGET_URL", "https://my-domain-111122223333.d.codeartifact.us-west-2.amazonaws.com/npm/my-repo/")
                    : ("AWS_REGION", "us-east-1")
                    : withoutMirrorTargetUrl staticEnvVars
                )
        (caRegion <$> rightToMaybe (resolveCodeArtifactConfig env)) `shouldBe` Just "us-west-2"

    it "falls the region back to AWS_REGION only when neither explicit key nor host supplies it" $ do
        -- A non-CodeArtifact mirror URL (no host region) and no explicit region: the
        -- process-wide AWS_REGION is the last resort.
        env <-
            expectEnv
                ( ("MIRROR_TARGET_CREDENTIAL_PROVIDER", "codeartifact")
                    : ("MIRROR_TARGET_CODEARTIFACT_DOMAIN", "d")
                    : ("MIRROR_TARGET_CODEARTIFACT_DOMAIN_OWNER", "111122223333")
                    : ("AWS_REGION", "ap-south-1")
                    : staticEnvVars
                )
        (caRegion <$> rightToMaybe (resolveCodeArtifactConfig env)) `shouldBe` Just "ap-south-1"

    it "fails loud, naming the owner key, on a hyphen-bearing non-CodeArtifact host" $ do
        -- B1: a host whose tail after the last hyphen is not a 12-digit account id is
        -- not a CodeArtifact endpoint, so its owner never mis-parses (e.g. owner "b").
        -- With domain + region supplied explicitly, only the owner is unresolved.
        env <-
            expectEnv
                ( ("MIRROR_TARGET_CREDENTIAL_PROVIDER", "codeartifact")
                    : ("MIRROR_TARGET_URL", "https://a-b.d.codeartifact.us-east-1.amazonaws.com/npm/r/")
                    : ("MIRROR_TARGET_CODEARTIFACT_DOMAIN", "a")
                    : ("MIRROR_TARGET_CODEARTIFACT_REGION", "us-east-1")
                    : withoutMirrorTargetUrl staticEnvVars
                )
        resolveCodeArtifactConfig env `shouldBe` Left [CodeArtifactConfigMissing "MIRROR_TARGET_CODEARTIFACT_DOMAIN_OWNER"]

    it "fails loud on an explicit owner that is not a 12-digit account id" $ do
        -- B1: a malformed explicit owner must be rejected, not sail through.
        env <-
            expectEnv
                ( ("MIRROR_TARGET_CREDENTIAL_PROVIDER", "codeartifact")
                    : ("MIRROR_TARGET_CODEARTIFACT_DOMAIN", "d")
                    : ("MIRROR_TARGET_CODEARTIFACT_DOMAIN_OWNER", "123")
                    : ("MIRROR_TARGET_CODEARTIFACT_REGION", "us-east-1")
                    : staticEnvVars
                )
        resolveCodeArtifactConfig env
            `shouldBe` Left [CodeArtifactConfigInvalid "MIRROR_TARGET_CODEARTIFACT_DOMAIN_OWNER" "expected a 12-digit AWS account id"]

    it "fails loud, naming each unresolved input, when neither key nor host supplies it" $ do
        -- A non-CodeArtifact mirror URL and no explicit keys / AWS_REGION: domain,
        -- owner, and region are all unresolved and all named in one aggregated failure.
        env <- expectEnv (("MIRROR_TARGET_CREDENTIAL_PROVIDER", "codeartifact") : staticEnvVars)
        resolveCodeArtifactConfig env
            `shouldBe` Left
                [ CodeArtifactConfigMissing "MIRROR_TARGET_CODEARTIFACT_DOMAIN"
                , CodeArtifactConfigMissing "MIRROR_TARGET_CODEARTIFACT_DOMAIN_OWNER"
                , CodeArtifactConfigMissing "MIRROR_TARGET_CODEARTIFACT_REGION"
                ]

-- ── config-derived cache settings ─────────────────────────────────────────────

cacheConfigSpec :: Spec
cacheConfigSpec = describe "cacheConfigFor" $
    it "maps the env cache tunables onto the metadata CacheConfig" $ do
        env <- expectEnv (("METADATA_CACHE_TTL_SECONDS", "45") : ("METADATA_CACHE_MAX_ENTRIES", "256") : staticEnvVars)
        cacheTtl (cacheConfigFor env) `shouldBe` (45 :: NominalDiffTime)
        cacheMaxEntries (cacheConfigFor env) `shouldBe` 256

-- ── config-driven serving ─────────────────────────────────────────────────────

composeBindingsSpec :: Spec
composeBindingsSpec = describe "planMounts / composeBindings (config-driven serving)" $ do
    it "produces one npm binding with packument-serve deps wired (served, not a 501 stub)" $ do
        env <- expectEnv staticEnvVars
        planFrom env Nothing >>= \case
            Left errs -> expectationFailure ("unexpected boot errors: " <> show errs)
            Right [binding] -> do
                bindingPrefix binding `shouldBe` ("npm" :| [])
                case bindingPackumentDeps binding of
                    Nothing -> expectationFailure "expected packument deps wired, got the 501 stub"
                    Just deps -> do
                        pdPrivateBaseUrl deps `shouldBe` "https://private.example.test"
                        pdPublicBaseUrl deps `shouldBe` "https://public.example.test"
                        -- With no PROXY_PUBLIC_URL the mount base falls back to the
                        -- derived relative prefix (the npm CLI cannot consume this;
                        -- the absolute form below is what a real client needs).
                        pdMountBaseUrl deps `shouldBe` "/npm"
                        -- The mirror-target endpoint is wired from the mount's config,
                        -- for the demand-driven mirror job's publish destination.
                        pdMirrorTarget deps `shouldBe` "https://mirror.example.test"
            Right other -> expectationFailure ("expected exactly one binding, got " <> show (length other))

    it "rewrites the tarball base to an absolute URL under PROXY_PUBLIC_URL" $ do
        -- With PROXY_PUBLIC_URL set, dist.tarball rewrites to an absolute URL a real
        -- npm client can fetch, instead of the npm-incompatible relative path.
        env <- expectEnv (("PROXY_PUBLIC_URL", "https://proxy.example.test") : staticEnvVars)
        planFrom env Nothing >>= \case
            Right [binding] -> case bindingPackumentDeps binding of
                Just deps -> pdMountBaseUrl deps `shouldBe` "https://proxy.example.test/npm"
                Nothing -> expectationFailure "expected packument deps wired"
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "drops a trailing slash on PROXY_PUBLIC_URL so the base joins with one separator" $ do
        env <- expectEnv (("PROXY_PUBLIC_URL", "https://proxy.example.test/") : staticEnvVars)
        planFrom env Nothing >>= \case
            Right [binding] -> case bindingPackumentDeps binding of
                Just deps -> pdMountBaseUrl deps `shouldBe` "https://proxy.example.test/npm"
                Nothing -> expectationFailure "expected packument deps wired"
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "carries the resolved rule policy onto the binding's packument deps" $ do
        env <- expectEnv staticEnvVars
        planFrom env Nothing >>= \case
            Right [binding] -> case bindingPackumentDeps binding of
                -- 'PreparedRule' has no 'Show' (it carries an evaluator), so assert on
                -- the count rather than the rules themselves.
                Just deps -> null (pdRules deps) `shouldBe` False
                Nothing -> expectationFailure "expected packument deps wired"
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "threads the inbound edge token, clock, and help message onto the deps" $ do
        -- Forces the remaining 'PackumentDeps' fields: the inbound token (from
        -- PROXY_AUTH_TOKEN), the injected clock ('pdNow'), and the operator help
        -- message — all wired by the composition root.
        env <- expectEnv (("PROXY_AUTH_TOKEN", "edge-secret") : ("PROXY_HELP_MESSAGE", "ask #platform") : staticEnvVars)
        providers <- expectProviders env
        config <- expectConfig env Nothing
        composeBindings mountBindingFor (pure fixedNow) providers config >>= \case
            Right [binding] -> case bindingPackumentDeps binding of
                Just deps -> do
                    fmap unSecret (pdInboundToken deps) `shouldBe` Just "edge-secret"
                    fmap unHelpMessage (pdHelp deps) `shouldBe` Just "ask #platform"
                    served <- pdNow deps
                    served `shouldBe` fixedNow
                Nothing -> expectationFailure "expected packument deps wired"
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "defaults the tarball-host policy to same-host (secure default)" $ do
        -- With PROXY_RESPECT_UPSTREAM_TARBALL_HOST unset, the deny-by-default
        -- reading of the allowlist is threaded onto every mount's deps.
        env <- expectEnv staticEnvVars
        planFrom env Nothing >>= \case
            Right [binding] -> case bindingPackumentDeps binding of
                Just deps -> pdTarballHostPolicy deps `shouldBe` SameHostAsPackument
                Nothing -> expectationFailure "expected packument deps wired"
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "relaxes the tarball-host policy when the operator opts in" $ do
        env <- expectEnv (("PROXY_RESPECT_UPSTREAM_TARBALL_HOST", "true") : staticEnvVars)
        planFrom env Nothing >>= \case
            Right [binding] -> case bindingPackumentDeps binding of
                Just deps -> pdTarballHostPolicy deps `shouldBe` AnyAllowlistedHost
                Nothing -> expectationFailure "expected packument deps wired"
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "defaults the response-bound budget to the secure defaults" $ do
        -- With no PROXY_MAX_* set, the deps carry Ecluse.Core.Security.defaultLimits — the
        -- secure-default body/version/nesting ceilings (security.md invariant 4).
        env <- expectEnv staticEnvVars
        planFrom env Nothing >>= \case
            Right [binding] -> case bindingPackumentDeps binding of
                Just deps -> pdLimits deps `shouldBe` defaultLimits
                Nothing -> expectationFailure "expected packument deps wired"
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "threads the operator's response-bound overrides onto the deps" $ do
        -- The three PROXY_MAX_* knobs flow from the environment layer onto every
        -- mount's Limits budget, so a deployment tightens or loosens the bounds.
        env <-
            expectEnv
                ( ("PROXY_MAX_RESPONSE_BYTES", "2048")
                    : ("PROXY_MAX_VERSION_COUNT", "10")
                    : ("PROXY_MAX_NESTING_DEPTH", "16")
                    : staticEnvVars
                )
        planFrom env Nothing >>= \case
            Right [binding] -> case bindingPackumentDeps binding of
                Just deps -> do
                    maxBodyBytes (pdLimits deps) `shouldBe` 2048
                    maxVersionCount (pdLimits deps) `shouldBe` 10
                    maxNestingDepth (pdLimits deps) `shouldBe` 16
                Nothing -> expectationFailure "expected packument deps wired"
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "defaults the public-integrity floor to SHA-256 onto the deps" $ do
        -- With PROXY_MIN_PUBLIC_INTEGRITY unset, every mount's deps carry the default
        -- SHA-256 floor the public admission gate enforces.
        env <- expectEnv staticEnvVars
        planFrom env Nothing >>= \case
            Right [binding] -> case bindingPackumentDeps binding of
                Just deps -> pdMinIntegrity deps `shouldBe` defaultMinIntegrity
                Nothing -> expectationFailure "expected packument deps wired"
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "threads a raised public-integrity floor onto the deps" $ do
        sha512Floor <- either (fail . toString) pure (mkMinIntegrity SHA512)
        env <- expectEnv (("PROXY_MIN_PUBLIC_INTEGRITY", "sha512") : staticEnvVars)
        planFrom env Nothing >>= \case
            Right [binding] -> case bindingPackumentDeps binding of
                Just deps -> pdMinIntegrity deps `shouldBe` sha512Floor
                Nothing -> expectationFailure "expected packument deps wired"
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "defaults the trusted-integrity floor to SHA-256 onto the deps" $ do
        env <- expectEnv staticEnvVars
        planFrom env Nothing >>= \case
            Right [binding] -> case bindingPackumentDeps binding of
                Just deps -> pdMinTrustedIntegrity deps `shouldBe` defaultMinTrustedIntegrity
                Nothing -> expectationFailure "expected packument deps wired"
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "threads a loosened trusted-integrity floor (sha1) onto the deps" $ do
        -- The trusted floor is loosenable below SHA-256, so a SHA-1 value flows from
        -- config to the deps the trusted gate consults — the asymmetry with the public
        -- floor's hard SHA-256 minimum.
        sha1Floor <- either (fail . toString) pure (mkMinTrustedIntegrity SHA1)
        env <- expectEnv (("PROXY_MIN_TRUSTED_INTEGRITY", "sha1") : staticEnvVars)
        planFrom env Nothing >>= \case
            Right [binding] -> case bindingPackumentDeps binding of
                Just deps -> pdMinTrustedIntegrity deps `shouldBe` sha1Floor
                Nothing -> expectationFailure "expected packument deps wired"
            other -> expectationFailure ("expected one binding, got " <> show (fmap length other))

    it "composeBindings is the listener-free Config -> [MountBinding] builder under planMounts" $ do
        -- The builder over an already-loaded Config is the testable core (it opens no
        -- listener, only 'prepare's each mount's rules); planMounts is just loadConfig
        -- sequenced into it.
        env <- expectEnv staticEnvVars
        providers <- expectProviders env
        config <- expectConfig env Nothing
        composeBindings mountBindingFor (pure fixedNow) providers config >>= \case
            Right bindings -> map bindingPrefix bindings `shouldBe` ["npm" :| []]
            Left errs -> expectationFailure ("unexpected boot errors: " <> show errs)

-- ── boot fail-fast ────────────────────────────────────────────────────────────

bootErrorSpec :: Spec
bootErrorSpec = describe "planMounts (fail fast at boot)" $ do
    it "fails on an unresolved rule policy (a typo'd rule type)" $ do
        env <- expectEnv staticEnvVars
        doc <- expectDoc "{\"rules\":{\"oops\":{\"type\":\"Nope\"}}}"
        planFrom env (Just doc) >>= \case
            Left errs -> errs `shouldBe` [PolicyBootError (UnknownRuleType "oops" "Nope")]
            Right _ -> expectationFailure "expected a policy boot error"

    it "fails on a configured mount whose ecosystem has no adapter" $ do
        -- A pypi mount: pypi has no registry client or renderer in this build, so
        -- 'mountBindingFor' returns Nothing — a loud boot failure, never a drop.
        env <- expectEnv staticEnvVars
        doc <- expectDoc (mountDoc "pypi" "static")
        planFrom env (Just doc) >>= \case
            Left errs -> errs `shouldBe` [MissingAdapter PyPI]
            Right _ -> expectationFailure "expected a missing-adapter boot error"

    it "fails on a mount naming a credential source with no initialized provider" $ do
        -- A codeartifact mount with no cloud leaf in this build: the credential
        -- reference does not resolve, an honest boot failure.
        env <- expectEnv staticEnvVars
        doc <- expectDoc (mountDoc "npm" "codeartifact")
        planFrom env (Just doc) >>= \case
            Left errs -> errs `shouldBe` [UnresolvedCredential Npm CodeArtifactCredential]
            Right _ -> expectationFailure "expected an unresolved-credential boot error"

    it "aggregates a missing adapter and an unresolved credential on one mount" $ do
        -- A pypi mount naming codeartifact fails both checks; both surface in one run
        -- rather than one-at-a-time.
        env <- expectEnv staticEnvVars
        doc <- expectDoc (mountDoc "pypi" "codeartifact")
        planFrom env (Just doc) >>= \case
            Left errs ->
                errs
                    `shouldMatchList` [ UnresolvedCredential PyPI CodeArtifactCredential
                                      , MissingAdapter PyPI
                                      ]
            Right _ -> expectationFailure "expected aggregated boot errors"

    it "fails the env single-mount when no static provider is initialized" $ do
        -- Without MIRROR_TARGET_TOKEN, the env single-mount's @static@ reference does
        -- not resolve, so even the default npm mount is an honest boot failure.
        env <- expectEnv noTokenEnvVars
        planFrom env Nothing >>= \case
            Left errs -> errs `shouldBe` [UnresolvedCredential Npm StaticCredential]
            Right _ -> expectationFailure "expected an unresolved-credential boot error"

    it "fails when a publication target is set without a publish-scope allow-list" $ do
        -- PUBLICATION_TARGET_URL set but PUBLISH_SCOPES empty: the anti-shadowing guard
        -- would have nothing to enforce, so the boot refuses rather than defaulting.
        env <- expectEnv (("PUBLICATION_TARGET_URL", "https://publish.example.test") : staticEnvVars)
        planFrom env Nothing >>= \case
            Left errs -> errs `shouldBe` [PublishScopesMissing]
            Right _ -> expectationFailure "expected a publish-scopes-missing boot error"

    it "fails when a static publish credential is set without a verifiable inbound edge" $ do
        -- PUBLICATION_TARGET_TOKEN set with PROXY_AUTH_TOKEN unset (the default open edge):
        -- Écluse would substitute its own write credential for a caller who forwards none,
        -- so any unauthenticated client could publish within scope. The boot refuses rather
        -- than leaving the internal credential coupled to no edge.
        env <-
            expectEnv
                ( [ ("PUBLICATION_TARGET_URL", "https://publish.example.test")
                  , ("PUBLISH_SCOPES", "@acme")
                  , ("PUBLICATION_TARGET_TOKEN", "publish-write-token")
                  ]
                    <> staticEnvVars
                )
        planFrom env Nothing >>= \case
            Left errs -> errs `shouldBe` [PublishStaticCredentialNeedsEdge]
            Right _ -> expectationFailure "expected a publish-static-credential-needs-edge boot error"

    it "accumulates both publish boot errors when scopes are missing and the static credential has no edge" $ do
        -- A publication target with no PUBLISH_SCOPES and a static credential behind an
        -- open edge trips both couplings at once: they surface together, in a stable order
        -- (scopes first, then the edge requirement), so the operator fixes both before the
        -- next boot rather than swatting them one reboot at a time.
        env <-
            expectEnv
                ( [ ("PUBLICATION_TARGET_URL", "https://publish.example.test")
                  , ("PUBLICATION_TARGET_TOKEN", "publish-write-token")
                  ]
                    <> staticEnvVars
                )
        planFrom env Nothing >>= \case
            Left errs -> errs `shouldBe` [PublishScopesMissing, PublishStaticCredentialNeedsEdge]
            Right _ -> expectationFailure "expected both publish boot errors, accumulated"

-- ── first-party publish wiring ────────────────────────────────────────────────

publishWiringSpec :: Spec
publishWiringSpec = describe "planMounts (first-party publish deps)" $ do
    it "wires the publication target and scope allow-list onto the mount when configured" $ do
        env <-
            expectEnv
                ( [ ("PUBLICATION_TARGET_URL", "https://publish.example.test")
                  , ("PUBLISH_SCOPES", "@acme, @beta")
                  ]
                    <> staticEnvVars
                )
        planFrom env Nothing >>= \case
            Right [binding] -> case bindingPublishDeps binding of
                Just deps -> do
                    pubTargetUrl deps `shouldBe` "https://publish.example.test"
                    pubScopes deps `shouldBe` [mkScope "acme", mkScope "beta"]
                Nothing -> expectationFailure "expected the mount to carry publish deps"
            _ -> expectationFailure "expected a single wired binding"

    it "boots a static publish credential when a verifiable inbound edge is configured" $ do
        -- The safe pairing — the positive control for the fail-loud boot test above: the
        -- same static publish credential boots once PROXY_AUTH_TOKEN gates the edge, so the
        -- internal credential is only ever reachable behind edge authentication.
        env <-
            expectEnv
                ( [ ("PUBLICATION_TARGET_URL", "https://publish.example.test")
                  , ("PUBLISH_SCOPES", "@acme")
                  , ("PUBLICATION_TARGET_TOKEN", "publish-write-token")
                  , ("PROXY_AUTH_TOKEN", "edge-token")
                  ]
                    <> staticEnvVars
                )
        planFrom env Nothing >>= \case
            Right [binding] -> case bindingPublishDeps binding of
                Just deps -> pubTargetUrl deps `shouldBe` "https://publish.example.test"
                Nothing -> expectationFailure "expected the mount to carry publish deps"
            _ -> expectationFailure "expected a single wired binding"

    it "leaves the publish path off (no publish deps) when no publication target is configured" $ do
        -- The opt-out: with no PUBLICATION_TARGET_URL the mount carries no publish deps,
        -- so a PUT /{pkg} is 405 — there is no implicit write path.
        env <- expectEnv staticEnvVars
        planFrom env Nothing >>= \case
            Right [binding] -> case bindingPublishDeps binding of
                Nothing -> pure ()
                Just _ -> expectationFailure "expected no publish deps when no publication target is configured"
            _ -> expectationFailure "expected a single wired binding"

-- ── rendering ─────────────────────────────────────────────────────────────────

renderSpec :: Spec
renderSpec = describe "renderBootError" $
    it "renders each boot-error kind as a distinct operator-facing line" $ do
        renderBootError (PolicyBootError (UnknownRuleType "x" "Y")) `shouldSatisfy` infixed "unknown type"
        renderBootError (MissingAdapter PyPI) `shouldSatisfy` infixed "no adapter"
        renderBootError (UnresolvedCredential Npm CodeArtifactCredential)
            `shouldSatisfy` infixed "not initialized"
        renderBootError (QueueProviderUnavailable PubSubQueue) `shouldSatisfy` infixed "not available"
        renderBootError QueueRegionMissing `shouldSatisfy` infixed "AWS_REGION"
        renderBootError (QueueUrlMissing SqsQueue) `shouldSatisfy` infixed "MIRROR_QUEUE_URL"
        renderBootError (QueueEndpointMalformed "x") `shouldSatisfy` infixed "endpoint"
        renderBootError (MirrorCredentialProviderUnavailable AdcCredential)
            `shouldSatisfy` infixed "gcp-artifact-registry"
        renderBootError (CodeArtifactConfigMissing "MIRROR_TARGET_CODEARTIFACT_DOMAIN")
            `shouldSatisfy` infixed "MIRROR_TARGET_CODEARTIFACT_DOMAIN"
        renderBootError (CodeArtifactConfigInvalid "MIRROR_TARGET_CODEARTIFACT_DOMAIN_OWNER" "expected a 12-digit AWS account id")
            `shouldSatisfy` infixed "12-digit"
        -- The mint-failure render makes the transient-vs-permanent distinction legible.
        renderBootError (CodeArtifactMintFailed "AccessDenied") `shouldSatisfy` infixed "transient"
        renderBootError PublishScopesMissing `shouldSatisfy` infixed "PUBLISH_SCOPES"
        renderBootError PublishStaticCredentialNeedsEdge `shouldSatisfy` infixed "PUBLICATION_TARGET_TOKEN"
  where
    infixed :: Text -> Text -> Bool
    infixed needle hay = needle `T.isInfixOf` hay

-- Build a 'Config' from an env + optional document, failing the test on a policy
-- error (the composeBindings examples want a successfully-loaded config).
expectConfig :: EnvConfig -> Maybe ConfigDoc -> IO Config
expectConfig env mDoc =
    either (\errs -> fail ("config load failed: " <> show errs)) pure (loadConfig env mDoc)
