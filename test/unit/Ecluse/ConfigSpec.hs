module Ecluse.ConfigSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (NominalDiffTime)
import Env qualified
import System.Environment (setEnv, unsetEnv)
import Test.Hspec

import Ecluse.Config
import Ecluse.Credential (mkSecret)
import Ecluse.Ecosystem (Ecosystem (..))
import Ecluse.Log (LogFormat (..))
import Ecluse.Package (HashAlg (SHA512), mkScope)
import Ecluse.Package.Integrity (defaultMinIntegrity, unMinIntegrity)
import Ecluse.Rules.Types (
    PrecededRule (..),
    Rule (..),
    defaultAllowIfPublishedBeforePrecedence,
    defaultDenyInstallTimeExecutionPrecedence,
 )
import Ecluse.Telemetry (TelemetrySwitch (..))

{- | Tests for the configuration boundary. They exercise the three promises of the
loader: the environment layer aggregates all errors at once (present \/ absent \/
malformed), the JSON document decoders are strict and fail loud (unknown keys,
unknown \/ effectful rule types, secrets), and the rule-policy merge resolves
add \/ override \/ suppress over the default while rejecting every unresolvable
reference. Pure and offline.
-}
spec :: Spec
spec = do
    backendSpec
    envLayerSpec
    documentDecodeSpec
    rulePolicySpec
    desugarSpec
    secretsSpec
    instancesSpec

-- ── derived instances & accessors ────────────────────────────────────────────

{- | Force the derived 'Eq' and 'Show' across the config types, and the opaque
accessors, on whole values — the determinism\/representation checks the rest of
the suite reaches through a single field.
-}
instancesSpec :: Spec
instancesSpec = describe "derived instances and accessors" $ do
    it "decodes equal documents from equal bytes (whole-record Eq)" $ do
        a <- expectDoc singleMountDoc
        b <- expectDoc singleMountDoc
        a `shouldBe` b

    it "parses equal env layers from equal inputs (whole-record Eq)" $ do
        a <- expectEnv fullEnv
        b <- expectEnv fullEnv
        a `shouldBe` b

    it "assembles equal configs from equal inputs (whole-record Eq)" $ do
        env <- expectEnv minimalEnv
        loadConfig env Nothing `shouldBe` loadConfig env Nothing

    it "shows a config document, mount, and rule entry without erroring" $ do
        doc <- expectDoc singleMountDoc
        -- Exercise Show on the document and its nested mount/registry/entry types.
        showText doc `shouldSatisfy` ("MountDoc" `isInfix`)

    it "shows the resolved config (env layer, mounts, mirror target)" $ do
        env <- expectEnv minimalEnv
        case loadConfig env Nothing of
            Left errs -> expectationFailure ("unexpected policy errors: " <> show errs)
            Right cfg -> do
                showText (configEnv cfg) `shouldSatisfy` ("EnvConfig" `isInfix`)
                showText cfg `shouldSatisfy` ("MirrorTarget" `isInfix`)

    it "shows a policy error and the default policy" $ do
        showText (UnknownRuleType "n" "T") `shouldSatisfy` ("UnknownRuleType" `isInfix`)
        showText defaultPolicy `shouldSatisfy` ("AllowIfPublishedBefore" `isInfix`)

    it "keys a decoded mount by its ecosystem (the mount map's key)" $ do
        doc <- expectDoc singleMountDoc
        Map.keys (docMounts doc) `shouldBe` [Npm]

-- ── backend enums & renderers ────────────────────────────────────────────────

backendSpec :: Spec
backendSpec = describe "backend selection" $ do
    describe "QueueBackend" $ do
        it "round-trips each backend through parse/render" $ do
            parseQueueBackend "sqs" `shouldBe` Right SqsQueue
            parseQueueBackend "pubsub" `shouldBe` Right PubSubQueue
            parseQueueBackend "memory" `shouldBe` Right MemoryQueue
            renderQueueBackend SqsQueue `shouldBe` "sqs"
            renderQueueBackend PubSubQueue `shouldBe` "pubsub"
            renderQueueBackend MemoryQueue `shouldBe` "memory"
        it "rejects an unknown name, naming the accepted set" $
            parseQueueBackend "kafka"
                `shouldBe` Left "unknown queue provider \"kafka\" (expected one of: sqs, pubsub, memory)"

    describe "CredentialBackend" $ do
        it "round-trips each backend through parse/render" $ do
            parseCredentialBackend "codeartifact" `shouldBe` Right CodeArtifactCredential
            parseCredentialBackend "static" `shouldBe` Right StaticCredential
            parseCredentialBackend "adc" `shouldBe` Right AdcCredential
            renderCredentialBackend CodeArtifactCredential `shouldBe` "codeartifact"
            renderCredentialBackend StaticCredential `shouldBe` "static"
            renderCredentialBackend AdcCredential `shouldBe` "adc"
        it "rejects an unknown name, naming the accepted set" $
            parseCredentialBackend "vault"
                `shouldBe` Left "unknown credential provider \"vault\" (expected one of: codeartifact, static, adc)"

    describe "Url" $ do
        it "trims surrounding whitespace and round-trips" $
            (unUrl <$> mkUrl "  https://x  ") `shouldBe` Right "https://x"
        it "rejects an all-whitespace value" $
            mkUrl "   " `shouldBe` Left "expected a non-empty URL"

    describe "renderPolicyError" $
        -- Each constructor renders a distinct, operator-facing line.
        it "renders every policy-error kind" $ do
            renderPolicyError (MissingRuleType "x") `shouldSatisfy` isInfix "missing"
            renderPolicyError (UnknownRuleType "x" "Y") `shouldSatisfy` isInfix "unknown type"
            renderPolicyError (MalformedRule "x" "bad") `shouldSatisfy` isInfix "bad"
            renderPolicyError (SuppressUnknownRule "x") `shouldSatisfy` isInfix "disables"

-- ── environment layer ────────────────────────────────────────────────────────

{- | A complete, valid environment: the three required URLs plus a value for the
defaulted variables, so a test can drop or corrupt one axis at a time.
-}
fullEnv :: [(String, String)]
fullEnv =
    [ ("PROXY_PORT", "8080")
    , ("PRIVATE_UPSTREAM_URL", "https://private.example.test")
    , ("PUBLIC_UPSTREAM_URL", "https://public.example.test")
    , ("MIRROR_TARGET_URL", "https://mirror.example.test")
    , ("MIRROR_QUEUE_PROVIDER", "pubsub")
    , ("MIRROR_QUEUE_URL", "projects/p/topics/t")
    , ("MIRROR_QUEUE_MEMORY_MAX_DEPTH", "777")
    , ("AWS_REGION", "eu-west-1")
    , ("PROXY_AUTH_TOKEN", "s3cr3t")
    , ("MIRROR_TARGET_TOKEN", "mirror-write")
    , ("PROXY_HELP_MESSAGE", "ask #platform")
    , ("CVE_SYNC_INTERVAL_SECONDS", "60")
    , ("METADATA_CACHE_TTL_SECONDS", "30")
    , ("METADATA_CACHE_MAX_ENTRIES", "256")
    , ("PROXY_MAX_RESPONSE_BYTES", "1048576")
    , ("PROXY_MAX_VERSION_COUNT", "5000")
    , ("PROXY_MAX_NESTING_DEPTH", "32")
    , ("PROXY_LOG_FORMAT", "console")
    , ("PROXY_TELEMETRY", "on")
    , ("PROXY_RESPECT_UPSTREAM_TARBALL_HOST", "true")
    , ("PROXY_PUBLIC_URL", "https://proxy.example.test")
    ]

-- The minimum valid environment: only the two required URLs, everything else
-- defaulted (MIRROR_TARGET_URL is optional — it folds onto the private upstream).
minimalEnv :: [(String, String)]
minimalEnv =
    [ ("PRIVATE_UPSTREAM_URL", "https://private.example.test")
    , ("MIRROR_TARGET_URL", "https://mirror.example.test")
    , ("MIRROR_QUEUE_URL", "https://sqs.example.test/q")
    ]

-- The CodeArtifact mirror-target credential keys and an SQS endpoint override,
-- layered over a base env to exercise the new explicit AWS settings.
codeArtifactEnv :: [(String, String)]
codeArtifactEnv =
    [ ("MIRROR_TARGET_CREDENTIAL_PROVIDER", "codeartifact")
    , ("MIRROR_TARGET_CODEARTIFACT_DOMAIN", "my-domain")
    , ("MIRROR_TARGET_CODEARTIFACT_DOMAIN_OWNER", "111122223333")
    , ("MIRROR_TARGET_CODEARTIFACT_REGION", "us-east-1")
    , ("MIRROR_TARGET_CODEARTIFACT_TOKEN_DURATION_SECONDS", "3600")
    , ("AWS_ENDPOINT_URL_SQS", "http://localhost:4566")
    ]

-- The names that failed to parse, regardless of their error kind.
failedNames :: Either [(String, Env.Error)] a -> [String]
failedNames = either (map fst) (const [])

envLayerSpec :: Spec
envLayerSpec = describe "parseEnvPure" $ do
    it "parses a fully-populated environment" $ do
        case parseEnvPure fullEnv of
            Left errs -> expectationFailure ("unexpected errors: " <> show errs)
            Right cfg -> do
                cfgPort cfg `shouldBe` 8080
                unUrl (cfgPrivateUpstream cfg) `shouldBe` "https://private.example.test"
                unUrl (cfgPublicUpstream cfg) `shouldBe` "https://public.example.test"
                fmap unUrl (cfgMirrorTarget cfg) `shouldBe` Just "https://mirror.example.test"
                cfgQueueBackend cfg `shouldBe` PubSubQueue
                fmap unUrl (cfgQueueUrl cfg) `shouldBe` Just "projects/p/topics/t"
                cfgQueueMemoryMaxDepth cfg `shouldBe` 777
                cfgAwsRegion cfg `shouldBe` Just "eu-west-1"
                cfgGoogleProject cfg `shouldBe` Nothing
                cfgAuthToken cfg `shouldBe` Just (mkSecret "s3cr3t")
                cfgMirrorTargetToken cfg `shouldBe` Just (mkSecret "mirror-write")
                cfgHelpMessage cfg `shouldBe` Just "ask #platform"
                cfgCveSyncInterval cfg `shouldBe` (60 :: NominalDiffTime)
                cfgCacheTtl cfg `shouldBe` (30 :: NominalDiffTime)
                cfgCacheMaxEntries cfg `shouldBe` 256
                cfgMaxResponseBytes cfg `shouldBe` 1048576
                cfgMaxVersionCount cfg `shouldBe` 5000
                cfgMaxNestingDepth cfg `shouldBe` 32
                cfgLogFormat cfg `shouldBe` ConsoleLog
                cfgTelemetry cfg `shouldBe` TelemetryOn
                cfgRespectUpstreamTarballHost cfg `shouldBe` True
                fmap unUrl (cfgPublicUrl cfg) `shouldBe` Just "https://proxy.example.test"
                -- Secret-redaction regression: the tokens are parsed and held,
                -- but must never reach a Show-based signal (a log, an error, a
                -- deriving Show) — they are redacted Secrets, not raw Text. See
                -- Ecluse.Credential.Secret.
                showText cfg `shouldNotSatisfy` ("s3cr3t" `isInfix`)
                showText cfg `shouldNotSatisfy` ("mirror-write" `isInfix`)
                showText cfg `shouldSatisfy` ("REDACTED" `isInfix`)

    it "applies the documented defaults for the optional variables" $ do
        case parseEnvPure minimalEnv of
            Left errs -> expectationFailure ("unexpected errors: " <> show errs)
            Right cfg -> do
                cfgPort cfg `shouldBe` 4873
                unUrl (cfgPublicUpstream cfg) `shouldBe` "https://registry.npmjs.org"
                cfgQueueBackend cfg `shouldBe` SqsQueue
                -- The in-memory queue cap defaults to a generous, memory-bounded depth.
                cfgQueueMemoryMaxDepth cfg `shouldBe` 50000
                cfgCveSyncInterval cfg `shouldBe` (3600 :: NominalDiffTime)
                cfgCacheTtl cfg `shouldBe` (60 :: NominalDiffTime)
                cfgCacheMaxEntries cfg `shouldBe` 1024
                -- The response-bound budget defaults to Ecluse.Security.defaultLimits:
                -- a 16 MiB body, 100k versions, 64 levels of nesting.
                cfgMaxResponseBytes cfg `shouldBe` 16 * 1024 * 1024
                cfgMaxVersionCount cfg `shouldBe` 100000
                cfgMaxNestingDepth cfg `shouldBe` 64
                cfgAwsRegion cfg `shouldBe` Nothing
                cfgAuthToken cfg `shouldBe` Nothing
                cfgPublicUrl cfg `shouldBe` Nothing
                cfgMirrorTargetToken cfg `shouldBe` Nothing
                cfgHelpMessage cfg `shouldBe` Nothing
                cfgLogFormat cfg `shouldBe` JsonLog
                -- Telemetry is opt-in: an unset PROXY_TELEMETRY leaves it off, so
                -- nothing is wired and nothing is emitted.
                cfgTelemetry cfg `shouldBe` TelemetryOff
                -- The secure default: a cross-host dist.tarball is NOT honoured
                -- unless the operator explicitly opts in.
                cfgRespectUpstreamTarballHost cfg `shouldBe` False
                -- The public-integrity floor defaults to SHA-256 (the hard minimum).
                cfgMinPublicIntegrity cfg `shouldBe` defaultMinIntegrity

    it "reports a single missing required variable against its own name" $
        failedNames (parseEnvPure (without "PRIVATE_UPSTREAM_URL" minimalEnv))
            `shouldBe` ["PRIVATE_UPSTREAM_URL"]

    it "reports the one hard-required variable (PRIVATE_UPSTREAM_URL) when nothing is set" $
        -- PRIVATE_UPSTREAM_URL is the single env-layer-required variable. MIRROR_TARGET_URL
        -- folds onto it when unset, and MIRROR_QUEUE_URL is now provider-conditional
        -- (required for sqs at planMirrorQueue, ignored by memory), so neither is
        -- required at the env layer. (Aggregation across multiple failures is covered by
        -- the malformed-values test below.)
        failedNames (parseEnvPure []) `shouldBe` ["PRIVATE_UPSTREAM_URL"]

    it "leaves the mirror-queue URL unset (provider-conditional) when MIRROR_QUEUE_URL is absent" $
        case parseEnvPure (without "MIRROR_QUEUE_URL" minimalEnv) of
            Left errs -> expectationFailure ("unexpected errors: " <> show errs)
            Right cfg -> cfgQueueUrl cfg `shouldBe` Nothing

    it "leaves the mirror target unset (to fold onto the private upstream) when MIRROR_TARGET_URL is absent" $
        case parseEnvPure (without "MIRROR_TARGET_URL" minimalEnv) of
            Left errs -> expectationFailure ("unexpected errors: " <> show errs)
            Right cfg -> cfgMirrorTarget cfg `shouldBe` Nothing

    it "defaults the mirror-target credential provider to static" $
        case parseEnvPure minimalEnv of
            Left errs -> expectationFailure ("unexpected errors: " <> show errs)
            Right cfg -> cfgMirrorTargetCredentialProvider cfg `shouldBe` StaticCredential

    it "parses the mirror-target credential provider, CodeArtifact inputs, and SQS endpoint override" $
        case parseEnvPure (codeArtifactEnv <> minimalEnv) of
            Left errs -> expectationFailure ("unexpected errors: " <> show errs)
            Right cfg -> do
                cfgMirrorTargetCredentialProvider cfg `shouldBe` CodeArtifactCredential
                cfgMirrorCodeArtifactDomain cfg `shouldBe` Just "my-domain"
                cfgMirrorCodeArtifactDomainOwner cfg `shouldBe` Just "111122223333"
                cfgMirrorCodeArtifactRegion cfg `shouldBe` Just "us-east-1"
                cfgMirrorCodeArtifactTokenDuration cfg `shouldBe` Just 3600
                cfgAwsEndpointUrlSqs cfg `shouldBe` Just "http://localhost:4566"

    it "maps gcp-artifact-registry onto the GCP (ADC) credential backend" $
        case parseEnvPure (("MIRROR_TARGET_CREDENTIAL_PROVIDER", "gcp-artifact-registry") : minimalEnv) of
            Left errs -> expectationFailure ("unexpected errors: " <> show errs)
            Right cfg -> cfgMirrorTargetCredentialProvider cfg `shouldBe` AdcCredential

    it "rejects an unknown mirror-target credential provider" $
        failedNames (parseEnvPure (("MIRROR_TARGET_CREDENTIAL_PROVIDER", "vault") : minimalEnv))
            `shouldBe` ["MIRROR_TARGET_CREDENTIAL_PROVIDER"]

    it "rejects a CodeArtifact token duration above the 12-hour cap" $
        failedNames (parseEnvPure (("MIRROR_TARGET_CODEARTIFACT_TOKEN_DURATION_SECONDS", "50000") : minimalEnv))
            `shouldBe` ["MIRROR_TARGET_CODEARTIFACT_TOKEN_DURATION_SECONDS"]

    it "rejects a non-positive in-memory queue cap (a zero cap would drop every job)" $
        failedNames (parseEnvPure (("MIRROR_QUEUE_MEMORY_MAX_DEPTH", "0") : minimalEnv))
            `shouldBe` ["MIRROR_QUEUE_MEMORY_MAX_DEPTH"]

    it "aggregates malformed values alongside missing ones" $
        -- A non-integer port and an unknown queue provider both fail, together with
        -- the still-missing required PRIVATE_UPSTREAM_URL — every problem in one report.
        -- MIRROR_QUEUE_URL is absent here but no longer an env-layer failure (it is
        -- provider-conditional), so it is not among the reported names.
        failedNames
            ( parseEnvPure
                [ ("PROXY_PORT", "not-a-number")
                , ("MIRROR_QUEUE_PROVIDER", "kafka")
                ]
            )
            `shouldMatchList` [ "PROXY_PORT"
                              , "MIRROR_QUEUE_PROVIDER"
                              , "PRIVATE_UPSTREAM_URL"
                              ]

    it "rejects an empty required URL rather than accepting a blank" $
        failedNames (parseEnvPure (set "PRIVATE_UPSTREAM_URL" "   " minimalEnv))
            `shouldBe` ["PRIVATE_UPSTREAM_URL"]

    it "rejects a negative CVE sync interval" $
        failedNames (parseEnvPure (set "CVE_SYNC_INTERVAL_SECONDS" "-5" minimalEnv))
            `shouldBe` ["CVE_SYNC_INTERVAL_SECONDS"]

    it "rejects an unknown log format against its own name" $
        failedNames (parseEnvPure (set "PROXY_LOG_FORMAT" "yaml" minimalEnv))
            `shouldBe` ["PROXY_LOG_FORMAT"]

    it "rejects a non-positive response-byte bound rather than fail-closing the proxy" $
        -- A zero body cap would refuse every upstream body; it is a degenerate budget,
        -- so it fails loudly against its own name instead of silently breaking fetches.
        failedNames (parseEnvPure (set "PROXY_MAX_RESPONSE_BYTES" "0" minimalEnv))
            `shouldBe` ["PROXY_MAX_RESPONSE_BYTES"]

    it "rejects a negative version-count bound against its own name" $
        failedNames (parseEnvPure (set "PROXY_MAX_VERSION_COUNT" "-1" minimalEnv))
            `shouldBe` ["PROXY_MAX_VERSION_COUNT"]

    it "rejects a non-positive nesting-depth bound against its own name" $
        failedNames (parseEnvPure (set "PROXY_MAX_NESTING_DEPTH" "0" minimalEnv))
            `shouldBe` ["PROXY_MAX_NESTING_DEPTH"]

    it "rejects an unknown telemetry switch against its own name" $
        failedNames (parseEnvPure (set "PROXY_TELEMETRY" "maybe" minimalEnv))
            `shouldBe` ["PROXY_TELEMETRY"]

    it "parses a raised public-integrity floor" $
        case parseEnvPure (set "PROXY_MIN_PUBLIC_INTEGRITY" "sha512" minimalEnv) of
            Left errs -> expectationFailure ("unexpected errors: " <> show errs)
            Right cfg -> (unMinIntegrity . cfgMinPublicIntegrity) cfg `shouldBe` SHA512

    it "rejects a sub-floor public-integrity algorithm (below SHA-256) against its own name" $
        -- A SHA-1 floor would admit collision-broken public versions; a sub-floor value
        -- is a configuration error, rejected loudly rather than silently clamped.
        failedNames (parseEnvPure (set "PROXY_MIN_PUBLIC_INTEGRITY" "sha1" minimalEnv))
            `shouldBe` ["PROXY_MIN_PUBLIC_INTEGRITY"]

    it "rejects an unknown public-integrity algorithm against its own name" $
        failedNames (parseEnvPure (set "PROXY_MIN_PUBLIC_INTEGRITY" "frobnicate" minimalEnv))
            `shouldBe` ["PROXY_MIN_PUBLIC_INTEGRITY"]

    it "parses the tarball-host toggle as a boolean" $
        -- The opt-in to honour a cross-host dist.tarball; "false" is the secure
        -- value the default also gives.
        case parseEnvPure (set "PROXY_RESPECT_UPSTREAM_TARBALL_HOST" "false" minimalEnv) of
            Left errs -> expectationFailure ("unexpected errors: " <> show errs)
            Right cfg -> cfgRespectUpstreamTarballHost cfg `shouldBe` False

    it "rejects a non-boolean tarball-host toggle rather than coercing it" $
        -- A security-relevant toggle must fail loudly on a typo, never silently
        -- fall back to a default the operator did not intend.
        failedNames (parseEnvPure (set "PROXY_RESPECT_UPSTREAM_TARBALL_HOST" "maybe" minimalEnv))
            `shouldBe` ["PROXY_RESPECT_UPSTREAM_TARBALL_HOST"]

    it "accepts the conventional boolean spellings (1/0, yes/no) case-insensitively"
        $
        -- The toggle takes the usual true/false synonyms an operator might reach
        -- for, so each documented spelling parses to its value rather than being
        -- rejected as a typo.
        forM_
            [ ("1", True)
            , ("0", False)
            , ("YES", True)
            , ("No", False)
            ]
        $ \(raw, expected) ->
            case parseEnvPure (set "PROXY_RESPECT_UPSTREAM_TARBALL_HOST" raw minimalEnv) of
                Left errs -> expectationFailure ("unexpected errors for " <> raw <> ": " <> show errs)
                Right cfg -> cfgRespectUpstreamTarballHost cfg `shouldBe` expected

    it "renders every aggregated error in the failure block" $ do
        -- The rendered block names each offending variable, so a launch failure is
        -- actionable from the logs alone. Two failures at once — a malformed port and
        -- the missing required upstream — both appear.
        let rendered = either renderEnvErrors (const "") (parseEnvPure [("PROXY_PORT", "not-a-number")])
        rendered `shouldSatisfy` ("PRIVATE_UPSTREAM_URL" `isInfix`)
        rendered `shouldSatisfy` ("PROXY_PORT" `isInfix`)

    it "renders each error kind (unset, empty, unread) with its own phrasing" $ do
        -- The renderer maps every envparse Error constructor to a distinct line,
        -- so the cause is legible regardless of how a variable went wrong.
        let rendered =
                renderEnvErrors
                    [ ("A", Env.UnsetError)
                    , ("B", Env.EmptyError)
                    , ("C", Env.UnreadError "boom")
                    ]
        rendered `shouldSatisfy` ("A: is required but unset" `isInfix`)
        rendered `shouldSatisfy` ("B: is set but empty" `isInfix`)
        rendered `shouldSatisfy` ("C: boom" `isInfix`)

    it "reads the live process environment via the IO entry point" $ do
        -- 'parseEnv' is the thin IO wrapper over the pure parser; set the required
        -- variables in-process, parse, and clean up so other examples are unaffected.
        traverse_ (uncurry setEnv) minimalEnv
        result <- parseEnv
        traverse_ (unsetEnv . fst) minimalEnv
        case result of
            Left errs -> expectationFailure ("unexpected env errors: " <> show errs)
            Right cfg -> unUrl (cfgPrivateUpstream cfg) `shouldBe` "https://private.example.test"

-- ── document decoding ────────────────────────────────────────────────────────

documentDecodeSpec :: Spec
documentDecodeSpec = describe "decodeDocument" $ do
    it "decodes a document with one mount and a rule patch" $
        case decodeDocument singleMountDoc of
            Left e -> expectationFailure ("unexpected decode error: " <> toString e)
            Right doc -> Map.keys (docMounts doc) `shouldBe` [Npm]

    it "decodes a document carrying only a rule policy (no mounts)" $
        case decodeDocument "{\"rules\":{\"min-age\":{\"ageSeconds\":1209600}}}" of
            Left e -> expectationFailure ("unexpected decode error: " <> toString e)
            Right doc -> docMounts doc `shouldBe` mempty

    it "keys a mount by its ecosystem name, deriving the prefix from it" $
        -- The @mounts@ object is keyed by ecosystem name (@npm@), never a path
        -- prefix; the served prefix is derived from the ecosystem downstream.
        case decodeDocument (mountDocForEcosystem "npm") of
            Left e -> expectationFailure ("unexpected decode error: " <> toString e)
            Right doc -> Map.keys (docMounts doc) `shouldBe` [Npm]

    it "rejects an unparseable JSON body" $
        decodeDocument "{not json" `shouldSatisfy` isLeft

    it "rejects an unknown top-level key, naming it (strict, not silently dropped)" $
        decodeDocument "{\"mountz\":{}}" `shouldSatisfy` decodeErrorMentions "mountz"

    it "rejects an unknown mount ecosystem key, naming it (strict, not silently dropped)" $
        -- A @mounts@ key that names no known ecosystem is a loud failure, not a
        -- silent skip — a typo'd or unsupported ecosystem must never vanish.
        decodeDocument (mountDocForEcosystem "npmm") `shouldSatisfy` decodeErrorMentions "npmm"

    it "rejects an unknown key inside a mount, naming it" $
        decodeDocument (mountDocWithExtraKey "baseURL") `shouldSatisfy` decodeErrorMentions "baseURL"

    it "rejects an unknown key inside the mirror target, naming it" $
        decodeDocument mirrorTargetWithExtraKey `shouldSatisfy` decodeErrorMentions "token"

    it "rejects an unknown key inside a rule entry, naming it" $
        decodeDocument "{\"rules\":{\"min-age\":{\"agSeconds\":10}}}"
            `shouldSatisfy` decodeErrorMentions "agSeconds"

    it "rejects an unknown queue backend in a mount" $
        decodeDocument (mountWithQueue "kafka") `shouldSatisfy` isLeft

    it "rejects an unknown credential backend in a mount" $
        decodeDocument (mountWithCredential "vault") `shouldSatisfy` isLeft

    it "rejects an empty mirror-target URL" $
        decodeDocument (mountWithMirrorUrl "") `shouldSatisfy` isLeft

    it "decodes the adc credential backend in a mount" $
        case decodeDocument (mountWithCredential "adc") of
            Left e -> expectationFailure ("unexpected decode error: " <> toString e)
            Right doc -> case Map.lookup Npm (docMounts doc) of
                Nothing -> expectationFailure "expected the npm mount"
                Just mdoc -> mtCredential (regMirrorTarget (mdocRegistries mdoc)) `shouldBe` AdcCredential

    it "rejects a non-string where a URL string is expected, naming the number kind" $
        -- A URL field given a number (not a string) is a typed decode failure,
        -- not a coerced value.
        decodeDocument
            "{\"mounts\":{\"npm\":{\"privateUpstream\":42,\"publicUpstream\":\"https://b\",\
            \\"mirrorTarget\":{\"url\":\"https://c\",\"credential\":\"static\",\"queue\":\"sqs\"}}}}"
            `shouldSatisfy` decodeErrorMentions "a number"

    it "rejects each non-string JSON kind in a string-valued field, naming the kind" $ do
        -- Spread the JSON kinds (array, object, boolean, null) across the
        -- string-valued fields so the error reports what was actually found.
        decodeDocument (mountWithCredential' "[\"static\"]")
            `shouldSatisfy` decodeErrorMentions "an array"
        decodeDocument (mountWithCredential' "{\"a\":1}")
            `shouldSatisfy` decodeErrorMentions "an object"
        decodeDocument (mountWithCredential' "true")
            `shouldSatisfy` decodeErrorMentions "a boolean"
        decodeDocument (mountWithCredential' "null")
            `shouldSatisfy` decodeErrorMentions "null"

    it "decodes the mount registries and backends faithfully" $
        case decodeDocument singleMountDoc of
            Left e -> expectationFailure ("unexpected decode error: " <> toString e)
            Right doc -> case Map.lookup Npm (docMounts doc) of
                Nothing -> expectationFailure "expected the npm mount"
                Just mdoc -> do
                    let reg = mdocRegistries mdoc
                    unUrl (regPrivateUpstream reg) `shouldBe` "https://private.example.test"
                    unUrl (regPublicUpstream reg) `shouldBe` "https://registry.npmjs.org"
                    let target = regMirrorTarget reg
                    unUrl (mtUrl target) `shouldBe` "https://mirror.example.test"
                    mtCredential target `shouldBe` CodeArtifactCredential
                    mtQueue target `shouldBe` SqsQueue

-- ── rule policy merge ────────────────────────────────────────────────────────

-- Resolve a JSON rules patch over the built-in default policy, returning the
-- resolved rules sorted by precedence for a stable comparison.
resolveJson :: ByteString -> Either [PolicyError] [PrecededRule]
resolveJson = resolveJsonOver defaultPolicy

-- Resolve a JSON rules patch over an arbitrary base policy. A per-mount
-- refinement merges over a shared policy that may carry any rule, so this
-- exercises the merge against bases beyond the single-rule default.
resolveJsonOver :: RulePolicy -> ByteString -> Either [PolicyError] [PrecededRule]
resolveJsonOver base body = case decodeDocument body of
    Left e -> Left [MalformedRule "<decode>" e]
    Right doc -> sortOn rulePrecedence . Map.elems . policyRules <$> resolvePolicy base (docRules doc)

-- A base policy carrying one of each rule kind by name, so a patch can override
-- or restate any of them (the multi-rule shared-policy case).
mixedBase :: RulePolicy
mixedBase =
    RulePolicy
        ( Map.fromList
            [ ("min-age", PrecededRule 100 (AllowIfPublishedBefore (7 * 86400)))
            , ("trusted", PrecededRule 200 (AllowScope (mkScope "myorg")))
            , ("deny-scripts", PrecededRule 300 DenyInstallTimeExecution)
            ]
        )

rulePolicySpec :: Spec
rulePolicySpec = describe "resolvePolicy" $ do
    it "applies the default policy unchanged with an empty patch" $
        sortOn rulePrecedence (Map.elems (policyRules defaultPolicy))
            `shouldBe` [PrecededRule defaultAllowIfPublishedBeforePrecedence (AllowIfPublishedBefore (7 * 86400))]

    it "overrides a default rule's value, keeping its precedence (a partial patch)" $
        -- `min-age` names the default, so {ageSeconds} widens its window to 14
        -- days while leaving its precedence untouched.
        resolveJson "{\"rules\":{\"min-age\":{\"ageSeconds\":1209600}}}"
            `shouldBe` Right [PrecededRule defaultAllowIfPublishedBeforePrecedence (AllowIfPublishedBefore 1209600)]

    it "overrides a default rule's precedence" $
        resolveJson "{\"rules\":{\"min-age\":{\"precedence\":150}}}"
            `shouldBe` Right [PrecededRule 150 (AllowIfPublishedBefore (7 * 86400))]

    it "adds a new rule that carries a full type at its type's default precedence" $
        resolveJson "{\"rules\":{\"deny-scripts\":{\"type\":\"DenyInstallTimeExecution\"}}}"
            `shouldBe` Right
                [ PrecededRule defaultAllowIfPublishedBeforePrecedence (AllowIfPublishedBefore (7 * 86400))
                , PrecededRule defaultDenyInstallTimeExecutionPrecedence DenyInstallTimeExecution
                ]

    it "adds a new rule with an explicit precedence" $
        resolveJson "{\"rules\":{\"deny-scripts\":{\"type\":\"DenyInstallTimeExecution\",\"precedence\":250}}}"
            `shouldBe` Right
                [ PrecededRule defaultAllowIfPublishedBeforePrecedence (AllowIfPublishedBefore (7 * 86400))
                , PrecededRule 250 DenyInstallTimeExecution
                ]

    it "suppresses a default rule with enabled:false" $
        resolveJson "{\"rules\":{\"min-age\":{\"enabled\":false}}}"
            `shouldBe` Right []

    it "adds an AllowScope rule from a scope field" $
        resolveJson "{\"rules\":{\"trusted\":{\"type\":\"AllowScope\",\"scope\":\"myorg\"}}}"
            `shouldSatisfy` containsAllowScope

    it "adds a new AllowIfPublishedBefore rule from a valid ageSeconds" $
        -- The success arm of *building* (not patching) an AllowIfPublishedBefore:
        -- a fresh min-age rule under a new name is constructed with the given
        -- window at the type's default precedence, beside the existing default.
        -- (Adding with a negative or absent ageSeconds is rejected below; this
        -- pins the valid-construction path.)
        resolveJson "{\"rules\":{\"young\":{\"type\":\"AllowIfPublishedBefore\",\"ageSeconds\":100}}}"
            `shouldSatisfy` either
                (const False)
                (elem (PrecededRule defaultAllowIfPublishedBeforePrecedence (AllowIfPublishedBefore 100)))

    it "accepts a restated type on a patch that matches the default's kind" $
        -- Naming the default's own type alongside an override is allowed; it must
        -- match the rule it patches.
        resolveJson "{\"rules\":{\"min-age\":{\"type\":\"AllowIfPublishedBefore\",\"ageSeconds\":100}}}"
            `shouldBe` Right [PrecededRule defaultAllowIfPublishedBeforePrecedence (AllowIfPublishedBefore 100)]

    it "rejects a restated type on a patch that changes the default's kind" $
        -- Re-typing min-age to a different known rule is a loud error, not a
        -- silent identity swap.
        resolveJson "{\"rules\":{\"min-age\":{\"type\":\"DenyInstallTimeExecution\"}}}"
            `shouldBe` Left [MalformedRule "min-age" "\"type\" \"DenyInstallTimeExecution\" does not match the default rule it patches"]

    it "rejects a restated unknown type on a patch" $
        resolveJson "{\"rules\":{\"min-age\":{\"type\":\"Bogus\"}}}"
            `shouldBe` Left [UnknownRuleType "min-age" "Bogus"]

    it "rejects a negative ageSeconds when adding a rule" $
        resolveJson "{\"rules\":{\"young\":{\"type\":\"AllowIfPublishedBefore\",\"ageSeconds\":-1}}}"
            `shouldBe` Left [MalformedRule "young" "\"ageSeconds\" must be non-negative"]

    it "rejects a negative ageSeconds when patching the default" $
        resolveJson "{\"rules\":{\"min-age\":{\"ageSeconds\":-1}}}"
            `shouldBe` Left [MalformedRule "min-age" "\"ageSeconds\" must be non-negative"]

    it "rejects adding an AllowIfPublishedBefore without ageSeconds" $
        resolveJson "{\"rules\":{\"young\":{\"type\":\"AllowIfPublishedBefore\"}}}"
            `shouldBe` Left [MalformedRule "young" "\"AllowIfPublishedBefore\" requires \"ageSeconds\""]

    describe "merging over a multi-rule shared policy" $ do
        -- A per-mount refinement merges over a shared policy that may hold any
        -- rule kind, so the patch must override, suppress, and restate each.
        it "overrides an AllowScope default's scope and precedence" $
            resolveJsonOver mixedBase "{\"rules\":{\"trusted\":{\"scope\":\"other\",\"precedence\":205}}}"
                `shouldSatisfy` hasRuleAtPrec 205 (AllowScope (mkScope "other"))

        it "keeps an AllowScope default's scope when only its precedence changes" $
            resolveJsonOver mixedBase "{\"rules\":{\"trusted\":{\"precedence\":210}}}"
                `shouldSatisfy` hasRuleAtPrec 210 (AllowScope (mkScope "myorg"))

        it "patches a DenyInstallTimeExecution default's precedence" $
            resolveJsonOver mixedBase "{\"rules\":{\"deny-scripts\":{\"precedence\":350}}}"
                `shouldSatisfy` hasRuleAtPrec 350 DenyInstallTimeExecution

        it "accepts a restated matching type on an AllowScope default" $
            resolveJsonOver mixedBase "{\"rules\":{\"trusted\":{\"type\":\"AllowScope\",\"scope\":\"acme\"}}}"
                `shouldSatisfy` hasRuleAtPrec 200 (AllowScope (mkScope "acme"))

        it "accepts a restated matching type on a DenyInstallTimeExecution default" $
            resolveJsonOver mixedBase "{\"rules\":{\"deny-scripts\":{\"type\":\"DenyInstallTimeExecution\"}}}"
                `shouldSatisfy` hasRuleAtPrec 300 DenyInstallTimeExecution

        it "rejects a restated mismatching type on a DenyInstallTimeExecution default" $
            resolveJsonOver mixedBase "{\"rules\":{\"deny-scripts\":{\"type\":\"AllowScope\"}}}"
                `shouldBe` Left [MalformedRule "deny-scripts" "\"type\" \"AllowScope\" does not match the default rule it patches"]

        it "suppresses one rule from a multi-rule base, keeping the rest" $
            resolveJsonOver mixedBase "{\"rules\":{\"trusted\":{\"enabled\":false}}}"
                `shouldBe` Right
                    [ PrecededRule 100 (AllowIfPublishedBefore (7 * 86400))
                    , PrecededRule 300 DenyInstallTimeExecution
                    ]

    describe "fail-loud merge references" $ do
        -- Each invalid patch is rejected with the specific error it should
        -- produce; the table makes the full set of rejected references visible.
        let cases :: [(String, ByteString, [PolicyError])]
            cases =
                [
                    ( "an unknown rule type (a typo'd deny must not vanish)"
                    , "{\"rules\":{\"deny-scripts\":{\"type\":\"DenyInstallTimeExecutio\"}}}"
                    , [UnknownRuleType "deny-scripts" "DenyInstallTimeExecutio"]
                    )
                ,
                    ( "the effectful AllowIfRemediatesCve type (unknown here, not a crash)"
                    , "{\"rules\":{\"cve\":{\"type\":\"AllowIfRemediatesCve\"}}}"
                    , [UnknownRuleType "cve" "AllowIfRemediatesCve"]
                    )
                ,
                    ( "a new name missing its type"
                    , "{\"rules\":{\"mystery\":{\"precedence\":120}}}"
                    , [MissingRuleType "mystery"]
                    )
                ,
                    ( "a suppression of a rule no default defines"
                    , "{\"rules\":{\"min-aeg\":{\"enabled\":false}}}"
                    , [SuppressUnknownRule "min-aeg"]
                    )
                ,
                    ( "an AllowScope add missing its scope value"
                    , "{\"rules\":{\"trusted\":{\"type\":\"AllowScope\"}}}"
                    , [MalformedRule "trusted" "\"AllowScope\" requires \"scope\""]
                    )
                ]
        for_ cases $ \(label, body, expected) ->
            it ("rejects " <> label) $
                resolveJson body `shouldBe` Left expected

    it "aggregates every merge error in one run (not fail-on-first)" $ do
        -- Two independent bad references in one patch; both must surface.
        let body =
                "{\"rules\":{\"bad-type\":{\"type\":\"Nope\"},\"ghost\":{\"enabled\":false}}}"
        case resolveJson body of
            Left errs ->
                errs
                    `shouldMatchList` [UnknownRuleType "bad-type" "Nope", SuppressUnknownRule "ghost"]
            Right rs -> expectationFailure ("expected aggregated errors, got " <> show rs)

-- ── single-mount desugaring & assembly ───────────────────────────────────────

desugarSpec :: Spec
desugarSpec = describe "loadConfig" $ do
    it "desugars the env single-mount onto the default policy with no document" $ do
        env <- expectEnv minimalEnv
        case loadConfig env Nothing of
            Left errs -> expectationFailure ("unexpected policy errors: " <> show errs)
            Right cfg -> do
                -- The env-only mount defaults to the npm ecosystem (its prefix
                -- derived as /npm), never the root /, which the no-root rule forbids.
                Map.keys (configMounts cfg) `shouldBe` [Npm]
                case Map.lookup Npm (configMounts cfg) of
                    Nothing -> expectationFailure "expected the npm mount"
                    Just mount -> do
                        mountEcosystem mount `shouldBe` Npm
                        mountPolicy mount
                            `shouldBe` [PrecededRule defaultAllowIfPublishedBeforePrecedence (AllowIfPublishedBefore (7 * 86400))]
                        let reg = mountRegistries mount
                        unUrl (regPrivateUpstream reg) `shouldBe` "https://private.example.test"
                        mtCredential (regMirrorTarget reg) `shouldBe` StaticCredential
                        mtQueue (regMirrorTarget reg) `shouldBe` SqsQueue
                        -- The explicit MIRROR_TARGET_URL in minimalEnv is used verbatim.
                        unUrl (mtUrl (regMirrorTarget reg)) `shouldBe` "https://mirror.example.test"

    it "folds an unset MIRROR_TARGET_URL onto the private upstream" $ do
        -- N7a: with MIRROR_TARGET_URL absent, the mirror target IS the private
        -- upstream (one registry, read and written) — the write credential does not fold.
        env <- expectEnv (without "MIRROR_TARGET_URL" minimalEnv)
        case loadConfig env Nothing of
            Left errs -> expectationFailure ("unexpected policy errors: " <> show errs)
            Right cfg -> case Map.lookup Npm (configMounts cfg) of
                Nothing -> expectationFailure "expected the npm mount"
                Just mount -> do
                    let reg = mountRegistries mount
                    unUrl (mtUrl (regMirrorTarget reg)) `shouldBe` "https://private.example.test"
                    unUrl (regPrivateUpstream reg) `shouldBe` "https://private.example.test"

    it "desugars the env single-mount onto a document-only policy patch" $ do
        -- A document with a rule policy but no mounts still produces the env
        -- single-mount (npm), now running on the merged policy.
        env <- expectEnv minimalEnv
        doc <- expectDoc "{\"rules\":{\"min-age\":{\"enabled\":false}}}"
        case loadConfig env (Just doc) of
            Left errs -> expectationFailure ("unexpected policy errors: " <> show errs)
            Right cfg -> case Map.lookup Npm (configMounts cfg) of
                Nothing -> expectationFailure "expected the npm mount"
                Just mount -> mountPolicy mount `shouldBe` []

    it "uses the document's declared mounts when present" $ do
        env <- expectEnv minimalEnv
        doc <- expectDoc singleMountDoc
        case loadConfig env (Just doc) of
            Left errs -> expectationFailure ("unexpected policy errors: " <> show errs)
            Right cfg -> Map.keys (configMounts cfg) `shouldBe` [Npm]

    it "applies a per-mount rule refinement over the shared policy" $ do
        -- The shared policy adds deny-scripts; the mount suppresses min-age, so
        -- the mount's resolved policy is the deny alone.
        env <- expectEnv minimalEnv
        doc <- expectDoc refinedMountDoc
        case loadConfig env (Just doc) of
            Left errs -> expectationFailure ("unexpected policy errors: " <> show errs)
            Right cfg -> case Map.lookup Npm (configMounts cfg) of
                Nothing -> expectationFailure "expected the npm mount"
                Just mount ->
                    mountPolicy mount `shouldBe` [PrecededRule defaultDenyInstallTimeExecutionPrecedence DenyInstallTimeExecution]

    it "surfaces a bad per-mount refinement as a policy error" $ do
        env <- expectEnv minimalEnv
        doc <- expectDoc badRefinementDoc
        loadConfig env (Just doc) `shouldBe` Left [UnknownRuleType "oops" "NoSuchRule"]

-- ── secrets ──────────────────────────────────────────────────────────────────

secretsSpec :: Spec
secretsSpec = describe "secrets are environment-only" $ do
    it "rejects a token field inside a rule entry, pointing to environment variables" $
        -- A secret must never live in the reviewable document; the decoder
        -- refuses it (naming the offending key) rather than carrying it.
        decodeDocument "{\"rules\":{\"min-age\":{\"token\":\"abc\"}}}"
            `shouldSatisfy` decodeErrorMentions "environment variables"

    it "rejects an authToken field inside a rule entry" $
        decodeDocument "{\"rules\":{\"min-age\":{\"authToken\":\"abc\"}}}" `shouldSatisfy` isLeft

-- ── fixtures & helpers ───────────────────────────────────────────────────────

-- A document with a single npm mount and a min-age override.
singleMountDoc :: ByteString
singleMountDoc =
    "{\"mounts\":{\"npm\":{\
    \\"privateUpstream\":\"https://private.example.test\",\
    \\"publicUpstream\":\"https://registry.npmjs.org\",\
    \\"mirrorTarget\":{\"url\":\"https://mirror.example.test\",\"credential\":\"codeartifact\",\"queue\":\"sqs\"}}},\
    \\"rules\":{\"min-age\":{\"ageSeconds\":1209600}}}"

-- A document whose top-level policy adds deny-scripts and whose mount suppresses
-- min-age, so the mount resolves to the deny alone.
refinedMountDoc :: ByteString
refinedMountDoc =
    "{\"mounts\":{\"npm\":{\
    \\"privateUpstream\":\"https://private.example.test\",\
    \\"publicUpstream\":\"https://registry.npmjs.org\",\
    \\"mirrorTarget\":{\"url\":\"https://mirror.example.test\",\"credential\":\"static\",\"queue\":\"sqs\"},\
    \\"rules\":{\"min-age\":{\"enabled\":false}}}},\
    \\"rules\":{\"deny-scripts\":{\"type\":\"DenyInstallTimeExecution\"}}}"

-- A document whose mount refinement names an unknown rule type.
badRefinementDoc :: ByteString
badRefinementDoc =
    "{\"mounts\":{\"npm\":{\
    \\"privateUpstream\":\"https://private.example.test\",\
    \\"publicUpstream\":\"https://registry.npmjs.org\",\
    \\"mirrorTarget\":{\"url\":\"https://mirror.example.test\",\"credential\":\"static\",\"queue\":\"sqs\"},\
    \\"rules\":{\"oops\":{\"type\":\"NoSuchRule\"}}}}}"

-- A bare single-mount document keyed by the given ecosystem name.
mountDocForEcosystem :: Text -> ByteString
mountDocForEcosystem eco =
    encodeUtf8
        ( "{\"mounts\":{\""
            <> eco
            <> "\":{\"privateUpstream\":\"https://a\",\"publicUpstream\":\"https://b\",\
               \\"mirrorTarget\":{\"url\":\"https://c\",\"credential\":\"static\",\"queue\":\"sqs\"}}}}"
        )

-- A single-mount document carrying an extra, unexpected key alongside the mount.
mountDocWithExtraKey :: Text -> ByteString
mountDocWithExtraKey extra =
    encodeUtf8
        ( "{\"mounts\":{\"npm\":{\"privateUpstream\":\"https://a\",\"publicUpstream\":\"https://b\",\
          \\"mirrorTarget\":{\"url\":\"https://c\",\"credential\":\"static\",\"queue\":\"sqs\"},\""
            <> extra
            <> "\":\"x\"}}}"
        )

-- A mount whose mirror target carries an unexpected key.
mirrorTargetWithExtraKey :: ByteString
mirrorTargetWithExtraKey =
    "{\"mounts\":{\"npm\":{\"privateUpstream\":\"https://a\",\"publicUpstream\":\"https://b\",\
    \\"mirrorTarget\":{\"url\":\"https://c\",\"credential\":\"static\",\"queue\":\"sqs\",\"token\":\"x\"}}}}"

-- A mount whose mirror target names the given queue backend.
mountWithQueue :: Text -> ByteString
mountWithQueue q =
    encodeUtf8
        ( "{\"mounts\":{\"npm\":{\"privateUpstream\":\"https://a\",\"publicUpstream\":\"https://b\",\
          \\"mirrorTarget\":{\"url\":\"https://c\",\"credential\":\"static\",\"queue\":\""
            <> q
            <> "\"}}}}"
        )

-- A mount whose mirror target names the given credential backend.
mountWithCredential :: Text -> ByteString
mountWithCredential c =
    encodeUtf8
        ( "{\"mounts\":{\"npm\":{\"privateUpstream\":\"https://a\",\"publicUpstream\":\"https://b\",\
          \\"mirrorTarget\":{\"url\":\"https://c\",\"credential\":\""
            <> c
            <> "\",\"queue\":\"sqs\"}}}}"
        )

-- A mount whose mirror-target @credential@ holds the given raw JSON value (no
-- automatic quoting), so a non-string value can be exercised.
mountWithCredential' :: Text -> ByteString
mountWithCredential' rawJson =
    encodeUtf8
        ( "{\"mounts\":{\"npm\":{\"privateUpstream\":\"https://a\",\"publicUpstream\":\"https://b\",\
          \\"mirrorTarget\":{\"url\":\"https://c\",\"credential\":"
            <> rawJson
            <> ",\"queue\":\"sqs\"}}}}"
        )

-- A mount whose mirror target has the given URL.
mountWithMirrorUrl :: Text -> ByteString
mountWithMirrorUrl u =
    encodeUtf8
        ( "{\"mounts\":{\"npm\":{\"privateUpstream\":\"https://a\",\"publicUpstream\":\"https://b\",\
          \\"mirrorTarget\":{\"url\":\""
            <> u
            <> "\",\"credential\":\"static\",\"queue\":\"sqs\"}}}}"
        )

-- Whether the resolved rule set contains an AllowScope for "myorg".
containsAllowScope :: Either [PolicyError] [PrecededRule] -> Bool
containsAllowScope (Right rs) = any isAllowScope rs
  where
    isAllowScope (PrecededRule _ (AllowScope _)) = True
    isAllowScope _ = False
containsAllowScope _ = False

-- Whether the resolved rule set contains exactly the given rule at the given
-- precedence.
hasRuleAtPrec :: Int -> Rule -> Either [PolicyError] [PrecededRule] -> Bool
hasRuleAtPrec prec rule (Right rs) = PrecededRule prec rule `elem` rs
hasRuleAtPrec _ _ _ = False

-- Parse an environment, failing the example on an unexpected error.
expectEnv :: [(String, String)] -> IO EnvConfig
expectEnv = either (\errs -> fail ("env parse failed: " <> show errs)) pure . parseEnvPure

-- Decode a document, failing the example on a decode error.
expectDoc :: ByteString -> IO ConfigDoc
expectDoc = either (\e -> fail ("document decode failed: " <> toString e)) pure . decodeDocument

-- Drop a variable from an environment list.
without :: String -> [(String, String)] -> [(String, String)]
without name = filter ((/= name) . fst)

-- Set (replacing) a variable in an environment list.
set :: String -> String -> [(String, String)] -> [(String, String)]
set name value env = (name, value) : without name env

-- Whether the needle occurs in the haystack 'Text'.
isInfix :: Text -> Text -> Bool
isInfix = T.isInfixOf

-- Whether a document decode failed with a message mentioning the given phrase.
decodeErrorMentions :: Text -> Either Text a -> Bool
decodeErrorMentions phrase (Left e) = phrase `isInfix` e
decodeErrorMentions _ (Right _) = False

-- 'show' a value as 'Text' (forcing its derived 'Show').
showText :: (Show a) => a -> Text
showText = show
