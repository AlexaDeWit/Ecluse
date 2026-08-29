-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Config.AesonSpec (spec) where

import Data.Text qualified as T

import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Config (
    AdvisoriesSettings (..),
    AppConfig (..),
    CacheSettings (..),
    Config (..),
    ConfigError,
    EgressSettings (..),
    IntegritySettings (..),
    LimitsSettings (..),
    ObservabilitySettings (..),
    QueueSettings (..),
    QueueTarget (..),
    RuntimeSettings (..),
    ServerSettings (..),
    loadConfig,
    queueUrlTarget,
    queueUrlText,
    renderConfigError,
    unUrl,
 )
import Ecluse.Core.Credential (unSecret)
import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Package.Merge (DivergencePolicy (FailClosed, Warn))
import Ecluse.Runtime.Log (LogLevel (DebugLevel, ErrorLevel, InfoLevel, WarnLevel))

spec :: Spec
spec = describe "decodeDocument" $ do
    it "decodes a document with one mount and a rule patch" $
        case loadConfig pubUrlEnv (Just singleMountDoc) of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> Map.keys (configMounts doc) `shouldBe` [Npm]

    it "decodes a document carrying only a rule policy (no mounts)" $
        case loadConfig [] (Just "{\"rules\":{\"min-age\":{\"ageSeconds\":1209600}}}") of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> configMounts doc `shouldBe` mempty

    it "keys a mount by its ecosystem name, deriving the prefix from it" $
        case loadConfig pubUrlEnv (Just (mountDocForEcosystem "npm")) of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> Map.keys (configMounts doc) `shouldBe` [Npm]

    it "rejects an unparseable JSON body" $
        loadConfig [] (Just "{not json") `shouldSatisfy` isLeft

    it "defaults observability.logLevel to info from the shipped baseline" $
        loadedLogLevel [] Nothing `shouldBe` Right InfoLevel

    it "parses every accepted observability.logLevel from the document" $ do
        loadedLogLevel [] (Just "{\"observability\":{\"logLevel\":\"debug\"}}") `shouldBe` Right DebugLevel
        loadedLogLevel [] (Just "{\"observability\":{\"logLevel\":\"info\"}}") `shouldBe` Right InfoLevel
        loadedLogLevel [] (Just "{\"observability\":{\"logLevel\":\"warn\"}}") `shouldBe` Right WarnLevel
        loadedLogLevel [] (Just "{\"observability\":{\"logLevel\":\"error\"}}") `shouldBe` Right ErrorLevel

    it "takes observability.logLevel from the environment layer" $
        loadedLogLevel [("ECLUSE_OBSERVABILITY__LOG_LEVEL", "warn")] Nothing `shouldBe` Right WarnLevel

    it "rejects an unknown observability.logLevel, naming the key and the accepted set" $ do
        loadConfig [("ECLUSE_OBSERVABILITY__LOG_LEVEL", "trace")] Nothing
            `shouldSatisfy` decodeErrorMentions "observability.logLevel"
        loadConfig [("ECLUSE_OBSERVABILITY__LOG_LEVEL", "trace")] Nothing
            `shouldSatisfy` decodeErrorMentions "expected one of: debug, info, warn, error"

    it "rejects an unknown key under observability, naming it" $
        loadConfig [] (Just "{\"observability\":{\"logLevl\":\"info\"}}")
            `shouldSatisfy` decodeErrorMentions "logLevl"

    it "rejects an unknown key under queue, naming it" $
        loadConfig [] (Just "{\"queue\":{\"maxRecieveCount\":5}}")
            `shouldSatisfy` decodeErrorMentions "maxRecieveCount"

    it "rejects an unknown top-level key, naming it (strict, not silently dropped)" $
        loadConfig [] (Just "{\"mountz\":{}}") `shouldSatisfy` decodeErrorMentions "mountz"

    it "rejects the ambient AWS SDK variables as document keys (environment, never config)" $ do
        -- A document-side awsSecretAccessKey is refused, never accepted and ignored,
        -- so "secrets never live in the structured config" stays structural.
        loadConfig [] (Just "{\"awsSecretAccessKey\":\"hunter2\"}") `shouldSatisfy` decodeErrorMentions "awsSecretAccessKey"
        loadConfig [] (Just "{\"awsRegion\":\"us-east-1\"}") `shouldSatisfy` decodeErrorMentions "awsRegion"

    it "rejects an unknown mount ecosystem key, naming it (strict, not silently dropped)" $
        loadConfig [] (Just (mountDocForEcosystem "npmm")) `shouldSatisfy` decodeErrorMentions "npmm"

    it "rejects an unknown key inside a mount, naming it" $
        loadConfig [] (Just (mountDocWithExtraKey "baseURL")) `shouldSatisfy` decodeErrorMentions "baseURL"

    it "keeps the shipped template mounts dormant when the overlay never mentions them" $
        case loadConfig [] Nothing of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> configMounts doc `shouldBe` mempty

    it "activates a mount from the environment layer alone" $
        case loadConfig
            ( pubUrlEnv
                <> [ ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM", "https://private.example.test")
                   , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET", "https://mirror.example.test")
                   , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN", "t")
                   ]
            )
            Nothing of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> Map.keys (configMounts doc) `shouldBe` [Npm]

    it "resolves a mount declared with no endpoint keys as the serve-only pure gate" $
        -- Mirroring is derived from the declared target, so a mount with no endpoint keys fronts
        -- only the template public upstream.
        case loadConfig pubUrlEnv (Just "{\"mounts\":{\"npm\":{}}}") of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> Map.keys (configMounts doc) `shouldBe` [Npm]

    it "resolves a mount declaring only a private upstream as serve-only over the merge" $
        case loadConfig pubUrlEnv (Just "{\"mounts\":{\"npm\":{\"privateUpstream\":\"https://private.example.test\"}}}") of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> Map.keys (configMounts doc) `shouldBe` [Npm]

    it "fails loudly when a mirrored mount (mirrorTarget declared) omits its private upstream" $
        -- The mirror must be readable back through the private leg, so a mirrored
        -- mount without one is refused. Only serve-only mounts may omit it.
        loadConfig
            []
            (Just "{\"mounts\":{\"npm\":{\"mirrorTarget\":\"https://mirror.example.test\",\"mirrorTargetToken\":\"t\"}}}")
            `shouldSatisfy` decodeErrorMentions "mounts.npm.privateUpstream"

    it "loads a mount whose mirror target is declared equal to its private upstream" $
        -- Equality with the private upstream is a valid arrangement. Only the
        -- declaration itself is mandatory.
        case loadConfig
            pubUrlEnv
            ( Just
                "{\"mounts\":{\"npm\":{\"privateUpstream\":\"https://one.example.test\",\
                \\"mirrorTarget\":\"https://one.example.test\",\"mirrorTargetToken\":\"t\"}}}"
            ) of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> Map.keys (configMounts doc) `shouldBe` [Npm]

    it "fails loudly when an environment token activates a mount that never writes" $
        -- A write token on a serve-only mount (no mirrorTarget) signals a
        -- misunderstanding and is refused, naming the offending key.
        loadConfig [("ECLUSE_MOUNTS__PYPI__MIRROR_TARGET_TOKEN", "t")] Nothing
            `shouldSatisfy` decodeErrorMentions "ECLUSE_MOUNTS__PYPI__MIRROR_TARGET_TOKEN"

    it "rejects a malformed publicationAllow entry (a wrong separator folds into one dead scope), naming publicationAllow" $
        -- A stray separator would otherwise fold into a single unmatchable scope that
        -- passes the non-empty boot check, refusing every publish only at request time.
        loadConfig [("ECLUSE_MOUNTS__NPM__PUBLICATION_ALLOW", "@acme;@beta")] Nothing
            `shouldSatisfy` decodeErrorMentions "invalid scope in publicationAllow"

    it "rejects a publicationAllow with an empty segment from a stray comma, naming publicationAllow" $
        loadConfig [("ECLUSE_MOUNTS__NPM__PUBLICATION_ALLOW", "@acme,,@beta")] Nothing
            `shouldSatisfy` decodeErrorMentions "invalid scope in publicationAllow"

    it "rejects an empty publicationAllow at load, naming publicationAllow" $
        -- A configured list that admits nothing would refuse every publish, so the load
        -- refuses it rather than binding a dead allow-list.
        loadConfig pubUrlEnv (Just "{\"mounts\":{\"npm\":{\"publicationAllow\":\"\"}}}")
            `shouldSatisfy` decodeErrorMentions "publicationAllow must name at least one scope"

    it "rejects publicationAllow on a mount whose ecosystem has no allow-list shape yet" $
        -- Without the per-ecosystem arm the entries would parse as npm scopes, so the key
        -- refuses at load and names the ecosystem it is unsupported for.
        loadConfig pubUrlEnv (Just "{\"mounts\":{\"pypi\":{\"publicationAllow\":\"@acme\"}}}")
            `shouldSatisfy` decodeErrorMentions "publicationAllow is not supported for pypi yet"

    describe "a publicationAllow entry reads through npm's own scope grammar" $
        -- The allow-list and the request path must not disagree about what a scope is, so the
        -- entry goes through the same splitter the route and the projection use.
        for_ scopeEntryVerdicts $ \(entry, valid) ->
            it (show entry <> (if valid then " is a scope" else " is refused")) $
                if valid
                    then loadPublicationAllow entry `shouldSatisfy` isRight
                    else loadPublicationAllow entry `shouldSatisfy` decodeErrorMentions "invalid scope in publicationAllow"

    it "accepts a well-formed comma-separated publicationAllow (trimmed, leading sigil tolerated)" $
        case loadConfig
            ( pubUrlEnv
                <> [ ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM", "https://private.example.test")
                   , ("ECLUSE_MOUNTS__NPM__PUBLICATION_ALLOW", "@acme, beta")
                   ]
            )
            Nothing of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> Map.keys (configMounts doc) `shouldBe` [Npm]

    it "reports every incomplete mirrored mount in one load, not only the first" $ do
        let doc =
                "{\"mounts\":{\"npm\":{\"mirrorTarget\":\"https://m1.example.test\",\"mirrorTargetToken\":\"t\"},\
                \\"pypi\":{\"mirrorTarget\":\"https://m2.example.test\",\"mirrorTargetToken\":\"t\"}}}"
        let outcome = loadConfig [] (Just doc)
        outcome `shouldSatisfy` decodeErrorMentions "mounts.npm.privateUpstream"
        outcome `shouldSatisfy` decodeErrorMentions "mounts.pypi.privateUpstream"

    it "loads the bounded serve and connection-pool defaults" $
        case loadConfig [] Nothing of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> do
                -- serveMaxInFlight is unset by default: the boot computes the
                -- effective capacity from the resolved capability count.
                rtServeMaxInFlight (cfgRuntime (configApp doc)) `shouldBe` Nothing
                -- publicConnectionsPerHost is unset by default too: the boot computes the effective
                -- pool from the file-descriptor limit, like the private pool.
                rtPublicConnectionsPerHost (cfgRuntime (configApp doc)) `shouldBe` Nothing

    it "rejects a zero cveDbPollInterval (a zero delay would spin the poll)" $
        loadConfig [] (Just "{\"cveDbPollInterval\":0}")
            `shouldSatisfy` decodeErrorMentions "cveDbPollInterval"

    it "rejects a zero advisories.pollInterval given through the environment" $
        loadConfig [("ECLUSE_ADVISORIES__POLL_INTERVAL", "0")] Nothing
            `shouldSatisfy` decodeErrorMentions "advisories.pollInterval"

    it "rejects an advisories.pollInterval whose microsecond conversion would overflow Int" $
        loadConfig [] (Just "{\"advisories\":{\"pollInterval\":9223372036855}}")
            `shouldSatisfy` decodeErrorMentions "advisories.pollInterval"

    it "rejects a zero advisories.compileInterval (a zero delay would spin the export loop)" $
        loadConfig [] (Just "{\"advisories\":{\"compileInterval\":0}}")
            `shouldSatisfy` decodeErrorMentions "advisories.compileInterval"

    it "rejects a zero advisories.compileInterval given through the environment" $
        loadConfig [("ECLUSE_ADVISORIES__COMPILE_INTERVAL", "0")] Nothing
            `shouldSatisfy` decodeErrorMentions "advisories.compileInterval"

    it "rejects an advisories.compileInterval whose microsecond conversion would overflow Int" $
        loadConfig [] (Just "{\"advisories\":{\"compileInterval\":9223372036855}}")
            `shouldSatisfy` decodeErrorMentions "advisories.compileInterval"

    it "rejects a fractional cache.ttl, naming the field (a fraction was silently truncated before)" $
        loadConfig [] (Just "{\"cache\":{\"ttl\":2.7}}")
            `shouldSatisfy` decodeErrorMentions "cache.ttl must be a non-negative integer count of seconds"

    it "rejects a huge-exponent cache.ttl without realising the integer (no boot hang or OOM)" $
        -- The env overlay JSON-decodes this to a Scientific verbatim, reaching
        -- parseSeconds' Number branch. A raw truncate would try to materialise an
        -- astronomically large Integer at boot. The bounded parse fails instantly.
        loadConfig [("ECLUSE_CACHE__TTL", "1e999999999999")] Nothing
            `shouldSatisfy` decodeErrorMentions "cache.ttl must be a non-negative integer count of seconds"

    it "rejects a huge-exponent advisories.pollInterval (the delay path is guarded too)" $
        loadConfig [("ECLUSE_ADVISORIES__POLL_INTERVAL", "1e999999999999")] Nothing
            `shouldSatisfy` decodeErrorMentions "advisories.pollInterval must be a non-negative integer count of seconds"

    it "accepts a zero and a positive integer cache.ttl (the pre-fix accepted forms, unchanged)" $ do
        case loadConfig [] (Just "{\"cache\":{\"ttl\":0}}") of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> csTtl (cfgCache (configApp doc)) `shouldBe` 0
        case loadConfig [("ECLUSE_CACHE__TTL", "120")] Nothing of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> csTtl (cfgCache (configApp doc)) `shouldBe` 120

    it "accepts a quoted integer cache.ttl and rejects a quoted fractional one (both branches agree)" $ do
        case loadConfig [] (Just "{\"cache\":{\"ttl\":\"120\"}}") of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> csTtl (cfgCache (configApp doc)) `shouldBe` 120
        loadConfig [] (Just "{\"cache\":{\"ttl\":\"2.7\"}}")
            `shouldSatisfy` decodeErrorMentions "cache.ttl must be a non-negative integer count of seconds"

    it "rejects a cache.ttl that is neither a string nor a number, naming the field" $
        loadConfig [] (Just "{\"cache\":{\"ttl\":true}}")
            `shouldSatisfy` decodeErrorMentions "cache.ttl must be a non-negative integer count of seconds"

    -- A quoted count has one spelling: a bare decimal run. The base prefixes, padding, and
    -- brackets that Haskell's `Read` took now fail the load instead.
    it "rejects a quoted cache.ttl written as hex, octal, padded, bracketed, or signed" $
        for_ (["0x10", " 120", "120 ", "(120)", "+120", "0o10"] :: [Text]) $ \spelling ->
            loadConfig [] (Just (encodeUtf8 @Text @ByteString ("{\"cache\":{\"ttl\":\"" <> spelling <> "\"}}")))
                `shouldSatisfy` decodeErrorMentions "cache.ttl must be a non-negative integer count of seconds"

    it "rejects a non-positive limits.maxAdvisoryDatabaseBytes" $
        loadConfig [] (Just "{\"limits\":{\"maxAdvisoryDatabaseBytes\":0}}")
            `shouldSatisfy` decodeErrorMentions "limits.maxAdvisoryDatabaseBytes"

    it "loads the shipped advisory-sync defaults (poll interval, byte cap, data dir, no store)" $
        case loadConfig [] Nothing of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> do
                advPollInterval (cfgAdvisories (configApp doc)) `shouldBe` 60
                limMaxAdvisoryDatabaseBytes (cfgLimits (configApp doc)) `shouldBe` 536870912
                -- Absolute on purpose: the shipped image sets no working directory, so a
                -- relative path lands in a root the container's user cannot write.
                advDataDir (cfgAdvisories (configApp doc)) `shouldBe` "/var/lib/ecluse/advisories"
                advUrl (cfgAdvisories (configApp doc)) `shouldBe` Nothing

    it "defaults the divergence policy to warn" $
        case loadConfig [] Nothing of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> intDivergencePolicy (cfgIntegrity (configApp doc)) `shouldBe` Warn

    it "parses ECLUSE_INTEGRITY__DIVERGENCE_POLICY=fail-closed from the environment" $
        case loadConfig [("ECLUSE_INTEGRITY__DIVERGENCE_POLICY", "fail-closed")] Nothing of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> intDivergencePolicy (cfgIntegrity (configApp doc)) `shouldBe` FailClosed

    it "rejects an unknown ECLUSE_INTEGRITY__DIVERGENCE_POLICY value, naming the field" $
        loadConfig [("ECLUSE_INTEGRITY__DIVERGENCE_POLICY", "drop")] Nothing
            `shouldSatisfy` decodeErrorMentions "divergencePolicy"

    it "leaves the runtime posture unset when the shipped defaults are all that apply" $ do
        -- Every runtime key unset: the boot resolves cores down its ladder, and the ladder's
        -- last rung takes its own default ceiling rather than one this layer supplies.
        case loadConfig [] Nothing of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> do
                rtCores (cfgRuntime (configApp doc)) `shouldBe` Nothing
                rtCoresCeiling (cfgRuntime (configApp doc)) `shouldBe` Nothing
                rtMaxHeapBytes (cfgRuntime (configApp doc)) `shouldBe` Nothing

    it "parses cores, coresCeiling, and maxHeapBytes from the environment layer" $ do
        case loadConfig [("ECLUSE_RUNTIME__CORES", "2"), ("ECLUSE_RUNTIME__CORES_CEILING", "16"), ("ECLUSE_RUNTIME__MAX_HEAP_BYTES", "419430400")] Nothing of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> do
                rtCores (cfgRuntime (configApp doc)) `shouldBe` Just 2
                rtCoresCeiling (cfgRuntime (configApp doc)) `shouldBe` Just 16
                rtMaxHeapBytes (cfgRuntime (configApp doc)) `shouldBe` Just 419430400

    it "rejects non-positive cores, coresCeiling, and maxHeapBytes" $ do
        loadConfig [("ECLUSE_RUNTIME__CORES", "0")] Nothing
            `shouldSatisfy` decodeErrorMentions "cores must be a positive integer"
        loadConfig [("ECLUSE_RUNTIME__CORES_CEILING", "0")] Nothing
            `shouldSatisfy` decodeErrorMentions "coresCeiling must be a positive integer"
        loadConfig [("ECLUSE_RUNTIME__CORES_CEILING", "-4")] Nothing
            `shouldSatisfy` decodeErrorMentions "coresCeiling must be a positive integer"
        loadConfig [("ECLUSE_RUNTIME__MAX_HEAP_BYTES", "-1")] Nothing
            `shouldSatisfy` decodeErrorMentions "maxHeapBytes must be a positive integer"

    it "parses an explicit serveMaxInFlight override" $ do
        case loadConfig [("ECLUSE_RUNTIME__SERVE_MAX_IN_FLIGHT", "24")] Nothing of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> rtServeMaxInFlight (cfgRuntime (configApp doc)) `shouldBe` Just 24

    it "parses an explicit privateConnectionsPerHost override" $ do
        -- The private pool default is computed from the file-descriptor limit, independent of the
        -- admission capacity because it streams outside admission. An operator can still pin it.
        case loadConfig [("ECLUSE_RUNTIME__PRIVATE_CONNECTIONS_PER_HOST", "256")] Nothing of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> rtPrivateConnectionsPerHost (cfgRuntime (configApp doc)) `shouldBe` Just 256

    it "leaves privateConnectionsPerHost unset when not configured (computed at boot)" $
        case loadConfig [] Nothing of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> rtPrivateConnectionsPerHost (cfgRuntime (configApp doc)) `shouldBe` Nothing

    it "rejects non-positive serve and connection capacities" $ do
        loadConfig [("ECLUSE_RUNTIME__SERVE_MAX_IN_FLIGHT", "0")] Nothing
            `shouldSatisfy` decodeErrorMentions "serveMaxInFlight must be a positive integer"
        loadConfig [("ECLUSE_RUNTIME__PUBLIC_CONNECTIONS_PER_HOST", "0")] Nothing
            `shouldSatisfy` decodeErrorMentions "publicConnectionsPerHost must be a positive integer"
        loadConfig [("ECLUSE_RUNTIME__PRIVATE_CONNECTIONS_PER_HOST", "0")] Nothing
            `shouldSatisfy` decodeErrorMentions "privateConnectionsPerHost must be a positive integer"

    it "leaves additionalBlockedRanges empty by default" $
        case loadConfig [] Nothing of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> egrAdditionalBlockedRanges (cfgEgress (configApp doc)) `shouldBe` []

    it "parses a comma-separated additionalBlockedRanges from the environment layer" $
        case loadConfig [("ECLUSE_EGRESS__ADDITIONAL_BLOCKED_RANGES", "203.0.113.0/24,2001:db8::/32")] Nothing of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> egrAdditionalBlockedRanges (cfgEgress (configApp doc)) `shouldBe` ["203.0.113.0/24", "2001:db8::/32"]

    it "trims whitespace around each additionalBlockedRanges entry" $
        case loadConfig [("ECLUSE_EGRESS__ADDITIONAL_BLOCKED_RANGES", " 203.0.113.0/24 , 2001:db8::/32 ")] Nothing of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> egrAdditionalBlockedRanges (cfgEgress (configApp doc)) `shouldBe` ["203.0.113.0/24", "2001:db8::/32"]

    it "rejects a malformed entry in additionalBlockedRanges, naming it (fails closed at boot)" $
        loadConfig [("ECLUSE_EGRESS__ADDITIONAL_BLOCKED_RANGES", "not-a-range")] Nothing
            `shouldSatisfy` decodeErrorMentions "invalid CIDR range"

    describe "registry URL entries (the egress gate authorises each entry's host:port pair)" $ do
        it "accepts an upstream URL with an explicit port" $
            case loadConfig
                ( pubUrlEnv
                    <> [ ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM", "https://repo.internal.example.test:8443/npm")
                       , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET", "https://mirror.example.test")
                       , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN", "t")
                       ]
                )
                Nothing of
                Left e -> expectationFailure ("unexpected decode error: " <> show e)
                Right doc -> Map.keys (configMounts doc) `shouldBe` [Npm]
        it "accepts an upstream URL with a bracketed IPv6 host and a port" $
            case loadConfig
                ( pubUrlEnv
                    <> [ ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM", "https://[2001:db8::10]:8443/npm")
                       , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET", "https://mirror.example.test")
                       , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN", "t")
                       ]
                )
                Nothing of
                Left e -> expectationFailure ("unexpected decode error: " <> show e)
                Right doc -> Map.keys (configMounts doc) `shouldBe` [Npm]
        it "rejects an upstream URL with a non-numeric port, naming the value (fails closed at boot)" $
            -- The gate refuses every fetch from an authority it cannot extract, so the
            -- misconfiguration surfaces at load, never as a mount that silently serves nothing.
            loadConfig [("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM", "https://repo.internal.example.test:9x9/npm")] Nothing
                `shouldSatisfy` decodeErrorMentions "decimal port in 1..65535"
        it "rejects an upstream URL with an out-of-range port" $
            loadConfig [("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM", "https://repo.internal.example.test:65536/npm")] Nothing
                `shouldSatisfy` decodeErrorMentions "decimal port in 1..65535"
        it "rejects an upstream URL with port 0" $
            loadConfig [("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM", "https://repo.internal.example.test:0/npm")] Nothing
                `shouldSatisfy` decodeErrorMentions "decimal port in 1..65535"
        it "rejects a mirror-target URL with a garbage port through the document layer" $
            loadConfig [] (Just (mountDocWithMirrorTarget "https://mirror.example.test:port/npm"))
                `shouldSatisfy` decodeErrorMentions "decimal port in 1..65535"

        -- Boot echoes a successful load key by key, warns on colliding endpoints, and
        -- prints a posture line per mount. Each line renders a configured registry URL
        -- as given. Refusing these shapes at load is what keeps a credential off those
        -- lines, so the refusal names the key and never the value.
        it "rejects an upstream URL carrying userinfo, naming the key and not the credential" $ do
            let outcome = loadConfig [("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM", "https://deploy:hunter2@repo.internal.example.test/npm")] Nothing
            outcome `shouldSatisfy` decodeErrorMentions "privateUpstream: registry URL must not carry userinfo"
            outcome `shouldSatisfy` (not . decodeErrorMentions "hunter2")

        it "rejects an upstream URL carrying a query string, naming the key" $
            loadConfig [("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM", "https://repo.internal.example.test/npm?token=abc")] Nothing
                `shouldSatisfy` decodeErrorMentions "privateUpstream: registry URL must not carry a query string"

        it "rejects an upstream URL carrying a fragment, naming the key" $
            loadConfig [("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM", "https://repo.internal.example.test/npm#frag")] Nothing
                `shouldSatisfy` decodeErrorMentions "privateUpstream: registry URL must not carry a fragment"

        -- A second endpoint key, so a key name hand-written into the wrong call site
        -- cannot pass by matching a neighbour's.
        it "rejects a mirror-target URL carrying userinfo, naming that key" $
            loadConfig [] (Just (mountDocWithMirrorTarget "https://deploy:hunter2@mirror.example.test/npm"))
                `shouldSatisfy` decodeErrorMentions "mirrorTarget: registry URL must not carry userinfo"

    describe "non-registry configured URLs (the same boot echo prints these keys)" $ do
        -- server.publicUrl, advisories.osvExportBaseUrl, and queue.url are not registry endpoints,
        -- so the mount-side refusal above never sees them. Each carries the refusal under its own key.
        it "rejects server.publicUrl carrying userinfo, naming the key and not the credential" $ do
            let outcome = loadConfig [("ECLUSE_SERVER__PUBLIC_URL", "https://deploy:hunter2@registry.example.test")] Nothing
            outcome `shouldSatisfy` decodeErrorMentions "server.publicUrl must not carry userinfo"
            outcome `shouldSatisfy` (not . decodeErrorMentions "hunter2")

        it "refuses a credential in server.publicUrl before the scheme check, which quotes the value" $ do
            -- A schemeless value falls to the scheme refusal, and that refusal echoes what
            -- it rejects. The credential refusal has to run ahead of it.
            let outcome = loadConfig [("ECLUSE_SERVER__PUBLIC_URL", "deploy:hunter2@registry.example.test")] Nothing
            outcome `shouldSatisfy` decodeErrorMentions "server.publicUrl must not carry userinfo"
            outcome `shouldSatisfy` (not . decodeErrorMentions "hunter2")

        it "rejects server.publicUrl carrying a query string, naming the key" $
            loadConfig [("ECLUSE_SERVER__PUBLIC_URL", "https://registry.example.test?token=abc")] Nothing
                `shouldSatisfy` decodeErrorMentions "server.publicUrl must not carry a query string"

        it "rejects server.publicUrl carrying a fragment, naming the key" $
            loadConfig [] (Just "{\"server\":{\"publicUrl\":\"https://registry.example.test#frag\"}}")
                `shouldSatisfy` decodeErrorMentions "server.publicUrl must not carry a fragment"

        it "rejects advisories.osvExportBaseUrl carrying userinfo, naming the key and not the credential" $ do
            let outcome = loadConfig [("ECLUSE_ADVISORIES__OSV_EXPORT_BASE_URL", "https://deploy:hunter2@osv.example.test")] Nothing
            outcome `shouldSatisfy` decodeErrorMentions "advisories.osvExportBaseUrl must not carry userinfo"
            outcome `shouldSatisfy` (not . decodeErrorMentions "hunter2")

        it "rejects advisories.osvExportBaseUrl carrying a query string, naming the key" $
            loadConfig [("ECLUSE_ADVISORIES__OSV_EXPORT_BASE_URL", "https://osv.example.test?sig=abc")] Nothing
                `shouldSatisfy` decodeErrorMentions "advisories.osvExportBaseUrl must not carry a query string"

        it "rejects advisories.osvExportBaseUrl carrying a fragment, naming the key" $
            loadConfig [] (Just "{\"advisories\":{\"osvExportBaseUrl\":\"https://osv.example.test#frag\"}}")
                `shouldSatisfy` decodeErrorMentions "advisories.osvExportBaseUrl must not carry a fragment"

        it "accepts a plain advisories.osvExportBaseUrl through both layers" $ do
            case loadConfig [("ECLUSE_ADVISORIES__OSV_EXPORT_BASE_URL", "https://osv.example.test/exports")] Nothing of
                Left e -> expectationFailure ("unexpected decode error: " <> show e)
                Right doc ->
                    unUrl (advOsvExportBaseUrl (cfgAdvisories (configApp doc)))
                        `shouldBe` "https://osv.example.test/exports"
            case loadConfig [] (Just "{\"advisories\":{\"osvExportBaseUrl\":\"http://localhost:8080/osv\"}}") of
                Left e -> expectationFailure ("unexpected decode error: " <> show e)
                Right doc ->
                    unUrl (advOsvExportBaseUrl (cfgAdvisories (configApp doc)))
                        `shouldBe` "http://localhost:8080/osv"

        -- queue.url goes to a cloud SDK, so no scheme or authority check in the parser
        -- quotes it. The unrecognised-shape boot error and the check-config queue line
        -- print it whole, and both run after a successful load.
        it "rejects queue.url carrying userinfo, naming the key and not the credential" $ do
            let outcome = loadConfig [("ECLUSE_QUEUE__URL", "https://deploy:hunter2@sqs.us-east-1.amazonaws.com/123456789012/mirror")] Nothing
            outcome `shouldSatisfy` decodeErrorMentions "queue.url must not carry userinfo"
            outcome `shouldSatisfy` (not . decodeErrorMentions "hunter2")

        it "names the key and the requirement in a queue.url refusal, never the value" $ do
            let outcome = loadConfig [] (Just "{\"queue\":{\"url\":\"https://deploy:hunter2@queue.example.test/q\"}}")
            outcome `shouldSatisfy` decodeErrorMentions "queue.url must not carry userinfo"
            outcome `shouldSatisfy` (not . decodeErrorMentions "hunter2")
            outcome `shouldSatisfy` (not . decodeErrorMentions "queue.example.test")

        it "rejects queue.url carrying a query string, naming the key" $
            loadConfig [("ECLUSE_QUEUE__URL", "https://sqs.us-east-1.amazonaws.com/123456789012/mirror?token=abc")] Nothing
                `shouldSatisfy` decodeErrorMentions "queue.url must not carry a query string"

        it "rejects queue.url carrying a fragment, naming the key" $
            loadConfig [] (Just "{\"queue\":{\"url\":\"https://sqs.us-east-1.amazonaws.com/123456789012/mirror#frag\"}}")
                `shouldSatisfy` decodeErrorMentions "queue.url must not carry a fragment"

        it "accepts a plain queue.url through both layers, deriving its backend at load" $ do
            loadedQueueUrl [("ECLUSE_QUEUE__URL", "https://sqs.us-east-1.amazonaws.com/123456789012/mirror")] Nothing
                `shouldBe` Right
                    (Just ("https://sqs.us-east-1.amazonaws.com/123456789012/mirror", Just (SqsTarget "us-east-1")))
            loadedQueueUrl [] (Just "{\"queue\":{\"url\":\"projects/acme/topics/mirror\"}}")
                `shouldBe` Right (Just ("projects/acme/topics/mirror", Just (PubSubTarget "acme" "mirror")))

        it "loads a queue.url whose shape names no backend, for the endpoint-override path" $
            -- The emulator URL matches no public shape. Refusing it at load would take the
            -- AWS_ENDPOINT_URL_SQS deployment with it, so the derived target is simply absent.
            loadedQueueUrl [("ECLUSE_QUEUE__URL", "http://ministack:4566/000000000000/mirror")] Nothing
                `shouldBe` Right (Just ("http://ministack:4566/000000000000/mirror", Nothing))

        it "rejects a blank queue.url, naming the key, through both layers" $ do
            loadConfig [("ECLUSE_QUEUE__URL", "   ")] Nothing
                `shouldSatisfy` decodeErrorMentions "queue.url must be a non-empty URL"
            loadConfig [] (Just "{\"queue\":{\"url\":\"\"}}")
                `shouldSatisfy` decodeErrorMentions "queue.url must be a non-empty URL"

        it "leaves queue.url unset, which is the in-memory rollover" $
            loadedQueueUrl [] Nothing `shouldBe` Right Nothing

    describe "field invariants (document and environment enforce the same bounds)" $ do
        it "accepts the listener-port range ends: 0 (OS-assigned) and 65535" $ do
            case loadConfig [] (Just "{\"server\":{\"port\":0}}") of
                Left e -> expectationFailure ("unexpected decode error: " <> show e)
                Right doc -> srvPort (cfgServer (configApp doc)) `shouldBe` 0
            case loadConfig [("ECLUSE_SERVER__PORT", "65535")] Nothing of
                Left e -> expectationFailure ("unexpected decode error: " <> show e)
                Right doc -> srvPort (cfgServer (configApp doc)) `shouldBe` 65535

        it "rejects a listener port outside 0..65535, through both layers" $ do
            loadConfig [] (Just "{\"server\":{\"port\":-1}}")
                `shouldSatisfy` decodeErrorMentions "server.port must be a port in 0..65535"
            loadConfig [("ECLUSE_SERVER__PORT", "65536")] Nothing
                `shouldSatisfy` decodeErrorMentions "server.port must be a port in 0..65535"

        it "rejects a non-positive shutdownDrainTimeout, through both layers" $ do
            loadConfig [] (Just "{\"server\":{\"shutdownDrainTimeout\":0}}")
                `shouldSatisfy` decodeErrorMentions "server.shutdownDrainTimeout must be a positive integer"
            loadConfig [("ECLUSE_SERVER__SHUTDOWN_DRAIN_TIMEOUT", "-5")] Nothing
                `shouldSatisfy` decodeErrorMentions "server.shutdownDrainTimeout must be a positive integer"

        it "rejects a non-positive queue.maxReceiveCount, through both layers" $ do
            -- A budget of zero would name a delivery no message can reach. The parser
            -- refuses it rather than letting the runtime floor mask it.
            loadConfig [] (Just "{\"queue\":{\"maxReceiveCount\":0}}")
                `shouldSatisfy` decodeErrorMentions "queue.maxReceiveCount must be a positive integer"
            loadConfig [("ECLUSE_QUEUE__MAX_RECEIVE_COUNT", "-2")] Nothing
                `shouldSatisfy` decodeErrorMentions "queue.maxReceiveCount must be a positive integer"

        it "rejects non-positive parser guards (maxVersionCount, maxNestingDepth), through both layers" $ do
            loadConfig [] (Just "{\"limits\":{\"maxVersionCount\":0}}")
                `shouldSatisfy` decodeErrorMentions "limits.maxVersionCount must be a positive integer"
            loadConfig [("ECLUSE_LIMITS__MAX_NESTING_DEPTH", "0")] Nothing
                `shouldSatisfy` decodeErrorMentions "limits.maxNestingDepth must be a positive integer"

        it "accepts an http public URL (loopback development deployments stay legal)" $
            case loadConfig [("ECLUSE_SERVER__PUBLIC_URL", "http://localhost:8080")] Nothing of
                Left e -> expectationFailure ("unexpected decode error: " <> show e)
                Right _ -> pure ()

        it "rejects a schemeless public URL, naming the field" $
            loadConfig [("ECLUSE_SERVER__PUBLIC_URL", "registry.example.test")] Nothing
                `shouldSatisfy` decodeErrorMentions "server.publicUrl must be an http:// or https:// URL"

        it "rejects a public URL with an undialable authority, through both layers" $ do
            loadConfig [] (Just "{\"server\":{\"publicUrl\":\"https://registry.example.test:9x9\"}}")
                `shouldSatisfy` decodeErrorMentions "server.publicUrl must carry a host"
            loadConfig [("ECLUSE_SERVER__PUBLIC_URL", "https://registry.example.test:0")] Nothing
                `shouldSatisfy` decodeErrorMentions "server.publicUrl must carry a host"

        it "accepts the CodeArtifact token-duration range ends: 900 and 43200" $ do
            let docFor (n :: Int) =
                    encodeUtf8 @Text @ByteString $
                        "{\"mounts\":{\"npm\":{\"privateUpstream\":\"https://a\",\
                        \\"mirrorTarget\":\"https://c\",\"mirrorTargetToken\":\"t\",\
                        \\"mirrorTokenDuration\":"
                            <> show n
                            <> "}}}"
            case loadConfig pubUrlEnv (Just (docFor 900)) of
                Left e -> expectationFailure ("unexpected decode error: " <> show e)
                Right doc -> Map.keys (configMounts doc) `shouldBe` [Npm]
            case loadConfig pubUrlEnv (Just (docFor 43200)) of
                Left e -> expectationFailure ("unexpected decode error: " <> show e)
                Right doc -> Map.keys (configMounts doc) `shouldBe` [Npm]

        it "rejects a CodeArtifact token duration outside 900..43200, through both layers" $ do
            loadConfig
                []
                (Just "{\"mounts\":{\"npm\":{\"mirrorTokenDuration\":899}}}")
                `shouldSatisfy` decodeErrorMentions "mirrorTokenDuration must be a duration in seconds within 900..43200"
            loadConfig [("ECLUSE_MOUNTS__NPM__MIRROR_TOKEN_DURATION", "43201")] Nothing
                `shouldSatisfy` decodeErrorMentions "mirrorTokenDuration must be a duration in seconds within 900..43200"

        it "rejects a quoted CodeArtifact token duration written as hex or padded" $
            for_ (["0x1000", " 3600", "(3600)"] :: [Text]) $ \spelling ->
                loadConfig [] (Just (encodeUtf8 @Text @ByteString ("{\"mounts\":{\"npm\":{\"mirrorTokenDuration\":\"" <> spelling <> "\"}}}")))
                    `shouldSatisfy` decodeErrorMentions "mirrorTokenDuration: invalid duration"

    describe "secret environment values (taken verbatim, never JSON-coerced)" $ do
        it "round-trips a JSON-looking authToken exactly" $
            for_ jsonLookingSecrets $ \payload ->
                case loadConfig (pubUrlEnv <> [("ECLUSE_SERVER__AUTH_TOKEN", payload)]) Nothing of
                    Left e -> expectationFailure ("unexpected decode error for " <> payload <> ": " <> show e)
                    Right doc ->
                        (unSecret <$> srvAuthToken (cfgServer (configApp doc)))
                            `shouldBe` Just (T.pack payload)

        it "loads JSON-looking mirror and publication tokens" $
            for_ jsonLookingSecrets $ \payload ->
                case loadConfig
                    ( pubUrlEnv
                        <> [ ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM", "https://private.example.test")
                           , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET", "https://mirror.example.test")
                           , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN", payload)
                           , ("ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET", "https://publish.example.test")
                           , ("ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET_TOKEN", payload)
                           ]
                    )
                    Nothing of
                    Left e -> expectationFailure ("unexpected decode error for " <> payload <> ": " <> show e)
                    Right doc -> Map.keys (configMounts doc) `shouldBe` [Npm]

-- server.publicUrl is required once a mount is active. This list supplies it, so each
-- decode example stays about its own concern.
pubUrlEnv :: [(String, String)]
pubUrlEnv = [("ECLUSE_SERVER__PUBLIC_URL", "https://registry.example.test")]

{- Each publicationAllow entry the loader must agree with the npm route about: the leading sigil is
optional, and anything that is not one usable path component is refused. -}
scopeEntryVerdicts :: [(Text, Bool)]
scopeEntryVerdicts =
    [ ("@scope", True)
    , ("scope", True)
    , ("@", False)
    , ("sc/ope", False)
    , ("sc@ope", False)
    , ("..", False)
    ]

-- Load a config whose npm mount allows exactly the given publicationAllow entry.
loadPublicationAllow :: Text -> Either [ConfigError] Config
loadPublicationAllow entry =
    loadConfig
        ( pubUrlEnv
            <> [ ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM", "https://private.example.test")
               , ("ECLUSE_MOUNTS__NPM__PUBLICATION_ALLOW", toString entry)
               ]
        )
        Nothing

-- Values the env layer would JSON-coerce into non-strings if secrets took the
-- ordinary parse path.
jsonLookingSecrets :: [String]
jsonLookingSecrets = ["12345", "true", "null"]

singleMountDoc :: ByteString
singleMountDoc =
    "{\"mounts\":{\"npm\":{\
    \\"privateUpstream\":\"https://private.example.test\",\
    \\"publicUpstream\":\"https://registry.npmjs.org\",\
    \\"mirrorTarget\":\"https://mirror.example.test\",\"mirrorTargetToken\":\"token\"}},\
    \\"rules\":{\"min-age\":{\"ageSeconds\":1209600}}}"

mountDocForEcosystem :: Text -> ByteString
mountDocForEcosystem eco =
    encodeUtf8 $
        "{\"mounts\":{\""
            <> eco
            <> "\":{\"privateUpstream\":\"https://a\",\"publicUpstream\":\"https://b\",\
               \\"mirrorTarget\":\"https://c\",\"mirrorTargetToken\":\"token\"}}}"

mountDocWithMirrorTarget :: Text -> ByteString
mountDocWithMirrorTarget target =
    encodeUtf8 $
        "{\"mounts\":{\"npm\":{\"privateUpstream\":\"https://a\",\"publicUpstream\":\"https://b\",\
        \\"mirrorTarget\":\""
            <> target
            <> "\"}}}"

mountDocWithExtraKey :: Text -> ByteString
mountDocWithExtraKey extra =
    encodeUtf8 $
        "{\"mounts\":{\"npm\":{\"privateUpstream\":\"https://a\",\"publicUpstream\":\"https://b\",\
        \\"mirrorTarget\":\"https://c\",\""
            <> extra
            <> "\":\"x\"}}}"

decodeErrorMentions :: Text -> Either [ConfigError] a -> Bool
decodeErrorMentions phrase (Left errs) = any (\err -> phrase `T.isInfixOf` renderConfigError err) errs
decodeErrorMentions _ (Right _) = False

{- The resolved log level. This helper flattens the error side to text, so each assertion
compares values instead of splitting on a case. -}
loadedLogLevel :: [(String, String)] -> Maybe ByteString -> Either Text LogLevel
loadedLogLevel envVars doc =
    bimap
        (T.unlines . map renderConfigError)
        (obsLogLevel . cfgObservability . configApp)
        (loadConfig envVars doc)

{- The resolved queue.url as its value and the backend the load derived from it, flattened the same
way. The type is abstract, so the assertion projects its two selectors rather than rebuilding it. -}
loadedQueueUrl :: [(String, String)] -> Maybe ByteString -> Either Text (Maybe (Text, Maybe QueueTarget))
loadedQueueUrl envVars doc =
    bimap
        (T.unlines . map renderConfigError)
        (fmap (\u -> (queueUrlText u, queueUrlTarget u)) . qsUrl . cfgQueue . configApp)
        (loadConfig envVars doc)
