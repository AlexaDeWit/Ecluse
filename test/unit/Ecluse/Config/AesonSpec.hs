-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Config.AesonSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (NominalDiffTime)
import Test.Hspec

import Ecluse.Composition.Support (codeArtifactMirrorUrl, completeMountDoc, expectAppConfig, expectConfig, npmMountDoc, pubUrlEnv)
import Ecluse.Config (
    AdvisoriesSettings (..),
    AppConfig (..),
    CacheSettings (..),
    Config (..),
    ConfigError,
    EgressSettings (..),
    FirstParty (FirstPartyNpmScopes, FirstPartyPyPI),
    LimitsSettings (..),
    MountConfig (mntFirstParty),
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
import Ecluse.Core.Package (mkScope)
import Ecluse.Core.Registry.PyPI.FirstParty (PyPIFirstParty (PyPIOwnedName, PyPIOwnedPrefix), mkPyPIPrefix)
import Ecluse.Runtime.Log (LogLevel (DebugLevel, ErrorLevel, InfoLevel, WarnLevel))

import Ecluse.Test.Package (unscopedPyPI)
import Ecluse.Test.Registry.PyPI (pypiEntryVerdicts)

spec :: Spec
spec = describe "decodeDocument" $ do
    it "decodes a document with one mount and a rule patch" $
        mountKeysOf pubUrlEnv (Just singleMountDoc) `shouldReturn` [Npm]

    it "decodes a document carrying only a rule policy (no mounts)" $
        mountKeysOf [] (Just "{\"rules\":{\"min-age\":{\"ageSeconds\":1209600}}}") `shouldReturn` []

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
        loadConfig [] (Just (completeMountDoc "npmm")) `shouldSatisfy` decodeErrorMentions "npmm"

    it "rejects an unknown key inside a mount, naming it" $
        loadConfig [] (Just (mountDocWithExtraKey "baseURL")) `shouldSatisfy` decodeErrorMentions "baseURL"

    it "keeps the shipped template mounts dormant when the overlay never mentions them" $
        mountKeysOf [] Nothing `shouldReturn` []

    it "activates a mount from the environment layer alone" $
        mountKeysOf
            ( pubUrlEnv
                <> [ ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__REGISTRY__URL", "https://private.example.test")
                   , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET__REGISTRY__URL", "https://mirror.example.test")
                   , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET__REGISTRY__TOKEN", "t")
                   ]
            )
            Nothing
            `shouldReturn` [Npm]

    it "resolves a mount declared with no endpoint keys as the serve-only pure gate" $
        -- Mirroring is derived from the declared target, so a mount with no endpoint keys fronts
        -- only the template public upstream.
        mountKeysOf pubUrlEnv (Just "{\"mounts\":{\"npm\":{}}}") `shouldReturn` [Npm]

    it "fails loudly when a mirrored mount (mirrorTarget declared) omits its private upstream" $
        -- The mirror must be readable back through the private leg, so a mirrored
        -- mount without one is refused. Only serve-only mounts may omit it.
        loadConfig
            []
            (Just (npmMountDoc ["\"mirrorTarget\":{\"registry\":{\"url\":\"https://mirror.example.test\",\"token\":\"t\"}}"]))
            `shouldSatisfy` decodeErrorMentions "mounts.npm.privateUpstream"

    it "loads a mount whose mirror target is declared equal to its private upstream" $
        -- Equality with the private upstream is a valid arrangement. Only the
        -- declaration itself is mandatory.
        mountKeysOf
            pubUrlEnv
            ( Just
                ( npmMountDoc
                    [ "\"privateUpstream\":{\"registry\":{\"url\":\"https://one.example.test\"}}"
                    , "\"mirrorTarget\":{\"registry\":{\"url\":\"https://one.example.test\",\"token\":\"t\"}}"
                    ]
                )
            )
            `shouldReturn` [Npm]

    it "fails loudly when an environment token declares a mirror target with no URL" $
        -- The write token lives under the target's tag, so a token alone declares the
        -- target and the missing url refuses, naming the key path.
        loadConfig [("ECLUSE_MOUNTS__PYPI__MIRROR_TARGET__REGISTRY__TOKEN", "t")] Nothing
            `shouldSatisfy` decodeErrorMentions "mirrorTarget.registry.url is required"

    it "rejects a malformed firstParty entry (a wrong separator folds into one dead scope), naming firstParty" $
        -- A stray separator would otherwise fold into a single unmatchable scope that
        -- passes the non-empty boot check, refusing every publish only at request time.
        loadConfig [("ECLUSE_MOUNTS__NPM__FIRST_PARTY", "@acme;@beta")] Nothing
            `shouldSatisfy` decodeErrorMentions "invalid scope in firstParty"

    it "rejects a firstParty with an empty segment from a stray comma, naming firstParty" $
        loadConfig [("ECLUSE_MOUNTS__NPM__FIRST_PARTY", "@acme,,@beta")] Nothing
            `shouldSatisfy` decodeErrorMentions "invalid scope in firstParty"

    it "rejects an empty firstParty at load, naming firstParty" $
        -- A configured list that admits nothing would refuse every publish, so the load
        -- refuses it rather than binding a privilege that covers nothing.
        loadConfig pubUrlEnv (Just "{\"mounts\":{\"npm\":{\"firstParty\":\"\"}}}")
            `shouldSatisfy` decodeErrorMentions "firstParty must name at least one scope"

    it "rejects firstParty on a mount whose ecosystem has no namespace shape yet" $
        -- Without the per-ecosystem arm the entries would parse as another ecosystem's, so the
        -- key refuses at load and names the ecosystem it is unsupported for.
        loadConfig pubUrlEnv (Just "{\"mounts\":{\"rubygems\":{\"firstParty\":\"acme\"}}}")
            `shouldSatisfy` decodeErrorMentions "firstParty is not supported for rubygems yet"

    it "rejects an empty PyPI firstParty at load, naming firstParty" $
        loadConfig pubUrlEnv (Just "{\"mounts\":{\"pypi\":{\"firstParty\":\"\"}}}")
            `shouldSatisfy` decodeErrorMentions "firstParty must name at least one distribution or prefix"

    it "reads a PyPI firstParty into canonical names and prefixes, in the order written" $
        -- The loaded value is what every consumer of the privilege derives from, so its shape is
        -- pinned here rather than inferred from a refusal.
        case (loadPyPIFirstParty "Acme_Tools, widgets-*", mkPyPIPrefix "widgets") of
            (Left e, _) -> expectationFailure ("unexpected decode error: " <> show e)
            (_, Nothing) -> expectationFailure "widgets is a valid prefix"
            (Right doc, Just prefix) ->
                (mntFirstParty <$> Map.lookup PyPI (cfgMounts (configApp doc)))
                    `shouldBe` Just
                        ( Just
                            ( FirstPartyPyPI
                                (PyPIOwnedName (unscopedPyPI "Acme_Tools") :| [PyPIOwnedPrefix prefix])
                            )
                        )

    describe "a PyPI firstParty entry reads through PEP 503's name grammar" $
        -- An entry no distribution name can equal privileges nothing, so it fails the load
        -- rather than binding at request time. A bare @*@ would privilege every name on PyPI.
        for_ pypiEntryVerdicts $ \(entry, valid) ->
            it (show entry <> (if valid then " is an entry" else " is refused")) $
                if valid
                    then loadPyPIFirstParty entry `shouldSatisfy` isRight
                    else loadPyPIFirstParty entry `shouldSatisfy` decodeErrorMentions "invalid entry in firstParty"

    describe "a firstParty entry reads through npm's own scope grammar" $
        -- The declaration and the request path must not disagree about what a scope is, so the
        -- entry goes through the same splitter the route and the projection use.
        for_ scopeEntryVerdicts $ \(entry, valid) ->
            it (show entry <> (if valid then " is a scope" else " is refused")) $
                if valid
                    then loadFirstParty entry `shouldSatisfy` isRight
                    else loadFirstParty entry `shouldSatisfy` decodeErrorMentions "invalid scope in firstParty"

    it "accepts a well-formed comma-separated firstParty (trimmed, leading sigil tolerated)" $ do
        -- The resolved value is what every consumer of the privilege derives from, so the
        -- trimming and the optional sigil are read back off it rather than off a bare load.
        config <-
            expectConfig
                ( pubUrlEnv
                    <> [ ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__REGISTRY__URL", "https://private.example.test")
                       , ("ECLUSE_MOUNTS__NPM__FIRST_PARTY", "@acme, beta")
                       ]
                )
                Nothing
        (mntFirstParty <$> Map.lookup Npm (cfgMounts (configApp config)))
            `shouldBe` Just (Just (FirstPartyNpmScopes (mkScope "acme" :| [mkScope "beta"])))

    it "reports every incomplete mirrored mount in one load, not only the first" $ do
        let doc =
                "{\"mounts\":{\"npm\":{\"mirrorTarget\":{\"registry\":{\"url\":\"https://m1.example.test\",\"token\":\"t\"}}},\
                \\"pypi\":{\"mirrorTarget\":{\"registry\":{\"url\":\"https://m2.example.test\",\"token\":\"t\"}}}}}"
        let outcome = loadConfig [] (Just doc)
        outcome `shouldSatisfy` decodeErrorMentions "mounts.npm.privateUpstream"
        outcome `shouldSatisfy` decodeErrorMentions "mounts.pypi.privateUpstream"

    it "loads the bounded serve and connection-pool defaults" $ do
        -- Both are unset by default: the boot computes the effective serve capacity from the
        -- resolved capability count, and both pools from the file-descriptor limit.
        runtime <- runtimeOf [] Nothing
        rtServeMaxInFlight runtime `shouldBe` Nothing
        rtPublicConnectionsPerHost runtime `shouldBe` Nothing

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
        -- A raw truncate would materialise an astronomically large Integer at boot.
        -- The env overlay passes the Scientific value to the bounded parser.
        loadConfig [("ECLUSE_CACHE__TTL", "1e999999999999")] Nothing
            `shouldSatisfy` decodeErrorMentions "cache.ttl must be a non-negative integer count of seconds"

    it "rejects a huge-exponent advisories.pollInterval (the delay path is guarded too)" $
        loadConfig [("ECLUSE_ADVISORIES__POLL_INTERVAL", "1e999999999999")] Nothing
            `shouldSatisfy` decodeErrorMentions "advisories.pollInterval must be a non-negative integer count of seconds"

    it "accepts a zero and a positive integer cache.ttl (the pre-fix accepted forms, unchanged)" $ do
        loadedTtl [] (Just "{\"cache\":{\"ttl\":0}}") `shouldReturn` 0
        loadedTtl [("ECLUSE_CACHE__TTL", "120")] Nothing `shouldReturn` 120

    it "accepts a quoted integer cache.ttl and rejects a quoted fractional one (both branches agree)" $ do
        loadedTtl [] (Just "{\"cache\":{\"ttl\":\"120\"}}") `shouldReturn` 120
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

    it "loads the shipped advisory-sync defaults (poll interval, byte cap, data dir, no store)" $ do
        app <- expectAppConfig [] Nothing
        advPollInterval (cfgAdvisories app) `shouldBe` 60
        limMaxAdvisoryDatabaseBytes (cfgLimits app) `shouldBe` 536870912
        -- Absolute on purpose: the shipped image sets no working directory, so a
        -- relative path lands in a root the container's user cannot write.
        advDataDir (cfgAdvisories app) `shouldBe` "/var/lib/ecluse/advisories"
        -- No default store: the artifact is unsigned and a bucket name is global, so a
        -- shipped one would name a bucket this project does not own.
        advUrl (cfgAdvisories app) `shouldBe` Nothing

    it "refuses a blank ECLUSE_ADVISORIES__URL rather than reading it as the erased key" $
        loadConfig [("ECLUSE_ADVISORIES__URL", "")] Nothing
            `shouldSatisfy` decodeErrorMentions "advisories.url"

    it "takes an explicit null as an absent advisory store, so a layer can withdraw one" $
        advUrl <$> advisoriesOf [] (Just "{\"advisories\":{\"url\":null}}") `shouldReturn` Nothing

    describe "advisories.quietTime" $ do
        it "ships seven days for npm and PyPI, and for the EPSS feed" $ do
            advisories <- advisoriesOf [] Nothing
            advQuietTime advisories `shouldBe` Map.fromList [(Npm, 604800), (PyPI, 604800)]
            advEpssQuietTime advisories `shouldBe` 604800

        it "takes an operator's threshold for one ecosystem and leaves the others shipped" $
            advQuietTime <$> advisoriesOf [("ECLUSE_ADVISORIES__QUIET_TIME__NPM", "86400")] Nothing
                `shouldReturn` Map.fromList [(Npm, 86400), (PyPI, 604800)]

        it "refuses a zero threshold, which would alarm on every compile" $
            loadConfig [] (Just "{\"advisories\":{\"quietTime\":{\"npm\":0}}}")
                `shouldSatisfy` decodeErrorMentions "advisories.quietTime.npm"

        it "refuses a threshold that is not a count of seconds" $
            loadConfig [] (Just "{\"advisories\":{\"epssQuietTime\":true}}")
                `shouldSatisfy` decodeErrorMentions "advisories.epssQuietTime"

        it "refuses an ecosystem this build does not serve, rather than configuring nothing" $
            loadConfig [] (Just "{\"advisories\":{\"quietTime\":{\"cargo\":604800}}}")
                `shouldSatisfy` decodeErrorMentions "Invalid ecosystem in advisories.quietTime: cargo"

    describe "advisories.maxAgeSeconds" $ do
        it "is unset by default, so each mount derives its own maximum" $
            advMaxAgeSeconds <$> advisoriesOf [] Nothing `shouldReturn` Nothing

        it "takes an operator's explicit maximum" $
            advMaxAgeSeconds <$> advisoriesOf [("ECLUSE_ADVISORIES__MAX_AGE_SECONDS", "518400")] Nothing
                `shouldReturn` Just 518400

        it "refuses zero, which would expire every push at once" $
            loadConfig [] (Just "{\"advisories\":{\"maxAgeSeconds\":0}}")
                `shouldSatisfy` decodeErrorMentions "advisories.maxAgeSeconds"

        it "refuses a value that is not a count of seconds" $
            loadConfig [] (Just "{\"advisories\":{\"maxAgeSeconds\":\"six days\"}}")
                `shouldSatisfy` decodeErrorMentions "advisories.maxAgeSeconds"

    describe "deprecated divergence configuration" $ do
        for_ ["ECLUSE_INTEGRITY__DIVERGENCE_POLICY", "ECLUSE_MOUNTS__NPM__INTEGRITY__DIVERGENCE_POLICY"] $ \key -> do
            for_ ["warn", " WARN "] $ \value ->
                it ("accepts the old alarm-only value at " <> key <> ": " <> value) $
                    loadConfig (pubUrlEnv <> [(key, value)]) Nothing `shouldSatisfy` isRight
            for_ ["fail-closed", "FAIL_CLOSED", "  FailClosed  "] $ \value ->
                it ("refuses the removed value at " <> key <> ": " <> value) $
                    loadConfig (pubUrlEnv <> [(key, value)]) Nothing
                        `shouldSatisfy` decodeErrorMentions "Remove this setting after accepting private preference"
            it ("rejects an unknown value at " <> key) $
                loadConfig (pubUrlEnv <> [(key, "drop")]) Nothing
                    `shouldSatisfy` decodeErrorMentions "divergencePolicy"
        for_ ["{\"integrity\":{\"divergencePolicy\":\"fail-closed\"}}", "{\"mounts\":{\"npm\":{\"integrity\":{\"divergencePolicy\":\"fail-closed\"}}}}"] $ \document ->
            it ("refuses a removed value in a document: " <> show document) $
                loadConfig pubUrlEnv (Just document)
                    `shouldSatisfy` decodeErrorMentions "Remove this setting after accepting private preference"
        for_ ["{\"integrity\":{\"divergencePolicy\":true}}", "{\"mounts\":{\"npm\":{\"integrity\":{\"divergencePolicy\":5}}}}"] $ \document ->
            it ("rejects a non-string legacy value: " <> show document) $
                loadConfig pubUrlEnv (Just document) `shouldSatisfy` decodeErrorMentions "expected a string"
        it "accepts absent legacy keys" $
            loadConfig [] Nothing `shouldSatisfy` isRight
        for_ ["{\"integrity\":{\"divergencePolicy\":null}}", "{\"mounts\":{\"npm\":{\"integrity\":{\"divergencePolicy\":null}}}}"] $ \document ->
            it ("treats a null legacy key as absent: " <> show document) $
                loadConfig pubUrlEnv (Just document) `shouldSatisfy` isRight

    it "leaves the runtime posture unset when the shipped defaults are all that apply" $ do
        -- Every runtime key unset: the boot resolves cores down its ladder, and the ladder's
        -- last rung takes its own default ceiling rather than one this layer supplies.
        runtime <- runtimeOf [] Nothing
        rtCores runtime `shouldBe` Nothing
        rtCoresCeiling runtime `shouldBe` Nothing
        rtMaxHeapBytes runtime `shouldBe` Nothing

    it "parses cores, coresCeiling, and maxHeapBytes from the environment layer" $ do
        runtime <- runtimeOf [("ECLUSE_RUNTIME__CORES", "2"), ("ECLUSE_RUNTIME__CORES_CEILING", "16"), ("ECLUSE_RUNTIME__MAX_HEAP_BYTES", "419430400")] Nothing
        rtCores runtime `shouldBe` Just 2
        rtCoresCeiling runtime `shouldBe` Just 16
        rtMaxHeapBytes runtime `shouldBe` Just 419430400

    it "rejects non-positive cores, coresCeiling, and maxHeapBytes" $ do
        loadConfig [("ECLUSE_RUNTIME__CORES", "0")] Nothing
            `shouldSatisfy` decodeErrorMentions "cores must be a positive integer"
        loadConfig [("ECLUSE_RUNTIME__CORES_CEILING", "0")] Nothing
            `shouldSatisfy` decodeErrorMentions "coresCeiling must be a positive integer"
        loadConfig [("ECLUSE_RUNTIME__CORES_CEILING", "-4")] Nothing
            `shouldSatisfy` decodeErrorMentions "coresCeiling must be a positive integer"
        loadConfig [("ECLUSE_RUNTIME__MAX_HEAP_BYTES", "-1")] Nothing
            `shouldSatisfy` decodeErrorMentions "maxHeapBytes must be a positive integer"

    it "parses an explicit serveMaxInFlight override" $
        rtServeMaxInFlight <$> runtimeOf [("ECLUSE_RUNTIME__SERVE_MAX_IN_FLIGHT", "24")] Nothing `shouldReturn` Just 24

    it "parses an explicit privateConnectionsPerHost override" $
        -- The private pool default is computed from the file-descriptor limit, independent of the
        -- admission capacity because it streams outside admission. An operator can still pin it.
        rtPrivateConnectionsPerHost <$> runtimeOf [("ECLUSE_RUNTIME__PRIVATE_CONNECTIONS_PER_HOST", "256")] Nothing
            `shouldReturn` Just 256

    it "leaves privateConnectionsPerHost unset when not configured (computed at boot)" $
        rtPrivateConnectionsPerHost <$> runtimeOf [] Nothing `shouldReturn` Nothing

    it "rejects non-positive serve and connection capacities" $ do
        loadConfig [("ECLUSE_RUNTIME__SERVE_MAX_IN_FLIGHT", "0")] Nothing
            `shouldSatisfy` decodeErrorMentions "serveMaxInFlight must be a positive integer"
        loadConfig [("ECLUSE_RUNTIME__PUBLIC_CONNECTIONS_PER_HOST", "0")] Nothing
            `shouldSatisfy` decodeErrorMentions "publicConnectionsPerHost must be a positive integer"
        loadConfig [("ECLUSE_RUNTIME__PRIVATE_CONNECTIONS_PER_HOST", "0")] Nothing
            `shouldSatisfy` decodeErrorMentions "privateConnectionsPerHost must be a positive integer"

    it "leaves additionalBlockedRanges empty by default" $
        egrAdditionalBlockedRanges . cfgEgress <$> expectAppConfig [] Nothing `shouldReturn` []

    it "parses a comma-separated additionalBlockedRanges from the environment layer" $
        egrAdditionalBlockedRanges . cfgEgress <$> expectAppConfig [("ECLUSE_EGRESS__ADDITIONAL_BLOCKED_RANGES", "203.0.113.0/24,2001:db8::/32")] Nothing
            `shouldReturn` ["203.0.113.0/24", "2001:db8::/32"]

    it "trims whitespace around each additionalBlockedRanges entry" $
        egrAdditionalBlockedRanges . cfgEgress <$> expectAppConfig [("ECLUSE_EGRESS__ADDITIONAL_BLOCKED_RANGES", " 203.0.113.0/24 , 2001:db8::/32 ")] Nothing
            `shouldReturn` ["203.0.113.0/24", "2001:db8::/32"]

    it "rejects a malformed entry in additionalBlockedRanges, naming it (fails closed at boot)" $
        loadConfig [("ECLUSE_EGRESS__ADDITIONAL_BLOCKED_RANGES", "not-a-range")] Nothing
            `shouldSatisfy` decodeErrorMentions "invalid CIDR range"

    describe "registry URL entries (the egress gate authorises each entry's host:port pair)" $ do
        it "accepts an upstream URL with an explicit port" $
            mountKeysOf (mirroredFrom "https://repo.internal.example.test:8443/npm") Nothing `shouldReturn` [Npm]
        it "accepts an upstream URL with a bracketed IPv6 host and a port" $
            mountKeysOf (mirroredFrom "https://[2001:db8::10]:8443/npm") Nothing `shouldReturn` [Npm]
        it "rejects an upstream URL with a non-numeric port, naming the value (fails closed at boot)" $
            -- The gate refuses every fetch from an authority it cannot extract, so the
            -- misconfiguration surfaces at load, never as a mount that silently serves nothing.
            loadConfig [("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__REGISTRY__URL", "https://repo.internal.example.test:9x9/npm")] Nothing
                `shouldSatisfy` decodeErrorMentions "decimal port in 1..65535"
        it "rejects an upstream URL with an out-of-range port" $
            loadConfig [("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__REGISTRY__URL", "https://repo.internal.example.test:65536/npm")] Nothing
                `shouldSatisfy` decodeErrorMentions "decimal port in 1..65535"
        it "rejects an upstream URL with port 0" $
            loadConfig [("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__REGISTRY__URL", "https://repo.internal.example.test:0/npm")] Nothing
                `shouldSatisfy` decodeErrorMentions "decimal port in 1..65535"
        it "rejects a mirror-target URL with a garbage port through the document layer" $
            loadConfig [] (Just (mountDocWithMirrorTarget "https://mirror.example.test:port/npm"))
                `shouldSatisfy` decodeErrorMentions "decimal port in 1..65535"

        -- Boot prints configured registry URLs after a successful load.
        -- Reject credentials before that output, naming only the key.
        it "rejects an upstream URL carrying userinfo, naming the key and not the credential" $ do
            let outcome = loadConfig [("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__REGISTRY__URL", "https://deploy:hunter2@repo.internal.example.test/npm")] Nothing
            outcome `shouldSatisfy` decodeErrorMentions "privateUpstream.registry.url: registry URL must not carry userinfo"
            outcome `shouldSatisfy` (not . decodeErrorMentions "hunter2")

        it "rejects an upstream URL carrying a query string, naming the key" $
            loadConfig [("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__REGISTRY__URL", "https://repo.internal.example.test/npm?token=abc")] Nothing
                `shouldSatisfy` decodeErrorMentions "privateUpstream.registry.url: registry URL must not carry a query string"

        it "rejects an upstream URL carrying a fragment, naming the key" $
            loadConfig [("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__REGISTRY__URL", "https://repo.internal.example.test/npm#frag")] Nothing
                `shouldSatisfy` decodeErrorMentions "privateUpstream.registry.url: registry URL must not carry a fragment"

        -- A second endpoint key, so a key name hand-written into the wrong call site
        -- cannot pass by matching a neighbour's.
        it "rejects a mirror-target URL carrying userinfo, naming that key" $
            loadConfig [] (Just (mountDocWithMirrorTarget "https://deploy:hunter2@mirror.example.test/npm"))
                `shouldSatisfy` decodeErrorMentions "mirrorTarget.registry.url: registry URL must not carry userinfo"

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
            let exportBaseUrl env doc = unUrl . advOsvExportBaseUrl <$> advisoriesOf env doc
            exportBaseUrl [("ECLUSE_ADVISORIES__OSV_EXPORT_BASE_URL", "https://osv.example.test/exports")] Nothing
                `shouldReturn` "https://osv.example.test/exports"
            exportBaseUrl [] (Just "{\"advisories\":{\"osvExportBaseUrl\":\"http://localhost:8080/osv\"}}")
                `shouldReturn` "http://localhost:8080/osv"

        -- The cloud SDK receives queue.url without a parser scheme or authority check.
        -- Boot errors and check-config print it whole after a successful load.
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
            let port env doc = srvPort . cfgServer <$> expectAppConfig env doc
            port [] (Just "{\"server\":{\"port\":0}}") `shouldReturn` 0
            port [("ECLUSE_SERVER__PORT", "65535")] Nothing `shouldReturn` 65535

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
            void (expectAppConfig [("ECLUSE_SERVER__PUBLIC_URL", "http://localhost:8080")] Nothing)

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
                    npmMountDoc
                        [ "\"privateUpstream\":{\"registry\":{\"url\":\"https://a\"}}"
                        , codeArtifactDurationDoc (show n)
                        ]
            for_ [900, 43200] $ \seconds ->
                mountKeysOf pubUrlEnv (Just (docFor seconds)) `shouldReturn` [Npm]

        it "rejects a CodeArtifact token duration outside 900..43200, through both layers" $ do
            loadConfig
                []
                (Just (npmMountDoc [codeArtifactDurationDoc "899"]))
                `shouldSatisfy` decodeErrorMentions "mirrorTarget.codeArtifact.tokenDuration must be a duration in seconds within 900..43200"
            loadConfig
                [ ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET__CODE_ARTIFACT__URL", toString @Text codeArtifactMirrorUrl)
                , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET__CODE_ARTIFACT__TOKEN_DURATION", "43201")
                ]
                Nothing
                `shouldSatisfy` decodeErrorMentions "mirrorTarget.codeArtifact.tokenDuration must be a duration in seconds within 900..43200"

        it "rejects a quoted CodeArtifact token duration written as hex or padded" $
            for_ (["0x1000", " 3600", "(3600)"] :: [Text]) $ \spelling ->
                loadConfig
                    []
                    (Just (npmMountDoc [codeArtifactDurationDoc ("\"" <> spelling <> "\"")]))
                    `shouldSatisfy` decodeErrorMentions "mirrorTarget.codeArtifact.tokenDuration: invalid duration"

    describe "secret environment values (taken verbatim, never JSON-coerced)" $ do
        it "round-trips a JSON-looking authToken exactly" $
            for_ jsonLookingSecrets $ \payload -> do
                app <- expectAppConfig (pubUrlEnv <> [("ECLUSE_SERVER__AUTH_TOKEN", payload)]) Nothing
                (unSecret <$> srvAuthToken (cfgServer app)) `shouldBe` Just (T.pack payload)

        it "loads JSON-looking mirror and publication tokens" $
            for_ jsonLookingSecrets $ \payload ->
                mountKeysOf
                    ( pubUrlEnv
                        <> [ ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__REGISTRY__URL", "https://private.example.test")
                           , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET__REGISTRY__URL", "https://mirror.example.test")
                           , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET__REGISTRY__TOKEN", payload)
                           , ("ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET__REGISTRY__URL", "https://publish.example.test")
                           , ("ECLUSE_MOUNTS__NPM__PUBLICATION_TARGET__REGISTRY__TOKEN", payload)
                           ]
                    )
                    Nothing
                    `shouldReturn` [Npm]

{- Each firstParty entry the loader must agree with the npm route about: the leading sigil is
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

-- Load a config whose pypi mount declares exactly the given firstParty value.
loadPyPIFirstParty :: Text -> Either [ConfigError] Config
loadPyPIFirstParty entry =
    loadConfig (pubUrlEnv <> [("ECLUSE_MOUNTS__PYPI__FIRST_PARTY", toString entry)]) Nothing

-- Load a config whose npm mount allows exactly the given firstParty entry.
loadFirstParty :: Text -> Either [ConfigError] Config
loadFirstParty entry =
    loadConfig
        ( pubUrlEnv
            <> [ ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__REGISTRY__URL", "https://private.example.test")
               , ("ECLUSE_MOUNTS__NPM__FIRST_PARTY", toString entry)
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
    \\"privateUpstream\":{\"registry\":{\"url\":\"https://private.example.test\"}},\
    \\"publicUpstream\":{\"registry\":{\"url\":\"https://registry.npmjs.org\"}},\
    \\"mirrorTarget\":{\"registry\":{\"url\":\"https://mirror.example.test\",\"token\":\"token\"}}}},\
    \\"rules\":{\"min-age\":{\"ageSeconds\":1209600}}}"

-- A codeArtifact mirror target carrying the given token duration, written as JSON.
codeArtifactDurationDoc :: Text -> Text
codeArtifactDurationDoc duration =
    "\"mirrorTarget\":{\"codeArtifact\":{\"url\":\"" <> codeArtifactMirrorUrl <> "\",\"tokenDuration\":" <> duration <> "}}"

mountDocWithMirrorTarget :: Text -> ByteString
mountDocWithMirrorTarget target =
    npmMountDoc
        [ "\"privateUpstream\":{\"registry\":{\"url\":\"https://a\"}}"
        , "\"mirrorTarget\":{\"registry\":{\"url\":\"" <> target <> "\",\"token\":\"token\"}}"
        ]

mountDocWithExtraKey :: Text -> ByteString
mountDocWithExtraKey extra =
    npmMountDoc
        [ "\"privateUpstream\":{\"registry\":{\"url\":\"https://a\"}}"
        , "\"" <> extra <> "\":\"x\""
        ]

decodeErrorMentions :: Text -> Either [ConfigError] a -> Bool
decodeErrorMentions phrase (Left errs) = any (\err -> phrase `T.isInfixOf` renderConfigError err) errs
decodeErrorMentions _ (Right _) = False

-- The ecosystems a load resolved a mount for, which is what the activation cases read.
mountKeysOf :: [(String, String)] -> Maybe ByteString -> IO [Ecosystem]
mountKeysOf envVars doc = Map.keys . configMounts <$> expectConfig envVars doc

advisoriesOf :: [(String, String)] -> Maybe ByteString -> IO AdvisoriesSettings
advisoriesOf envVars doc = cfgAdvisories <$> expectAppConfig envVars doc

runtimeOf :: [(String, String)] -> Maybe ByteString -> IO RuntimeSettings
runtimeOf envVars doc = cfgRuntime <$> expectAppConfig envVars doc

loadedTtl :: [(String, String)] -> Maybe ByteString -> IO NominalDiffTime
loadedTtl envVars doc = csTtl . cfgCache <$> expectAppConfig envVars doc

-- A mirrored npm mount reading back through the given private upstream.
mirroredFrom :: String -> [(String, String)]
mirroredFrom privateUrl =
    pubUrlEnv
        <> [ ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM__REGISTRY__URL", privateUrl)
           , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET__REGISTRY__URL", "https://mirror.example.test")
           , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET__REGISTRY__TOKEN", "t")
           ]

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
