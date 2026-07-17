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
    Config (..),
    ConfigError,
    EgressSettings (..),
    IntegritySettings (..),
    RuntimeSettings (..),
    ServerSettings (..),
    loadConfig,
    renderConfigError,
 )
import Ecluse.Core.Credential (unSecret)
import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Package.Merge (DivergencePolicy (FailClosed, Warn))

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

    it "rejects an unknown top-level key, naming it (strict, not silently dropped)" $
        loadConfig [] (Just "{\"mountz\":{}}") `shouldSatisfy` decodeErrorMentions "mountz"

    it "rejects the ambient AWS SDK variables as document keys (environment, never config)" $ do
        -- A document-side awsSecretAccessKey used to be silently accepted-and-ignored;
        -- rejecting it keeps "secrets never live in the structured config" structural.
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
        -- Mirroring is derived from the declared target: no mirrorTarget means
        -- serve-only, and with no private upstream either, the mount fronts only
        -- the template public upstream (the pure public gate).
        case loadConfig pubUrlEnv (Just "{\"mounts\":{\"npm\":{}}}") of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> Map.keys (configMounts doc) `shouldBe` [Npm]

    it "resolves a mount declaring only a private upstream as serve-only over the merge" $
        case loadConfig pubUrlEnv (Just "{\"mounts\":{\"npm\":{\"privateUpstream\":\"https://private.example.test\"}}}") of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> Map.keys (configMounts doc) `shouldBe` [Npm]

    it "fails loudly when a mirrored mount (mirrorTarget declared) omits its private upstream" $
        -- The mirror must be readable back through the private leg, so a mirrored
        -- mount without one is refused; only serve-only mounts may omit it.
        loadConfig
            []
            (Just "{\"mounts\":{\"npm\":{\"mirrorTarget\":\"https://mirror.example.test\",\"mirrorTargetToken\":\"t\"}}}")
            `shouldSatisfy` decodeErrorMentions "mounts.npm.privateUpstream"

    it "loads a mount whose mirror target is declared equal to its private upstream" $
        -- Equality with the private upstream is a valid arrangement; only the
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
                -- serveMaxInFlight is unset by default: the effective capacity is
                -- computed at boot from the resolved capability count (issue #634).
                rtServeMaxInFlight (cfgRuntime (configApp doc)) `shouldBe` Nothing
                -- publicConnectionsPerHost is unset by default too: the effective
                -- pool is computed at boot from the file-descriptor limit, like
                -- the private pool.
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

    it "rejects a non-positive advisories.maxDatabaseBytes" $
        loadConfig [] (Just "{\"advisories\":{\"maxDatabaseBytes\":0}}")
            `shouldSatisfy` decodeErrorMentions "advisories.maxDatabaseBytes"

    it "loads the shipped advisory-sync defaults (poll interval, byte cap, no bucket)" $
        case loadConfig [] Nothing of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> do
                advPollInterval (cfgAdvisories (configApp doc)) `shouldBe` 60
                advMaxDatabaseBytes (cfgAdvisories (configApp doc)) `shouldBe` 536870912
                advBucket (cfgAdvisories (configApp doc)) `shouldBe` Nothing

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

    it "leaves the runtime posture unset when cores and maxHeapBytes are omitted" $ do
        case loadConfig [] Nothing of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> do
                rtCores (cfgRuntime (configApp doc)) `shouldBe` Nothing
                rtMaxHeapBytes (cfgRuntime (configApp doc)) `shouldBe` Nothing

    it "parses cores and maxHeapBytes from the environment layer" $ do
        case loadConfig [("ECLUSE_RUNTIME__CORES", "2"), ("ECLUSE_RUNTIME__MAX_HEAP_BYTES", "419430400")] Nothing of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> do
                rtCores (cfgRuntime (configApp doc)) `shouldBe` Just 2
                rtMaxHeapBytes (cfgRuntime (configApp doc)) `shouldBe` Just 419430400

    it "rejects non-positive cores and maxHeapBytes" $ do
        loadConfig [("ECLUSE_RUNTIME__CORES", "0")] Nothing
            `shouldSatisfy` decodeErrorMentions "cores must be a positive integer"
        loadConfig [("ECLUSE_RUNTIME__MAX_HEAP_BYTES", "-1")] Nothing
            `shouldSatisfy` decodeErrorMentions "maxHeapBytes must be a positive integer"

    it "parses an explicit serveMaxInFlight override" $ do
        case loadConfig [("ECLUSE_RUNTIME__SERVE_MAX_IN_FLIGHT", "24")] Nothing of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> rtServeMaxInFlight (cfgRuntime (configApp doc)) `shouldBe` Just 24

    it "parses an explicit privateConnectionsPerHost override" $ do
        -- The private pool defaults to a value computed from the file-descriptor limit,
        -- independent of the admission capacity (it streams outside admission); an
        -- operator who knows their fan-out can still pin it.
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
            -- The gate refuses every fetch from an authority it cannot extract, so
            -- the misconfiguration surfaces at load, never as a mount that
            -- silently serves nothing.
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
                        \\"mirrorCodeArtifactTokenDuration\":"
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
                (Just "{\"mounts\":{\"npm\":{\"mirrorCodeArtifactTokenDuration\":899}}}")
                `shouldSatisfy` decodeErrorMentions "mirrorCodeArtifactTokenDuration must be a duration in seconds within 900..43200"
            loadConfig [("ECLUSE_MOUNTS__NPM__MIRROR_CODE_ARTIFACT_TOKEN_DURATION", "43201")] Nothing
                `shouldSatisfy` decodeErrorMentions "mirrorCodeArtifactTokenDuration must be a duration in seconds within 900..43200"

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

-- server.publicUrl is required once a mount is active; supplied here so each
-- decode example stays about its own concern.
pubUrlEnv :: [(String, String)]
pubUrlEnv = [("ECLUSE_SERVER__PUBLIC_URL", "https://registry.example.test")]

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
