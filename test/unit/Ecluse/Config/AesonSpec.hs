-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Config.AesonSpec (spec) where

import Data.Text qualified as T

import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Config (AppConfig (..), Config (..), ConfigError, loadConfig, renderConfigError)
import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Package.Merge (DivergencePolicy (FailClosed, Warn))

spec :: Spec
spec = describe "decodeDocument" $ do
    it "decodes a document with one mount and a rule patch" $
        case loadConfig [] (Just singleMountDoc) of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> Map.keys (configMounts doc) `shouldBe` [Npm]

    it "decodes a document carrying only a rule policy (no mounts)" $
        case loadConfig [] (Just "{\"rules\":{\"min-age\":{\"ageSeconds\":1209600}}}") of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> configMounts doc `shouldBe` mempty

    it "keys a mount by its ecosystem name, deriving the prefix from it" $
        case loadConfig [] (Just (mountDocForEcosystem "npm")) of
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
            [ ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM", "https://private.example.test")
            , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET", "https://mirror.example.test")
            , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN", "t")
            ]
            Nothing of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> Map.keys (configMounts doc) `shouldBe` [Npm]

    it "resolves a mount declared with no endpoint keys as the serve-only pure gate" $
        -- Mirroring is derived from the declared target: no mirrorTarget means
        -- serve-only, and with no private upstream either, the mount fronts only
        -- the template public upstream (the pure public gate).
        case loadConfig [] (Just "{\"mounts\":{\"npm\":{}}}") of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> Map.keys (configMounts doc) `shouldBe` [Npm]

    it "resolves a mount declaring only a private upstream as serve-only over the merge" $
        case loadConfig [] (Just "{\"mounts\":{\"npm\":{\"privateUpstream\":\"https://private.example.test\"}}}") of
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
            []
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
                cfgServeMaxInFlight (configApp doc) `shouldBe` Nothing
                -- publicConnectionsPerHost is unset by default too: the effective
                -- pool is computed at boot from the file-descriptor limit, like
                -- the private pool.
                cfgPublicConnectionsPerHost (configApp doc) `shouldBe` Nothing

    it "rejects a zero cveDbPollInterval (a zero delay would spin the poll)" $
        loadConfig [] (Just "{\"cveDbPollInterval\":0}")
            `shouldSatisfy` decodeErrorMentions "cveDbPollInterval"

    it "rejects a zero cveDbPollInterval given through the environment" $
        loadConfig [("ECLUSE_CVE_DB_POLL_INTERVAL", "0")] Nothing
            `shouldSatisfy` decodeErrorMentions "cveDbPollInterval"

    it "rejects a cveDbPollInterval whose microsecond conversion would overflow Int" $
        loadConfig [] (Just "{\"cveDbPollInterval\":9223372036855}")
            `shouldSatisfy` decodeErrorMentions "cveDbPollInterval"

    it "rejects a zero cveSyncInterval (a zero delay would spin the export loop)" $
        loadConfig [] (Just "{\"cveSyncInterval\":0}")
            `shouldSatisfy` decodeErrorMentions "cveSyncInterval"

    it "rejects a zero cveSyncInterval given through the environment" $
        loadConfig [("ECLUSE_CVE_SYNC_INTERVAL", "0")] Nothing
            `shouldSatisfy` decodeErrorMentions "cveSyncInterval"

    it "rejects a cveSyncInterval whose microsecond conversion would overflow Int" $
        loadConfig [] (Just "{\"cveSyncInterval\":9223372036855}")
            `shouldSatisfy` decodeErrorMentions "cveSyncInterval"

    it "rejects a non-positive maxOsvDbBytes" $
        loadConfig [] (Just "{\"maxOsvDbBytes\":0}")
            `shouldSatisfy` decodeErrorMentions "maxOsvDbBytes"

    it "loads the shipped advisory-sync defaults (poll interval, byte cap, no bucket)" $
        case loadConfig [] Nothing of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> do
                cfgCveDbPollInterval (configApp doc) `shouldBe` 60
                cfgMaxOsvDbBytes (configApp doc) `shouldBe` 536870912
                cfgVulnerabilityDatabaseBucket (configApp doc) `shouldBe` Nothing

    it "defaults the divergence policy to warn" $
        case loadConfig [] Nothing of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> cfgDivergencePolicy (configApp doc) `shouldBe` Warn

    it "parses ECLUSE_DIVERGENCE_POLICY=fail-closed from the environment" $
        case loadConfig [("ECLUSE_DIVERGENCE_POLICY", "fail-closed")] Nothing of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> cfgDivergencePolicy (configApp doc) `shouldBe` FailClosed

    it "rejects an unknown ECLUSE_DIVERGENCE_POLICY value, naming the field" $
        loadConfig [("ECLUSE_DIVERGENCE_POLICY", "drop")] Nothing
            `shouldSatisfy` decodeErrorMentions "divergencePolicy"

    it "leaves the runtime posture unset when cores and maxHeapBytes are omitted" $ do
        case loadConfig [] Nothing of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> do
                cfgCores (configApp doc) `shouldBe` Nothing
                cfgMaxHeapBytes (configApp doc) `shouldBe` Nothing

    it "parses cores and maxHeapBytes from the environment layer" $ do
        case loadConfig [("ECLUSE_CORES", "2"), ("ECLUSE_MAX_HEAP_BYTES", "419430400")] Nothing of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> do
                cfgCores (configApp doc) `shouldBe` Just 2
                cfgMaxHeapBytes (configApp doc) `shouldBe` Just 419430400

    it "rejects non-positive cores and maxHeapBytes" $ do
        loadConfig [("ECLUSE_CORES", "0")] Nothing
            `shouldSatisfy` decodeErrorMentions "cores must be a positive integer"
        loadConfig [("ECLUSE_MAX_HEAP_BYTES", "-1")] Nothing
            `shouldSatisfy` decodeErrorMentions "maxHeapBytes must be a positive integer"

    it "parses an explicit serveMaxInFlight override" $ do
        case loadConfig [("ECLUSE_SERVE_MAX_IN_FLIGHT", "24")] Nothing of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> cfgServeMaxInFlight (configApp doc) `shouldBe` Just 24

    it "parses an explicit privateConnectionsPerHost override" $ do
        -- The private pool defaults to a value computed from the file-descriptor limit,
        -- independent of the admission capacity (it streams outside admission); an
        -- operator who knows their fan-out can still pin it.
        case loadConfig [("ECLUSE_PRIVATE_CONNECTIONS_PER_HOST", "256")] Nothing of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> cfgPrivateConnectionsPerHost (configApp doc) `shouldBe` Just 256

    it "leaves privateConnectionsPerHost unset when not configured (computed at boot)" $
        case loadConfig [] Nothing of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> cfgPrivateConnectionsPerHost (configApp doc) `shouldBe` Nothing

    it "rejects non-positive serve and connection capacities" $ do
        loadConfig [("ECLUSE_SERVE_MAX_IN_FLIGHT", "0")] Nothing
            `shouldSatisfy` decodeErrorMentions "serveMaxInFlight must be a positive integer"
        loadConfig [("ECLUSE_PUBLIC_CONNECTIONS_PER_HOST", "0")] Nothing
            `shouldSatisfy` decodeErrorMentions "publicConnectionsPerHost must be a positive integer"
        loadConfig [("ECLUSE_PRIVATE_CONNECTIONS_PER_HOST", "0")] Nothing
            `shouldSatisfy` decodeErrorMentions "privateConnectionsPerHost must be a positive integer"

    it "leaves additionalBlockedRanges empty by default" $
        case loadConfig [] Nothing of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> cfgAdditionalBlockedRanges (configApp doc) `shouldBe` []

    it "parses a comma-separated additionalBlockedRanges from the environment layer" $
        case loadConfig [("ECLUSE_ADDITIONAL_BLOCKED_RANGES", "203.0.113.0/24,2001:db8::/32")] Nothing of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> cfgAdditionalBlockedRanges (configApp doc) `shouldBe` ["203.0.113.0/24", "2001:db8::/32"]

    it "trims whitespace around each additionalBlockedRanges entry" $
        case loadConfig [("ECLUSE_ADDITIONAL_BLOCKED_RANGES", " 203.0.113.0/24 , 2001:db8::/32 ")] Nothing of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> cfgAdditionalBlockedRanges (configApp doc) `shouldBe` ["203.0.113.0/24", "2001:db8::/32"]

    it "rejects a malformed entry in additionalBlockedRanges, naming it (fails closed at boot)" $
        loadConfig [("ECLUSE_ADDITIONAL_BLOCKED_RANGES", "not-a-range")] Nothing
            `shouldSatisfy` decodeErrorMentions "invalid CIDR range"

    describe "registry URL entries (the egress gate authorises each entry's host:port pair)" $ do
        it "accepts an upstream URL with an explicit port" $
            case loadConfig
                [ ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM", "https://repo.internal.example.test:8443/npm")
                , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET", "https://mirror.example.test")
                , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN", "t")
                ]
                Nothing of
                Left e -> expectationFailure ("unexpected decode error: " <> show e)
                Right doc -> Map.keys (configMounts doc) `shouldBe` [Npm]
        it "accepts an upstream URL with a bracketed IPv6 host and a port" $
            case loadConfig
                [ ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM", "https://[2001:db8::10]:8443/npm")
                , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET", "https://mirror.example.test")
                , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN", "t")
                ]
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

singleMountDoc :: ByteString
singleMountDoc =
    "{\"queueBackend\":\"sqs\",\"mounts\":{\"npm\":{\
    \\"privateUpstream\":\"https://private.example.test\",\
    \\"publicUpstream\":\"https://registry.npmjs.org\",\
    \\"respectUpstreamTarballHost\":false,\
    \\"mirrorTarget\":\"https://mirror.example.test\",\"mirrorTargetToken\":\"token\"}},\
    \\"rules\":{\"min-age\":{\"ageSeconds\":1209600}}}"

mountDocForEcosystem :: Text -> ByteString
mountDocForEcosystem eco =
    encodeUtf8 $
        "{\"mounts\":{\""
            <> eco
            <> "\":{\"privateUpstream\":\"https://a\",\"publicUpstream\":\"https://b\",\"respectUpstreamTarballHost\":false,\
               \\"mirrorTarget\":\"https://c\",\"mirrorTargetToken\":\"token\"}}}"

mountDocWithMirrorTarget :: Text -> ByteString
mountDocWithMirrorTarget target =
    encodeUtf8 $
        "{\"mounts\":{\"npm\":{\"privateUpstream\":\"https://a\",\"publicUpstream\":\"https://b\",\"respectUpstreamTarballHost\":false,\
        \\"mirrorTarget\":\""
            <> target
            <> "\"}}}"

mountDocWithExtraKey :: Text -> ByteString
mountDocWithExtraKey extra =
    encodeUtf8 $
        "{\"mounts\":{\"npm\":{\"privateUpstream\":\"https://a\",\"publicUpstream\":\"https://b\",\"respectUpstreamTarballHost\":false,\
        \\"mirrorTarget\":\"https://c\",\""
            <> extra
            <> "\":\"x\"}}}"

decodeErrorMentions :: Text -> Either [ConfigError] a -> Bool
decodeErrorMentions phrase (Left errs) = any (\err -> phrase `T.isInfixOf` renderConfigError err) errs
decodeErrorMentions _ (Right _) = False
