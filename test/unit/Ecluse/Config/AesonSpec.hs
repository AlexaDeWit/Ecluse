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

    it "rejects an unknown mount ecosystem key, naming it (strict, not silently dropped)" $
        loadConfig [] (Just (mountDocForEcosystem "npmm")) `shouldSatisfy` decodeErrorMentions "npmm"

    it "rejects an unknown key inside a mount, naming it" $
        loadConfig [] (Just (mountDocWithExtraKey "baseURL")) `shouldSatisfy` decodeErrorMentions "baseURL"

    it "keeps the shipped template mounts dormant when the overlay never mentions them" $
        case loadConfig [] Nothing of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> configMounts doc `shouldBe` mempty

    it "activates a mount from the environment layer alone (the one-variable launch)" $
        case loadConfig [("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM", "https://private.example.test")] Nothing of
            Left e -> expectationFailure ("unexpected decode error: " <> show e)
            Right doc -> Map.keys (configMounts doc) `shouldBe` [Npm]

    it "fails loudly when a document-declared mount omits its private upstream" $
        loadConfig [] (Just "{\"mounts\":{\"npm\":{}}}")
            `shouldSatisfy` decodeErrorMentions "mounts.npm.privateUpstream"

    it "fails loudly when an environment key activates a mount without an upstream" $
        loadConfig [("ECLUSE_MOUNTS__PYPI__CREDENTIAL_PROVIDER", "static")] Nothing
            `shouldSatisfy` decodeErrorMentions "ECLUSE_MOUNTS__PYPI__PRIVATE_UPSTREAM"

    it "reports every incomplete active mount in one load, not only the first" $ do
        let outcome = loadConfig [] (Just "{\"mounts\":{\"npm\":{},\"pypi\":{}}}")
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

singleMountDoc :: ByteString
singleMountDoc =
    "{\"queueBackend\":\"sqs\",\"mounts\":{\"npm\":{\
    \\"privateUpstream\":\"https://private.example.test\",\
    \\"publicUpstream\":\"https://registry.npmjs.org\",\
    \\"respectUpstreamTarballHost\":false,\
    \\"mirrorTarget\":\"https://mirror.example.test\",\"credentialProvider\":\"codeartifact\"}},\
    \\"rules\":{\"min-age\":{\"ageSeconds\":1209600}}}"

mountDocForEcosystem :: Text -> ByteString
mountDocForEcosystem eco =
    encodeUtf8 $
        "{\"mounts\":{\""
            <> eco
            <> "\":{\"privateUpstream\":\"https://a\",\"publicUpstream\":\"https://b\",\"respectUpstreamTarballHost\":false,\
               \\"mirrorTarget\":\"https://c\",\"credentialProvider\":\"static\"}}}"

mountDocWithExtraKey :: Text -> ByteString
mountDocWithExtraKey extra =
    encodeUtf8 $
        "{\"mounts\":{\"npm\":{\"privateUpstream\":\"https://a\",\"publicUpstream\":\"https://b\",\"respectUpstreamTarballHost\":false,\
        \\"mirrorTarget\":\"https://c\",\"credentialProvider\":\"static\",\""
            <> extra
            <> "\":\"x\"}}}"

decodeErrorMentions :: Text -> Either [ConfigError] a -> Bool
decodeErrorMentions phrase (Left errs) = any (\err -> phrase `T.isInfixOf` renderConfigError err) errs
decodeErrorMentions _ (Right _) = False
