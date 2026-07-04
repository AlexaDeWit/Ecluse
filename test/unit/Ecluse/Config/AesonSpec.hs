{-# LANGUAGE OverloadedStrings #-}

module Ecluse.Config.AesonSpec (spec) where

import Data.Text qualified as T

import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Config (AppConfig (..), Config (..), ConfigError, loadConfig, renderConfigError)
import Ecluse.Core.Ecosystem (Ecosystem (..))

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
