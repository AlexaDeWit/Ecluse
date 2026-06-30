module Ecluse.ConfigSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Test.Hspec

import Ecluse.Config

import Ecluse.Core.Ecosystem (Ecosystem (..))
import Ecluse.Core.Rules.Types (
    PrecededRule (..),
    Rule (..),
    defaultAllowIfOlderThanPrecedence,
    defaultDenyInstallTimeExecutionPrecedence,
 )
import Ecluse.Core.Security.Egress (registryUrlText)

{- | Tests for the configuration boundary. They exercise the three promises of the
loader: the environment layer aggregates all errors at once (present \/ absent \/
malformed), the JSON document decoders are strict and fail loud (unknown keys,
unknown \/ effectful rule types, secrets), and the rule-policy merge resolves
add \/ override \/ suppress over the default while rejecting every unresolvable
reference. Pure and offline.
-}
spec :: Spec
spec = do
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
        showText doc `shouldSatisfy` ("MountConfig" `isInfix`)

    it "shows the resolved config (env layer, mounts, mirror target)" $ do
        env <- expectEnv minimalEnv
        case loadConfig env Nothing of
            Left errs -> expectationFailure ("unexpected policy errors: " <> show errs)
            Right cfg -> do
                showText (configApp cfg) `shouldSatisfy` ("AppConfig" `isInfix`)
                showText cfg `shouldSatisfy` ("MirrorTarget" `isInfix`)

    it "shows a policy error and the default policy" $ do
        showText (UnknownRuleType "n" "T") `shouldSatisfy` ("UnknownRuleType" `isInfix`)
        showText defaultPolicy `shouldSatisfy` ("AllowIfOlderThan" `isInfix`)

    it "keys a decoded mount by its ecosystem (the mount map's key)" $ do
        doc <- expectDoc singleMountDoc
        Map.keys (configMounts doc) `shouldBe` [Npm]

-- ── environment layer ────────────────────────────────────────────────────────

{- | A complete, valid environment: the three required URLs plus a value for the
defaulted variables, so a test can drop or corrupt one axis at a time.
-}
fullEnv :: [(String, String)]
fullEnv =
    [ ("ECLUSE_PORT", "8080")
    , ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM", "https://private.example.test")
    , ("ECLUSE_MOUNTS__NPM__PUBLIC_UPSTREAM", "https://public.example.test")
    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET", "https://mirror.example.test")
    , ("ECLUSE_QUEUE_BACKEND", "pubsub")
    , ("ECLUSE_QUEUE_URL", "projects/p/topics/t")
    , ("ECLUSE_QUEUE_MEMORY_MAX_DEPTH", "777")
    , ("ECLUSE_AWS_REGION", "eu-west-1")
    , ("ECLUSE_AUTH_TOKEN", "s3cr3t")
    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET_TOKEN", "mirror-write")
    , ("ECLUSE_HELP_MESSAGE", "ask #platform")
    , ("ECLUSE_CVE_SYNC_INTERVAL", "60")
    , ("ECLUSE_CACHE_TTL", "30")
    , ("ECLUSE_CACHE_MAX_ENTRIES", "256")
    , ("ECLUSE_CACHE_MAX_BYTES", "134217728")
    , ("ECLUSE_MAX_RESPONSE_BYTES", "1048576")
    , ("ECLUSE_MAX_VERSION_COUNT", "5000")
    , ("ECLUSE_MAX_NESTING_DEPTH", "32")
    , ("ECLUSE_LOG_FORMAT", "console")
    , ("ECLUSE_TELEMETRY", "on")
    , ("ECLUSE_MOUNTS__NPM__RESPECT_UPSTREAM_TARBALL_HOST", "true")
    , ("ECLUSE_PUBLIC_URL", "https://proxy.example.test")
    ]

-- The minimum valid environment: only the two required URLs, everything else
-- defaulted (ECLUSE_MOUNTS__NPM__MIRROR_TARGET is optional — it folds onto the private upstream).
minimalEnv :: [(String, String)]
minimalEnv =
    [ ("ECLUSE_MOUNTS__NPM__PRIVATE_UPSTREAM", "https://private.example.test")
    , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET", "https://mirror.example.test")
    , ("ECLUSE_QUEUE_URL", "https://sqs.example.test/q")
    , ("ECLUSE_QUEUE_BACKEND", "sqs")
    ]

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
                            `shouldBe` [PrecededRule defaultAllowIfOlderThanPrecedence (AllowIfOlderThan (7 * 86400))]
                        let reg = mountRegistries mount
                        registryUrlText (regPrivateUpstream reg) `shouldBe` "https://private.example.test"
                        mtCredential (regMirrorTarget reg) `shouldBe` CodeArtifactCredential
                        mtQueue (regMirrorTarget reg) `shouldBe` SqsQueue
                        -- The explicit ECLUSE_MOUNTS__NPM__MIRROR_TARGET in minimalEnv is used verbatim.
                        registryUrlText (mtUrl (regMirrorTarget reg)) `shouldBe` "https://mirror.example.test"

    it "folds an unset ECLUSE_MOUNTS__NPM__MIRROR_TARGET onto the private upstream" $ do
        -- N7a: with ECLUSE_MOUNTS__NPM__MIRROR_TARGET absent, the mirror target IS the private
        -- upstream (one registry, read and written) — the write credential does not fold.
        env <- expectEnv (without "ECLUSE_MOUNTS__NPM__MIRROR_TARGET" minimalEnv)
        case loadConfig env Nothing of
            Left errs -> expectationFailure ("unexpected policy errors: " <> show errs)
            Right cfg -> case Map.lookup Npm (configMounts cfg) of
                Nothing -> expectationFailure "expected the npm mount"
                Just mount -> do
                    let reg = mountRegistries mount
                    registryUrlText (mtUrl (regMirrorTarget reg)) `shouldBe` "https://private.example.test"
                    registryUrlText (regPrivateUpstream reg) `shouldBe` "https://private.example.test"

    it "desugars the env single-mount onto a document-only policy patch" $ do
        -- A document with a rule policy but no mounts still produces the env
        -- single-mount (npm), now running on the merged policy.
        env <- expectEnv minimalEnv
        let doc = "{\"rules\":{\"min-age\":{\"enabled\":false}}}"
        case loadConfig env (Just doc) of
            Left errs -> expectationFailure ("unexpected policy errors: " <> show errs)
            Right cfg -> case Map.lookup Npm (configMounts cfg) of
                Nothing -> expectationFailure "expected the npm mount"
                Just mount -> mountPolicy mount `shouldBe` []

    it "uses the document's declared mounts when present" $ do
        env <- expectEnv minimalEnv
        let doc = singleMountDoc
        case loadConfig env (Just doc) of
            Left errs -> expectationFailure ("unexpected policy errors: " <> show errs)
            Right cfg -> Map.keys (configMounts cfg) `shouldBe` [Npm]

    it "applies a per-mount rule refinement over the shared policy" $ do
        -- The shared policy adds deny-scripts; the mount suppresses min-age, so
        -- the mount's resolved policy is the deny alone.
        env <- expectEnv minimalEnv
        let doc = refinedMountDoc
        case loadConfig env (Just doc) of
            Left errs -> expectationFailure ("unexpected policy errors: " <> show errs)
            Right cfg -> case Map.lookup Npm (configMounts cfg) of
                Nothing -> expectationFailure "expected the npm mount"
                Just mount ->
                    mountPolicy mount `shouldBe` [PrecededRule defaultDenyInstallTimeExecutionPrecedence DenyInstallTimeExecution]

    it "surfaces a bad per-mount refinement as a policy error" $ do
        env <- expectEnv minimalEnv
        let doc = badRefinementDoc
        loadConfig env (Just doc) `shouldBe` Left [PolicyErrors [UnknownRuleType "oops" "NoSuchRule"]]

-- ── secrets ──────────────────────────────────────────────────────────────────

secretsSpec :: Spec
secretsSpec = describe "secrets are environment-only" $ do
    it "rejects a token field inside a rule entry, pointing to environment variables" $
        -- A secret must never live in the reviewable document; the decoder
        -- refuses it (naming the offending key) rather than carrying it.
        loadConfig [] (Just "{\"rules\":{\"min-age\":{\"token\":\"abc\"}}}")
            `shouldSatisfy` decodeErrorMentions "environment variables"

    it "rejects an authToken field inside a rule entry" $
        loadConfig [] (Just "{\"rules\":{\"min-age\":{\"authToken\":\"abc\"}}}") `shouldSatisfy` isLeft

-- ── fixtures & helpers ───────────────────────────────────────────────────────

-- A document with a single npm mount and a min-age override.
singleMountDoc :: ByteString
singleMountDoc =
    "{\"queueBackend\":\"sqs\",\"mounts\":{\"npm\":{\
    \\"privateUpstream\":\"https://private.example.test\",\
    \\"publicUpstream\":\"https://registry.npmjs.org\",\
    \\"respectUpstreamTarballHost\":false,\
    \\"mirrorTarget\":\"https://mirror.example.test\",\"credentialProvider\":\"codeartifact\"}},\
    \\"rules\":{\"min-age\":{\"ageSeconds\":1209600}}}"

-- A document whose top-level policy adds deny-scripts and whose mount suppresses
-- min-age, so the mount resolves to the deny alone.
refinedMountDoc :: ByteString
refinedMountDoc =
    "{\"mounts\":{\"npm\":{\
    \\"privateUpstream\":\"https://private.example.test\",\
    \\"publicUpstream\":\"https://registry.npmjs.org\",\
    \\"respectUpstreamTarballHost\":false,\
    \\"mirrorTarget\":\"https://mirror.example.test\",\"credentialProvider\":\"static\",\
    \\"rules\":{\"min-age\":{\"enabled\":false}}}},\
    \\"rules\":{\"deny-scripts\":{\"type\":\"DenyInstallTimeExecution\"}}}"

-- A document whose mount refinement names an unknown rule type.
badRefinementDoc :: ByteString
badRefinementDoc =
    "{\"mounts\":{\"npm\":{\
    \\"privateUpstream\":\"https://private.example.test\",\
    \\"publicUpstream\":\"https://registry.npmjs.org\",\
    \\"respectUpstreamTarballHost\":false,\
    \\"mirrorTarget\":\"https://mirror.example.test\",\"credentialProvider\":\"static\",\
    \\"rules\":{\"oops\":{\"type\":\"NoSuchRule\"}}}}}"

-- Parse an environment, failing the example on an unexpected error.
expectEnv :: [(String, String)] -> IO [(String, String)]
expectEnv = pure

-- Decode a document, failing the example on a decode error.
expectDoc :: ByteString -> IO Config
expectDoc doc = case loadConfig [] (Just doc) of
    Left e -> fail ("document decode failed: " <> show e)
    Right c -> pure c

-- Drop a variable from an environment list.
without :: String -> [(String, String)] -> [(String, String)]
without name = filter ((/= name) . fst)

-- Set (replacing) a variable in an environment list.

-- Whether the needle occurs in the haystack 'Text'.
isInfix :: Text -> Text -> Bool
isInfix = T.isInfixOf

-- Whether a document decode failed with a message mentioning the given phrase.
decodeErrorMentions :: Text -> Either [ConfigError] a -> Bool
decodeErrorMentions phrase (Left errs) = any (\err -> phrase `isInfix` renderConfigError err) errs
decodeErrorMentions _ (Right _) = False

-- 'show' a value as 'Text' (forcing its derived 'Show').
showText :: (Show a) => a -> Text
showText = show
