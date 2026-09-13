-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}

-- | Loader and mount-resolution contracts against the shipped defaults.
module Ecluse.ConfigSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Composition.Support (codeArtifactEnvVars, expectConfig)
import Ecluse.Config (
    AppConfig (cfgQueue),
    Config (configApp, configMounts),
    ConfigError (MountMissingPrivateUpstream, PublicUrlRequired),
    Mount (mountRegistries),
    MountMode (Mirrored, ServeOnly),
    MountRegistries (regMode),
    QueueSettings (qsMaxReceiveCount),
    RulePolicy (..),
    advisoryAgeLines,
    defaultPolicy,
    loadConfig,
    mountEpssRequirement,
    mountPostureLines,
    renderConfigError,
    resolvedKeyProvenance,
 )
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Osv.Schema (EpssRequirement (..))
import Ecluse.Core.Queue (DeliveryBudget (DeliveryBudget), defaultDeliveryBudget)
import Ecluse.Core.Security.Egress (mkRegistryUrl)

-- | Configuration loading and operator diagnostics.
spec :: Spec
spec = do
    describe "the embedded default configuration" $ do
        it "loads as a valid, self-contained backbone with no operator overlay" $
            loadConfig [] Nothing `shouldSatisfy` isRight

        it "ships exactly the expected baseline rules under their default names" $
            case defaultPolicy of
                RulePolicy rules ->
                    Map.keys rules `shouldMatchList` ["min-age", "remediation-fast-track"]

        it "pins the shipped redelivery budget to the one a directly-built backend holds" $
            -- A drift changes when deployments and test doubles retire poison messages.
            case (loadConfig [] Nothing, defaultDeliveryBudget) of
                (Right cfg, DeliveryBudget budget) ->
                    qsMaxReceiveCount (cfgQueue (configApp cfg)) `shouldBe` budget
                (Left errs, _) -> expectationFailure ("the embedded defaults failed to load: " <> show errs)

    describe "configuration type error paths" $ do
        forM_
            [ ("server.port", "ECLUSE_SERVER__PORT", "{\"server\":{\"port\":\"bad\"}}")
            , ("server.shutdownDrainTimeout", "ECLUSE_SERVER__SHUTDOWN_DRAIN_TIMEOUT", "{\"server\":{\"shutdownDrainTimeout\":\"bad\"}}")
            , ("limits.maxVersionCount", "ECLUSE_LIMITS__MAX_VERSION_COUNT", "{\"limits\":{\"maxVersionCount\":\"bad\"}}")
            , ("runtime.cores", "ECLUSE_RUNTIME__CORES", "{\"runtime\":{\"cores\":\"bad\"}}")
            ]
            $ \(field, envKey, doc) ->
                it ("names " <> toString field <> " through document and environment loads") $
                    forM_ [loadConfig [] (Just doc), loadConfig [(envKey, "bad")] Nothing] $ \result ->
                        first (any (T.isInfixOf ("$." <> field) . renderConfigError)) result
                            `shouldBe` Left True

        it "names a nested group with the wrong type" $
            first
                (any (T.isInfixOf "$.advisories.quietTime" . renderConfigError))
                (loadConfig [] (Just "{\"advisories\":{\"quietTime\":false}}"))
                `shouldBe` Left True

        it "omits supplied values from typed credential errors" $
            case loadConfig [("ECLUSE_SERVER", "{\"authToken\":[\"credential-sentinel\"]}")] Nothing of
                Left errs -> do
                    let messages = map renderConfigError errs
                    messages `shouldSatisfy` any (T.isInfixOf "server.authToken")
                    messages `shouldSatisfy` (not . any (T.isInfixOf "credential-sentinel"))
                Right _ -> expectationFailure "expected a credential type error"

    describe "mount modes (mirroring derived from the declared target)" $ do
        it "resolves a declared mirrorTarget to a mirrored mount" $ do
            cfg <- configFor (npmMountDoc [("privateUpstream", "https://priv.example.test"), ("mirrorTarget", "https://mirror.example.test")])
            modeOf cfg `shouldSatisfy` \case Just (Mirrored _) -> True; _ -> False
            mountPostureLines cfg `shouldSatisfy` any (T.isInfixOf "mirrored")

        it "resolves an absent mirrorTarget to a serve-only mount over the private merge" $ do
            cfg <- configFor (npmMountDoc [("privateUpstream", "https://priv.example.test")])
            modeOf cfg `shouldBe` (Just . ServeOnly . rightToMaybe . mkRegistryUrl) "https://priv.example.test"
            mountPostureLines cfg `shouldSatisfy` any (T.isInfixOf "serve-only")

        it "resolves enabled alone to the serve-only pure public gate" $ do
            cfg <- configFor "{\"mounts\":{\"npm\":{\"enabled\":true}}}"
            modeOf cfg `shouldBe` Just (ServeOnly Nothing)
            mountPostureLines cfg `shouldSatisfy` any (T.isInfixOf "pure public gate")

        it "switches a declared mount off under enabled: false (keys kept, nothing served)" $ do
            cfg <-
                configFor
                    "{\"mounts\":{\"npm\":{\"enabled\":false,\
                    \\"privateUpstream\":{\"registry\":{\"url\":\"https://priv.example.test\"}}}}}"
            Map.keys (configMounts cfg) `shouldBe` []

        it "requires the private upstream on a mirrored mount (the mirror must read back)" $
            loadConfig pubUrlEnv (Just (npmMountDoc [("mirrorTarget", "https://mirror.example.test")]))
                `shouldBe` Left [MountMissingPrivateUpstream Npm]

        it "requires server.publicUrl once any mount is active, aggregated with the mount errors" $ do
            -- Écluse rewrites served tarball URLs against the proxy's own base URL. A
            -- missing base URL fails here, not client by client at install time.
            loadConfig [] (Just "{\"mounts\":{\"npm\":{\"enabled\":true}}}")
                `shouldBe` Left [PublicUrlRequired]
            loadConfig [] (Just (npmMountDoc [("mirrorTarget", "https://mirror.example.test")]))
                `shouldBe` Left [PublicUrlRequired, MountMissingPrivateUpstream Npm]

        it "prints the Verdaccio store's declared deletion consent in the mount posture" $ do
            permitted <- configFor (verdaccioMountDoc "\"permitDeletion\":true,")
            mountPostureLines permitted `shouldSatisfy` any (T.isInfixOf "which permits deletion")
            withheld <- configFor (verdaccioMountDoc "")
            mountPostureLines withheld `shouldSatisfy` any (T.isInfixOf "which withholds deletion")

    describe "maintenance client posture" $ do
        it "appends a live-environment notice for CodeArtifact and Verdaccio" $ do
            codeArtifact <- expectConfig codeArtifactEnvVars Nothing
            verdaccio <- configFor (verdaccioMountDoc "")
            forM_ [codeArtifact, verdaccio] $ \cfg ->
                drop 1 (mountPostureLines cfg)
                    `shouldBe` ["mount \"npm\": the store maintenance client is built at boot against the live environment. check-config does not attempt this build."]

        it "adds no notice for registry targets or serve-only mounts" $ do
            registry <- configFor (npmMountDoc [("privateUpstream", "https://priv.example.test"), ("mirrorTarget", "https://mirror.example.test")])
            private <- configFor (npmMountDoc [("privateUpstream", "https://priv.example.test")])
            public <- configFor "{\"mounts\":{\"npm\":{\"enabled\":true}}}"
            forM_ [registry, private, public] $ \cfg ->
                length (mountPostureLines cfg) `shouldBe` 1

    describe "the advisory push-age limit reported at boot" $ do
        it "derives six days from the shipped seven-day quarantine, naming the rule" $ do
            cfg <- configFor privateMountDoc
            advisoryAgeLines cfg
                `shouldBe` ["mount \"npm\": CVE-based denial refuses on an advisory push older than 6 days, derived a day ahead of this mount's earliest AllowIfOlderThan quarantine of 7 days"]

        it "names an explicit maximum as its own basis" $ do
            cfg <- expectConfig (pubUrlEnv <> [("ECLUSE_ADVISORIES__MAX_AGE_SECONDS", "3600")]) (Just privateMountDoc)
            advisoryAgeLines cfg `shouldSatisfy` any (T.isInfixOf "1 hour, set by advisories.maxAgeSeconds")

        it "holds the floor under a two-day quarantine" $ do
            cfg <- expectConfig (pubUrlEnv <> [("ECLUSE_RULES", "{\"min-age\":{\"ageSeconds\":172800}}")]) (Just privateMountDoc)
            advisoryAgeLines cfg `shouldSatisfy` any (T.isInfixOf "3 days, the shipped floor")

        it "reports nothing for a mount whose rules read no advisory database" $ do
            cfg <- expectConfig (pubUrlEnv <> [("ECLUSE_RULES", "{\"remediation-fast-track\":{\"enabled\":false}}")]) (Just privateMountDoc)
            advisoryAgeLines cfg `shouldBe` []

    describe "mountEpssRequirement" $ do
        it "requires inherited EPSS rules only where the mount keeps them" $ do
            cfg <- configFor "{\"rules\":{\"risk\":{\"type\":\"DenyIfEpss\",\"minEpss\":0.5}},\"mounts\":{\"npm\":{\"enabled\":true},\"pypi\":{\"enabled\":true,\"rules\":{\"risk\":{\"enabled\":false}}}}}"
            Map.map mountEpssRequirement (configMounts cfg) `shouldBe` Map.fromList [(Npm, EpssRequired), (PyPI, EpssOptional)]

        it "requires mount additions regardless of name, skip alignment, or maximum threshold" $ do
            cfg <- configFor "{\"mounts\":{\"npm\":{\"enabled\":true,\"rules\":{\"renamed-risk\":{\"type\":\"DenyIfEpss\",\"minEpss\":1,\"onUnavailable\":\"skip\"}}},\"pypi\":{\"enabled\":true}}}"
            Map.map mountEpssRequirement (configMounts cfg) `shouldBe` Map.fromList [(Npm, EpssRequired), (PyPI, EpssOptional)]

        it "does not require EPSS for the shipped policy" $ do
            cfg <- configFor privateMountDoc
            Map.map mountEpssRequirement (configMounts cfg) `shouldBe` Map.singleton Npm EpssOptional

    describe "resolvedKeyProvenance" $ do
        it "labels each resolved key with the layer that supplied it" $ do
            let provenance =
                    resolvedKeyProvenance
                        [("ECLUSE_SERVER__PORT", "4873")]
                        (Just "{\"server\":{\"helpMessage\":\"ask platform-eng\"}}")
            provenance `shouldSatisfy` elem "config: server.port = 4873 (environment)"
            provenance `shouldSatisfy` elem "config: server.helpMessage = ask platform-eng (document)"
            provenance `shouldSatisfy` elem "config: observability.logFormat = json (default)"

        it "redacts secret-typed keys whatever layer supplies them" $ do
            let provenance =
                    resolvedKeyProvenance
                        [ ("ECLUSE_SERVER__AUTH_TOKEN", "hunter2")
                        , ("ECLUSE_MOUNTS__NPM__MIRROR_TARGET__VERDACCIO__TOKEN", "hunter3")
                        ]
                        Nothing
            provenance `shouldSatisfy` elem "config: server.authToken = <redacted> (environment)"
            provenance
                `shouldSatisfy` elem "config: mounts.npm.mirrorTarget.verdaccio.token = <redacted> (environment)"
            provenance `shouldSatisfy` (not . any (T.isInfixOf "hunter"))

-- | The client-facing base URL every active-mount load needs (server.publicUrl).
pubUrlEnv :: [(String, String)]
pubUrlEnv = [("ECLUSE_SERVER__PUBLIC_URL", "https://registry.example.test")]

-- | Load a config document under the client-facing base URL every active mount needs.
configFor :: ByteString -> IO Config
configFor doc = expectConfig pubUrlEnv (Just doc)

-- | The serve-only npm mount the advisory-age cases load, which carries the shipped rule policy.
privateMountDoc :: ByteString
privateMountDoc = npmMountDoc [("privateUpstream", "https://priv.example.test")]

-- | An npm mount document declaring each named endpoint at its URL under the @registry@ tag.
npmMountDoc :: [(Text, Text)] -> ByteString
npmMountDoc endpoints =
    encodeUtf8 ("{\"mounts\":{\"npm\":{" <> T.intercalate "," (map endpoint endpoints) <> "}}}")
  where
    endpoint (key, url) = "\"" <> key <> "\":{\"registry\":{\"url\":\"" <> url <> "\"" <> write key <> "}}"
    -- The registry tag requires a static write token on a mirror target and admits none elsewhere.
    write key = if key == "mirrorTarget" then ",\"token\":\"t\"" else ""

-- | A mirrored npm mount on Verdaccio, with the given extra keys written under that tag.
verdaccioMountDoc :: Text -> ByteString
verdaccioMountDoc extra =
    encodeUtf8 $
        "{\"mounts\":{\"npm\":{\
        \\"privateUpstream\":{\"verdaccio\":{\"url\":\"https://priv.example.test\"}},\
        \\"mirrorTarget\":{\"verdaccio\":{"
            <> extra
            <> "\"url\":\"https://verdaccio.example.test\",\"token\":\"t\"}}}}}"

-- | The npm mount's resolved mode, when the config serves one.
modeOf :: Config -> Maybe MountMode
modeOf cfg = regMode . mountRegistries <$> Map.lookup Npm (configMounts cfg)
