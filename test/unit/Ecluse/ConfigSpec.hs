-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}

module Ecluse.ConfigSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Composition.Support (expectConfig)
import Ecluse.Config (
    AppConfig (cfgQueue),
    Config (configApp, configMounts),
    ConfigError (MountMissingPrivateUpstream, PublicUrlRequired),
    Mount (mountRegistries),
    MountMode (Mirrored, ServeOnly),
    MountRegistries (regMode),
    QueueSettings (qsMaxReceiveCount),
    RulePolicy (..),
    defaultPolicy,
    loadConfig,
    mountPostureLines,
    resolvedKeyProvenance,
 )
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Queue (DeliveryBudget (DeliveryBudget), defaultDeliveryBudget)
import Ecluse.Core.Security.Egress (mkRegistryUrl)

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
            -- The operator-visible YAML and the fallback a config-free backend holds state the same
            -- policy default. If they drift, a deployment and a test double retire poison messages
            -- at different counts.
            case (loadConfig [] Nothing, defaultDeliveryBudget) of
                (Right cfg, DeliveryBudget budget) ->
                    qsMaxReceiveCount (cfgQueue (configApp cfg)) `shouldBe` budget
                (Left errs, _) -> expectationFailure ("the embedded defaults failed to load: " <> show errs)

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
            let provenance = resolvedKeyProvenance [("ECLUSE_SERVER__AUTH_TOKEN", "hunter2")] Nothing
            provenance `shouldSatisfy` elem "config: server.authToken = <redacted> (environment)"
            provenance `shouldSatisfy` (not . any (T.isInfixOf "hunter2"))

-- | The client-facing base URL every active-mount load needs (server.publicUrl).
pubUrlEnv :: [(String, String)]
pubUrlEnv = [("ECLUSE_SERVER__PUBLIC_URL", "https://registry.example.test")]

-- | Load a config document under the client-facing base URL every active mount needs.
configFor :: ByteString -> IO Config
configFor doc = expectConfig pubUrlEnv (Just doc)

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
