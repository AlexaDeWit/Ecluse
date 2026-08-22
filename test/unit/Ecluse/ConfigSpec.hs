-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}

module Ecluse.ConfigSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Config (
    AppConfig (cfgQueue),
    Config (configApp, configMounts),
    ConfigError (MirrorSettingWithoutWrite, MountMissingPrivateUpstream, PublicUrlRequired),
    Mount (mountRegistries),
    MountMode (Mirrored, ServeOnly),
    MountRegistries (regMode),
    QueueSettings (qsMaxReceiveCount),
    RulePolicy (..),
    defaultPolicy,
    loadConfig,
    mountCollisionWarnings,
    mountPostureLines,
    renderConfigError,
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
            -- Two places state the same policy default: the operator-visible YAML, and
            -- the value a backend built without config falls back to. They must agree.
            -- If they drift, a deployment and a test double retire poison messages at
            -- different delivery counts.
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
            cfg <- configFor (bareNpmMountDoc [("privateUpstream", "https://priv.example.test")])
            modeOf cfg `shouldBe` (Just . ServeOnly . rightToMaybe . mkRegistryUrl) "https://priv.example.test"
            mountPostureLines cfg `shouldSatisfy` any (T.isInfixOf "serve-only")

        it "resolves enabled alone to the serve-only pure public gate" $ do
            cfg <- configFor "{\"mounts\":{\"npm\":{\"enabled\":true}}}"
            modeOf cfg `shouldBe` Just (ServeOnly Nothing)
            mountPostureLines cfg `shouldSatisfy` any (T.isInfixOf "pure public gate")

        it "switches a declared mount off under enabled: false (keys kept, nothing served)" $ do
            cfg <- configFor "{\"mounts\":{\"npm\":{\"enabled\":false,\"privateUpstream\":\"https://priv.example.test\"}}}"
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

        it "refuses each leftover mirror-write setting on a serve-only mount, aggregated" $
            loadConfig
                pubUrlEnv
                (Just "{\"mounts\":{\"npm\":{\"mirrorTargetToken\":\"t\",\"mirrorCodeArtifactTokenDuration\":3600}}}")
                `shouldBe` Left
                    [ MirrorSettingWithoutWrite Npm "mirrorTargetToken"
                    , MirrorSettingWithoutWrite Npm "mirrorCodeArtifactTokenDuration"
                    ]

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

    describe "mountCollisionWarnings" $ do
        it "is silent when every registry endpoint is distinct" $ do
            cfg <-
                configFor
                    (npmMountDoc [("privateUpstream", "https://priv.example.test"), ("mirrorTarget", "https://mirror.example.test")])
            mountCollisionWarnings cfg `shouldBe` []

        it "warns when the mirror target is declared equal to the private upstream" $
            shouldWarnOnce
                ( npmMountDoc
                    [ ("privateUpstream", "https://priv.example.test")
                    , ("mirrorTarget", "https://priv.example.test")
                    ]
                )
                ["mirrorTarget", "privateUpstream", "https://priv.example.test"]

        it "warns when the mirror target equals the public upstream" $
            shouldWarnOnce
                (npmMountDoc [("privateUpstream", "https://priv.example.test"), ("mirrorTarget", "https://registry.npmjs.org")])
                ["mirrorTarget", "publicUpstream"]

        it "warns when the private and public upstreams collide" $
            shouldWarnOnce
                (npmMountDoc [("privateUpstream", "https://registry.npmjs.org"), ("mirrorTarget", "https://mirror.example.test")])
                ["privateUpstream", "publicUpstream"]

        it "warns when the mirror target equals the publication target" $
            shouldWarnOnce
                ( npmMountDoc
                    [ ("privateUpstream", "https://priv.example.test")
                    , ("mirrorTarget", "https://mirror.example.test")
                    , ("publicationTarget", "https://mirror.example.test")
                    , ("publishAllow", "@acme")
                    ]
                )
                ["mirrorTarget", "publicationTarget"]

        it "does not warn the documented publication-onto-private arrangement" $ do
            cfg <-
                configFor
                    ( npmMountDoc
                        [ ("privateUpstream", "https://priv.example.test")
                        , ("mirrorTarget", "https://mirror.example.test")
                        , ("publicationTarget", "https://priv.example.test")
                        , ("publishAllow", "@acme")
                        ]
                    )
            mountCollisionWarnings cfg `shouldBe` []

        it "ignores a trailing-slash difference when comparing endpoints" $
            shouldWarnOnce
                (npmMountDoc [("privateUpstream", "https://priv.example.test"), ("mirrorTarget", "https://priv.example.test/")])
                ["mirrorTarget", "privateUpstream"]

-- | The client-facing base URL every active-mount load needs (server.publicUrl).
pubUrlEnv :: [(String, String)]
pubUrlEnv = [("ECLUSE_SERVER__PUBLIC_URL", "https://registry.example.test")]

-- | Load a config document, failing the test on any load error.
configFor :: ByteString -> IO Config
configFor doc = either (\errs -> fail ("config load failed: " <> show (map renderConfigError errs))) pure (loadConfig pubUrlEnv (Just doc))

-- | Assert exactly one collision warning whose text carries every phrase.
shouldWarnOnce :: ByteString -> [Text] -> Expectation
shouldWarnOnce doc phrases = do
    cfg <- configFor doc
    case mountCollisionWarnings cfg of
        [warning] -> traverse_ (\phrase -> warning `shouldSatisfy` T.isInfixOf phrase) phrases
        warnings -> expectationFailure ("expected exactly one collision warning, got " <> show warnings)

{- | An npm mount document with the given string fields. The shipped npm template
supplies the rest: the public upstream and the tarball-host posture. The document
carries a static @mirrorTargetToken@, so the non-CodeArtifact mirror targets these
collision cases use derive a valid write credential and the config loads.
-}
npmMountDoc :: [(Text, Text)] -> ByteString
npmMountDoc fields = bareNpmMountDoc (fields <> [("mirrorTargetToken", "t")])

{- | As 'npmMountDoc' but with no implicit write token, for the serve-only cases
(where a leftover write setting is itself the refusal under test).
-}
bareNpmMountDoc :: [(Text, Text)] -> ByteString
bareNpmMountDoc fields =
    encodeUtf8 ("{\"mounts\":{\"npm\":{" <> T.intercalate "," (map field fields) <> "}}}")
  where
    field (key, value) = "\"" <> key <> "\":\"" <> value <> "\""

-- | The npm mount's resolved mode, when the config serves one.
modeOf :: Config -> Maybe MountMode
modeOf cfg = regMode . mountRegistries <$> Map.lookup Npm (configMounts cfg)
