-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}

module Ecluse.ConfigSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Config (
    Config (configMounts),
    ConfigError (MirrorSettingWithoutWrite, MountMissingPrivateUpstream, PublicUrlRequired),
    Mount (mountRegistries),
    MountMode (Mirrored, ServeOnly),
    MountRegistries (regMode),
    RulePolicy (..),
    defaultPolicy,
    loadConfig,
    mountCollisionWarnings,
    mountPostureLines,
    renderConfigError,
 )
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
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
            -- Served tarball URLs are rewritten against the proxy's own base URL;
            -- omitting it fails here, not client by client at install time.
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

{- | Load a config document, failing the test on any load error.
| The client-facing base URL every active-mount load needs (server.publicUrl).
-}
pubUrlEnv :: [(String, String)]
pubUrlEnv = [("ECLUSE_SERVER__PUBLIC_URL", "https://registry.example.test")]

configFor :: ByteString -> IO Config
configFor doc = either (\errs -> fail ("config load failed: " <> show (map renderConfigError errs))) pure (loadConfig pubUrlEnv (Just doc))

-- | Assert exactly one collision warning whose text carries every phrase.
shouldWarnOnce :: ByteString -> [Text] -> Expectation
shouldWarnOnce doc phrases = do
    cfg <- configFor doc
    case mountCollisionWarnings cfg of
        [warning] -> traverse_ (\phrase -> warning `shouldSatisfy` T.isInfixOf phrase) phrases
        warnings -> expectationFailure ("expected exactly one collision warning, got " <> show warnings)

{- | An npm mount document with the given string fields; the shipped npm template
supplies the rest (public upstream, tarball-host posture). A static
@mirrorTargetToken@ is added so the (non-CodeArtifact) mirror targets these
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

-- | The served npm mount's resolved mode, when one is served.
modeOf :: Config -> Maybe MountMode
modeOf cfg = regMode . mountRegistries <$> Map.lookup Npm (configMounts cfg)
