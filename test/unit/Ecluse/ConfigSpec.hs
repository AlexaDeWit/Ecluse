-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE OverloadedStrings #-}

module Ecluse.ConfigSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Hspec

import Ecluse.Config (Config, RulePolicy (..), defaultPolicy, loadConfig, mountCollisionWarnings, renderConfigError)

spec :: Spec
spec = do
    describe "the embedded default configuration" $ do
        it "loads as a valid, self-contained backbone with no operator overlay" $
            loadConfig [] Nothing `shouldSatisfy` isRight

        it "ships exactly the expected baseline rules under their default names" $
            case defaultPolicy of
                RulePolicy rules ->
                    Map.keys rules `shouldMatchList` ["min-age", "remediation-fast-track"]

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
                    , ("publishScopes", "@acme")
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
                        , ("publishScopes", "@acme")
                        ]
                    )
            mountCollisionWarnings cfg `shouldBe` []

        it "ignores a trailing-slash difference when comparing endpoints" $
            shouldWarnOnce
                (npmMountDoc [("privateUpstream", "https://priv.example.test"), ("mirrorTarget", "https://priv.example.test/")])
                ["mirrorTarget", "privateUpstream"]

-- | Load a config document, failing the test on any load error.
configFor :: ByteString -> IO Config
configFor doc = either (\errs -> fail ("config load failed: " <> show (map renderConfigError errs))) pure (loadConfig [] (Just doc))

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
npmMountDoc fields =
    encodeUtf8 ("{\"mounts\":{\"npm\":{" <> T.intercalate "," (map field (fields <> [("mirrorTargetToken", "t")])) <> "}}}")
  where
    field (key, value) = "\"" <> key <> "\":\"" <> value <> "\""
