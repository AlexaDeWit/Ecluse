-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Config.MirrorSpec (spec) where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Composition.Support (expectConfig, overrideEnv, pubUrlEnv, pypiMirrorEnv)
import Ecluse.Config
import Ecluse.Core.Credential (unSecret)
import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Security.Egress (registryUrlText)

spec :: Spec
spec = describe "native mirror declarations" $ do
    it "keeps mirror state independent from the advisory store" $ do
        config <- expectConfig pypiMirrorEnv Nothing
        fmap advisoryStoreUrlText (cfgMirrorState (configApp config)) `shouldBe` Just "s3://mirror-state/records"
        fmap advisoryStoreUrlText (advUrl (cfgAdvisories (configApp config))) `shouldBe` Nothing
    it "preserves the separate upload endpoint and credential username" $ do
        config <- expectConfig pypiMirrorEnv Nothing
        let target = regMirrorTarget . mountRegistries =<< Map.lookup PyPI (configMounts config)
        fmap (registryUrlText . mtUploadUrl) target `shouldBe` Just "https://target.example.test/legacy/"
        fmap mtUsername target `shouldBe` Just (Just "publisher")
    describe "publication credentials" $ do
        for_ [("REGISTRY", "https://publish.example.test/legacy/"), ("CODE_ARTIFACT", codeArtifactHost <> "/pypi/cache/")] $ \(tag, url) ->
            it ("refuses a username without a token for " <> tag) $
                loadConfig (overrideEnv (publicationPrefix tag <> "USERNAME") "aws" (publicationEnv tag url)) Nothing
                    `shouldBe` Left [ParseError "publicationTarget.username requires a static token"]
        it "preserves a generic credential pair" $ do
            config <-
                expectConfig
                    ( overrideEnv (publicationPrefix "REGISTRY" <> "USERNAME") "publisher" $
                        overrideEnv (publicationPrefix "REGISTRY" <> "TOKEN") "publish-token" $
                            publicationEnv "REGISTRY" "https://publish.example.test/legacy/"
                    )
                    Nothing
            let target = mntPublicationTarget =<< Map.lookup PyPI (cfgMounts (configApp config))
            fmap peUsername target `shouldBe` Just (Just "publisher")
            fmap (fmap unSecret . peToken) target `shouldBe` Just (Just "publish-token")
        it "refuses a non-aws CodeArtifact username with a token" $
            loadConfig (overrideEnv (publicationPrefix "CODE_ARTIFACT" <> "USERNAME") "publisher" codeArtifactPublicationEnv) Nothing
                `shouldBe` Left [ParseError "CodeArtifact requires the aws credential username"]
        it "accepts CodeArtifact publication without static credentials" $ do
            config <- expectConfig (publicationEnv "CODE_ARTIFACT" (codeArtifactHost <> "/pypi/cache/")) Nothing
            let target = mntPublicationTarget =<< Map.lookup PyPI (cfgMounts (configApp config))
            fmap peUsername target `shouldBe` Just Nothing
            fmap peToken target `shouldBe` Just Nothing
        for_ [Nothing, Just "aws"] $ \username ->
            it ("accepts the CodeArtifact username " <> show username) $ do
                let env = maybe codeArtifactPublicationEnv (\name -> overrideEnv (publicationPrefix "CODE_ARTIFACT" <> "USERNAME") name codeArtifactPublicationEnv) username
                config <- expectConfig env Nothing
                let target = mntPublicationTarget =<< Map.lookup PyPI (cfgMounts (configApp config))
                fmap peUsername target `shouldBe` Just (toText <$> username)
    describe "CodeArtifact upload endpoints" $ do
        for_ ["/pypi/cache/", "/pypi/cache/legacy/"] $ \path ->
            it ("accepts the native upload path " <> path) $ do
                config <- expectConfig (codeArtifactEnv (codeArtifactHost <> path)) Nothing
                let target = regMirrorTarget . mountRegistries =<< Map.lookup PyPI (configMounts config)
                fmap mtUsername target `shouldBe` Just (Just "aws")
                fmap (registryUrlText . mtUploadUrl) target `shouldBe` Just (toText (codeArtifactHost <> path))
        for_
            [ ("repository root", codeArtifactHost <> "/pypi/other/")
            , ("repository upload path", codeArtifactHost <> "/pypi/other/legacy/")
            , ("domain", "https://other-123456789012.d.codeartifact.us-east-1.amazonaws.com/pypi/cache/legacy/")
            , ("account", "https://mirror-999999999999.d.codeartifact.us-east-1.amazonaws.com/pypi/cache/legacy/")
            , ("region", "https://mirror-123456789012.d.codeartifact.eu-west-1.amazonaws.com/pypi/cache/legacy/")
            ]
            $ \(identityPart, upload) ->
                it ("refuses an upload to another " <> identityPart) $
                    loadConfig (codeArtifactEnv upload) Nothing
                        `shouldBe` Left [ParseError "CodeArtifact uploadUrl must address the same mirror repository"]
        for_ ["/pypi/cache/simple/", "/pypi/cache/legacy/extra/", "/npm/cache/legacy/", "/legacy/"] $ \path ->
            it ("refuses an unsupported upload path " <> path) $
                loadConfig (codeArtifactEnv (codeArtifactHost <> path)) Nothing
                    `shouldBe` Left [ParseError "mirrorTarget.codeArtifact.uploadUrl must address /pypi/{repository}/ or /pypi/{repository}/legacy/"]
        it "refuses an upload host outside CodeArtifact" $
            loadConfig (codeArtifactEnv "https://target.example.test/pypi/cache/legacy/") Nothing
                `shouldBe` Left [CodeArtifactHostMismatch PyPI "mirrorTarget.codeArtifact.uploadUrl"]
        it "keeps the read root separate from the native upload path" $
            loadConfig
                (overrideEnv (codeArtifactPrefix <> "URL") (codeArtifactHost <> "/pypi/cache/legacy/") (codeArtifactEnv (codeArtifactHost <> "/pypi/cache/legacy/")))
                Nothing
                `shouldBe` Left [CodeArtifactRepositoryMissing PyPI "mirrorTarget.codeArtifact.url" "pypi"]
        it "refuses the PyPI upload suffix on a Cargo mount" $
            loadConfig
                ( pubUrlEnv
                    <> [ ("ECLUSE_MOUNTS__CARGO__MIRROR_TARGET__CODE_ARTIFACT__URL", codeArtifactHost <> "/cargo/cache/")
                       , ("ECLUSE_MOUNTS__CARGO__MIRROR_TARGET__CODE_ARTIFACT__UPLOAD_URL", codeArtifactHost <> "/cargo/cache/legacy/")
                       ]
                )
                Nothing
                `shouldBe` Left [ParseError "mirrorTarget.codeArtifact.uploadUrl must address /cargo/{repository}/"]
    it "refuses malformed state URLs through the existing object-store parser" $
        loadConfig (overrideEnv stateKey "https://target.example.test/state" pypiMirrorEnv) Nothing `shouldSatisfy` isLeft
    it "does not require state or upload settings for a serve-only mount" $
        loadConfig (pubUrlEnv <> [("ECLUSE_MOUNTS__PYPI__ENABLED", "true")]) Nothing `shouldSatisfy` isRight
    it "preserves an explicitly empty Basic username but refuses a delimiter" $ do
        loadConfig (overrideEnv usernameKey "" pypiMirrorEnv) Nothing `shouldSatisfy` isRight
        loadConfig (overrideEnv usernameKey "user:other" pypiMirrorEnv) Nothing `shouldSatisfy` isLeft
    it "keeps numeric usernames as text in the environment overlay" $
        loadConfig (overrideEnv usernameKey "12345" pypiMirrorEnv) Nothing `shouldSatisfy` isRight
    it "applies the mount request cap and permits a larger wheel bound" $ do
        baseline <- expectConfig pypiMirrorEnv Nothing
        configured <- expectConfig (overrideEnv "ECLUSE_MOUNTS__PYPI__MAX_REQUEST_BYTES" "52428800" pypiMirrorEnv) Nothing
        let cap = fmap mntMaxRequestBytes . Map.lookup PyPI . cfgMounts . configApp
        cap baseline `shouldBe` Just Nothing
        cap configured `shouldBe` Just (Just 52428800)

stateKey, usernameKey :: String
stateKey = "ECLUSE_MIRROR_STATE__URL"
usernameKey = "ECLUSE_MOUNTS__PYPI__MIRROR_TARGET__REGISTRY__USERNAME"

codeArtifactPrefix, codeArtifactHost :: String
codeArtifactPrefix = "ECLUSE_MOUNTS__PYPI__MIRROR_TARGET__CODE_ARTIFACT__"
codeArtifactHost = "https://mirror-123456789012.d.codeartifact.us-east-1.amazonaws.com"

codeArtifactEnv :: String -> [(String, String)]
codeArtifactEnv upload =
    [ (codeArtifactPrefix <> "URL", codeArtifactHost <> "/pypi/cache/")
    , (codeArtifactPrefix <> "UPLOAD_URL", upload)
    ]
        <> filter (not . List.isPrefixOf "ECLUSE_MOUNTS__PYPI__MIRROR_TARGET__" . fst) pypiMirrorEnv

publicationPrefix :: String -> String
publicationPrefix tag = "ECLUSE_MOUNTS__PYPI__PUBLICATION_TARGET__" <> tag <> "__"

publicationEnv :: String -> String -> [(String, String)]
publicationEnv tag url =
    pubUrlEnv
        <> [ ("ECLUSE_MOUNTS__PYPI__ENABLED", "true")
           , (publicationPrefix tag <> "URL", url)
           ]

codeArtifactPublicationEnv :: [(String, String)]
codeArtifactPublicationEnv =
    overrideEnv (publicationPrefix "CODE_ARTIFACT" <> "TOKEN") "publish-token" $
        publicationEnv "CODE_ARTIFACT" (codeArtifactHost <> "/pypi/cache/")
