-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Config.MirrorSpec (spec) where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Test.Hspec

import Ecluse.Composition.Support (expectConfig, overrideEnv, pubUrlEnv, pypiMirrorEnv)
import Ecluse.Config
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
    it "refuses an upload endpoint on a public upstream host" $
        loadConfig (overrideEnv uploadKey "https://pypi.org/legacy/" pypiMirrorEnv) Nothing `shouldSatisfy` isLeft
    it "binds CodeArtifact uploads to the configured mirror repository" $ do
        let prefix = "ECLUSE_MOUNTS__PYPI__MIRROR_TARGET__"
            endpoint = "https://mirror-123456789012.d.codeartifact.us-east-1.amazonaws.com/pypi/cache/"
            configured =
                [(prefix <> "CODE_ARTIFACT__URL", endpoint), (prefix <> "CODE_ARTIFACT__UPLOAD_URL", endpoint)]
                    <> filter (not . List.isPrefixOf prefix . fst) pypiMirrorEnv
        config <- expectConfig configured Nothing
        let target = regMirrorTarget . mountRegistries =<< Map.lookup PyPI (configMounts config)
        fmap mtUsername target `shouldBe` Just (Just "aws")
        loadConfig (overrideEnv (prefix <> "CODE_ARTIFACT__UPLOAD_URL") "https://mirror-123456789012.d.codeartifact.us-east-1.amazonaws.com/pypi/other/" configured) Nothing `shouldSatisfy` isLeft
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

stateKey, uploadKey, usernameKey :: String
stateKey = "ECLUSE_MIRROR_STATE__URL"
uploadKey = "ECLUSE_MOUNTS__PYPI__MIRROR_TARGET__REGISTRY__UPLOAD_URL"
usernameKey = "ECLUSE_MOUNTS__PYPI__MIRROR_TARGET__REGISTRY__USERNAME"
