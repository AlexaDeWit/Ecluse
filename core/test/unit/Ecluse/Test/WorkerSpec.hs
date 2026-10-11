-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Worker fixtures retain the selected adapter's artifact capability.
module Ecluse.Test.WorkerSpec (spec) where

import Data.Time (getCurrentTime)
import Test.Hspec

import Ecluse.Core.Registry (FetchFault (FetchUrlUnformable), UrlFormationError (EmptyBaseUrl))
import Ecluse.Core.Registry.Adapter.Capability (AdapterArtifact (artifactByUrl))
import Ecluse.Core.Registry.Adapter.Types (RegistryAdapter (adapterArtifact))
import Ecluse.Core.Registry.Metadata (VersionEvaluation (VersionMissing))
import Ecluse.Core.Registry.Npm.Adapter (npmAdapter)
import Ecluse.Core.Registry.Publish (MirrorPublish (..))
import Ecluse.Core.Registry.PyPI.Adapter (pypiAdapter)
import Ecluse.Core.Worker (WorkerPolicy (wpArtifact))
import Ecluse.Test.Worker (npmPolicyWith, policyWith)

-- | Pin the public contract through deterministic inputs.
spec :: Spec
spec = describe "policyWith" $ do
    it "uses the supplied adapter's artifact request builder" $ do
        let adapter = pypiAdapter{adapterArtifact = (adapterArtifact pypiAdapter){artifactByUrl = \_ _ -> Left EmptyBaseUrl}}
            policy = policyWith adapter getCurrentTime 1 unusedPublish (\_ _ -> pure VersionMissing) []
        void (artifactByUrl (wpArtifact policy) Nothing "https://example.test/file.whl") `shouldBe` Left EmptyBaseUrl
    it "retains the npm wrapper's native artifact builder" $ do
        let policy = npmPolicyWith getCurrentTime 1 unusedPublish (\_ _ -> pure VersionMissing) []
            url = "https://registry.npmjs.org/is-odd/-/is-odd-1.0.0.tgz"
        void (artifactByUrl (wpArtifact policy) Nothing url) `shouldBe` void (artifactByUrl (adapterArtifact npmAdapter) Nothing url)

unusedPublish :: MirrorPublish
unusedPublish = MirrorPublish (\_ -> pure (Left (FetchUrlUnformable EmptyBaseUrl))) (\_ _ _ _ -> pure (Right ()))
