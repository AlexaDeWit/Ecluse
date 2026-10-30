-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Cargo's private-copy state records only private rows that match their public source.
module Ecluse.BenchLoad.Cargo.CopyStateSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Ratio ((%))
import Data.Set qualified as Set
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (status200, status404)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec

import Ecluse.BenchLoad.Cargo (crateStubWith, indexStubWithArtifacts)
import Ecluse.BenchLoad.Cargo.CopyState (withCopyState)
import Ecluse.BenchLoad.Fixture (fetchChecked, loadCorpusBodies, loadCorpusCuts, primeETag)
import Ecluse.BenchLoad.Harness (LoadKnobs (lkUpstreamLatencyMicros), defaultLoadKnobs)
import Ecluse.BenchLoad.ProxyProcess (ProxySettings (..), mirrorStateBucket, proxySettings)
import Ecluse.Core.Ecosystem (Ecosystem (Cargo))
import Ecluse.Core.Mirror.State (StateTarget, recordVersions, stateTarget)
import Ecluse.Core.Mirror.State.Codec (chargedDecodeProjectRecord)
import Ecluse.Core.Mirror.State.Internal (projectKey)
import Ecluse.Core.Package (PackageName, mkPackageName)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Test.Corpus (CorpusPackage, cargoCorpusPackages, cpName)
import Ecluse.Test.Corpus.Cargo (cargoVersionKeys)
import Ecluse.Test.Corpus.Subset (newestCargoShare)
import Ecluse.Test.Support (expectRight, expectRightText)

spec :: Spec
spec = describe "withCopyState" $ do
    it "records the private copy's rows that match their public source, behind the copy-state hosts" $ do
        cut <- loadCorpusCuts (newestCargoShare (5 % 100)) tokioCapture
        cutVersions <- expectRightText (cargoVersionKeys . toStrict =<< maybeToRight "the cut holds no tokio capture" (Map.lookup "tokio" cut))
        private <- indexStub cut
        public <- indexStub =<< loadCorpusBodies tokioCapture
        withPreparedState private public $ \settings target stateUrl -> do
            (psPublicHost settings, psPrivateHost settings) `shouldBe` (Just "localhost", Just "127.0.0.1")
            void (primeETag stateUrl)
            response <- fetchChecked status200 [] stateUrl
            record <- expectRight =<< chargedDecodeProjectRecord (const pass) defaultLimits target tokio (toStrict (HTTP.responseBody response))
            Map.keysSet (recordVersions record) `shouldBe` Set.fromList cutVersions
    it "gives an unmatched private overlay no record" $ do
        public <- indexStub =<< loadCorpusBodies tokioCapture
        withPreparedState (crateStubWith (const (pure [])) 0 "trusted") public $ \_ _ stateUrl ->
            void (fetchChecked status404 [] stateUrl)
  where
    indexStub bodies = do
        rewritten <- newIORef mempty
        pure (indexStubWithArtifacts rewritten 0 bodies Map.empty)

tokio :: PackageName
tokio = mkPackageName Cargo Nothing "tokio"

-- Only the tokio capture, so the state fixture's other corpus reads answer 404 and are skipped.
tokioCapture :: [CorpusPackage]
tokioCapture = filter ((== "tokio") . cpName) cargoCorpusPackages

-- Prepare state over both stubs and pass the bound settings, the record target and tokio's state URL.
withPreparedState :: Application -> Application -> (ProxySettings -> StateTarget -> Text -> IO a) -> IO a
withPreparedState private public check =
    withCopyState defaultLoadKnobs{lkUpstreamLatencyMicros = 0} $ \configure ->
        testWithApplication (pure private) $ \privatePort ->
            testWithApplication (pure public) $ \publicPort -> do
                adjust <- configure privatePort publicPort
                let settings = adjust (proxySettings Cargo 0)
                    target = stateTarget ("https://127.0.0.1:" <> show privatePort)
                statePort <- expectRightText (maybeToRight "the state fixture bound no state port" (psMirrorStatePort settings))
                check settings target ("http://127.0.0.1:" <> show statePort <> "/" <> mirrorStateBucket <> "/" <> projectKey target tokio <> "/project.json")
