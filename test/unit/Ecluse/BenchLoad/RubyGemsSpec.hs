-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native load fixtures preserve source fields and expose the required work before timing.
module Ecluse.BenchLoad.RubyGemsSpec (spec) where

import Data.Aeson (encode, object, withObject, (.:), (.=))
import Data.Aeson.Types (parseEither)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (isInfixOf, lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (hContentLength, methodGet, status200, status404)
import Network.HTTP.Types.Header (hTransferEncoding)
import Network.Wai (queryString, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec

import Ecluse.BenchLoad.CapturedArtifact (SelectedArtifact (..))
import Ecluse.BenchLoad.Error (BenchLoadError (BenchLoadError))
import Ecluse.BenchLoad.Fixture (fetchChecked, loadCorpusBodies, longCacheTtl)
import Ecluse.BenchLoad.Harness (LoadKnobs (..), Scenario (..), UpstreamFixture (fixtureScenarios), defaultLoadKnobs)
import Ecluse.BenchLoad.PrivateCopy (PrivateCopy (pcPreflight, pcUrl))
import Ecluse.BenchLoad.RubyGems (DependencyWorkload (..), RelayFixture (..), RelayShape (..), dependencyPackages, infoStubWithArtifacts, relayArtifactFixture, relayFixture, rubyGemsFixture, rubyGemsPrivateCopy, selectWarmArtifacts)
import Ecluse.Core.Registry.RubyGems.Route (rubyGemsRoutes)
import Ecluse.Core.Server.Route (RouteName (RouteName))
import Ecluse.Test.Corpus (cpName, rubyGemsCorpusPackages)
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Registry.RubyGems.Reference (RubyInfoRow (..), referenceInfo, runRubyReference)
import Ecluse.Test.Server.Route (claimedOn)
import Ecluse.Test.Support (expectRight)
import Ecluse.Test.Wai (localhost)

spec :: Spec
spec = do
    describe "native measurement work" $ do
        it "keeps batch, original quick-spec and complete-copy work before replay patterns" $
            map scenarioName (fixtureScenarios rubyGemsFixture) `shouldSatisfy` isInfixOf ["batch-dependencies", "quick-gemspec", "gem-verified-known-length", "gem-verified-slow-readers", "worker-mirroring", "pattern-hot-set-control"]
        it "retains the shared stress settings and unlimited-only worker" $ do
            let selected = [(scenarioName item, scenarioConcurrencyScale item, scenarioServiceTime item, scenarioUpstreamLatencyMicros item) | item <- fixtureScenarios rubyGemsFixture, scenarioName item `elem` ["gem-ceiling", "herd", "herd-under-heavy-private", "warm-under-cold", "ramp"]]
            selected `shouldBe` [("gem-ceiling", 4, True, Just 2000), ("herd", 1, False, Nothing), ("herd-under-heavy-private", 1, False, Nothing), ("warm-under-cold", 1, False, Nothing), ("ramp", 1, False, Nothing)]
            [scenarioInProcess item | item <- fixtureScenarios rubyGemsFixture, scenarioName item == "worker-mirroring"] `shouldBe` [True]
        it "declares verified-read observations and retains slow-delivery work in the loaded pass" $ do
            let observed = [(scenarioName item, scenarioVerifiedArtifactMetrics item, scenarioServiceTime item) | item <- fixtureScenarios rubyGemsFixture, scenarioName item `elem` ["gem-verified-known-length", "gem-verified-slow-readers"]]
            observed `shouldBe` [("gem-verified-known-length", True, True), ("gem-verified-slow-readers", True, False)]
        it "resolves every real native workload pin before selecting artifacts" $ do
            captures <- loadCorpusBodies rubyGemsCorpusPackages
            selected <- selectWarmArtifacts rubyGemsCorpusPackages captures
            Map.keys selected `shouldMatchList` ["rails", "nokogiri", "aws-sdk-ec2"]
            for_ (Map.elems selected) $ \artifact -> saUpstreamUrl artifact `shouldBe` "https://rubygems.org/" <> saProxyPath artifact
        it "fails missing capture data instead of supplying selected placeholders" $
            selectWarmArtifacts rubyGemsCorpusPackages Map.empty `shouldThrow` (\(BenchLoadError _) -> True)
    describe "native dependency working sets" $ do
        it "selects the weighted corpus, one history and the configured many-project set" $ do
            fmap (map cpName) (dependencyPackages RepeatedCorpus defaultLoadKnobs) `shouldBe` Right ["rails", "nokogiri", "aws-sdk-ec2"]
            fmap (map cpName) (dependencyPackages OneProject defaultLoadKnobs) `shouldBe` Right ["rails"]
            fmap (map cpName) (dependencyPackages ManyProjects defaultLoadKnobs) `shouldBe` Right ["rails", "nokogiri", "aws-sdk-ec2"]
            fmap (map cpName) (dependencyPackages ManyProjects defaultLoadKnobs{lkWorkingSet = 2}) `shouldBe` Right ["rails", "nokogiri"]
        it "refuses a many-project singleton before reading captures or starting a proxy" $
            for_ [-1, 0, 1] $ \count -> do
                dependencyPackages ManyProjects defaultLoadKnobs{lkWorkingSet = count} `shouldSatisfy` isLeft
                case find ((== "dependencies-working-set-many") . scenarioName) (fixtureScenarios rubyGemsFixture) of
                    Nothing -> expectationFailure "missing native many-project scenario"
                    Just item ->
                        scenarioBoot item defaultLoadKnobs{lkWorkingSet = count} (const (expectationFailure "singleton reached measurement"))
                            `shouldThrow` (\(BenchLoadError message) -> message == "dependencies-working-set-many requires at least two corpus projects")
        for_ [RepeatedCorpus, OneProject, ManyProjects] $ \workload ->
            it ("preflights every selected source and rejects each omitted reply for " <> show workload) $ do
                packages <- expectRight (dependencyPackages workload defaultLoadKnobs)
                replies <- Map.fromList <$> traverse (\package -> (cpName package,) <$> dependencyReply [cpName package]) packages
                empty <- dependencyReply []
                let names = map cpName packages
                    run available = do
                        seen <- newIORef []
                        let app request respond = do
                                let name = fromMaybe "" (join (lookup "gems" (queryString request)))
                                modifyIORef' seen (decodeUtf8 name :)
                                respond (responseLBS status200 [] (Map.findWithDefault empty (decodeUtf8 name) available))
                        testWithApplication (pure app) $ \port -> do
                            let urls = map (pcUrl rubyGemsPrivateCopy port) names
                            for_ urls $ \url -> do
                                request <- HTTP.parseRequest (toString url)
                                path <- expectRight (maybeToRight "dependency target is outside its mount" (T.stripPrefix "/rubygems/" (decodeUtf8 (HTTP.path request))))
                                claimedOn rubyGemsRoutes methodGet (T.splitOn "/" path) `shouldBe` Just (RouteName "dependencies")
                            pcPreflight rubyGemsPrivateCopy urls
                        readIORef seen
                run replies `shouldReturn` reverse names
                for_ names $ \name ->
                    run (Map.delete name replies) `shouldThrow` (\(BenchLoadError _) -> True)
    describe "native relay target and source selection" $
        for_ [False, True] $ \privateHit ->
            for_ [("ordinary", OrdinaryRelay), ("known-length", KnownLengthRelay), ("chunked", SlowChunkedRelay)] $ \(framing, shape) ->
                it ("routes " <> (if privateHit then "private " else "public ") <> framing <> " targets through the production artifact route") $ do
                    let payload = LBS.replicate 32 97
                        fixture = relayFixture privateHit shape defaultLoadKnobs{lkPayloadBytes = 32, lkUpstreamLatencyMicros = 0}
                    rfCacheTtlSeconds fixture `shouldBe` if privateHit then longCacheTtl else 0
                    requests <- traverse (HTTP.parseRequest . toString) (rfTargets fixture 8080)
                    map HTTP.path requests `shouldMatchList` ["/rubygems/gems/rails-9999.0.2.gem", "/rubygems/gems/nokogiri-9999.0.2.gem", "/rubygems/gems/aws-sdk-ec2-9999.0.2.gem"]
                    testWithApplication (pure (rfPrivate fixture)) $ \privatePort ->
                        testWithApplication (pure (rfPublic fixture)) $ \publicPort ->
                            for_ requests $ \request -> do
                                path <- expectRight (maybeToRight "relay target is outside its mount" (T.stripPrefix "/rubygems/" (decodeUtf8 (HTTP.path request))))
                                claimedOn rubyGemsRoutes methodGet (T.splitOn "/" path) `shouldBe` Just (RouteName "artifact")
                                private <- fetchChecked (if privateHit then status200 else status404) [] (localhost privatePort <> "/" <> path)
                                public <- fetchChecked (if privateHit then status404 else status200) [] (localhost publicPort <> "/" <> path)
                                HTTP.responseBody private `shouldBe` if privateHit then payload else "missing"
                                HTTP.responseBody public `shouldBe` if privateHit then "missing" else payload
    describe "verified relay source framing" $
        for_ [KnownLengthRelay, SlowChunkedRelay] $ \shape ->
            it (if shape == KnownLengthRelay then "transmits a genuine known Content-Length" else "transmits chunked bytes without a declared length") $ do
                let payload = LBS.replicate (128 * 1024) 97
                testWithApplication (pure (relayArtifactFixture shape 0 payload)) $ \port -> do
                    response <- fetchChecked status200 [] (localhost port <> "/gems/rails-9999.0.2.gem")
                    HTTP.responseBody response `shouldBe` payload
                    if shape == KnownLengthRelay
                        then lookup hContentLength (HTTP.responseHeaders response) `shouldBe` Just "131072"
                        else do
                            lookup hContentLength (HTTP.responseHeaders response) `shouldBe` Nothing
                            lookup hTransferEncoding (HTTP.responseHeaders response) `shouldBe` Just "chunked"
    describe "native relay fixture" $
        it "changes only checksums and serves exact selected bytes under native artifact paths" $ do
            raw <- readFileLBS "bench/corpus/rubygems/nokogiri.info"
            let payload = "independently owned relay bytes"
                artifacts = Map.singleton "/gems/selected.gem" payload
            testWithApplication (pure (infoStubWithArtifacts 0 (Map.singleton "nokogiri" raw) artifacts)) $ \port -> do
                response <- fetchChecked status200 [] (localhost port <> "/info/nokogiri")
                original <- expectRight (referenceInfo (toStrict raw))
                rewritten <- expectRight (referenceInfo (toStrict (HTTP.responseBody response)))
                map withoutChecksum rewritten `shouldBe` map withoutChecksum original
                map (lookup "checksum" . rowFields) rewritten `shouldBe` replicate (length rewritten) (Just (hexSha256Of (toStrict payload)))
                LBS.length (HTTP.responseBody response) `shouldBe` LBS.length raw
                selected <- fetchChecked status200 [] (localhost port <> "/gems/selected.gem")
                HTTP.responseBody selected `shouldBe` payload
                void (fetchChecked status404 [] (localhost port <> "/gems/absent.gem"))
  where
    withoutChecksum row = row{rowFields = filter ((/= "checksum") . fst) (rowFields row)}

dependencyReply :: [Text] -> IO LByteString
dependencyReply names = do
    let rows = [object ["name" .= name, "version" .= ("1.0.0" :: Text), "platform" .= ("ruby" :: Text), "dependencies" .= ([] :: [Text])] | name <- names]
    encoded <- runRubyReference "encode-dependencies" (toStrict (encode rows))
    fromStrict <$> expectRight (parseEither (withObject "Ruby-generated dependency reply" (\fields -> BS.pack <$> fields .: "bytes")) encoded)
