-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native load fixtures preserve source fields and expose the required work before timing.
module Ecluse.BenchLoad.RubyGemsSpec (spec) where

import Data.Aeson ((.:))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (isInfixOf, lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (diffUTCTime, nominalDay)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (hContentLength, methodGet, status200, status400, status404)
import Network.HTTP.Types.Header (hTransferEncoding)
import Network.Wai (pathInfo, rawPathInfo, rawQueryString, requestMethod, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec

import Ecluse.BenchLoad.CapturedArtifact (SelectedArtifact (..))
import Ecluse.BenchLoad.Error (BenchLoadError (BenchLoadError))
import Ecluse.BenchLoad.Fixture (fetchChecked, loadCorpusBodies, longCacheTtl)
import Ecluse.BenchLoad.Harness (Driver (DriveReplay), LoadKnobs (..), Scenario (..), UpstreamFixture (fixtureScenarios), defaultLoadKnobs, warmUp)
import Ecluse.BenchLoad.Oha (OhaReport (ohaStatusCounts, ohaSuccessLatencies))
import Ecluse.BenchLoad.PatternScenario (PatternProvider (..), patternRequestUrls)
import Ecluse.BenchLoad.Patterns (ClientTrace (ClientTrace), RequestTrace (RequestTrace))
import Ecluse.BenchLoad.PrivateCopy (PrivateCopy (pcPreflight, pcUrl))
import Ecluse.BenchLoad.Replay (Replay (Replay), ReplayReport (replayHttp), runReplay)
import Ecluse.BenchLoad.RubyGems (InfoWorkload (..), RelayFixture (..), RelayShape (..), infoPackages, infoStubWithArtifacts, relayArtifactFixture, relayFixture, rubyGemsFixture, rubyGemsPatternProvider, rubyGemsPrivateCopy, selectWarmArtifacts)
import Ecluse.Core.Ecosystem (Ecosystem (RubyGems))
import Ecluse.Core.Package (PackageDetails (pkgPublishedAt), PackageInfo (infoVersions))
import Ecluse.Core.Registry.Metadata (Manifest (manifestInfo))
import Ecluse.Core.Registry.RubyGems.Metadata (gemReadWith)
import Ecluse.Core.Registry.RubyGems.Route (rubyGemsRoutes)
import Ecluse.Core.Server.Route (RouteName (RouteName))
import Ecluse.Test.Corpus (CaptureRecord (crCapturedAt), CorpusPackage (cpPackage), cpName, readCaptureRecords, readCorpusPins, rubyGemsCaptureUpstream, rubyGemsCorpusPackages)
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Registry.Metadata.Fetch (captureManifest)
import Ecluse.Test.Registry.RubyGems.Reference (RubyInfoRow (..), referenceInfo)
import Ecluse.Test.Server.Route (claimedOn)
import Ecluse.Test.Support (expectRight, expectRightText)
import Ecluse.Test.Wai (localhost)

spec :: Spec
spec = do
    describe "native measurement work" $ do
        it "keeps original quick-spec, directory and complete-copy work before replay patterns" $
            map scenarioName (fixtureScenarios rubyGemsFixture) `shouldSatisfy` isInfixOf ["quick-gemspec", "versions-directory", "worker-mirroring", "pattern-hot-set-control"]
        it "retains the shared stress settings and unlimited-only worker" $ do
            let selected = [(scenarioName item, scenarioConcurrencyScale item, scenarioServiceTime item, scenarioUpstreamLatencyMicros item) | item <- fixtureScenarios rubyGemsFixture, scenarioName item `elem` ["gem-ceiling", "herd", "herd-under-heavy-private", "warm-under-cold", "ramp"]]
            selected `shouldBe` [("gem-ceiling", 4, True, Just 2000), ("herd", 1, False, Nothing), ("herd-under-heavy-private", 1, False, Nothing), ("warm-under-cold", 1, False, Nothing), ("ramp", 1, False, Nothing)]
            [scenarioInProcess item | item <- fixtureScenarios rubyGemsFixture, scenarioName item == "worker-mirroring"] `shouldBe` [True]
        it "resolves every real native workload pin before selecting artifacts" $ do
            captures <- loadCorpusBodies rubyGemsCorpusPackages
            selected <- selectWarmArtifacts rubyGemsCorpusPackages captures
            Map.keys selected `shouldMatchList` ["rails", "nokogiri", "aws-sdk-ec2"]
            for_ (Map.elems selected) $ \artifact -> saUpstreamUrl artifact `shouldBe` "https://rubygems.org/" <> saProxyPath artifact
        it "fails missing capture data instead of supplying selected placeholders" $
            selectWarmArtifacts rubyGemsCorpusPackages Map.empty `shouldThrow` (\(BenchLoadError _) -> True)
        it "keeps every native workload pin outside quarantine at its authenticated capture time" $ do
            captures <- loadCorpusBodies rubyGemsCorpusPackages
            records <- readCaptureRecords RubyGems >>= expectRight
            pins <- readCorpusPins (.: "rubygemsPins") >>= expectRight
            for_ rubyGemsCorpusPackages $ \package -> do
                let name = cpName package
                captured <- expectRightText (maybeToRight "missing authenticated capture record" (Map.lookup name records))
                bytes <- expectRightText (maybeToRight "missing authenticated capture bytes" (Map.lookup name captures))
                version <- expectRightText (maybeToRight "missing native workload pin" (Map.lookup name pins))
                manifest <- captureManifest (gemReadWith (const pass) (const pass) "https://rubygems.org") rubyGemsCaptureUpstream (cpPackage package) [toStrict bytes] >>= expectRight
                details <- expectRightText (maybeToRight "workload pin is absent from native metadata" (Map.lookup version (infoVersions (manifestInfo manifest))))
                published <- expectRightText (maybeToRight "workload pin has an unknown platform publication time" (pkgPublishedAt details))
                diffUTCTime (crCapturedAt captured) published `shouldSatisfy` (>= 7 * nominalDay)
    describe "native finite replay URLs" $ do
        for_ [False, True] $ \withArtifacts ->
            it (if withArtifacts then "preserves native listing and artifact order from a cold start" else "replays per-gem info requests from a cold start") $ do
                let provider = rubyGemsPatternProvider
                    names = map cpName rubyGemsCorpusPackages
                captures <- loadCorpusBodies rubyGemsCorpusPackages
                selected <- ppSelectedArtifacts provider ("pinned" <$ guard withArtifacts) rubyGemsCorpusPackages captures
                Map.keys selected `shouldMatchList` if withArtifacts then names else []
                let replies = Map.fromList [(name, infoReply) | name <- names]
                seen <- newIORef []
                let artifacts = Map.fromList [(encodeUtf8 ("/rubygems/" <> saProxyPath artifact), "owned native artifact bytes") | artifact <- Map.elems selected]
                    app request respond = do
                        modifyIORef' seen ((rawPathInfo request, rawQueryString request) :)
                        let route = case pathInfo request of
                                "rubygems" : path -> claimedOn rubyGemsRoutes (requestMethod request) path
                                _ -> Nothing
                            body = case route of
                                Just (RouteName "info") -> case pathInfo request of
                                    ["rubygems", "info", name] | BS.null (rawQueryString request) -> Map.lookup name replies
                                    _ -> Nothing
                                Just (RouteName "artifact") | BS.null (rawQueryString request) -> Map.lookup (rawPathInfo request) artifacts
                                _ -> Nothing
                        respond (maybe (responseLBS status400 [] "invalid native request") (responseLBS status200 []) body)
                testWithApplication (pure app) $ \port -> do
                    let replay root = Replay (RequestTrace [ClientTrace 0 0 names] names) 5_000_000 (patternRequestUrls provider root selected) (pure "")
                        native = replay (ppRootUrl provider port)
                        expected = concat [[(encodeUtf8 ("/rubygems/info/" <> name), "")] <> [(encodeUtf8 ("/rubygems/" <> saProxyPath artifact), "") | artifact <- maybeToList (Map.lookup name selected)] | name <- names]
                    warmUp (DriveReplay native)
                    readIORef seen `shouldReturn` []
                    report <- replayHttp <$> runReplay native
                    ohaStatusCounts report `shouldBe` Map.singleton "200" (length expected)
                    reverse <$> readIORef seen `shouldReturn` expected
                    writeIORef seen []
                    malformed <- replayHttp <$> runReplay (replay (pcUrl rubyGemsPrivateCopy port ""))
                    ohaStatusCounts malformed `shouldBe` Map.singleton "400" (length expected)
                    ohaSuccessLatencies malformed `shouldBe` []
        it "rejects unsupported selected-version modes before replay" $
            ppSelectedArtifacts rubyGemsPatternProvider (Just "latest") [] Map.empty `shouldThrow` (\(BenchLoadError _) -> True)
    describe "native info working sets" $ do
        it "selects the weighted corpus, one history and the configured many-project set" $ do
            fmap (map cpName) (infoPackages RepeatedCorpus defaultLoadKnobs) `shouldBe` Right ["rails", "nokogiri", "aws-sdk-ec2"]
            fmap (map cpName) (infoPackages OneProject defaultLoadKnobs) `shouldBe` Right ["rails"]
            fmap (map cpName) (infoPackages ManyProjects defaultLoadKnobs) `shouldBe` Right ["rails", "nokogiri", "aws-sdk-ec2"]
            fmap (map cpName) (infoPackages ManyProjects defaultLoadKnobs{lkWorkingSet = 2}) `shouldBe` Right ["rails", "nokogiri"]
        it "refuses a many-project singleton before reading captures or starting a proxy" $
            for_ [-1, 0, 1] $ \count -> do
                fmap (map cpName) (infoPackages ManyProjects defaultLoadKnobs{lkWorkingSet = count})
                    `shouldBe` Left "info-working-set-many requires at least two corpus projects"
                case find ((== "info-working-set-many") . scenarioName) (fixtureScenarios rubyGemsFixture) of
                    Nothing -> expectationFailure "missing native many-project scenario"
                    Just item ->
                        scenarioBoot item defaultLoadKnobs{lkWorkingSet = count} (const (expectationFailure "singleton reached measurement"))
                            `shouldThrow` (\(BenchLoadError message) -> message == "info-working-set-many requires at least two corpus projects")
        for_ [RepeatedCorpus, OneProject, ManyProjects] $ \workload ->
            it ("preflights every selected source and rejects each omitted reply for " <> show workload) $ do
                packages <- expectRight (infoPackages workload defaultLoadKnobs)
                let replies = Map.fromList [(cpName package, infoReply) | package <- packages]
                    emptyReply = "---\n"
                let names = map cpName packages
                    run :: Map Text LByteString -> IO [Text]
                    run available = do
                        seen <- newIORef []
                        let app request respond = do
                                let name = case pathInfo request of
                                        ["rubygems", "info", package] -> package
                                        _ -> ""
                                modifyIORef' seen (name :)
                                respond (responseLBS status200 [] (Map.findWithDefault emptyReply name available))
                        testWithApplication (pure app) $ \port -> do
                            let urls = map (pcUrl rubyGemsPrivateCopy port) names
                            for_ urls $ \url -> do
                                request <- HTTP.parseRequest (toString url)
                                path <- expectRightText (maybeToRight "info target is outside its mount" (T.stripPrefix "/rubygems/" (decodeUtf8 (HTTP.path request))))
                                claimedOn rubyGemsRoutes methodGet (T.splitOn "/" path) `shouldBe` Just (RouteName "info")
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
                                path <- expectRightText (maybeToRight "relay target is outside its mount" (T.stripPrefix "/rubygems/" (decodeUtf8 (HTTP.path request))))
                                claimedOn rubyGemsRoutes methodGet (T.splitOn "/" path) `shouldBe` Just (RouteName "artifact")
                                private <- fetchChecked (if privateHit then status200 else status404) [] (localhost privatePort <> "/" <> path)
                                public <- fetchChecked (if privateHit then status404 else status200) [] (localhost publicPort <> "/" <> path)
                                HTTP.responseBody private `shouldBe` if privateHit then payload else "missing"
                                HTTP.responseBody public `shouldBe` if privateHit then "missing" else payload
    describe "streaming relay source framing" $
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

infoReply :: LByteString
infoReply = "---\n1.0.0 |checksum:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef\n"
