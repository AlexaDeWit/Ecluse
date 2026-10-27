-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Native batch limits and representation headers exercise the routed admission path.
module Ecluse.Core.Registry.RubyGems.HandlersSpec (spec) where

import Codec.Compression.GZip qualified as GZip
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Network.HTTP.Types (Header, Method, Status, status200, status304, status400, status401, status403, status404, status406, status502, status503)
import Network.Wai (Request (..), defaultRequest, pathInfo, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Test (SRequest (SRequest), SResponse (..), runSession, setPath, srequest)
import Test.Hspec

import Ecluse.Core.Credential (mkSecret)
import Ecluse.Core.Registry.RubyGems.Adapter (rubyGemsAdapter)
import Ecluse.Core.Registry.RubyGems.Credential (rubyReadCredential)
import Ecluse.Core.Registry.RubyGems.FirstParty (projectFirstPartyEntry, rubyGemsFirstPartyName)
import Ecluse.Core.Registry.RubyGems.Handlers (parseBatchQuery)
import Ecluse.Core.Registry.RubyGems.Project (projectName)
import Ecluse.Core.Registry.RubyGems.Route (rubyGemsRouter)
import Ecluse.Core.Rules (prepare)
import Ecluse.Core.Rules.Types (Rule (AllowIfOlderThan))
import Ecluse.Core.Security (Limits (..), defaultLimits)
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Admission (newServeAdmissionTuned)
import Ecluse.Core.Server.Admission.Artifact (newVerifiedArtifactBudget)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (ChargeFactors))
import Ecluse.Core.Server.Cache (Source (Source), newMetadataCacheWithProvider)
import Ecluse.Core.Server.Cache.Backend (BackendStorage (ExternalStorage))
import Ecluse.Core.Server.Cache.Provider (cacheProvider)
import Ecluse.Core.Server.Context (MountBinding (..), PackumentDeps (..), ReadSourceSelection (MergePrivateAndPublic, PrivateOnly), RequestCtx (RequestCtx), ResponseAction (..), RouteAction (RouteAction), ServeRuntime (srMetadataCache), pdPublicBaseUrl, runHandler)
import Ecluse.Core.Server.Contract (responseToWai)
import Ecluse.Core.Server.Encoding (MetadataCompression (GzipWhenAccepted))
import Ecluse.Core.Server.Upstream (MirrorServePlan (NoMirrorWrite))
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Log (newTestLogEnv)
import Ecluse.Test.Port (noopMetricsPort)
import Ecluse.Test.Registry.RubyGems.Fixtures (RubyRelease (..), loadRubyReleasesFrom, rubyRelease)
import Ecluse.Test.Rules (atDefaultPrecedence, inertRuleDeps)
import Ecluse.Test.Server.Cache (cachedMetadata, externalOperations)
import Ecluse.Test.Server.Handler (testServeRuntimeWith)
import Ecluse.Test.Server.Mount (serveDepsFor)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = do
    describe "bounded native query syntax" $ do
        it "accepts the released Bundler default batch of 100 names" $
            fmap length (parseBatchQuery defaultLimits ("?gems=" <> encodeUtf8 (T.intercalate "," ["gem" <> show n | n <- [1 :: Int .. 100]]))) `shouldBe` Right 100
        it "accepts a valid default batch with long legacy names" $
            fmap length (parseBatchQuery defaultLimits ("?gems=" <> encodeUtf8 (T.intercalate "," [T.replicate 200 "a" <> show n | n <- [1 :: Int .. 100]]))) `shouldBe` Right 100
        it "refuses malformed escapes, duplicate parameters, unknown parameters and empty names" $
            for_ ["?gems=one&gems=two", "?gems=one&other=two", "?gems=one%2", "?gems=one,,two", "?gems=", "?gems"] $
                \query -> parseBatchQuery defaultLimits query `shouldSatisfy` isLeft
        it "refuses the raw byte cap before parsing" $
            parseBatchQuery defaultLimits{maxMetadataBytes = 8} "?gems=example" `shouldSatisfy` isLeft
    describe "one-ticket native batching" $ do
        it "serves two gems with a single CPU slot and no nested admission" $
            withNative $ \runtime binding hits -> do
                response <- capture runtime binding "GET" "/api/v1/dependencies?gems=ecluse-fixture-root,ecluse-fixture-leaf" []
                simpleStatus response `shouldBe` status200
                simpleBody response `shouldSatisfy` BS.isInfixOf "ecluse-fixture-root" . LBS.toStrict
                simpleBody response `shouldSatisfy` BS.isInfixOf "ecluse-fixture-leaf" . LBS.toStrict
                length <$> readIORef hits `shouldReturn` 2
        it "answers an empty capability probe without upstream reads" $
            withNative $ \runtime binding hits -> do
                response <- capture runtime binding "GET" "/api/v1/dependencies" []
                simpleStatus response `shouldBe` status200
                simpleBody response `shouldBe` "\x04\x08[\x00"
                readIORef hits `shouldReturn` []
        it "refuses malformed and oversized batches before reads" $
            withNative $ \runtime binding hits -> do
                for_ ["/api/v1/dependencies?gems=one&gems=two", "/api/v1/dependencies?gems=" <> encodeUtf8 (T.intercalate "," ["gem" <> show n | n <- [1 :: Int .. 101]])] $ \path -> do
                    response <- capture runtime binding "GET" path []
                    simpleStatus response `shouldBe` status400
                readIORef hits `shouldReturn` []
        it "counts artifact rows cumulatively across named gems" $
            withNative $ \runtime binding hits -> do
                let deps = bindingPackumentDeps binding
                    bounded = binding{bindingPackumentDeps = deps{pdLimits = (pdLimits deps){maxArtifactCount = 3}}}
                response <- capture runtime bounded "GET" "/api/v1/dependencies?gems=ecluse-fixture-root,ecluse-fixture-leaf" []
                simpleStatus response `shouldBe` status400
                length <$> readIORef hits `shouldReturn` 2
        it "keeps protected Basic reads and caller presentation" $
            withNative $ \runtime binding hits -> do
                let secured = binding{bindingPackumentDeps = (bindingPackumentDeps binding){pdInboundToken = Just (mkSecret "secret")}}
                for_ ["/quick/Marshal.4.8/ecluse-fixture-root-1.0.0.gemspec.rz", "/gems/ecluse-fixture-root-1.0.0.gem"] $ \path -> do
                    unauthorised <- capture runtime secured "GET" path []
                    simpleStatus unauthorised `shouldBe` status401
                readIORef hits `shouldReturn` []
                denied <- capture runtime secured "GET" "/api/v1/dependencies?gems=ecluse-fixture-root" [("Authorization", "secret")]
                simpleStatus denied `shouldBe` status401
                allowed <- capture runtime secured "GET" "/api/v1/dependencies?gems=ecluse-fixture-root" [("Authorization", "Basic YWxpY2U6c2VjcmV0")]
                simpleStatus allowed `shouldBe` status200
                observed <- readIORef hits
                map (lookup "Authorization" . snd) observed `shouldBe` [Just "Basic YWxpY2U6c2VjcmV0"]
        it "fails a batch when cumulative actual upstream bytes exceed the cap" $
            withNative $ \runtime binding _ -> do
                root <- BS.readFile "test/fixtures/rubygems/info/ecluse-fixture-root"
                leaf <- BS.readFile "test/fixtures/rubygems/info/ecluse-fixture-leaf"
                let deps = bindingPackumentDeps binding
                    cap = max (BS.length root) (BS.length leaf) + 1
                    bounded = binding{bindingPackumentDeps = deps{pdLimits = (pdLimits deps){maxMetadataBytes = cap}}}
                response <- capture runtime bounded "GET" "/api/v1/dependencies?gems=ecluse-fixture-root,ecluse-fixture-leaf" []
                simpleStatus response `shouldBe` status502
        it "relays original validated compact bytes and shares HEAD admission" $
            withNative $ \runtime binding _ -> do
                original <- BS.readFile "test/fixtures/rubygems/quick/Marshal.4.8/ecluse-fixture-root-1.0.0.gemspec.rz"
                response <- capture runtime binding "GET" "/quick/Marshal.4.8/ecluse-fixture-root-1.0.0.gemspec.rz" []
                simpleStatus response `shouldBe` status200
                simpleBody response `shouldBe` fromStrict original
                headed <- capture runtime binding "HEAD" "/quick/Marshal.4.8/ecluse-fixture-root-1.0.0.gemspec.rz" []
                simpleStatus headed `shouldBe` status200
                simpleBody headed `shouldBe` ""
                lookup "Content-Length" (simpleHeaders headed) `shouldBe` Just (show (BS.length original))
        it "resolves an exact archive through the shared artifact relay" $
            withNative $ \runtime binding _ -> do
                original <- BS.readFile "test/fixtures/rubygems/gems/ecluse-fixture-root-1.0.0.gem"
                response <- capture runtime binding "GET" "/gems/ecluse-fixture-root-1.0.0.gem" []
                simpleStatus response `shouldBe` status200
                simpleBody response `shouldBe` fromStrict original
        it "shares GET admission with HEAD and withholds its body" $
            withNative $ \runtime binding _ -> do
                headed <- capture runtime binding "HEAD" "/api/v1/dependencies?gems=ecluse-fixture-root" []
                simpleStatus headed `shouldBe` status200
                simpleBody headed `shouldBe` ""
                probe <- capture runtime binding "HEAD" "/versions" []
                simpleStatus probe `shouldBe` status404

    describe "first-party coordinate candidates" $ do
        for_ ["GET", "HEAD"] $ \method ->
            for_ ["/gems/acme-1-1.0.gem", "/quick/Marshal.4.8/acme-1-1.0.gemspec.rz"] $ \path ->
                it ("continues after an absent owned candidate for " <> show method <> " " <> show path) $
                    withAmbiguousNative status404 $ \runtime binding hits release -> do
                        response <- capture runtime binding method path []
                        let body = if path == "/gems/acme-1-1.0.gem" then releaseArchive release else releaseCompact release
                        checkCoordinateResponse method path body response
                        observed <- readIORef hits
                        observed `shouldSatisfy` elem ["private", "info", "acme"]
                        observed `shouldSatisfy` elem ["public", "info", "acme-1"]
                        observed `shouldSatisfy` notElem ["public", "info", "acme"]
        for_ ["GET", "HEAD"] $ \method ->
            for_ ["/gems/acme-1.0.gem", "/quick/Marshal.4.8/acme-1.0.gemspec.rz"] $ \path ->
                it ("keeps actual owned absence as a 404 for " <> show method <> " " <> show path) $
                    withAmbiguousNative status404 $ \runtime binding hits _ -> do
                        response <- capture runtime binding method path []
                        simpleStatus response `shouldBe` status404
                        readIORef hits `shouldReturn` [["private", "info", "acme"]]
        for_ [(status401, status401), (status403, status403), (status502, status503), (status503, status503)] $ \(upstream, expected) ->
            for_ ["GET", "HEAD"] $ \method ->
                for_ ["/gems/acme-1-1.0.gem", "/quick/Marshal.4.8/acme-1-1.0.gemspec.rz"] $ \path ->
                    it ("preserves an owned candidate refusal " <> show upstream <> " for " <> show method <> " " <> show path) $
                        withAmbiguousNative upstream $ \runtime binding hits _ -> do
                            response <- capture runtime binding method path []
                            simpleStatus response `shouldBe` expected
                            readIORef hits `shouldReturn` [["private", "info", "acme"]]

    describe "policy-denied coordinate candidates" $ do
        for_ ["GET", "HEAD"] $ \method ->
            for_ ["/gems/acme-1-1.0.gem", "/quick/Marshal.4.8/acme-1-1.0.gemspec.rz"] $ \path ->
                it ("admits the longer gem beside an unrelated denied candidate for " <> show method <> " " <> show path) $
                    withCoordinateSources PublicCandidateSource $ \runtime binding hits release -> do
                        response <- capture runtime binding method path []
                        let body = if path == "/gems/acme-1-1.0.gem" then releaseArchive release else releaseCompact release
                        checkCoordinateResponse method path body response
                        observed <- readIORef hits
                        observed `shouldSatisfy` elem ["public", "info", "acme"]
                        observed `shouldSatisfy` elem ["public", "info", "acme-1"]
                        observed `shouldSatisfy` all (\parts -> take 1 parts == ["public"])
                        observed `shouldSatisfy` notElem ["public", "gems", "acme-2.0.gem"]
                        observed `shouldSatisfy` notElem ["public", "quick", "Marshal.4.8", "acme-2.0.gemspec.rz"]
        for_ ["GET", "HEAD"] $ \method ->
            for_ ["/gems/acme-2.0.gem", "/quick/Marshal.4.8/acme-2.0.gemspec.rz"] $ \path ->
                for_ [False, True] $ \warm ->
                    it ("retains denial for the actual recent coordinate for " <> show (warm, method, path)) $
                        withCoordinateSources PublicCandidateSource $ \uncached original hits _ -> do
                            let binding = coordinateByteCap 200 original
                            runtime <- if warm then warmCoordinateCache uncached binding hits else pure uncached
                            response <- capture runtime binding method path []
                            simpleStatus response `shouldBe` status403
                            readIORef hits `shouldReturn` [["public", "info", "acme"] | not warm]

    describe "coordinate source document limits" $ do
        for_ ["GET", "HEAD"] $ \method ->
            for_ ["/gems/acme-1-1.0.gem", "/quick/Marshal.4.8/acme-1-1.0.gemspec.rz"] $ \path -> do
                it ("counts warm denied source bytes for " <> show method <> " " <> show path) $
                    withCoordinateSources PublicCandidateSource $ \uncached original hits release -> do
                        let binding = coordinateByteCap 200 original
                            (recent, admitted) = coordinateListings "" release
                        map BS.length [recent, admitted] `shouldSatisfy` all (<= 200)
                        BS.length recent + BS.length admitted `shouldSatisfy` (> 200)
                        runtime <- warmCoordinateCache uncached binding hits
                        response <- capture runtime binding method path []
                        simpleStatus response `shouldBe` status400
                        readIORef hits `shouldReturn` []
                for_ [False, True] $ \warm -> do
                    it ("refuses one byte below the source total for " <> show (warm, method, path)) $
                        withCoordinateSourceHeader coordinateBudgetHeader PublicCandidateSource $ \uncached original hits release -> do
                            let (recent, admitted) = coordinateListings coordinateBudgetHeader release
                                cap = BS.length recent + BS.length admitted - 1
                                binding = coordinateByteCap cap original
                            map BS.length [recent, admitted] `shouldSatisfy` all (<= cap)
                            runtime <- if warm then warmCoordinateCache uncached binding hits else pure uncached
                            response <- capture runtime binding method path []
                            simpleStatus response `shouldBe` if warm then status400 else status502
                            observed <- readIORef hits
                            if warm
                                then observed `shouldBe` []
                                else sort observed `shouldBe` [["public", "info", "acme"], ["public", "info", "acme-1"]]
                    it ("accepts the exact input total for " <> show (warm, method, path)) $
                        withCoordinateSourceHeader coordinateBudgetHeader PublicCandidateSource $ \uncached original hits release -> do
                            let (recent, admitted) = coordinateListings coordinateBudgetHeader release
                                compact = path == "/quick/Marshal.4.8/acme-1-1.0.gemspec.rz"
                                body = if compact then releaseCompact release else releaseArchive release
                                sourceTotal = BS.length recent + BS.length admitted
                                cap = sourceTotal + if compact && not warm then BS.length body else 0
                                binding = coordinateByteCap cap original
                            runtime <- if warm then warmCoordinateCache uncached binding hits else pure uncached
                            response <- capture runtime binding method path []
                            checkCoordinateResponse method path body response
                            observed <- readIORef hits
                            let listings = filter (\parts -> take 2 parts == ["public", "info"]) observed
                            let selectedRead = [["public", "info", "acme-1"] | not compact]
                                candidateReads = if warm then [] else [["public", "info", "acme"], ["public", "info", "acme-1"]]
                            sort listings `shouldBe` sort (candidateReads <> selectedRead)

        for_ ["GET", "HEAD"] $ \method ->
            it ("refuses the final cold compact bytes one byte below the wire total for " <> show method) $
                withCoordinateSourceHeader coordinateBudgetHeader PublicCandidateSource $ \runtime original hits release -> do
                    let (recent, admitted) = coordinateListings coordinateBudgetHeader release
                        sourceTotal = BS.length recent + BS.length admitted
                        cap = sourceTotal + BS.length (releaseCompact release) - 1
                        binding = coordinateByteCap cap original
                    sourceTotal `shouldSatisfy` (<= cap)
                    response <- capture runtime binding method "/quick/Marshal.4.8/acme-1-1.0.gemspec.rz" []
                    simpleStatus response `shouldBe` status502
                    readIORef hits
                        `shouldReturn` [ ["public", "info", "acme"]
                                       , ["public", "info", "acme-1"]
                                       , ["public", "quick", "Marshal.4.8", "acme-1-1.0.gemspec.rz"]
                                       ]

    describe "native metadata representations" $ do
        for_ metadataPaths $ \path ->
            it ("shares coding, HEAD and conditional responses for " <> toString (decodeUtf8 path :: Text)) $
                withNative $ \runtime original _ -> do
                    let binding = compressing original
                        serve method = capture runtime binding method path
                    plain <- serve "GET" []
                    packed <- serve "GET" [("Accept-Encoding", "gzip")]
                    map simpleStatus [plain, packed] `shouldBe` [status200, status200]
                    GZip.decompress (simpleBody packed) `shouldBe` simpleBody plain
                    lookup "Content-Encoding" (simpleHeaders packed) `shouldBe` Just "gzip"
                    map (lookup "Vary" . simpleHeaders) [plain, packed] `shouldBe` replicate 2 (Just "Accept-Encoding")
                    plainTag <- responseTag plain
                    gzipTag <- responseTag packed
                    gzipTag `shouldNotBe` plainTag
                    for_ [([], plain, plainTag), ([("Accept-Encoding", "gzip")], packed, gzipTag)] $ \(offered, fetched, tag) -> do
                        headed <- serve "HEAD" offered
                        simpleStatus headed `shouldBe` status200
                        simpleBody headed `shouldBe` ""
                        lookup "Content-Length" (simpleHeaders headed) `shouldBe` Just (show (LBS.length (simpleBody fetched)))
                        lookup "ETag" (simpleHeaders headed) `shouldBe` Just tag
                        for_ ["GET", "HEAD"] $ \method -> do
                            matched <- serve method (("If-None-Match", tag) : offered)
                            simpleStatus matched `shouldBe` status304
                            simpleBody matched `shouldBe` ""
                            lookup "ETag" (simpleHeaders matched) `shouldBe` Just tag
                            lookup "Vary" (simpleHeaders matched) `shouldBe` Just "Accept-Encoding"
                    crossed <- serve "GET" [("Accept-Encoding", "gzip"), ("If-None-Match", plainTag)]
                    simpleStatus crossed `shouldBe` status200
                    excluded <- serve "GET" [("Accept-Encoding", "gzip;q=0, identity;q=0"), ("If-None-Match", "*")]
                    simpleStatus excluded `shouldBe` status406
        it "binds a batch validator to its complete ordered admitted bytes" $
            withNative $ \runtime original _ -> do
                let binding = compressing original
                    firstPath = "/api/v1/dependencies?gems=ecluse-fixture-root"
                    paths =
                        [ firstPath
                        , "/api/v1/dependencies?gems=ecluse-fixture-root,ecluse-fixture-leaf"
                        , "/api/v1/dependencies?gems=ecluse-fixture-leaf,ecluse-fixture-root"
                        ]
                initial <- capture runtime binding "GET" firstPath []
                tag <- responseTag initial
                responses <- traverse (\path -> capture runtime binding "GET" path [("If-None-Match", tag)]) paths
                map simpleStatus responses `shouldBe` [status304, status200, status200]
                tags <- traverse responseTag responses
                length (ordNub tags) `shouldBe` 3
        it "does not let a wildcard validator bypass authentication, bounds or coordinate admission" $
            withNative $ \runtime original hits -> do
                let binding = compressing original
                    secured = binding{bindingPackumentDeps = (bindingPackumentDeps binding){pdInboundToken = Just (mkSecret "secret")}}
                    wildcard = [("If-None-Match", "*")]
                for_ metadataPaths $ \path -> do
                    refused <- capture runtime secured "GET" path wildcard
                    simpleStatus refused `shouldBe` status401
                readIORef hits `shouldReturn` []
                malformed <- capture runtime binding "GET" "/api/v1/dependencies?gems=one&gems=two" wildcard
                simpleStatus malformed `shouldBe` status400
                readIORef hits `shouldReturn` []
                missing <- capture runtime binding "GET" "/quick/Marshal.4.8/ecluse-fixture-root-99.0.0.gemspec.rz" wildcard
                simpleStatus missing `shouldBe` status404
                let deps = bindingPackumentDeps binding
                    bounded = binding{bindingPackumentDeps = deps{pdLimits = (pdLimits deps){maxArtifactCount = 3}}}
                oversized <- capture runtime bounded "GET" "/api/v1/dependencies?gems=ecluse-fixture-root,ecluse-fixture-leaf" wildcard
                simpleStatus oversized `shouldBe` status400

metadataPaths :: [ByteString]
metadataPaths =
    [ "/api/v1/dependencies"
    , "/api/v1/dependencies?gems=ecluse-fixture-root"
    , "/api/v1/dependencies?gems=ecluse-fixture-root,ecluse-fixture-leaf"
    , "/quick/Marshal.4.8/ecluse-fixture-root-1.0.0.gemspec.rz"
    ]

compressing :: MountBinding -> MountBinding
compressing binding = binding{bindingPackumentDeps = (bindingPackumentDeps binding){pdMetadataCompression = GzipWhenAccepted}}

responseTag :: SResponse -> IO ByteString
responseTag response = maybe (expectationFailure "the admitted metadata response omitted its ETag" >> pure "") pure (lookup "ETag" (simpleHeaders response))

withNative :: (ServeRuntime -> MountBinding -> IORef [([Text], [Header])] -> IO a) -> IO a
withNative use = do
    root <- BS.readFile "test/fixtures/rubygems/info/ecluse-fixture-root"
    leaf <- BS.readFile "test/fixtures/rubygems/info/ecluse-fixture-leaf"
    compact <- BS.readFile "test/fixtures/rubygems/quick/Marshal.4.8/ecluse-fixture-root-1.0.0.gemspec.rz"
    archive <- BS.readFile "test/fixtures/rubygems/gems/ecluse-fixture-root-1.0.0.gem"
    hits <- newIORef []
    let fixtures = Map.fromList [("ecluse-fixture-root", root), ("ecluse-fixture-leaf", leaf)]
        upstream request respond = do
            modifyIORef' hits (<> [(pathInfo request, requestHeaders request)])
            case pathInfo request of
                ["info", name] | Just body <- Map.lookup name fixtures -> respond (responseLBS status200 [] (fromStrict body))
                ["quick", "Marshal.4.8", "ecluse-fixture-root-1.0.0.gemspec.rz"] -> respond (responseLBS status200 [] (fromStrict compact))
                ["gems", "ecluse-fixture-root-1.0.0.gem"] -> respond (responseLBS status200 [] (fromStrict archive))
                _ -> respond (responseLBS status404 [] "")
    testWithApplication (pure upstream) $ \port -> do
        let origin = loopbackRegistryUrl ("http://localhost:" <> show port)
        withNativeOrigins (Just origin) origin (BS.length archive) $ \runtime binding -> use runtime binding hits

withNativeOrigins :: Maybe RegistryUrl -> RegistryUrl -> Int -> (ServeRuntime -> MountBinding -> IO a) -> IO a
withNativeOrigins private public cap use = withSweeper $ \deadlines -> do
    admission <- newServeAdmissionTuned deadlines 1 0 0
    runtime <- testServeRuntimeWith deadlines admission noopMetricsPort
    verified <- expectRight =<< newVerifiedArtifactBudget deadlines cap cap
    let adapter = rubyGemsAdapter (ChargeFactors 1000 1000 1000)
        deps = (serveDepsFor adapter private public NoMirrorWrite [] (pure (UTCTime (fromGregorian 2030 1 1) 0))){pdReadSourceSelection = PrivateOnly, pdVerifiedArtifacts = Just verified}
        binding = MountBinding ("rubygems" :| []) rubyGemsRouter rubyReadCredential deps Nothing
    use runtime binding

data CoordinateSources = OwnedCandidateSource Status | PublicCandidateSource

withAmbiguousNative :: Status -> (ServeRuntime -> MountBinding -> IORef [[Text]] -> RubyRelease -> IO a) -> IO a
withAmbiguousNative = withCoordinateSources . OwnedCandidateSource

withCoordinateSources :: CoordinateSources -> (ServeRuntime -> MountBinding -> IORef [[Text]] -> RubyRelease -> IO a) -> IO a
withCoordinateSources = withCoordinateSourceHeader ""

withCoordinateSourceHeader :: ByteString -> CoordinateSources -> (ServeRuntime -> MountBinding -> IORef [[Text]] -> RubyRelease -> IO a) -> IO a
withCoordinateSourceHeader header sources use = do
    releases <- loadRubyReleasesFrom "test/fixtures/rubygems/coordinate-ambiguity"
    release <- rubyRelease releases "acme-1" "1.0" "ruby"
    hits <- newIORef []
    owned <- expectRight (projectFirstPartyEntry "acme")
    let age = case sources of OwnedCandidateSource _ -> 0; PublicCandidateSource -> 604800
    rules <- prepare inertRuleDeps [atDefaultPrecedence (AllowIfOlderThan age)]
    let (recent, admitted) = coordinateListings header release
        upstream request respond = do
            atomicModifyIORef' hits (\events -> (events <> [pathInfo request], ()))
            let answer status bytes = respond (responseLBS status [] (fromStrict bytes))
            case pathInfo request of
                ["private", "info", "acme"] -> answer (case sources of OwnedCandidateSource status -> status; PublicCandidateSource -> status404) ""
                ["public", "info", "acme"] -> answer status200 recent
                ["public", "info", "acme-1"] -> answer status200 admitted
                ["public", "gems", "acme-1-1.0.gem"] -> respond (responseLBS status200 [("Content-Length", show (BS.length (releaseArchive release)))] (fromStrict (releaseArchive release)))
                ["public", "quick", "Marshal.4.8", "acme-1-1.0.gemspec.rz"] -> answer status200 (releaseCompact release)
                _ -> answer status404 ""
    testWithApplication (pure upstream) $ \port -> do
        let base = "http://localhost:" <> show port
            private = case sources of OwnedCandidateSource _ -> Just (loopbackRegistryUrl (base <> "/private/")); PublicCandidateSource -> Nothing
            firstParty = case sources of OwnedCandidateSource _ -> rubyGemsFirstPartyName (owned :| []); PublicCandidateSource -> const False
        withNativeOrigins private (loopbackRegistryUrl (base <> "/public/")) (BS.length (releaseArchive release)) $ \runtime binding -> do
            let deps = bindingPackumentDeps binding
                selected = binding{bindingPackumentDeps = deps{pdReadSourceSelection = MergePrivateAndPublic, pdFirstParty = firstParty, pdRules = rules}}
            use runtime selected hits release

checkCoordinateResponse :: Method -> ByteString -> ByteString -> SResponse -> Expectation
checkCoordinateResponse method path body response = do
    simpleStatus response `shouldBe` status200
    simpleBody response `shouldBe` if method == "HEAD" then "" else fromStrict body
    let advertised = lookup "Content-Length" (simpleHeaders response)
        expected = show (BS.length body)
    if method == "HEAD" || path == "/gems/acme-1-1.0.gem"
        then advertised `shouldBe` Just expected
        else for_ advertised (`shouldBe` expected)

coordinateListings :: ByteString -> RubyRelease -> (ByteString, ByteString)
coordinateListings header release =
    ( header <> encodeUtf8 ("---\n2.0 |checksum:" <> releaseChecksum release <> ",ruby:>= 3.2,rubygems:>= 3.4,created_at:2029-12-31T00:00:00Z\n")
    , header <> encodeUtf8 ("---\n" <> releaseInfoRow release <> "\n")
    )

-- The native header is opaque. Its padding lets the source cap also fit an inflated compact specification.
coordinateBudgetHeader :: ByteString
coordinateBudgetHeader = "created_at: 2020-01-01T00:00:00Z\nx-test-padding: " <> BS.replicate 256 120 <> "\n"

coordinateByteCap :: Int -> MountBinding -> MountBinding
coordinateByteCap cap binding =
    let deps = bindingPackumentDeps binding
     in binding{bindingPackumentDeps = deps{pdLimits = (pdLimits deps){maxMetadataBytes = cap}}}

warmCoordinateCache :: ServeRuntime -> MountBinding -> IORef [[Text]] -> IO ServeRuntime
warmCoordinateCache uncached binding hits = do
    values <- newIORef Map.empty
    let full = externalOperations (\_ key -> Map.lookup key <$> readIORef values) (\key value -> atomicModifyIORef' values (\stored -> (Map.insert key value stored, ())))
    cache <- newMetadataCacheWithProvider (cacheProvider (ExternalStorage 100000) (Just full) Nothing Nothing)
    let runtime = uncached{srMetadataCache = cache}
        source = Source (registryUrlText (pdPublicBaseUrl (bindingPackumentDeps binding)))
    denied <- capture runtime binding "GET" "/api/v1/dependencies?gems=acme" []
    simpleStatus denied `shouldBe` status403
    admitted <- capture runtime binding "GET" "/api/v1/dependencies?gems=acme-1" []
    simpleStatus admitted `shouldBe` status200
    readIORef hits `shouldReturn` [["public", "info", "acme"], ["public", "info", "acme-1"]]
    for_ ["acme", "acme-1"] $ \raw -> do
        name <- expectRight (projectName raw)
        retained <- cachedMetadata noopMetricsPort cache source name
        retained `shouldSatisfy` isJust
    writeIORef hits []
    repeatedDenied <- capture runtime binding "GET" "/api/v1/dependencies?gems=acme" []
    simpleStatus repeatedDenied `shouldBe` status403
    simpleBody repeatedDenied `shouldBe` simpleBody denied
    repeatedAdmitted <- capture runtime binding "GET" "/api/v1/dependencies?gems=acme-1" []
    simpleStatus repeatedAdmitted `shouldBe` status200
    simpleBody repeatedAdmitted `shouldBe` simpleBody admitted
    readIORef hits `shouldReturn` []
    pure runtime

capture :: ServeRuntime -> MountBinding -> Method -> ByteString -> [Header] -> IO SResponse
capture runtime binding method path headers = do
    logEnv <- newTestLogEnv
    let request = setPath defaultRequest{requestMethod = method, requestHeaders = headers} path
    runSession (srequest (SRequest request "")) $ \incoming respond ->
        case rubyGemsRouter (requestMethod incoming) (requestHeaders incoming) (pathInfo incoming) of
            RouteAction contract action -> case action of
                AnswerLocally answer -> respond (responseToWai contract answer)
                AnswerRefusal answer -> respond (responseToWai contract (answer (pdHelp (bindingPackumentDeps binding))))
                RunPipeline _ run -> runHandler logEnv mempty (RequestCtx runtime binding) (run incoming (respond . responseToWai contract))
