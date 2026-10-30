-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Native info and coordinate requests exercise admission and response contracts.
module Ecluse.Core.Registry.RubyGems.HandlersSpec (spec) where

import Codec.Compression.GZip qualified as GZip
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Katip (LogEnv)
import Network.HTTP.Types (Header, Method, Status, status200, status206, status304, status400, status401, status403, status404, status406, status502, status503)
import Network.Wai (Request (..), defaultRequest, pathInfo, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Test (SRequest (SRequest), SResponse (..), runSession, setPath, srequest)
import Test.Hspec

import Ecluse.Core.Credential (mkSecret)
import Ecluse.Core.Cve.Types (DbEtag (DbEtag))
import Ecluse.Core.Package (HashAlg (SHA512))
import Ecluse.Core.Package.Integrity (mkMinIntegrity, mkMinTrustedIntegrity)
import Ecluse.Core.Registry.RubyGems.Adapter (rubyGemsAdapter)
import Ecluse.Core.Registry.RubyGems.Credential (rubyReadCredential)
import Ecluse.Core.Registry.RubyGems.Directory (GemDirectory (GemDirectory), directoryWeight)
import Ecluse.Core.Registry.RubyGems.FirstParty (projectFirstPartyEntry, rubyGemsFirstPartyName)
import Ecluse.Core.Registry.RubyGems.Project (projectName)
import Ecluse.Core.Registry.RubyGems.Route (rubyGemsRouter)
import Ecluse.Core.Rules (PreparedRule (prepEval), RuleEval (PerInstallEvidence, PerPackage, PerVersion), prepare)
import Ecluse.Core.Rules.Types (FailureAlignment (FailDeny), Inability (NoDatabaseLoaded), Rule (AllowIfOlderThan), RuleEvidence (evVersion), RuleVerdict (CannotVet))
import Ecluse.Core.Security (Limits (..))
import Ecluse.Core.Security.Egress (RegistryUrl, registryUrlText)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Admission (newServeAdmissionTuned)
import Ecluse.Core.Server.Admission.Meter (MeterSettings (..), meterSnapshot, newMemoryMeter)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (ChargeFactors), MeterSnapshot (snChargedBytes))
import Ecluse.Core.Server.Cache (Source (Source), newMetadataCacheWithProvider)
import Ecluse.Core.Server.Cache.Backend (BackendStorage (ExternalStorage))
import Ecluse.Core.Server.Cache.Provider (cacheProvider)
import Ecluse.Core.Server.Context (MountBinding (..), PackumentDeps (..), ReadSourceSelection (MergePrivateAndPublic, PrivateOnly), RequestCtx (RequestCtx), ResponseAction (..), RouteAction (RouteAction), ServeRuntime (srMemoryMeter, srMetadataCache, srMetrics), pdPublicBaseUrl, runHandler)
import Ecluse.Core.Server.Contract (responseToWai)
import Ecluse.Core.Server.Encoding (MetadataCompression (GzipWhenAccepted))
import Ecluse.Core.Server.Upstream (MirrorServePlan (NoMirrorWrite))
import Ecluse.Core.Telemetry.Metrics qualified as Metric
import Ecluse.Core.Telemetry.Record (MetricsPort (mpRuleDenial))
import Ecluse.Core.Version (renderVersion)
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Log (captureJsonLog, lineMessage, newTestLogEnv)
import Ecluse.Test.Port (noopMetricsPort)
import Ecluse.Test.Registry.RubyGems.Fixtures (RubyRelease (..), loadRubyReleasesFrom, rubyRelease)
import Ecluse.Test.Rules (atDefaultPrecedence, inertRuleDeps)
import Ecluse.Test.Server.Cache (cachedMetadata, externalOperations)
import Ecluse.Test.Server.Handler (testServeRuntimeWith)
import Ecluse.Test.Server.Mount (serveDepsFor, withPrivateBaseUrl)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = do
    describe "native protocol routes" $ do
        for_ ["GET", "HEAD"] $ \method ->
            for_ ["/api/v1/dependencies", "/api/v1/dependencies?gems=ecluse-fixture-root", "/api/v1/dependencies?gems=one&gems=two"] $ \path ->
                it ("refuses checksum-free dependency fallback for " <> show (method, path)) $
                    withNative $ \runtime binding hits -> do
                        response <- capture runtime binding method path []
                        simpleStatus response `shouldBe` status404
                        readIORef hits `shouldReturn` []
        it "authenticates admitted info before reading a private source" $
            withNative $ \runtime binding hits -> do
                let secured = binding{bindingPackumentDeps = (bindingPackumentDeps binding){pdInboundToken = Just (mkSecret "secret")}}
                denied <- capture runtime secured "GET" "/info/ecluse-fixture-root" []
                simpleStatus denied `shouldBe` status401
                readIORef hits `shouldReturn` []
                allowed <- capture runtime secured "GET" "/info/ecluse-fixture-root" [("Authorization", "Basic YWxpY2U6c2VjcmV0")]
                simpleStatus allowed `shouldBe` status200
                observed <- readIORef hits
                map (lookup "Authorization" . snd) observed `shouldBe` [Just "Basic YWxpY2U6c2VjcmV0"]
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
                headed <- capture runtime binding "HEAD" "/info/ecluse-fixture-root" []
                simpleStatus headed `shouldBe` status200
                simpleBody headed `shouldBe` ""
                probe <- capture runtime binding "HEAD" "/versions" []
                simpleStatus probe `shouldBe` status200

    describe "current Compact Index authority" $ do
        it "authorises private reads before304 while reusing the public snapshot" $
            withDirectorySources $ \runtime binding hits privateStatus -> do
                let auth = [("Authorization", "Basic YWxpY2U6c2VjcmV0")]
                firstReply <- capture runtime binding "GET" "/versions" auth
                simpleStatus firstReply `shouldBe` status200
                validator <- maybe (expectationFailure "missing directory ETag" >> fail "missing directory ETag") pure (lookup "ETag" (simpleHeaders firstReply))
                unchanged <- capture runtime binding "GET" "/versions" (("If-None-Match", validator) : auth)
                simpleStatus unchanged `shouldBe` status304
                events <- readIORef hits
                map fst events `shouldBe` [["private", "versions"], ["public", "versions"], ["private", "versions"]]
                map (lookup "Authorization" . snd) events `shouldBe` [Just "Basic YWxpY2U6c2VjcmV0", Nothing, Just "Basic YWxpY2U6c2VjcmV0"]
                writeIORef privateStatus status403
                refused <- capture runtime binding "GET" "/versions" (("If-None-Match", validator) : auth)
                simpleStatus refused `shouldBe` status403
                finalEvents <- readIORef hits
                map fst finalEvents `shouldBe` [["private", "versions"], ["public", "versions"], ["private", "versions"], ["private", "versions"]]
        it "honours matching validators before replacing modified Range responses" $
            withDirectorySources $ \runtime binding _ _ -> do
                original <- capture runtime binding "GET" "/versions" []
                validator <- maybe (expectationFailure "missing directory ETag" >> fail "missing directory ETag") pure (lookup "ETag" (simpleHeaders original))
                unchanged <- capture runtime binding "GET" "/versions" [("Range", "bytes=10-"), ("If-None-Match", validator)]
                simpleStatus unchanged `shouldBe` status304
                simpleBody unchanged `shouldBe` ""
                replacement <- capture runtime binding "GET" "/versions" [("Range", "bytes=10-"), ("If-None-Match", "\"old-directory\"")]
                simpleStatus replacement `shouldBe` status200
                simpleBody replacement `shouldBe` simpleBody original
                lookup "Content-Range" (simpleHeaders replacement) `shouldBe` Nothing
                lookup "Repr-Digest" (simpleHeaders replacement) `shouldBe` Just "sha-256=\"xaNf1tjTCVlsE8k7j2wsswDnodbM0L3OP3+Q1vuN02c=\""
        it "gives back its directory read charges before it writes the merged body" $
            withDirectorySources $ \runtime binding _ _ -> do
                (held, response) <- chargedAtResponse runtime binding "/versions"
                simpleStatus response `shouldBe` status200
                -- The merge charge over the packed public and private views, which share no name.
                held `shouldBe` 2 * (directoryWeight (GemDirectory "public-gem 1.0 \n") + BS.length "private-gem 1.0 \n") + 4096
        it "refuses a merged directory one byte over the metadata bound" $
            withDirectorySources $ \runtime binding _ _ -> do
                let bounded limit = binding{bindingPackumentDeps = (bindingPackumentDeps binding){pdLimits = (pdLimits (bindingPackumentDeps binding)){maxMetadataBytes = limit}}}
                -- The header and the packed public and private rows take 37 bytes.
                simpleStatus <$> capture runtime (bounded 37) "GET" "/versions" [] `shouldReturn` status200
                simpleStatus <$> capture runtime (bounded 36) "GET" "/versions" [] `shouldReturn` status502
        it "keeps a native400 directory limit failure at the capability boundary" $
            withDirectorySources $ \runtime binding hits privateStatus -> do
                writeIORef privateStatus status400
                response <- capture runtime binding "GET" "/versions" []
                simpleStatus response `shouldBe` status502
                LBS.toStrict (simpleBody response) `shouldSatisfy` BS.isInfixOf "5,000"
                map fst <$> readIORef hits `shouldReturn` [["private", "versions"]]
        for_ [status206, status304] $ \originStatus ->
            it ("rejects an unsolicited directory response with " <> show originStatus) $
                withDirectorySources $ \runtime binding _ privateStatus -> do
                    writeIORef privateStatus originStatus
                    response <- capture runtime binding "GET" "/versions" []
                    simpleStatus response `shouldBe` status502
        it "serves exact admitted info rows without acquiring a directory" $
            withNative $ \runtime binding hits -> do
                original <- BS.readFile "test/fixtures/rubygems/info/ecluse-fixture-root"
                response <- capture runtime binding "GET" "/info/ecluse-fixture-root" []
                simpleStatus response `shouldBe` status200
                simpleBody response `shouldBe` fromStrict original
                map fst <$> readIORef hits `shouldReturn` [["info", "ecluse-fixture-root"]]

    describe "examined per-gem source documents" $ do
        for_ [False, True] $ \warm ->
            for_ [False, True] $ \exact ->
                for_ ["GET", "HEAD"] $ \method ->
                    it ("bounds denied public documents beside private survivors for " <> show (warm, exact, method)) $ do
                        privateStatus <- newIORef status200
                        withCoordinateSourceHeader coordinateBudgetHeader (CachedDeniedCandidateSource privateStatus) $ \uncached original hits release -> do
                            let (denied, paddedPrivate) = coordinateListings coordinateBudgetHeader release
                                private = BS.drop (BS.length coordinateBudgetHeader) paddedPrivate
                                oneNameBytes = BS.length denied + BS.length private
                                cap = oneNameBytes - if exact then 0 else 1
                                binding = coordinateByteCap cap original
                                deps = bindingPackumentDeps binding
                                public = binding{bindingPackumentDeps = withPrivateBaseUrl Nothing deps}
                            runtime <- if warm then warmPublicDocuments uncached public hits [("acme-1", status403)] else pure uncached
                            response <- capture runtime binding method "/info/acme-1" []
                            simpleStatus response `shouldBe` if exact then status200 else status502
                            when exact $
                                if method == "HEAD"
                                    then simpleBody response `shouldBe` ""
                                    else simpleBody response `shouldBe` fromStrict private
                            sort <$> readIORef hits `shouldReturn` if warm then [["private", "info", "acme-1"]] else [["private", "info", "acme-1"], ["public", "info", "acme-1"]]
        for_ [status401, status403] $ \refused ->
            for_ ["GET", "HEAD"] $ \method ->
                it ("rechecks private authority after warm info success for " <> show (refused, method)) $ do
                    privateStatus <- newIORef status200
                    withCoordinateSourceHeader coordinateBudgetHeader (CachedDeniedCandidateSource privateStatus) $ \uncached binding hits release -> do
                        let public = binding{bindingPackumentDeps = withPrivateBaseUrl Nothing (bindingPackumentDeps binding)}
                        runtime <- warmPublicDocuments uncached public hits [("acme-1", status403)]
                        allowed <- capture runtime binding "GET" "/info/acme-1" []
                        simpleStatus allowed `shouldBe` status200
                        simpleBody allowed `shouldBe` fromStrict (BS.drop (BS.length coordinateBudgetHeader) (snd (coordinateListings coordinateBudgetHeader release)))
                        sort <$> readIORef hits `shouldReturn` [["private", "info", "acme-1"]]
                        writeIORef hits []
                        writeIORef privateStatus refused
                        let cap = BS.length (fst (coordinateListings coordinateBudgetHeader release)) - 1
                        denied <- capture runtime (coordinateByteCap cap binding) method "/info/acme-1" []
                        simpleStatus denied `shouldBe` refused
                        readIORef hits `shouldReturn` [["private", "info", "acme-1"]]
        for_ ["GET", "HEAD"] $ \method ->
            it ("does not use a cached public document for owned private absence for " <> show method) $ do
                privateStatus <- newIORef status200
                withCoordinateSourceHeader coordinateBudgetHeader (CachedDeniedCandidateSource privateStatus) $ \uncached original hits release -> do
                    let public = original{bindingPackumentDeps = withPrivateBaseUrl Nothing (bindingPackumentDeps original)}
                    runtime <- warmPublicDocuments uncached public hits [("acme", status403), ("acme-1", status403)]
                    owned <- expectRight (projectFirstPartyEntry "acme")
                    let deps = bindingPackumentDeps original
                        binding = original{bindingPackumentDeps = deps{pdFirstParty = rubyGemsFirstPartyName (owned :| [])}}
                    writeIORef privateStatus status404
                    let cap = BS.length (fst (coordinateListings coordinateBudgetHeader release)) - 1
                    response <- capture runtime (coordinateByteCap cap binding) method "/info/acme" []
                    simpleStatus response `shouldBe` status404
                    readIORef hits `shouldReturn` [["private", "info", "acme"]]

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

    describe "partially denied coordinate inventory" $ do
        for_ ["GET", "HEAD"] $ \method -> do
            for_ ["/gems/acme-1-2.0.gem", "/quick/Marshal.4.8/acme-1-2.0.gemspec.rz"] $ \path ->
                it ("retains the denied coordinate beside an admitted sibling for " <> show (method, path)) $
                    withCoordinateSources PublicMixedCandidateSource $ \runtime binding hits _ -> do
                        response <- capture runtime binding method path []
                        simpleStatus response `shouldBe` status403
                        readIORef hits `shouldReturn` [["public", "info", "acme"], ["public", "info", "acme-1"]]
            for_ ["/gems/acme-1-1.0.gem", "/quick/Marshal.4.8/acme-1-1.0.gemspec.rz"] $ \path ->
                it ("serves the admitted sibling beside a denied coordinate for " <> show (method, path)) $
                    withCoordinateSources PublicMixedCandidateSource $ \runtime binding hits release -> do
                        response <- capture runtime binding method path []
                        let body = if path == "/gems/acme-1-1.0.gem" then releaseArchive release else releaseCompact release
                        checkCoordinateResponse method path body response
                        observed <- readIORef hits
                        observed `shouldSatisfy` all (\parts -> take 1 parts == ["public"])
                        observed `shouldSatisfy` notElem ["public", "gems", "acme-1-2.0.gem"]
                        observed `shouldSatisfy` notElem ["public", "quick", "Marshal.4.8", "acme-1-2.0.gemspec.rz"]
            for_ ["/gems/acme-1-2.0-java.gem", "/quick/Marshal.4.8/acme-1-2.0-java.gemspec.rz", "/gems/acme-1-3.0.gem", "/quick/Marshal.4.8/acme-1-3.0.gemspec.rz"] $ \path ->
                it ("keeps an absent native coordinate absent beside a denied version for " <> show (method, path)) $
                    withCoordinateSources PublicMixedCandidateSource $ \runtime binding hits _ -> do
                        response <- capture runtime binding method path []
                        simpleStatus response `shouldBe` status404
                        observed <- readIORef hits
                        observed `shouldSatisfy` all (\parts -> take 2 parts == ["public", "info"])

        let loggedCases =
                [("public", PublicMixedCandidateSource, "GET", "/quick/Marshal.4.8/acme-1-2.0.gemspec.rz", status403, 1, False), ("public", PublicMixedCandidateSource, "GET", "/quick/Marshal.4.8/acme-1-1.0.gemspec.rz", status200, 0, False)]
                    <> [(label, sources, method, path, status403, 1, False) | (label, sources) <- [("private sibling", MixedPolicyCandidateSource), ("settled private absence", PublicDeniedPrivateCandidateSource status404)], method <- ["GET", "HEAD"], path <- ["/gems/acme-1-2.0.gem", "/quick/Marshal.4.8/acme-1-2.0.gemspec.rz"]]
                    <> [("absent raw coordinate", PublicDeniedPrivateCandidateSource status404, method, path, status404, 0, False) | method <- ["GET", "HEAD"], path <- ["/gems/acme-1-3.0.gem", "/quick/Marshal.4.8/acme-1-3.0.gemspec.rz", "/gems/acme-1-2.0-java.gem", "/quick/Marshal.4.8/acme-1-2.0-java.gemspec.rz"]]
                    <> [("checked zero-version inventory", CheckedEmptyCandidateSource, method, path, status404, 0, False) | method <- ["GET", "HEAD"], path <- ["/gems/acme-1-2.0.gem", "/quick/Marshal.4.8/acme-1-2.0.gemspec.rz"]]
                    <> [("matching transient inability", PublicDeniedPrivateCandidateSource status404, method, path, status503, 1, True) | method <- ["GET", "HEAD"], path <- ["/gems/acme-1-2.0.gem", "/quick/Marshal.4.8/acme-1-2.0.gemspec.rz"]]
        for_ loggedCases $ \(label, sources, method, path, expected, count, missingDatabase) ->
            it ("logs only the final matching rejected coordinate with its gate tag for " <> toString (label :: Text) <> " " <> show (method, path)) $
                withCoordinateSources sources $ \originalRuntime original hits _ -> do
                    tags <- newIORef (0 :: Int)
                    denials <- newIORef []
                    let deps = bindingPackumentDeps original
                        tag = atomicModifyIORef' tags (\previous -> let next = previous + 1 in (next, Just (DbEtag ("gate-" <> show next))))
                        binding = original{bindingPackumentDeps = deps{pdAdvisoryEtag = tag, pdRules = if missingDatabase then map (missingDatabaseForVersion "2.0") (pdRules deps) else pdRules deps}}
                        metrics = (srMetrics originalRuntime){mpRuleDenial = \rule reason -> atomicModifyIORef' denials (\seen -> (seen <> [(rule, reason)], ()))}
                        runtime = originalRuntime{srMetrics = metrics}
                    (response, logged) <- captureJsonLog (\logEnv -> captureWithLog logEnv runtime binding method path [])
                    simpleStatus response `shouldBe` expected
                    let rejected = filter ((== Just "denied") . lineMessage) (T.lines logged)
                    length rejected `shouldBe` count
                    readIORef tags `shouldReturn` 2
                    readIORef denials `shouldReturn` replicate count (if missingDatabase then (Nothing, Metric.ReasonUnavailable) else (Just "BlockedByDefault", Metric.ReasonPolicy))
                    for_ rejected $ \line -> do
                        line `shouldSatisfy` T.isInfixOf "\"package\":\"acme-1\""
                        line `shouldSatisfy` T.isInfixOf "\"version\":\"2.0\""
                        line `shouldSatisfy` T.isInfixOf "\"active_advisory_db_etag\":\"gate-2\""
                    observed <- readIORef hits
                    case sources of
                        MixedPolicyCandidateSource -> sort observed `shouldBe` coordinatePrivateMetadataReads
                        PublicDeniedPrivateCandidateSource _ -> sort observed `shouldBe` coordinatePrivateMetadataReads
                        CheckedEmptyCandidateSource -> sort observed `shouldBe` coordinatePrivateMetadataReads
                        _ -> observed `shouldSatisfy` notElem ["public", "gems", "acme-1-2.0.gem"]
        for_ ["GET", "HEAD"] $ \method ->
            for_ ["/gems/acme-1-2.0.gem", "/quick/Marshal.4.8/acme-1-2.0.gemspec.rz"] $ \path ->
                it ("preserves a transient missing-database inability beside an admitted sibling for " <> show (method, path)) $
                    withCoordinateSources PublicMixedCandidateSource $ \runtime original hits _ -> do
                        let deps = bindingPackumentDeps original
                            binding = original{bindingPackumentDeps = deps{pdRules = map (missingDatabaseForVersion "2.0") (pdRules deps)}}
                        response <- capture runtime binding method path []
                        simpleStatus response `shouldBe` status503
                        when (method == "GET") (simpleBody response `shouldSatisfy` BS.isInfixOf "no advisory database loaded" . LBS.toStrict)
                        readIORef hits `shouldReturn` [["public", "info", "acme"], ["public", "info", "acme-1"]]

    describe "checked no-survivor native admission" $ do
        it "preserves the ordinary listing refusal after settled private absence" $
            withCoordinateSources (PublicDeniedPrivateCandidateSource status404) $ \runtime binding hits _ -> do
                response <- capture runtime binding "GET" "/info/acme-1" []
                simpleStatus response `shouldBe` status503
                sort <$> readIORef hits `shouldReturn` [["private", "info", "acme-1"], ["public", "info", "acme-1"]]
        for_ ["GET", "HEAD"] $ \method ->
            for_ ["/gems/acme-1-2.0.gem", "/quick/Marshal.4.8/acme-1-2.0.gemspec.rz"] $ \path -> do
                it ("keeps invalid inventory on its existing failure path for " <> show (method, path)) $
                    withCoordinateSources InvalidInventoryCandidateSource $ \runtime binding hits _ -> do
                        response <- capture runtime binding method path []
                        simpleStatus response `shouldBe` status503
                        sort <$> readIORef hits `shouldReturn` coordinatePrivateMetadataReads
                for_ [(status401, status401), (status403, status403), (status502, status503), (status503, status503)] $ \(upstream, expected) ->
                    it ("preserves the actual private refusal beside an all-denied public source for " <> show (upstream, method, path)) $
                        withCoordinateSources (PublicDeniedPrivateCandidateSource upstream) $ \runtime binding hits _ -> do
                            response <- capture runtime binding method path []
                            simpleStatus response `shouldBe` expected
                            sort <$> readIORef hits `shouldReturn` coordinatePrivateMetadataReads
                for_ [False, True] $ \privateOnly ->
                    it ("keeps owned private absence without public fallback for " <> show (privateOnly, method, path)) $
                        withCoordinateSources (PublicDeniedPrivateCandidateSource status404) $ \runtime original hits _ -> do
                            owned <- expectRight (projectFirstPartyEntry "acme-1")
                            let deps = bindingPackumentDeps original
                                binding = original{bindingPackumentDeps = deps{pdFirstParty = rubyGemsFirstPartyName (owned :| []), pdReadSourceSelection = if privateOnly then PrivateOnly else MergePrivateAndPublic}}
                            response <- capture runtime binding method path []
                            simpleStatus response `shouldBe` status404
                            sort <$> readIORef hits `shouldReturn` ([["private", "info", "acme"], ["private", "info", "acme-1"]] <> [["public", "info", "acme"] | not privateOnly])

    describe "partial coordinate integrity exclusions" $ do
        for_ [False, True] $ \trusted ->
            for_ ["GET", "HEAD"] $ \method ->
                for_ ["/gems/acme-1-2.0.gem", "/quick/Marshal.4.8/acme-1-2.0.gemspec.rz"] $ \path ->
                    it ("retains an excluded checked file beside a surviving contribution for " <> show (trusted, method, path)) $
                        withCoordinateSources (if trusted then MixedTrustedIntegrityCandidateSource else MixedPublicIntegrityCandidateSource) $ \runtime original hits _ -> do
                            publicFloor <- expectRight (mkMinIntegrity SHA512)
                            trustedFloor <- expectRight (mkMinTrustedIntegrity SHA512)
                            let deps = bindingPackumentDeps original
                                selected = if trusted then deps{pdMinTrustedIntegrity = trustedFloor} else deps{pdMinIntegrity = publicFloor}
                                binding = original{bindingPackumentDeps = selected}
                            response <- capture runtime binding method path []
                            simpleStatus response `shouldBe` status403
                            observed <- readIORef hits
                            sort observed `shouldBe` coordinatePrivateMetadataReads

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
                    when (path == "/versions" || "/info/" `BS.isPrefixOf` path) $ do
                        lookup "Repr-Digest" (simpleHeaders plain) `shouldSatisfy` isJust
                        lookup "Repr-Digest" (simpleHeaders packed) `shouldBe` lookup "Repr-Digest" (simpleHeaders plain)
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
                            ranged <- serve method (("Range", "bytes=10-") : ("If-None-Match", tag) : offered)
                            simpleStatus ranged `shouldBe` status304
                            simpleBody ranged `shouldBe` ""
                            lookup "ETag" (simpleHeaders ranged) `shouldBe` Just tag
                    crossed <- serve "GET" [("Accept-Encoding", "gzip"), ("If-None-Match", plainTag)]
                    simpleStatus crossed `shouldBe` status200
                    excluded <- serve "GET" [("Accept-Encoding", "gzip;q=0, identity;q=0"), ("If-None-Match", "*")]
                    simpleStatus excluded `shouldBe` status406
        it "does not let a wildcard validator bypass authentication, bounds or coordinate admission" $
            withNative $ \runtime original hits -> do
                let binding = compressing original
                    secured = binding{bindingPackumentDeps = (bindingPackumentDeps binding){pdInboundToken = Just (mkSecret "secret")}}
                    wildcard = [("If-None-Match", "*")]
                for_ metadataPaths $ \path -> do
                    refused <- capture runtime secured "GET" path wildcard
                    simpleStatus refused `shouldBe` status401
                readIORef hits `shouldReturn` []
                missing <- capture runtime binding "GET" "/quick/Marshal.4.8/ecluse-fixture-root-99.0.0.gemspec.rz" wildcard
                simpleStatus missing `shouldBe` status404
                let capped = binding{bindingPackumentDeps = (bindingPackumentDeps binding){pdLimits = (pdLimits (bindingPackumentDeps binding)){maxMetadataBytes = 16}}}
                bounded <- capture runtime capped "GET" "/versions" wildcard
                simpleStatus bounded `shouldBe` status502

metadataPaths :: [ByteString]
metadataPaths =
    [ "/versions"
    , "/info/ecluse-fixture-root"
    , "/quick/Marshal.4.8/ecluse-fixture-root-1.0.0.gemspec.rz"
    ]

compressing :: MountBinding -> MountBinding
compressing binding = binding{bindingPackumentDeps = (bindingPackumentDeps binding){pdMetadataCompression = GzipWhenAccepted}}

responseTag :: SResponse -> IO ByteString
responseTag response = maybe (expectationFailure "the admitted metadata response omitted its ETag" >> pure "") pure (lookup "ETag" (simpleHeaders response))

withDirectorySources :: (ServeRuntime -> MountBinding -> IORef [([Text], [Header])] -> IORef Status -> IO a) -> IO a
withDirectorySources use = do
    hits <- newIORef []
    privateStatus <- newIORef status200
    let upstream request respond = do
            modifyIORef' hits (<> [(pathInfo request, requestHeaders request)])
            case pathInfo request of
                ["private", "versions"] -> do
                    status <- readIORef privateStatus
                    respond (responseLBS status [] "---\nprivate-gem 1.0 \n")
                ["public", "versions"] -> respond (responseLBS status200 [] "---\npublic-gem 1.0 \n")
                _ -> respond (responseLBS status404 [] "")
    testWithApplication (pure upstream) $ \port -> do
        let base = "http://localhost:" <> show port
        withNativeOrigins (Just (loopbackRegistryUrl (base <> "/private/"))) (loopbackRegistryUrl (base <> "/public/")) $ \runtime binding -> do
            let deps = bindingPackumentDeps binding
            use runtime binding{bindingPackumentDeps = deps{pdReadSourceSelection = MergePrivateAndPublic}} hits privateStatus

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
                ["versions"] -> respond (responseLBS status200 [] "---\necluse-fixture-leaf 1.0.0 \necluse-fixture-root 1.0.0 \n")
                ["info", name] | Just body <- Map.lookup name fixtures -> respond (responseLBS status200 [] (fromStrict body))
                ["quick", "Marshal.4.8", "ecluse-fixture-root-1.0.0.gemspec.rz"] -> respond (responseLBS status200 [] (fromStrict compact))
                ["gems", "ecluse-fixture-root-1.0.0.gem"] -> respond (responseLBS status200 [] (fromStrict archive))
                _ -> respond (responseLBS status404 [] "")
    testWithApplication (pure upstream) $ \port -> do
        let origin = loopbackRegistryUrl ("http://localhost:" <> show port)
        withNativeOrigins (Just origin) origin $ \runtime binding -> use runtime binding hits

withNativeOrigins :: Maybe RegistryUrl -> RegistryUrl -> (ServeRuntime -> MountBinding -> IO a) -> IO a
withNativeOrigins private public use = withSweeper $ \deadlines -> do
    admission <- newServeAdmissionTuned deadlines 1 0 0
    runtime <- testServeRuntimeWith deadlines admission noopMetricsPort
    let adapter = rubyGemsAdapter (ChargeFactors 1000 1000 1000)
        deps = (serveDepsFor adapter private public NoMirrorWrite [] (pure (UTCTime (fromGregorian 2030 1 1) 0))){pdReadSourceSelection = PrivateOnly}
        binding = MountBinding ("rubygems" :| []) rubyGemsRouter rubyReadCredential deps Nothing
    use runtime binding

data CoordinateSources
    = OwnedCandidateSource Status
    | PublicCandidateSource
    | PublicMixedCandidateSource
    | MixedPolicyCandidateSource
    | CachedDeniedCandidateSource (IORef Status)
    | PublicDeniedPrivateCandidateSource Status
    | CheckedEmptyCandidateSource
    | InvalidInventoryCandidateSource
    | MixedPublicIntegrityCandidateSource
    | MixedTrustedIntegrityCandidateSource

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
    let age = case sources of OwnedCandidateSource _ -> 0; _ -> 604800
    rules <- prepare inertRuleDeps [atDefaultPrecedence (AllowIfOlderThan age)]
    let (recent, admitted) = coordinateListings header release
        mixed = admitted <> BS.drop (BS.length header + 4) recent
        upstream request respond = do
            atomicModifyIORef' hits (\events -> (events <> [pathInfo request], ()))
            let answer status bytes = respond (responseLBS status [] (fromStrict bytes))
                answerBatchPrivate variable = do
                    status <- readIORef variable
                    answer status (if status == status200 then BS.drop (BS.length header) admitted else "")
            case pathInfo request of
                ["private", "info", "acme"] -> case sources of
                    OwnedCandidateSource status -> answer status ""
                    CachedDeniedCandidateSource variable -> answerBatchPrivate variable
                    _ -> answer status404 ""
                ["private", "info", "acme-1"] -> case sources of
                    MixedPolicyCandidateSource -> answer status200 admitted
                    CachedDeniedCandidateSource variable -> answerBatchPrivate variable
                    PublicDeniedPrivateCandidateSource status -> answer status ""
                    MixedPublicIntegrityCandidateSource -> answer status200 admitted
                    MixedTrustedIntegrityCandidateSource -> answer status200 recent
                    _ -> answer status404 ""
                ["public", "info", "acme"] -> case sources of
                    OwnedCandidateSource _ -> answer status200 recent
                    PublicCandidateSource -> answer status200 recent
                    CachedDeniedCandidateSource _ -> answer status200 recent
                    _ -> answer status404 ""
                ["public", "info", "acme-1"] -> answer status200 (case sources of PublicMixedCandidateSource -> mixed; MixedPolicyCandidateSource -> recent; CachedDeniedCandidateSource _ -> recent; PublicDeniedPrivateCandidateSource _ -> recent; CheckedEmptyCandidateSource -> "---\n"; InvalidInventoryCandidateSource -> "---\n2.0 |checksum:invalid\n"; MixedPublicIntegrityCandidateSource -> mixed; _ -> admitted)
                ["public", "gems", "acme-1-1.0.gem"] -> respond (responseLBS status200 [("Content-Length", show (BS.length (releaseArchive release)))] (fromStrict (releaseArchive release)))
                ["public", "quick", "Marshal.4.8", "acme-1-1.0.gemspec.rz"] -> answer status200 (releaseCompact release)
                _ -> answer status404 ""
    testWithApplication (pure upstream) $ \port -> do
        let base = "http://localhost:" <> show port
            private = case sources of
                OwnedCandidateSource _ -> Just privateBase
                MixedPolicyCandidateSource -> Just privateBase
                CachedDeniedCandidateSource _ -> Just privateBase
                PublicDeniedPrivateCandidateSource _ -> Just privateBase
                CheckedEmptyCandidateSource -> Just privateBase
                InvalidInventoryCandidateSource -> Just privateBase
                MixedPublicIntegrityCandidateSource -> Just privateBase
                MixedTrustedIntegrityCandidateSource -> Just privateBase
                _ -> Nothing
            privateBase = loopbackRegistryUrl (base <> "/private/")
            firstParty = case sources of OwnedCandidateSource _ -> rubyGemsFirstPartyName (owned :| []); _ -> const False
        withNativeOrigins private (loopbackRegistryUrl (base <> "/public/")) $ \runtime binding -> do
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

coordinatePrivateMetadataReads :: [[Text]]
coordinatePrivateMetadataReads = [["private", "info", "acme"], ["private", "info", "acme-1"], ["public", "info", "acme"], ["public", "info", "acme-1"]]

missingDatabaseForVersion :: Text -> PreparedRule -> PreparedRule
missingDatabaseForVersion version rule = case prepEval rule of
    PerVersion evaluate -> rule{prepEval = PerVersion (\ctx evidence -> if renderVersion (evVersion evidence) == version then pure (CannotVet FailDeny NoDatabaseLoaded) else evaluate ctx evidence)}
    PerPackage _ -> rule
    PerInstallEvidence _ -> rule

coordinateByteCap :: Int -> MountBinding -> MountBinding
coordinateByteCap cap binding =
    let deps = bindingPackumentDeps binding
     in binding{bindingPackumentDeps = deps{pdLimits = (pdLimits deps){maxMetadataBytes = cap}}}

warmCoordinateCache :: ServeRuntime -> MountBinding -> IORef [[Text]] -> IO ServeRuntime
warmCoordinateCache uncached binding hits = warmPublicDocuments uncached binding hits [("acme", status403), ("acme-1", status200)]

warmPublicDocuments :: ServeRuntime -> MountBinding -> IORef [[Text]] -> [(Text, Status)] -> IO ServeRuntime
warmPublicDocuments uncached binding hits documents = do
    values <- newIORef Map.empty
    let full = externalOperations (\_ key -> Map.lookup key <$> readIORef values) (\key value -> atomicModifyIORef' values (\stored -> (Map.insert key value stored, ())))
    cache <- newMetadataCacheWithProvider (cacheProvider (ExternalStorage 100000) Nothing (Just full) Nothing Nothing)
    let runtime = uncached{srMetadataCache = cache}
        source = Source (registryUrlText (pdPublicBaseUrl (bindingPackumentDeps binding)))
        listing name = capture runtime binding "GET" ("/info/" <> encodeUtf8 name) []
    responses <- forM documents $ \(name, expected) -> do
        response <- listing name
        simpleStatus response `shouldBe` expected
        pure response
    readIORef hits `shouldReturn` [["public", "info", name] | (name, _) <- documents]
    for_ documents $ \(raw, _) -> do
        name <- expectRight (projectName raw)
        retained <- cachedMetadata noopMetricsPort cache source name
        retained `shouldSatisfy` isJust
    writeIORef hits []
    for_ (zip documents responses) $ \((name, expected), original) -> do
        repeated <- listing name
        simpleStatus repeated `shouldBe` expected
        simpleBody repeated `shouldBe` simpleBody original
    readIORef hits `shouldReturn` []
    pure runtime

-- What the request holds when its response starts, under a meter that charges each byte.
chargedAtResponse :: ServeRuntime -> MountBinding -> ByteString -> IO (Int, SResponse)
chargedAtResponse runtime binding path = withSweeper $ \deadlines -> do
    meter <- newMemoryMeter deadlines MeterSettings{msBudgetBytes = maxBound `div` 2, msStepBytes = 1, msEntryRoom = 1000, msEntryWaitMicros = 0}
    held <- newIORef 0
    logEnv <- newTestLogEnv
    let metered = runtime{srMemoryMeter = meter}
        observe = meterSnapshot meter >>= writeIORef held . snChargedBytes
    response <- runSession (srequest (SRequest (setPath defaultRequest path) "")) $ \incoming respond ->
        case rubyGemsRouter (requestMethod incoming) (requestHeaders incoming) (pathInfo incoming) of
            RouteAction contract (RunPipeline _ run) -> runHandler logEnv mempty (RequestCtx metered binding) (run incoming (\reply -> observe >> respond (responseToWai contract reply)))
            _ -> fail "expected a pipeline route"
    (,response) <$> readIORef held

capture :: ServeRuntime -> MountBinding -> Method -> ByteString -> [Header] -> IO SResponse
capture runtime binding method path headers = do
    logEnv <- newTestLogEnv
    captureWithLog logEnv runtime binding method path headers

captureWithLog :: LogEnv -> ServeRuntime -> MountBinding -> Method -> ByteString -> [Header] -> IO SResponse
captureWithLog logEnv runtime binding method path headers = do
    let request = setPath defaultRequest{requestMethod = method, requestHeaders = headers} path
    runSession (srequest (SRequest request "")) $ \incoming respond ->
        case rubyGemsRouter (requestMethod incoming) (requestHeaders incoming) (pathInfo incoming) of
            RouteAction contract action -> case action of
                AnswerLocally answer -> respond (responseToWai contract answer)
                AnswerRefusal answer -> respond (responseToWai contract (answer (pdHelp (bindingPackumentDeps binding))))
                RunPipeline _ run -> runHandler logEnv mempty (RequestCtx runtime binding) (run incoming (respond . responseToWai contract))
