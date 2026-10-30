-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Origin authority and namespace transitions through HTTP.
Private responses read the same store the Dredger uses.
-}
module Ecluse.Core.Server.Pipeline.OriginIntegrationSpec (spec) where

import Data.Aeson (Value (String))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Ecluse.Composition (firstPartyName)
import Ecluse.Config.Types (FirstParty (FirstPartyNpmScopes))
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageName, mkPackageName, mkScope)
import Ecluse.Core.Registry.Maintenance (StoredVersion (StoredVersion, storedVersion), VersionPresence (VersionServed))
import Ecluse.Core.Registry.Sweep.Package (sweepPackageGroup)
import Ecluse.Core.Registry.Sweep.Types (SweepMount (smFirstParty, smStore), newSweepState)
import Ecluse.Core.Rules (prepare)
import Ecluse.Core.Rules.Types (Rule (DenyByIdentity))
import Ecluse.Core.Server.Context (PackumentDeps (..))
import Ecluse.Core.Telemetry.Metrics (SweepResult (SweepGuardSkipped))
import Ecluse.Core.Telemetry.Upstream qualified as PublicRead
import Ecluse.Core.Version (mkVersion, renderVersion)
import Ecluse.Runtime.Env (Env (envPublicControl))
import Ecluse.Runtime.Log (DdContext (DdContext), LogFormat (JsonLog), LogLevel (InfoLevel), newLogEnv)
import Ecluse.Server.Pipeline.TestSupport
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Json (fieldAt)
import Ecluse.Test.Log (captureStdout, lineMessage, newTestLogEnv)
import Ecluse.Test.Maintenance (FakeStore (fakeMaintenance, readFakeContents), FakeStoreConfig (fakeContents, fakeManifests), defaultFakeStoreConfig, newFakeStore)
import Ecluse.Test.Package (sampleManifest)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Registry.Npm (VersionSpec (vsIntegrity), versionSpec, versionValue)
import Ecluse.Test.Registry.Public (PublicFixture (..), newPublicFixture)
import Ecluse.Test.Rules (atDefaultPrecedence, inertRuleDeps)
import Ecluse.Test.Sweep (RecordedSweep (recPorts, recResults), recordingPorts, testMount, testPacing)
import Ecluse.Test.Wai
import Katip (Environment (Environment), closeScribes)
import Network.HTTP.Types (status200, status401, status403, status429, status503, statusCode)
import Network.Wai (Request (pathInfo, requestHeaders), responseLBS)
import Network.Wai.Test (simpleBody)
import Test.Hspec
import UnliftIO (bracket)

-- | Verify private authority, public fallback, and retained copies after a namespace declaration.
spec :: Spec
spec = do
    credentialSpec
    privateAuthoritySpec
    privateAuthorisationSpec
    namespaceTransitionSpec
    partialAvailabilitySpec
    cooldownSpec

privateAuthorisationSpec :: Spec
privateAuthorisationSpec = describe "private authorisation refusal" $ do
    for_ [status401, status403] $ \upstreamStatus -> do
        it (caseLabel ("integration/origin/native-001/" <> show (statusCode upstreamStatus)) (toText ("logs a fixed warning without private details for HTTP " <> show (statusCode upstreamStatus) :: String))) $ do
            privateUp <- upstreamRespondingWith (responseLBS upstreamStatus [("WWW-Authenticate", "secret-realm")] "secret-upstream-body")
            publicUp <- servingUpstream (encodePackument (admittingPublic "1.0.0"))
            queue <- newTestMemoryQueue
            logged <-
                captureStdout $
                    bracket
                        (newLogEnv JsonLog InfoLevel (DdContext "ecluse" Nothing Nothing Nothing) (Environment "test"))
                        (void . closeScribes)
                        ( \logEnv -> withProxyOver logEnv queue privateUp publicUp Nothing id $ \app _ _ -> do
                            response <- getThing (Just "secret-client-token") app
                            status response `shouldBe` 403
                        )
            let refusals = filter ((== Just "the upstream refused metadata access") . lineMessage) (T.lines logged)
            length refusals `shouldBe` 1
            for_ refusals $ \line -> line `shouldSatisfy` T.isInfixOf "\"status\":\"warn\""
            for_ ["secret-realm", "secret-upstream-body", "secret-client-token"] $ \secret ->
                logged `shouldSatisfy` (not . T.isInfixOf secret)

        it (caseLabel ("integration/origin/native-002/" <> show (statusCode upstreamStatus)) (toText ("retains transient private failure when public metadata refuses HTTP " <> show (statusCode upstreamStatus) :: String))) $ do
            privateUp <- failingUpstream
            publicUp <- upstreamRespondingWith (responseLBS upstreamStatus [] "public refusal")
            withProxy privateUp publicUp Nothing $ \app -> do
                response <- getThing Nothing app
                status response `shouldBe` 503
                servedVersions response `shouldBe` []

        for_ [False, True] $ \firstParty ->
            it (caseLabel ("integration/origin/native-003/" <> show (statusCode upstreamStatus) <> "/" <> (if firstParty then "first-party" else "merged")) (toText ("refuses metadata HTTP " <> show (statusCode upstreamStatus) <> ", firstParty=" <> show firstParty :: String))) $ do
                let metadata = encodePackument (privatePackument [("1.0.0", plainVersion "1.0.0")] "1.0.0")
                privateUp <- upstreamRespondingWith (responseLBS upstreamStatus [("WWW-Authenticate", "private-secret"), ("Set-Cookie", "private-secret")] metadata)
                publicUp <- servingUpstream (encodePackument (admittingPublic "1.0.0"))
                queue <- newTestMemoryQueue
                withProxyEnvQueueDeps queue privateUp publicUp Nothing (\d -> d{pdFirstParty = const firstParty}) $ \app env _ -> do
                    for_ [getThingWith, headThingWith] $ \fetch -> do
                        for_ [[], [("If-None-Match", "*")]] $ \validators -> do
                            response <- fetch (("Authorization", "Bearer client-token") : validators) app
                            status response `shouldBe` 403
                            header "WWW-Authenticate" response `shouldBe` Nothing
                            header "Set-Cookie" response `shouldBe` Nothing
                            header "Retry-After" response `shouldBe` Nothing
                            servedVersions response `shouldBe` []
                    seenAuth privateUp `shouldReturn` replicate 4 (Just "Bearer client-token")
                    seenAuth publicUp `shouldReturn` replicate (if firstParty then 0 else 4) Nothing
                    drainJobs env `shouldReturn` []

        it (caseLabel ("integration/origin/native-004/" <> show (statusCode upstreamStatus)) (toText ("retains metadata HTTP " <> show (statusCode upstreamStatus) <> " when its error body is truncated" :: String))) $ do
            privateUp <- upstreamRespondingWith (truncatedResponse upstreamStatus "short")
            publicUp <- servingUpstream (encodePackument (admittingPublic "1.0.0"))
            withProxy privateUp publicUp Nothing $ \app -> do
                response <- getThing Nothing app
                status response `shouldBe` 403
                servedVersions response `shouldBe` []

        it (caseLabel ("integration/origin/native-005/" <> show (statusCode upstreamStatus)) (toText ("keeps public HTTP " <> show (statusCode upstreamStatus) <> " from withholding a private contribution" :: String))) $ do
            privateUp <- servingUpstream (encodePackument (privatePackument [("1.0.0", plainVersion "1.0.0")] "1.0.0"))
            publicUp <- upstreamRespondingWith (responseLBS upstreamStatus [] "unavailable")
            withProxy privateUp publicUp Nothing $ \app -> do
                response <- getThing Nothing app
                status response `shouldBe` 200
                servedVersions response `shouldBe` ["1.0.0"]

    for_ privateOutcomes $ \(outcomeId, label, makePrivateUpstream, firstPartyStatus) ->
        for_ [False, True] $ \firstParty ->
            it (caseLabel ("integration/origin/native-006/" <> (if firstParty then "first-party" else "merged") <> "/" <> outcomeId) (toText ("preserves private " <> label <> " policy, firstParty=" <> show firstParty :: String))) $ do
                privateUp <- makePrivateUpstream
                publicUp <- servingUpstream (encodePackument (admittingPublic "1.0.0"))
                queue <- newTestMemoryQueue
                withProxyEnvQueueDeps queue privateUp publicUp Nothing (\d -> d{pdFirstParty = const firstParty}) $ \app _ _ -> do
                    let served = not firstParty && outcomeId == "absent"
                    response <- getThing Nothing app
                    status response `shouldBe` if served then 200 else firstPartyStatus
                    servedVersions response `shouldBe` ["1.0.0" | served]
                    seenAuth publicUp `shouldReturn` [Nothing | not firstParty]

-- The private outcomes a first-party name must tell apart, with the status each renders for it.
-- A merged name serves the public set only after a private 404.
privateOutcomes :: [(Text, String, IO Upstream, Int)]
privateOutcomes =
    -- Only the origin's own 404 settles the question. A 5xx leaves it open with a 503, and an
    -- undecodable body is an invalid reply with a 502.
    [ ("absent", "HTTP 404", absentUpstream, 404)
    , ("unavailable", "HTTP 503", upstreamRespondingWith (responseLBS status503 [] "unavailable"), 503)
    , ("malformed", "an undecodable body", servingUpstream "this is not json at all", 502)
    ]

credentialSpec :: Spec
credentialSpec = describe "credential authority (forward-to-private, strip-before-public)" $
    it (caseLabel "integration/origin/native-007" "forwards the client credential to the private upstream and NEVER to the public upstream") $ do
        privateUp <- servingUpstream (encodePackument (privatePackument [("1.0.0", plainVersion "1.0.0")] "1.0.0"))
        publicUp <-
            servingUpstream
                (encodePackument (packument [("2.0.0", plainVersion "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 30)]))
        withProxy privateUp publicUp Nothing $ \app -> do
            _ <- getThing (Just "client-secret-token") app
            privAuth <- seenAuth privateUp
            pubAuth <- seenAuth publicUp
            privAuth `shouldBe` [Just "Bearer client-secret-token"]
            pubAuth `shouldBe` [Nothing]

privateAuthoritySpec :: Spec
privateAuthoritySpec = describe "private origin is the per-client authority (not cached across clients)" $ do
    it (caseLabel "integration/origin/native-008" "re-consults the private upstream per client within the TTL -- each client's token reaches it") $ do
        privateUp <- servingUpstream (encodePackument (privatePackument [("1.0.0", plainVersion "1.0.0")] "1.0.0"))
        publicUp <-
            servingUpstream
                (encodePackument (packument [("2.0.0", plainVersion "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 30)]))
        withProxy privateUp publicUp Nothing $ \app -> do
            _ <- getThing (Just "tokenA") app
            _ <- getThing (Just "tokenB") app
            privAuth <- seenAuth privateUp
            pubAuth <- seenAuth publicUp
            privAuth `shouldBe` [Just "Bearer tokenA", Just "Bearer tokenB"]
            pubAuth `shouldBe` [Nothing, Nothing]

    it (caseLabel "integration/origin/native-009" "serves byte-identical bodies across identical repeat requests (the assembled representation is reused)") $ do
        (privateUp, publicUp) <- twoServingUpstreams
        withProxy privateUp publicUp Nothing $ \app -> do
            firstResp <- getThing (Just "tokenA") app
            secondResp <- getThing (Just "tokenA") app
            status firstResp `shouldBe` 200
            simpleBody secondResp `shouldBe` simpleBody firstResp
            header "ETag" secondResp `shouldBe` header "ETag" firstResp
            -- The reuse never skips the per-request private authorisation.
            seenAuth privateUp `shouldReturn` [Just "Bearer tokenA", Just "Bearer tokenA"]

    it (caseLabel "integration/origin/native-010" "never serves one client's assembled document to another with a different private view") $ do
        -- The private upstream answers per credential. The assembled store is keyed by content, so
        -- client B's entry can never answer client A.
        let perToken req = case lookupAuth (requestHeaders req) of
                Just "Bearer token-a" -> encodePackument (privatePackument [("9.0.0", plainVersion "9.0.0")] "9.0.0")
                _ -> encodePackument (privatePackument [("9.0.1", plainVersion "9.0.1")] "9.0.1")
        privateUp <- servingUpstreamPer perToken
        publicUp <-
            servingUpstream
                (encodePackument (packument [("2.0.0", plainVersion "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 30)]))
        withProxy privateUp publicUp Nothing $ \app -> do
            respA <- getThing (Just "token-a") app
            respB <- getThing (Just "token-b") app
            respA2 <- getThing (Just "token-a") app
            servedVersions respA `shouldBe` ["2.0.0", "9.0.0"]
            servedVersions respB `shouldBe` ["2.0.0", "9.0.1"]
            servedVersions respA2 `shouldBe` ["2.0.0", "9.0.0"]
            simpleBody respA2 `shouldBe` simpleBody respA

namespaceTransitionSpec :: Spec
namespaceTransitionSpec = describe "declaring a namespace after public ingestion" $
    it (caseLabel "integration/origin/native-011" "keeps the public copy and genuine private release served and protected under an identity deny") $ do
        let retained = [StoredVersion (mkVersion Npm v) VersionServed Nothing | v <- ["1.0.0", "9.0.0"]]
            inventory = Map.singleton transitionName retained
        store <-
            newFakeStore
                defaultFakeStoreConfig
                    { fakeContents = inventory
                    , fakeManifests = Map.singleton transitionName (sampleManifest transitionName (map storedVersion retained))
                    }
        privateUp <- servingUpstreamIO (storedTransitionBody store)
        publicUp <- servingUpstreamIO (pure . transitionBody ["1.0.0", "2.0.0"])
        absentPrivate <- absentUpstream
        withProxy absentPrivate publicUp Nothing $ \app -> do
            original <- getPath (transitionArtifactPath "1.0.0") app
            status original `shouldBe` 200
            simpleBody original `shouldBe` publicTarballBytes
        queue <- newTestMemoryQueue
        withProxyEnvQueue queue privateUp publicUp Nothing $ \app _env _port -> do
            beforeResponse <- getPath "/npm/@acme/thing" app
            status beforeResponse `shouldBe` 200
            servedVersions beforeResponse `shouldBe` ["1.0.0", "2.0.0", "9.0.0"]
        publicRequests <- seenAuth publicUp
        publicRequests `shouldSatisfy` (not . null)
        rules <- prepare inertRuleDeps [atDefaultPrecedence (DenyByIdentity "@acme/thing")]
        let declared = firstPartyName (FirstPartyNpmScopes (mkScope "acme" :| []))
            afterDeclaration deps = deps{pdFirstParty = declared, pdRules = rules}
            maintenance = fakeMaintenance store
            protectedMount = (testMount maintenance rules [DenyByIdentity "@acme/thing"]){smFirstParty = declared}
        recorded <- recordingPorts Nothing
        counters <- newSweepState
        sweepPackageGroup testPacing (recPorts recorded) counters protectedMount transitionName [(smStore protectedMount, retained)] `shouldReturn` Nothing
        recResults recorded `shouldReturn` [SweepGuardSkipped, SweepGuardSkipped]
        readFakeContents store `shouldReturn` inventory
        withProxyEnvQueueDeps queue privateUp publicUp Nothing afterDeclaration $ \app env _port -> do
            afterResponse <- getPath "/npm/@acme/thing" app
            status afterResponse `shouldBe` 200
            servedVersions afterResponse `shouldBe` ["1.0.0", "9.0.0"]
            for_ ["1.0.0", "9.0.0"] $ \version -> do
                artifact <- getPath (transitionArtifactPath version) app
                status artifact `shouldBe` 200
                simpleBody artifact `shouldBe` transitionBytes version
            drainJobs env `shouldReturn` []
        seenAuth publicUp `shouldReturn` publicRequests
        readFakeContents store `shouldReturn` inventory

transitionName :: PackageName
transitionName = mkPackageName Npm (Just (mkScope "acme")) "thing"

-- The mirrored public version and the genuine private release carry different bytes.
transitionBytes :: Text -> LByteString
transitionBytes "9.0.0" = privateTarballBytes
transitionBytes _ = publicTarballBytes

transitionArtifactPath :: Text -> ByteString
transitionArtifactPath version = "/npm/@acme/thing/-/thing-" <> encodeUtf8 version <> ".tgz"

transitionDocument :: [Text] -> LByteString
transitionDocument versions = encodePackument (packumentNamed "@acme/thing" objects "1.0.0" [(v, publishedDaysAgo 30) | v <- versions])
  where
    objects =
        [ ( v
          , versionValue
                ( (versionSpec "@acme/thing" v ("https://upstream.example/@acme/thing/-/thing-" <> v <> ".tgz"))
                    { vsIntegrity = Just (sriFor (decodeUtf8 (toStrict (transitionBytes v))))
                    }
                )
          )
        | v <- versions
        ]

transitionBody :: [Text] -> Request -> LByteString
transitionBody versions req =
    case find (\v -> T.intercalate "/" (pathInfo req) == "@acme/thing/-/thing-" <> v <> ".tgz") versions of
        Just version -> transitionBytes version
        Nothing -> transitionDocument versions

storedTransitionBody :: FakeStore -> Request -> IO LByteString
storedTransitionBody store req = do
    inventory <- readFakeContents store
    let versions = map (renderVersion . storedVersion) (Map.findWithDefault [] transitionName inventory)
    pure (transitionBody versions req)

partialAvailabilitySpec :: Spec
partialAvailabilitySpec = describe "partial-upstream availability" $ do
    it (caseLabel "integration/origin/native-012" "refuses with 503 when the private upstream is unavailable, serving no public versions") $ do
        privateUp <- failingUpstream
        publicUp <-
            servingUpstream
                (encodePackument (packument [("2.0.0", plainVersion "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 30)]))
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 503
            servedVersions resp `shouldBe` []

    it (caseLabel "integration/origin/native-013" "serves the private set when the public upstream is unavailable") $ do
        privateUp <- servingUpstream (encodePackument (privatePackument [("1.0.0", plainVersion "1.0.0")] "1.0.0"))
        publicUp <- failingUpstream
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["1.0.0"]

    it (caseLabel "integration/origin/native-014" "refuses with 502 when the private body is unparseable") $ do
        privateUp <- servingUpstream "this is not json at all"
        publicUp <-
            servingUpstream
                (encodePackument (packument [("2.0.0", plainVersion "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 30)]))
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 502
            servedVersions resp `shouldBe` []

    it (caseLabel "integration/origin/native-015" "refuses with 502 when the private body decodes but does not project to a packument") $ do
        privateUp <- servingUpstream "[1, 2, 3]"
        publicUp <-
            servingUpstream
                (encodePackument (packument [("2.0.0", plainVersion "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 30)]))
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 502
            servedVersions resp `shouldBe` []

    it (caseLabel "integration/origin/native-016" "refuses with 502 when the private leg self-reports a different package, serving no public versions") $ do
        privateUp <-
            servingUpstream
                (encodePackument (packumentNamed "other" [("1.0.0", plainVersion "1.0.0")] "1.0.0" [("1.0.0", publishedDaysAgo 30)]))
        publicUp <-
            servingUpstream
                (encodePackument (packument [("2.0.0", plainVersion "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 30)]))
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 502
            servedVersions resp `shouldBe` []

    it (caseLabel "integration/origin/native-017" "drops a public leg that self-reports a different package, serving the private set (200)") $ do
        privateUp <- servingUpstream (encodePackument (privatePackument [("1.0.0", plainVersion "1.0.0")] "1.0.0"))
        publicUp <-
            servingUpstream
                (encodePackument (packumentNamed "other" [("2.0.0", plainVersion "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 30)]))
        withProxy privateUp publicUp Nothing $ \app -> do
            resp <- getThing Nothing app
            status resp `shouldBe` 200
            servedVersions resp `shouldBe` ["1.0.0"]
            fieldAt "name" (decodedBody resp) `shouldBe` Just (String "thing")

cooldownSpec :: Spec
cooldownSpec = describe "public authority cooldown" $
    it (caseLabel "integration/origin/native-018" "refuses locally during public cooldown and resumes after its deadline") $ do
        fixture <- newPublicFixture
        busy <- newIORef True
        privateUp <- absentUpstream
        publicUp <- recordingUpstreamIO $ \request -> do
            throttled <- readIORef busy
            pure $
                if throttled
                    then responseLBS status429 [("Retry-After", "60")] "busy"
                    else responseLBS status200 [] (encodePackument (packument [("1.0.0", selfHostedVersion (selfBaseUrl request) "1.0.0")] "1.0.0" [("1.0.0", publishedDaysAgo 30)]))
        queue <- newTestMemoryQueue
        logEnv <- newTestLogEnv
        withProxyOverRuntime (\env -> env{envPublicControl = pfControl fixture}) logEnv queue privateUp publicUp Nothing id $ \app _ _ -> do
            firstResponse <- getThing Nothing app
            status firstResponse `shouldBe` 503
            header "Retry-After" firstResponse `shouldBe` Just "60"
            pfAdvance fixture 10
            secondResponse <- getThing Nothing app
            status secondResponse `shouldBe` 503
            header "Retry-After" secondResponse `shouldBe` Just "50"
            seenAuth publicUp `shouldReturn` [Nothing]
            pfRequests fixture `shouldReturn` [("localhost", PublicRead.Listing, PublicRead.ResponseStatus 429)]
            writeIORef busy False
            pfAdvance fixture 50
            recovered <- getThing Nothing app
            status recovered `shouldBe` 200
            seenAuth publicUp `shouldReturn` [Nothing, Nothing]
            pfRequests fixture `shouldReturn` [("localhost", PublicRead.Listing, PublicRead.ResponseStatus 429), ("localhost", PublicRead.Listing, PublicRead.ResponseStatus 200)]
