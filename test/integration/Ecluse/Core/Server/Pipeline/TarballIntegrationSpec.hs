-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Exercise artifact routes against local private and public upstreams.
module Ecluse.Core.Server.Pipeline.TarballIntegrationSpec (spec) where

import Control.Exception (try)
import Data.Aeson (object, (.=))
import Data.Text qualified as T
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (mkPackageName)
import Ecluse.Core.Rules (prepare)
import Ecluse.Core.Rules.Types (Rule (AllowIfOlderThan, DenyIfDeprecated))
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Context (PackumentDeps (..))
import Ecluse.Core.Server.Upstream (MirrorServePlan (NoMirrorWrite))
import Ecluse.Core.Version (mkVersion)
import Ecluse.Server.Pipeline.TestSupport
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Json (withKeys)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Rules (admissionOnly, atDefaultPrecedence, inertRuleDeps)
import Ecluse.Test.Server.Mount (overPrivateBaseUrl, withMirrorPlan, withPrivateBaseUrl)
import Ecluse.Test.Wai
import Network.HTTP.Types (methodGet, methodHead, status200, status401, status403, status404, status503, statusCode)
import Network.HTTP.Types.Header (hIfNoneMatch)
import Network.Wai (defaultRequest, requestHeaders, responseLBS)
import Network.Wai.Test (SResponse (..), simpleBody)
import Test.Hspec

-- | Pin artifact responses, origin selection, and mirror jobs.
spec :: Spec
spec = do
    tarballSpec
    availabilitySpec
    privateAuthorisationSpec
    divergentPrivateSpec

divergentPrivateSpec :: Spec
divergentPrivateSpec = describe "divergent private version" $
    it (caseLabel "integration/npm-artifact/native-001" "retains metadata and artifact access before and after detecting the conflict") $ do
        privateUp <- conditionalArtifactUpstream "1.0.0" privateTarballBytes
        publicUp <-
            servingUpstream $
                encodePackument $
                    packument
                        [("1.0.0", versionObject "1.0.0" (sriFor "public-conflict") False)]
                        "1.0.0"
                        [("1.0.0", publishedDaysAgo 30)]
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            for_ [False, True] $ \readMetadata -> do
                when readMetadata $ do
                    metadata <- getThing Nothing app
                    status metadata `shouldBe` 200
                    servedVersions metadata `shouldBe` ["1.0.0"]
                    servedLatest metadata `shouldBe` Just "1.0.0"
                    servedIntegrity "1.0.0" metadata `shouldBe` Just (sriFor "1.0.0")
                    headResponse <- headThing Nothing app
                    status headResponse `shouldBe` 200
                    simpleBody headResponse `shouldBe` ""
                    case header "ETag" metadata of
                        Nothing -> expectationFailure "missing metadata ETag"
                        Just validator ->
                            for_ [methodGet, methodHead] $ \method -> do
                                unchanged <- requestAt method "/npm/thing" defaultRequest{requestHeaders = [(hIfNoneMatch, validator)]} app
                                status unchanged `shouldBe` 304
                                simpleBody unchanged `shouldBe` ""
                publicReads <- seenAuth publicUp
                for_ [methodGet, methodHead] $ \method ->
                    for_ [[], [(hIfNoneMatch, "\"v1\"")]] $ \validators -> do
                        response <- requestAt method "/npm/thing/-/thing-1.0.0.tgz" defaultRequest{requestHeaders = validators} app
                        status response `shouldBe` if null validators then 200 else 304
                        simpleBody response
                            `shouldBe` if method == methodGet && null validators then privateTarballBytes else ""
                seenAuth publicUp `shouldReturn` publicReads
            drainJobs env `shouldReturn` []

privateAuthorisationSpec :: Spec
privateAuthorisationSpec = describe "private artifact authorisation" $ do
    for_ [status401, status403] $ \upstreamStatus -> do
        for_ [False, True] $ \firstParty ->
            it (caseLabel ("integration/npm-artifact/native-002/" <> show (statusCode upstreamStatus) <> "/" <> (if firstParty then "first-party" else "merged")) (toText ("refuses HTTP " <> show (statusCode upstreamStatus) <> ", firstParty=" <> show firstParty :: String))) $ do
                privateUp <- recordingUpstream $ \req ->
                    if lookupAuth (requestHeaders req) == Just "Bearer valid"
                        then responseLBS status200 [] privateTarballBytes
                        else responseLBS upstreamStatus [("WWW-Authenticate", "private-secret"), ("Set-Cookie", "private-secret")] "private-secret"
                publicUp <- artifactUpstream "1.0.0" publicTarballBytes
                queue <- newTestMemoryQueue
                withProxyEnvQueueDeps queue privateUp publicUp Nothing (\d -> d{pdFirstParty = const firstParty}) $ \app env _ -> do
                    patched <- getTarball "1.0.0" (Just "valid") app
                    simpleBody patched `shouldBe` privateTarballBytes
                    for_ [methodGet, methodHead] $ \method ->
                        for_ [[], [(hIfNoneMatch, "*")]] $ \validators -> do
                            let req = defaultRequest{requestHeaders = ("Authorization", "Bearer invalid") : validators}
                            response <- requestAt method "/npm/thing/-/thing-1.0.0.tgz" req app
                            status response `shouldBe` 403
                            simpleBody response `shouldNotBe` privateTarballBytes
                            simpleBody response `shouldNotBe` publicTarballBytes
                            decodeUtf8 (toStrict (simpleBody response)) `shouldSatisfy` (not . T.isInfixOf "private-secret")
                            header "WWW-Authenticate" response `shouldBe` Nothing
                            header "Set-Cookie" response `shouldBe` Nothing
                            header "Retry-After" response `shouldBe` Nothing
                            when (method == methodHead) (simpleBody response `shouldBe` "")
                    seenAuth publicUp `shouldReturn` []
                    drainJobs env `shouldReturn` []

        it (caseLabel ("integration/npm-artifact/native-003/" <> show (statusCode upstreamStatus)) (toText ("retains artifact HTTP " <> show (statusCode upstreamStatus) <> " with a truncated error body" :: String))) $ do
            privateUp <- upstreamRespondingWith (truncatedResponse upstreamStatus "short")
            publicUp <- artifactUpstream "1.0.0" publicTarballBytes
            withProxyEnv privateUp publicUp Nothing $ \app env -> do
                response <- getTarball "1.0.0" Nothing app
                status response `shouldBe` 403
                seenAuth publicUp `shouldReturn` []
                drainJobs env `shouldReturn` []

    for_ [status404, status503] $ \upstreamStatus ->
        for_ [False, True] $ \firstParty ->
            it (caseLabel ("integration/npm-artifact/native-004/" <> show (statusCode upstreamStatus) <> "/" <> (if firstParty then "first-party" else "merged")) (toText ("preserves artifact HTTP " <> show (statusCode upstreamStatus) <> " policy, firstParty=" <> show firstParty :: String))) $ do
                privateUp <- upstreamRespondingWith (responseLBS upstreamStatus [] "unavailable")
                publicUp <- artifactUpstream "1.0.0" publicTarballBytes
                queue <- newTestMemoryQueue
                withProxyEnvQueueDeps queue privateUp publicUp Nothing (\d -> d{pdFirstParty = const firstParty}) $ \app _ _ -> do
                    -- Only a merged name's settled private absence reaches the public leg.
                    let served = not firstParty && upstreamStatus == status404
                    response <- getTarball "1.0.0" Nothing app
                    status response `shouldBe` if served then 200 else statusCode upstreamStatus
                    if served
                        then simpleBody response `shouldBe` publicTarballBytes
                        else seenAuth publicUp `shouldReturn` []

tarballSpec :: Spec
tarballSpec = describe "artifact (tarball) path" $ do
    it (caseLabel "integration/npm-artifact/native-005" "streams the private artifact on a private hit (a conventional stable read, public never consulted)") $ do
        privateUp <- privateArtifactHit "1.0.0" privateTarballBytes
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- getTarball "1.0.0" (Just "client-token") app
            status resp `shouldBe` 200
            simpleBody resp `shouldBe` privateTarballBytes
            seenAuth privateUp `shouldReturn` [Just "Bearer client-token"]
            seenArtifactMethods privateUp `shouldReturn` [methodGet]
            seenAuth publicUp `shouldReturn` []
            drainJobs env `shouldReturn` []

    it (caseLabel "integration/npm-artifact/native-006" "does not fetch the private packument on a tarball request (no metadata round-trip)") $ do
        privateUp <- privateArtifactHit "1.0.0" privateTarballBytes
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app _env -> do
            resp <- getTarball "1.0.0" (Just "client-token") app
            status resp `shouldBe` 200
            allRequests <- seenAuth privateUp
            tarballRequests <- seenArtifactMethods privateUp
            length allRequests `shouldBe` 1
            length tarballRequests `shouldBe` 1

    it (caseLabel "integration/npm-artifact/native-007" "relays the upstream status and content headers through on a private hit") $ do
        privateUp <- privateArtifactHitWithHeader "Content-Type" "application/octet-stream" "1.0.0" privateTarballBytes
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app _env -> do
            resp <- getTarball "1.0.0" (Just "client-token") app
            status resp `shouldBe` 200
            reason resp `shouldBe` "OK"
            header "Content-Type" resp `shouldBe` Just "application/octet-stream"
            simpleBody resp `shouldBe` privateTarballBytes

    it (caseLabel "integration/npm-artifact/native-008" "falls through to the public origin when the private upstream URL is unformable") $ do
        privateUp <- privateArtifactHit "1.0.0" privateTarballBytes
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        queue <- newTestMemoryQueue
        let breakPrivate = withPrivateBaseUrl (Just (loopbackRegistryUrl ""))
        withProxyEnvQueueDeps queue privateUp publicUp Nothing breakPrivate $ \app _env _port -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 200
            simpleBody resp `shouldBe` publicTarballBytes

    it (caseLabel "integration/npm-artifact/native-009" "401s a tarball request that fails edge authentication, before any upstream fetch") $ do
        privateUp <- privateArtifactHit "1.0.0" privateTarballBytes
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp (Just "edge-secret") $ \app _env -> do
            resp <- getTarball "1.0.0" (Just "wrong-token") app
            status resp `shouldBe` 401
            seenAuth privateUp `shouldReturn` []
            seenAuth publicUp `shouldReturn` []

    it (caseLabel "integration/npm-artifact/native-010" "forwards the client credential to the private origin, never to the public") $ do
        privateUp <- privateArtifactMiss
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app _env -> do
            _ <- getTarball "1.0.0" (Just "client-secret-token") app
            privAuth <- seenAuth privateUp
            pubAuth <- seenAuth publicUp
            privAuth `shouldBe` [Just "Bearer client-secret-token"]
            pubAuth `shouldBe` [Nothing, Nothing]

    it (caseLabel "integration/npm-artifact/native-011" "on a private miss: gates the version, streams from public, and enqueues a mirror job") $ do
        privateUp <- privateArtifactMiss
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        queue <- newTestMemoryQueue
        withProxyEnvQueue queue privateUp publicUp Nothing $ \app env _publicPort -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 200
            simpleBody resp `shouldBe` publicTarballBytes
            jobs <- drainJobs env
            map jobShape jobs
                `shouldBe` [
                               ( mkPackageName Npm Nothing "thing"
                               , mkVersion Npm "1.0.0"
                               , Just "thing-1.0.0.tgz"
                               )
                           ]

    it (caseLabel "integration/npm-artifact/native-012" "serve-only mount: streams the admitted public artifact and enqueues nothing (NoMirrorWrite)") $ do
        privateUp <- privateArtifactMiss
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        queue <- newTestMemoryQueue
        withProxyEnvQueueDeps queue privateUp publicUp Nothing (withMirrorPlan NoMirrorWrite) $ \app env _publicPort -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 200
            simpleBody resp `shouldBe` publicTarballBytes
            drainJobs env `shouldReturn` []

    it (caseLabel "integration/npm-artifact/native-013" "rejects a too-new version with 403 and enqueues nothing (policy denial)") $ do
        privateUp <- privateArtifactMiss
        let tooNew base = packument [("2.0.0", selfHostedVersion base "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 3)]
        publicUp <- artifactUpstreamServing (encodePackument . tooNew) publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- getTarball "2.0.0" Nothing app
            status resp `shouldBe` 403
            reason resp `shouldBe` "Forbidden"
            drainJobs env `shouldReturn` []

    it (caseLabel "integration/npm-artifact/native-014" "refuses a hashless public version with 403 before fetching the artifact (integrity-presence policy)") $ do
        privateUp <- privateArtifactMiss
        let hashless base = packument [("1.0.0", selfHostedHashless base "1.0.0")] "1.0.0" [("1.0.0", publishedDaysAgo 30)]
        publicUp <- artifactUpstreamServing (encodePackument . hashless) publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 403
            reason resp `shouldBe` "Forbidden"
            seenAuth publicUp `shouldReturn` [Nothing]
            drainJobs env `shouldReturn` []

    it (caseLabel "integration/npm-artifact/native-015" "refuses an empty-digest public version with 403 MissingIntegrity before fetching the artifact") $ do
        privateUp <- privateArtifactMiss
        let emptyDigest base = packument [("1.0.0", selfHostedEmptyDigest base "1.0.0")] "1.0.0" [("1.0.0", publishedDaysAgo 30)]
        publicUp <- artifactUpstreamServing (encodePackument . emptyDigest) publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 403
            reason resp `shouldBe` "Forbidden"
            decodedBody resp
                `shouldBe` object ["error" .= ("this version carries no integrity digest and cannot be served from a public upstream" :: Text)]
            seenAuth publicUp `shouldReturn` [Nothing]
            drainJobs env `shouldReturn` []

    it (caseLabel "integration/npm-artifact/native-016" "refuses a below-floor public version with 403 before fetching the artifact (integrity floor)") $ do
        privateUp <- privateArtifactMiss
        let belowFloor base = packument [("1.0.0", selfHostedShasumOnly base "1.0.0")] "1.0.0" [("1.0.0", publishedDaysAgo 30)]
        publicUp <- artifactUpstreamServing (encodePackument . belowFloor) publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 403
            reason resp `shouldBe` "Forbidden"
            seenAuth publicUp `shouldReturn` [Nothing]
            drainJobs env `shouldReturn` []

    it (caseLabel "integration/npm-artifact/native-017" "admits a digest-bearing public version on the artifact path (no regression)") $ do
        privateUp <- privateArtifactMiss
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app _env -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 200
            simpleBody resp `shouldBe` publicTarballBytes

    it (caseLabel "integration/npm-artifact/native-018" "serves a hashless private artifact from the private origin (no serve-time floor on the private leg)") $ do
        privateUp <- privateArtifactHitHashless "1.0.0" privateTarballBytes
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- getTarball "1.0.0" (Just "client-token") app
            status resp `shouldBe` 200
            simpleBody resp `shouldBe` privateTarballBytes
            seenAuth publicUp `shouldReturn` []
            drainJobs env `shouldReturn` []

    it (caseLabel "integration/npm-artifact/native-019" "serves a SHA-1-only private artifact from the private origin under the default trusted floor") $ do
        privateUp <- privateArtifactHitShasumOnly "1.0.0" privateTarballBytes
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- getTarball "1.0.0" (Just "client-token") app
            status resp `shouldBe` 200
            simpleBody resp `shouldBe` privateTarballBytes
            seenAuth publicUp `shouldReturn` []
            drainJobs env `shouldReturn` []

    it (caseLabel "integration/npm-artifact/native-020" "relays a public artifact 404 verbatim and enqueues nothing (the relay verdict gates the back-fill)") $ do
        -- An admitted version can lose its artifact before the download.
        privateUp <- privateArtifactMiss
        publicUp <- artifactUpstreamAnswering "1.0.0" (responseLBS status404 [] "gone")
        queue <- newTestMemoryQueue
        withProxyEnvQueue queue privateUp publicUp Nothing $ \app env _port -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 404
            drainJobs env `shouldReturn` []

    it (caseLabel "integration/npm-artifact/native-021" "relays an oddly-shaped public 2xx verbatim (body untouched) and enqueues nothing") $ do
        -- The relay verdict gates mirroring without validating the served bytes.
        privateUp <- privateArtifactMiss
        publicUp <- artifactUpstreamAnswering "1.0.0" (responseLBS status200 [("Content-Type", "text/html")] "<html>not a tarball</html>")
        queue <- newTestMemoryQueue
        withProxyEnvQueue queue privateUp publicUp Nothing $ \app env _port -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 200
            simpleBody resp `shouldBe` "<html>not a tarball</html>"
            drainJobs env `shouldReturn` []

    it (caseLabel "integration/npm-artifact/native-022" "503s when the public upstream is unavailable (transient), enqueuing nothing") $ do
        privateUp <- privateArtifactMiss
        publicUp <- failingUpstream
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 503
            reason resp `shouldBe` "Service Unavailable"
            decodedBody resp `shouldBe` object ["error" .= ("the upstream registry was unavailable" :: Text)]
            drainJobs env `shouldReturn` []

    it (caseLabel "integration/npm-artifact/native-023" "404s a version absent from the public metadata (forwarded miss), enqueuing nothing") $ do
        privateUp <- privateArtifactMiss
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- getTarball "9.9.9" Nothing app
            status resp `shouldBe` 404
            reason resp `shouldBe` "Not Found"
            drainJobs env `shouldReturn` []

    it (caseLabel "integration/npm-artifact/native-024" "serves the artifact even when the enqueue fails (best-effort, non-blocking)") $ do
        privateUp <- privateArtifactMiss
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        failingQueue <- newFailingQueue
        withProxyEnvQueue failingQueue privateUp publicUp Nothing $ \app _env _port -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 200
            simpleBody resp `shouldBe` publicTarballBytes

    it (caseLabel "integration/npm-artifact/native-025" "500s when an admitted artifact's upstream URL cannot be formed (internal fault)") $ do
        privateUp <- privateArtifactMiss
        let badVersion = "1.0.0["
            admitting base = packument [(badVersion, selfHostedVersion base badVersion)] badVersion [(badVersion, publishedDaysAgo 30)]
        publicUp <- artifactUpstreamServing (encodePackument . admitting) publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- getTarball badVersion Nothing app
            status resp `shouldBe` 500
            reason resp `shouldBe` "Internal Server Error"
            decodedBody resp `shouldBe` object ["error" .= ("could not form the upstream artifact URL" :: Text)]
            drainJobs env `shouldReturn` []

    it (caseLabel "integration/npm-artifact/native-026" "gates a lockfile install hitting the tarball URL with no preceding packument request") $ do
        privateUp <- privateArtifactMiss
        let tooNew base = packument [("2.0.0", selfHostedVersion base "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 3)]
        publicUp <- artifactUpstreamServing (encodePackument . tooNew) publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app _env ->
            getTarball "2.0.0" Nothing app >>= \resp -> status resp `shouldBe` 403

    it (caseLabel "integration/npm-artifact/native-027" "fails internally on a mid-stream private failure, never falling through to public") $ do
        privateUp <- privateArtifactMidStreamFailure
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            (_ :: Either SomeException SResponse) <- try (getTarball "1.0.0" (Just "client-token") app)
            seenAuth publicUp `shouldReturn` []
            drainJobs env `shouldReturn` []

    it (caseLabel "integration/npm-artifact/native-028" "serves a same-host public artifact at its honoured dist.tarball location") $ do
        privateUp <- privateArtifactMiss
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app _env -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 200
            simpleBody resp `shouldBe` publicTarballBytes

    it (caseLabel "integration/npm-artifact/native-029" "drops a cross-host public dist.tarball at projection, so the artifact is a 404 and no fetch") $ do
        -- Projection uses the same authority policy as the artifact route.
        privateUp <- privateArtifactMiss
        publicUp <- crossHostPublicUpstream "cross.localhost" "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 404
            reason resp `shouldBe` "Not Found"
            seenAuth publicUp `shouldReturn` [Nothing]
            drainJobs env `shouldReturn` []

    it (caseLabel "integration/npm-artifact/native-030" "404s a requested filename absent from the version's artifacts (selection by filename)") $ do
        privateUp <- privateArtifactMiss
        publicUp <- honouredPathUpstream "1.0.0" "thing-1.0.0-alt.tgz" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 404
            reason resp `shouldBe` "Not Found"
            drainJobs env `shouldReturn` []

    it (caseLabel "integration/npm-artifact/native-031" "honours a non-conventional public dist.tarball path (not a reconstructed /-/ URL)") $ do
        privateUp <- privateArtifactMiss
        publicUp <- honouredPathUpstream "1.0.0" "thing-1.0.0.tgz" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app _env -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 200
            simpleBody resp `shouldBe` publicTarballBytes

    it (caseLabel "integration/npm-artifact/native-032" "does not reach an off-convention private tarball (a files-host/CDN path): a miss that falls through to public") $ do
        privateUp <- offConventionPrivateUpstream "thing-1.0.0.tgz" privateTarballBytes
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app _env -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 200
            simpleBody resp `shouldBe` publicTarballBytes

    it (caseLabel "integration/npm-artifact/native-033" "reads the same-host conventional URL, ignoring the private packument's declared dist.tarball") $ do
        privateUp <- crossHostPublicUpstream "cross.localhost" "1.0.0" privateTarballBytes
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app _env -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 200
            simpleBody resp `shouldBe` privateTarballBytes
            seenAuth publicUp `shouldReturn` []

    it (caseLabel "integration/npm-artifact/native-034" "serves a same-host private dist.tarball on an internal-IP private origin (trusted-origin exempt from the internal-range block)") $ do
        privateUp <- privateArtifactHit "1.0.0" privateTarballBytes
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        let internalIpPrivate = overPrivateBaseUrl (loopbackRegistryUrl . T.replace "localhost" "127.0.0.1" . registryUrlText)
        queue <- newTestMemoryQueue
        withProxyEnvQueueDeps queue privateUp publicUp Nothing internalIpPrivate $ \app _env _port -> do
            resp <- getTarball "1.0.0" (Just "client-token") app
            status resp `shouldBe` 200
            simpleBody resp `shouldBe` privateTarballBytes
            seenAuth publicUp `shouldReturn` []

    conditionalArtifactSpec

    headTarballSpec

conditionalArtifactSpec :: Spec
conditionalArtifactSpec = describe "pass-through conditional GET on the artifact path" $ do
    it (caseLabel "integration/npm-artifact/native-035" "relays a private-upstream 304 back as a bodiless 304 (never re-downloaded, never fallen through)") $ do
        privateUp <- conditionalArtifactUpstream "1.0.0" privateTarballBytes
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- getTarballWith "1.0.0" [(hIfNoneMatch, "\"v1\"")] app
            status resp `shouldBe` 304
            simpleBody resp `shouldBe` ""
            header "ETag" resp `shouldBe` Just "\"v1\""
            seenAuth publicUp `shouldReturn` []
            drainJobs env `shouldReturn` []

    it (caseLabel "integration/npm-artifact/native-036" "relays a public-upstream 304 back as a bodiless 304 on a private miss") $ do
        privateUp <- privateArtifactMiss
        publicUp <- conditionalArtifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app _env -> do
            resp <- getTarballWith "1.0.0" [(hIfNoneMatch, "\"v1\"")] app
            status resp `shouldBe` 304
            simpleBody resp `shouldBe` ""
            header "ETag" resp `shouldBe` Just "\"v1\""

    it (caseLabel "integration/npm-artifact/native-037" "forwards the client's If-None-Match onto BOTH the private and public artifact upstream requests") $ do
        privateUp <- privateArtifactMiss
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app _env -> do
            _ <- getTarballWith "1.0.0" [(hIfNoneMatch, "\"client-etag\"")] app
            seenArtifactValidators privateUp `shouldReturn` [Just "\"client-etag\""]
            seenArtifactValidators publicUp `shouldReturn` [Just "\"client-etag\""]

    it (caseLabel "integration/npm-artifact/native-038" "streams a non-conditional artifact GET normally (no regression)") $ do
        privateUp <- conditionalArtifactUpstream "1.0.0" privateTarballBytes
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app _env -> do
            resp <- getTarball "1.0.0" Nothing app
            status resp `shouldBe` 200
            simpleBody resp `shouldBe` privateTarballBytes
            seenArtifactValidators privateUp `shouldReturn` [Nothing]

headTarballSpec :: Spec
headTarballSpec = describe "HEAD on a tarball route (no full-artifact body pump)" $ do
    it (caseLabel "integration/npm-artifact/native-039" "answers a private-hit HEAD by probing upstream as a HEAD, never a body GET") $ do
        privateUp <- privateArtifactHit "1.0.0" privateTarballBytes
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- headTarball "1.0.0" (Just "client-token") app
            status resp `shouldBe` 200
            simpleBody resp `shouldBe` ""
            seenArtifactMethods privateUp `shouldReturn` [methodHead]
            seenAuth publicUp `shouldReturn` []
            drainJobs env `shouldReturn` []

    it (caseLabel "integration/npm-artifact/native-040" "answers a public-admit HEAD by probing upstream as a HEAD, enqueuing nothing") $ do
        privateUp <- privateArtifactMiss
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- headTarball "1.0.0" Nothing app
            status resp `shouldBe` 200
            simpleBody resp `shouldBe` ""
            seenArtifactMethods publicUp `shouldReturn` [methodHead]
            drainJobs env `shouldReturn` []

    it (caseLabel "integration/npm-artifact/native-041" "falls a private-artifact-miss HEAD through to a public HEAD (recoverable miss, both probed as HEAD)") $ do
        privateUp <- privateArtifactMiss
        publicUp <- artifactUpstream "1.0.0" publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- headTarball "1.0.0" (Just "client-token") app
            status resp `shouldBe` 200
            simpleBody resp `shouldBe` ""
            seenArtifactMethods privateUp `shouldReturn` [methodHead]
            seenArtifactMethods publicUp `shouldReturn` [methodHead]
            drainJobs env `shouldReturn` []

    it (caseLabel "integration/npm-artifact/native-042" "denies a too-new version with 403 and an empty body, never touching the artifact") $ do
        privateUp <- privateArtifactMiss
        let tooNew base = packument [("2.0.0", selfHostedVersion base "2.0.0")] "2.0.0" [("2.0.0", publishedDaysAgo 3)]
        publicUp <- artifactUpstreamServing (encodePackument . tooNew) publicTarballBytes
        withProxyEnv privateUp publicUp Nothing $ \app env -> do
            resp <- headTarball "2.0.0" Nothing app
            status resp `shouldBe` 403
            simpleBody resp `shouldBe` ""
            seenArtifactMethods publicUp `shouldReturn` []
            drainJobs env `shouldReturn` []

availabilitySpec :: Spec
availabilitySpec = describe "deprecated public artifacts" $
    for_ [("obsolete release line" :: Text, 403), ("", 200)] $ \(notice, expected) ->
        it (caseLabel ("integration/npm-artifact/native-043/" <> (if expected == 403 then "denied" else "allowed")) (toText ("applies the typed deprecation flag before fetching, notice=" <> toString notice :: String))) $ do
            privateUp <- privateArtifactMiss
            let metadata base = packument [("1.0.0", withKeys ["deprecated" .= notice] (selfHostedVersion base "1.0.0"))] "1.0.0" [("1.0.0", publishedDaysAgo 30)]
            publicUp <- artifactUpstreamServing (encodePackument . metadata) publicTarballBytes
            queue <- newTestMemoryQueue
            rules <- prepare inertRuleDeps [admissionOnly (atDefaultPrecedence DenyIfDeprecated), atDefaultPrecedence (AllowIfOlderThan 0)]
            withProxyEnvQueueDeps queue privateUp publicUp Nothing (\deps -> deps{pdRules = rules}) $ \app env _ -> do
                response <- getTarball "1.0.0" Nothing app
                status response `shouldBe` expected
                when (expected == 403) $ do
                    seenAuth publicUp `shouldReturn` [Nothing]
                    drainJobs env `shouldReturn` []
