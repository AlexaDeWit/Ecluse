-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
-- TupleSections pairs optional media-type values with their header names.
{-# LANGUAGE TupleSections #-}

{- | Publish routing and credential behaviour through the application
and a captured target.
-}
module Ecluse.Core.Server.Pipeline.PublishIntegrationSpec (spec) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Network.HTTP.Types (RequestHeaders, hAuthorization, hContentType, methodPost, methodPut, mkStatus)
import Network.Wai (
    Application,
    Request (requestBodyLength, requestHeaders, requestMethod),
    RequestBodyLength (ChunkedBody, KnownLength),
    consumeRequestBodyStrict,
    responseLBS,
 )
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Test (
    SRequest (SRequest),
    SResponse (simpleBody, simpleHeaders),
    defaultRequest,
    runSession,
    setPath,
    srequest,
 )
import Test.Hspec
import UnliftIO (async, timeout, wait)
import UnliftIO.Concurrent (threadDelay)

import Ecluse.Core.Credential (ClientCredential (ClientCredential), Secret, bareCredential, mkSecret)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Package (mkScope)
import Ecluse.Core.Registry.Adapter.Types (RegistryAdapter (adapterProjectName))
import Ecluse.Core.Registry.Npm.Adapter (npmAdapter, npmPublish)
import Ecluse.Core.Registry.Npm.Publish qualified as NpmPublish
import Ecluse.Core.Registry.PyPI.Adapter (pypiAdapter, pypiPublish)
import Ecluse.Core.Registry.PyPI.Project (canonicalName)
import Ecluse.Core.Security (defaultLimits, maxMetadataBytes, maxPublishRequestBytes)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Admission.Bytes (ByteAdmission, newByteAdmission, newByteAdmissionTuned)
import Ecluse.Core.Server.Context (PublishDeps (..))
import Ecluse.Runtime.Server (application, mkServerConfig)
import Ecluse.Runtime.Test.Support (newTestEnv)
import Ecluse.Service (mountBindingFor)
import Ecluse.Test.Server.Mount (inertDepsFor, inertPackumentDeps)
import Ecluse.Test.Stub (Captured (capBody, capMethod, capPath), Stub, allCaptured, headerValue, stubPort, withStubHeaders)
import Ecluse.Test.Wai (status)

withTarget :: Int -> LByteString -> (Int -> Stub -> IO a) -> IO a
withTarget code body k =
    withStubHeaders (mkStatus code "OK") [(hContentType, "application/json")] body $ \stub ->
        k (stubPort stub) stub

targetSaw :: Stub -> IO [(Maybe ByteString, ByteString)]
targetSaw stub = map (\cap -> (headerValue "Authorization" cap, capBody cap)) <$> allCaptured stub

publishDepsAt :: Int -> Maybe Secret -> ByteAdmission -> PublishDeps
publishDepsAt targetPort staticToken bodyBudget =
    PublishDeps
        { pubTargetUrl = loopbackRegistryUrl ("http://127.0.0.1:" <> show targetPort)
        , pubAllowed = NpmPublish.npmPublishAllowed [mkScope "acme"]
        , pubStaticCredential = bareCredential <$> staticToken
        , pubMaxRequestBytes = 26214400
        , pubInboundToken = Nothing
        , pubLimits = defaultLimits{maxPublishRequestBytes = 26214400}
        , pubBodyBudget = bodyBudget
        , pubHelp = Nothing
        , pubProjectName = adapterProjectName npmAdapter
        , pubAdapter = npmPublish
        }

proxyOver :: Maybe PublishDeps -> IO Application
proxyOver publishDeps = do
    env <- newTestEnv
    let cfg = mkServerConfig (maybeToList (mountBindingFor Npm inertPackumentDeps publishDeps))
    pure (application cfg env)

proxyWith :: Maybe (ByteAdmission -> PublishDeps) -> IO Application
proxyWith mkPublishDeps =
    forM mkPublishDeps (\mk -> mk <$> newByteAdmission (128 * 1024 * 1024)) >>= proxyOver

-- The body cap fires before relay, so the target port is an unconnectable placeholder.
cappedProxyWith :: Int -> IO Application
cappedProxyWith cap =
    proxyWith (Just (\bodyBudget -> (publishDepsAt 1 Nothing bodyBudget){pubMaxRequestBytes = cap}))

putPublish :: ByteString -> Maybe Text -> LByteString -> Application -> IO SResponse
putPublish = putPublishAs ChunkedBody

putPublishKnownLength :: ByteString -> Maybe Text -> LByteString -> Application -> IO SResponse
putPublishKnownLength path bearer body =
    putPublishAs (KnownLength (fromIntegral (LBS.length body))) path bearer body

putPublishAs :: RequestBodyLength -> ByteString -> Maybe Text -> LByteString -> Application -> IO SResponse
putPublishAs bodyLen path bearer body =
    runSession (srequest (SRequest req body))
  where
    req =
        (setPath defaultRequest{requestMethod = methodPut, requestHeaders = auth} path)
            { requestBodyLength = bodyLen
            }
    auth = maybe [] (\t -> [(hAuthorization, "Bearer " <> encodeUtf8 t)]) bearer

-- A representative npm publish document body whose declared identity (@_id@,
-- top-level @name@) agrees with the @\@acme\/widget@ URL the tests publish to.
publishBody :: LByteString
publishBody = "{\"_id\":\"@acme/widget\",\"name\":\"@acme/widget\",\"versions\":{}}"

-- A legitimate npm client's shape: @_id@, top-level @name@, and the one @versions[].name@
-- all agree with the @\@acme\/widget@ URL, so the agreement check must still relay it.
matchingVersionBody :: LByteString
matchingVersionBody =
    "{\"_id\":\"@acme/widget\",\"name\":\"@acme/widget\",\"versions\":{\"1.0.0\":{\"name\":\"@acme/widget\",\"version\":\"1.0.0\"}}}"

-- Publish documents whose declared identity disagrees with the in-scope URL name
-- @\@acme\/widget@ on one field: the anti-shadowing bypass a crafted body attempts.
mismatchedIdBody :: LByteString
mismatchedIdBody =
    "{\"_id\":\"@victim/target\",\"name\":\"@acme/widget\",\"versions\":{}}"

mismatchedNameBody :: LByteString
mismatchedNameBody =
    "{\"_id\":\"@acme/widget\",\"name\":\"@victim/target\",\"versions\":{}}"

mismatchedVersionNameBody :: LByteString
mismatchedVersionNameBody =
    "{\"_id\":\"@acme/widget\",\"name\":\"@acme/widget\",\"versions\":{\"1.0.0\":{\"name\":\"@victim/target\",\"version\":\"1.0.0\"}}}"

-- | Publish guards and the target-visible request and response contract.
spec :: Spec
spec = do
    pypiPublishSpec
    npmPublishSpec

npmPublishSpec :: Spec
npmPublishSpec = describe "first-party npm publication relay" $ do
    it (caseLabel "integration/publication/native-001" "503s a publish shed at the aggregate body-byte budget while the capacity is held") $ do
        -- A target that parks the first publish while it holds the whole budget. With zero
        -- waiter room the second sheds at the door, and the first completes once released.
        gate <- newEmptyMVar
        arrived <- newIORef (0 :: Int)
        let blockingApp req respond = do
                _ <- consumeRequestBodyStrict req
                modifyIORef' arrived (+ 1)
                takeMVar gate
                respond (responseLBS (mkStatus 201 "OK") [(hContentType, "application/json")] "{}")
        testWithApplication (pure blockingApp) $ \targetPort -> do
            tightBudget <- newByteAdmissionTuned 1 0 50_000
            app <- proxyOver (Just (publishDepsAt targetPort Nothing tightBudget))
            firstPublish <- async (putPublish "/npm/@acme/widget" (Just "publisher-token") publishBody app)
            -- Wait until the first publish holds the budget (its body reached the
            -- parked target), so the second genuinely contends.
            let awaitHold (n :: Int) = do
                    held <- readIORef arrived
                    when (held == 0 && n > 0) (threadDelay 10_000 >> awaitHold (n - 1))
            awaitHold 500
            secondPublish <- putPublish "/npm/@acme/widget" (Just "publisher-token") publishBody app
            status secondPublish `shouldBe` 503
            putMVar gate ()
            wait firstPublish >>= \firstResp -> status firstResp `shouldBe` 201

    it (caseLabel "integration/publication/native-002" "answers an over-cap chunked publish with the documented 413, not the perimeter's neutral 500 (issue #849)") $ do
        -- A chunked body declares no length, so the counted bounded read enforces the cap as a
        -- value: a fail-closed 413, never a throw through the perimeter's neutral 500.
        app <- cappedProxyWith 8
        resp <- putPublish "/npm/@acme/widget" (Just "publisher-token") publishBody app
        status resp `shouldBe` 413

    it (caseLabel "integration/publication/native-003" "answers an over-cap known-length publish with the documented 413 (Content-Length fast-fail)") $ do
        -- A declared Content-Length over the cap fails closed before the route reads a byte.
        -- Both over-cap shapes render the same 413 through the route contract.
        app <- cappedProxyWith 8
        resp <- putPublishKnownLength "/npm/@acme/widget" (Just "publisher-token") publishBody app
        status resp `shouldBe` 413

    it (caseLabel "integration/publication/native-004" "relays an in-scope publish with the publisher's forwarded credential and returns the target's response") $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> do
            app <- proxyWith (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/@acme/widget" (Just "publisher-token") publishBody app
            status resp `shouldBe` 201
            simpleBody resp `shouldBe` "{\"success\":true}"
            seen <- targetSaw target
            seen `shouldBe` [(Just "Bearer publisher-token", LBS.toStrict publishBody)]

    forM_
        [ ("route-1", "substitutes the static publish token after authenticating the edge token", Just "edge-token", Just "publish-token", Just "edge-token", Just "Bearer publish-token")
        , ("route-2", "preserves a closed-edge token when no static publish token is configured", Just "edge-token", Nothing, Just "edge-token", Just "Bearer edge-token")
        , ("route-3", "preserves the caller token on an open edge even with static publish dependencies", Nothing, Just "publish-token", Just "publisher-token", Just "Bearer publisher-token")
        , ("route-4", "keeps a tokenless open-edge publish anonymous even with static publish dependencies", Nothing, Just "publish-token", Nothing, Nothing)
        ]
        $ \(routeId, label, edgeToken, staticToken, presented, expected) ->
            it (caseLabel ("integration/publication/native-005/" <> routeId) (toText (label :: String))) $
                withTarget 201 "{\"success\":true}" $ \targetPort target -> do
                    app <- proxyWith (Just (\budget -> (publishDepsAt targetPort (mkSecret <$> staticToken) budget){pubInboundToken = mkSecret <$> edgeToken}))
                    resp <- putPublish "/npm/@acme/widget" presented publishBody app
                    status resp `shouldBe` 201
                    targetSaw target `shouldReturn` [(expected, LBS.toStrict publishBody)]

    forM_ [Nothing, Just (mkSecret "publish-token")] $ \staticToken ->
        forM_ [Nothing, Just "wrong-token"] $ \presented ->
            it (caseLabel ("integration/publication/native-006/" <> (if isJust staticToken then "static" else "forwarded") <> "/" <> (if isJust presented then "wrong" else "absent")) (toText ("refuses an unauthenticated closed-edge publish before relay: " <> show (staticToken, presented) :: String))) $
                withTarget 201 "{}" $ \targetPort target -> do
                    app <- proxyWith (Just (\budget -> (publishDepsAt targetPort staticToken budget){pubInboundToken = Just (mkSecret "edge-token")}))
                    resp <- putPublish "/npm/@acme/widget" presented publishBody app
                    status resp `shouldBe` 401
                    targetSaw target `shouldReturn` []

    it (caseLabel "integration/publication/native-007" "refuses Basic authentication on the npm publish route before relay") $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> do
            app <- proxyWith (Just (\budget -> (publishDepsAt targetPort (Just (mkSecret "publish-token")) budget){pubInboundToken = Just (mkSecret "edge-token")}))
            let req = setPath defaultRequest{requestMethod = methodPut, requestHeaders = [(hAuthorization, "Basic dXNlcjplZGdlLXRva2Vu")]} "/npm/@acme/widget"
            resp <- runSession (srequest (SRequest req publishBody)) app
            status resp `shouldBe` 401
            targetSaw target `shouldReturn` []

    it (caseLabel "integration/publication/native-008" "refuses an out-of-scope publish with 403 BEFORE any upstream write (anti-shadowing guard)") $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> do
            app <- proxyWith (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/@other/widget" (Just "publisher-token") publishBody app
            status resp `shouldBe` 403
            targetSaw target `shouldReturn` []

    it (caseLabel "integration/publication/native-009" "refuses an unscoped publish with 403 (an unscoped name is within no scope)") $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> do
            app <- proxyWith (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/widget" (Just "publisher-token") publishBody app
            status resp `shouldBe` 403
            targetSaw target `shouldReturn` []

    it (caseLabel "integration/publication/native-010" "refuses a scope that only prefixes an allowed one (@acme-evil vs the allowed @acme) -- exact match") $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> do
            app <- proxyWith (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/@acme-evil/widget" (Just "publisher-token") publishBody app
            status resp `shouldBe` 403
            targetSaw target `shouldReturn` []

    it (caseLabel "integration/publication/native-011" "sends no Authorization header for an anonymous in-scope publish without a static token") $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> do
            app <- proxyWith (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/@acme/widget" Nothing publishBody app
            status resp `shouldBe` 201
            seen <- targetSaw target
            map fst seen `shouldBe` [Nothing]

    it (caseLabel "integration/publication/native-012" "405s a publish when no publication target is configured (the opt-in is off)") $
        withTarget 201 "{\"success\":true}" $ \_targetPort target -> do
            app <- proxyWith Nothing
            resp <- putPublish "/npm/@acme/widget" (Just "publisher-token") publishBody app
            status resp `shouldBe` 405
            -- The refusal names the missing setting and the publisher's way forward, in
            -- terms no one ecosystem's client owns.
            let body = decodeUtf8 (LBS.toStrict (simpleBody resp)) :: Text
            body `shouldSatisfy` T.isInfixOf "no publication target is configured"
            body `shouldSatisfy` T.isInfixOf "publish directly to the registry you intend to publish to"
            targetSaw target `shouldReturn` []

    it (caseLabel "integration/publication/native-013" "relays the publication target's own error status (e.g. a 409 the registry returns) to the client") $
        withTarget 409 "{\"error\":\"version already exists\"}" $ \targetPort _target -> do
            app <- proxyWith (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/@acme/widget" (Just "publisher-token") publishBody app
            status resp `shouldBe` 409
            simpleBody resp `shouldBe` "{\"error\":\"version already exists\"}"

    -- An unformable target URL is this proxy's own misconfiguration (500). A target that
    -- never answers is an upstream outage (502). Both legs of that split are pinned here.
    it (caseLabel "integration/publication/native-014" "500s a publish whose publication target URL cannot be formed (a misconfiguration, not an outage)") $ do
        app <- proxyWith (Just (\budget -> (publishDepsAt 1 Nothing budget){pubTargetUrl = loopbackRegistryUrl ""}))
        resp <- putPublish "/npm/@acme/widget" (Just "publisher-token") publishBody app
        status resp `shouldBe` 500

    it (caseLabel "integration/publication/native-015" "502s a publish the publication target never answers (an outage, not a misconfiguration)") $ do
        -- Port 1 is an unconnectable placeholder, so the relay reports a transport fault.
        app <- proxyWith (Just (publishDepsAt 1 Nothing))
        resp <- putPublish "/npm/@acme/widget" (Just "publisher-token") publishBody app
        status resp `shouldBe` 502

    -- An in-scope URL whose body declares a different package would publish a name the scope
    -- guard never authorised, so the guard checks @_id@, @name@, and @versions[].name@ first.
    it (caseLabel "integration/publication/native-016" "refuses a publish whose body _id disagrees with the in-scope URL name (403 before any relay)") $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> do
            app <- proxyWith (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/@acme/widget" (Just "publisher-token") mismatchedIdBody app
            status resp `shouldBe` 403
            targetSaw target `shouldReturn` []

    it (caseLabel "integration/publication/native-017" "refuses a publish whose body top-level name disagrees with the in-scope URL name (403 before any relay)") $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> do
            app <- proxyWith (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/@acme/widget" (Just "publisher-token") mismatchedNameBody app
            status resp `shouldBe` 403
            targetSaw target `shouldReturn` []

    it (caseLabel "integration/publication/native-018" "refuses a publish whose body versions[].name disagrees with the in-scope URL name (403 before any relay)") $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> do
            app <- proxyWith (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/@acme/widget" (Just "publisher-token") mismatchedVersionNameBody app
            status resp `shouldBe` 403
            targetSaw target `shouldReturn` []

    it (caseLabel "integration/publication/native-019" "relays an in-scope publish whose body _id / name / versions[].name all agree with the URL name") $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> do
            app <- proxyWith (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/@acme/widget" (Just "publisher-token") matchingVersionBody app
            status resp `shouldBe` 201
            seen <- targetSaw target
            map fst seen `shouldBe` [Just "Bearer publisher-token"]

pypiProxy :: Maybe (ByteAdmission -> PublishDeps) -> IO Application
pypiProxy build = do
    env <- newTestEnv
    deps <- traverse (\mk -> mk <$> newByteAdmission (128 * 1024 * 1024)) build
    pure (application (mkServerConfig (maybeToList (mountBindingFor PyPI (inertDepsFor pypiAdapter) deps))) env)

pypiDeps :: Int -> ByteAdmission -> PublishDeps
pypiDeps port budget =
    (publishDepsAt port Nothing budget)
        { pubTargetUrl = loopbackRegistryUrl ("http://127.0.0.1:" <> show port <> "/legacy/")
        , pubAdapter = pypiPublish
        , pubProjectName = adapterProjectName pypiAdapter
        , pubAllowed = (== "acme-widget") . canonicalName
        }

multipartType :: ByteString
multipartType = "multipart/form-data; boundary=relay-boundary"

multipartBody :: [ByteString] -> LByteString
multipartBody names = fromStrict (foldMap field names <> file <> "--relay-boundary--\r\n")
  where
    field value = "--relay-boundary\r\nContent-Disposition: form-data; name=\"name\"\r\n\r\n" <> value <> "\r\n"
    file = "--relay-boundary\r\nContent-Disposition: form-data; name=\"content\"; filename=\"acme_widget-1.0.tar.gz\"\r\nContent-Type: application/octet-stream\r\n\r\noriginal archive bytes\r\n"

postUpload :: RequestBodyLength -> RequestHeaders -> LByteString -> Application -> IO SResponse
postUpload bodyLength headers body = runSession (srequest (SRequest request body))
  where
    request =
        (setPath defaultRequest "/pypi/legacy/")
            { requestMethod = methodPost
            , requestBodyLength = bodyLength
            , requestHeaders = headers
            }

pypiPublishSpec :: Spec
pypiPublishSpec = describe "first-party PyPI publication relay" $ do
    it "returns 405 and POST Allow when publication is disabled" $ do
        app <- pypiProxy Nothing
        response <- postUpload ChunkedBody [] "" app
        status response `shouldBe` 405
        lookup "Allow" (simpleHeaders response) `shouldBe` Just "POST"

    for_ [(201, "accepted"), (400, "File already exists."), (401, "private refusal"), (409, "conflict")] $ \(code, responseBody) ->
        it ("preserves the original upload and native response " <> show code) $
            withTarget code responseBody $ \port target -> do
                app <- pypiProxy (Just (pypiDeps port))
                let body = multipartBody ["Acme_Widget", "acme-widget"]
                    auth = "Basic OnBhc3N3b3Jk"
                response <- postUpload ChunkedBody [("Content-Type", multipartType), ("Authorization", auth)] body app
                status response `shouldBe` code
                simpleBody response `shouldBe` responseBody
                lookup "Content-Type" (simpleHeaders response) `shouldBe` Just "text/plain"
                targetSaw target `shouldReturn` [(Just auth, LBS.toStrict body)]
                captured <- allCaptured target
                map (headerValue "Content-Type") captured `shouldBe` [Just multipartType]
                map capMethod captured `shouldBe` ["POST"]
                map capPath captured `shouldBe` ["/legacy/"]

    for_ [[], [""], ["@invalid/name"], ["acme-widget", "victim"], ["victim", "acme-widget"]] $ \names ->
        it ("refuses absent, invalid, or conflicting names " <> show names) $
            withTarget 201 "accepted" $ \port target -> do
                app <- pypiProxy (Just (pypiDeps port))
                response <- postUpload ChunkedBody [("Content-Type", multipartType)] (multipartBody names) app
                status response `shouldBe` 400
                targetSaw target `shouldReturn` []

    for_ [Nothing, Just "multipart/form-data; boundary=wrong", Just "multipart/form-data; boundary=relay-boundary; boundary=other"] $ \contentType ->
        it ("refuses missing or ambiguous framing " <> show contentType) $
            withTarget 201 "accepted" $ \port target -> do
                app <- pypiProxy (Just (pypiDeps port))
                response <- postUpload ChunkedBody (maybeToList (("Content-Type",) <$> contentType)) (multipartBody ["acme-widget"]) app
                status response `shouldBe` 400
                targetSaw target `shouldReturn` []

    for_ ["trailing bytes", "--relay-boundary\r\nContent-Disposition: form-data; name=\"name\"\r\n\r\nvictim\r\n--relay-boundary--\r\n"] $ \suffix ->
        it "refuses trailing data or a forbidden name after the closing delimiter" $
            withTarget 201 "accepted" $ \port target -> do
                app <- pypiProxy (Just (pypiDeps port))
                response <- postUpload ChunkedBody [("Content-Type", multipartType)] (multipartBody ["acme-widget"] <> suffix) app
                status response `shouldBe` 400
                targetSaw target `shouldReturn` []

    it "refuses an agreed name outside the first-party declaration" $
        withTarget 201 "accepted" $ \port target -> do
            app <- pypiProxy (Just (pypiDeps port))
            response <- postUpload ChunkedBody [("Content-Type", multipartType)] (multipartBody ["victim"]) app
            status response `shouldBe` 403
            targetSaw target `shouldReturn` []

    it "authenticates before parsing malformed multipart content" $
        withTarget 201 "accepted" $ \port target -> do
            app <- pypiProxy (Just (\budget -> (pypiDeps port budget){pubInboundToken = Just (mkSecret "edge-token")}))
            response <- postUpload ChunkedBody [] "malformed" app
            status response `shouldBe` 401
            targetSaw target `shouldReturn` []

    for_ [ChunkedBody, KnownLength 4096] $ \bodyLength ->
        it ("enforces the request limit before relay for " <> show bodyLength) $
            withTarget 201 "accepted" $ \port target -> do
                app <- pypiProxy (Just (\budget -> (pypiDeps port budget){pubMaxRequestBytes = 8}))
                response <- postUpload bodyLength [("Content-Type", multipartType)] (multipartBody ["acme-widget"]) app
                status response `shouldBe` 413
                targetSaw target `shouldReturn` []

    it "bounds the target's raw response body" $
        withTarget 201 (LBS.replicate 256 120) $ \port _ -> do
            app <- pypiProxy (Just (\budget -> (pypiDeps port budget){pubLimits = defaultLimits{maxMetadataBytes = 128}}))
            response <- postUpload ChunkedBody [("Content-Type", multipartType)] (multipartBody ["acme-widget"]) app
            status response `shouldBe` 502

    it "keeps the Basic username while authenticating the password at the edge" $
        withTarget 201 "accepted" $ \port target -> do
            app <- pypiProxy (Just (\budget -> (pypiDeps port budget){pubInboundToken = Just (mkSecret "edge-token")}))
            let body = multipartBody ["acme-widget"]
                auth = "Basic dXNlcjplZGdlLXRva2Vu"
            response <- postUpload ChunkedBody [("Content-Type", multipartType), ("Authorization", auth)] body app
            status response `shouldBe` 201
            targetSaw target `shouldReturn` [(Just auth, LBS.toStrict body)]

    it "uses the declared static Basic pair only after edge authentication" $
        withTarget 201 "accepted" $ \port target -> do
            app <-
                pypiProxy
                    ( Just
                        ( \budget ->
                            (pypiDeps port budget)
                                { pubInboundToken = Just (mkSecret "edge-token")
                                , pubStaticCredential = Just (ClientCredential (Just "user") (mkSecret "password"))
                                }
                        )
                    )
            let body = multipartBody ["acme-widget"]
            response <- postUpload ChunkedBody [("Content-Type", multipartType), ("Authorization", "Basic dXNlcjplZGdlLXRva2Vu")] body app
            status response `shouldBe` 201
            targetSaw target `shouldReturn` [(Just "Basic dXNlcjpwYXNzd29yZA==", LBS.toStrict body)]

    it "holds byte admission until the publication response completes" $ do
        release <- newEmptyMVar
        entered <- newEmptyMVar
        let blockingApp request respond = do
                _ <- consumeRequestBodyStrict request
                putMVar entered ()
                takeMVar release
                respond (responseLBS (mkStatus 201 "") [] "accepted")
        testWithApplication (pure blockingApp) $ \port -> do
            budget <- newByteAdmissionTuned 1 0 50_000
            app <- pypiProxy (Just (const (pypiDeps port budget)))
            let send = postUpload ChunkedBody [("Content-Type", multipartType)] (multipartBody ["acme-widget"]) app
            first <- async send
            arrival <- timeout 5_000_000 (takeMVar entered)
            arrival `shouldBe` Just ()
            second <- send
            status second `shouldBe` 503
            putMVar release ()
            completed <- wait first
            status completed `shouldBe` 201
