-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
-- TupleSections pairs optional media-type values with their header names.
{-# LANGUAGE TupleSections #-}

-- | Publish routing and credentials through the application and a captured target.
module Ecluse.Core.Server.Pipeline.PublishIntegrationSpec (spec) where

import Data.Aeson (Value (Object, String), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Text qualified as T
import Network.HTTP.Types (RequestHeaders, ResponseHeaders, Status, hAuthorization, hContentType, methodPost, methodPut, mkStatus)
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
import Ecluse.Core.Deadline (Deadlines)
import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI))
import Ecluse.Core.Package (mkScope)
import Ecluse.Core.Registry.Adapter.Types (RegistryAdapter (adapterProjectName))
import Ecluse.Core.Registry.Cargo.Adapter (cargoAdapter)
import Ecluse.Core.Registry.Cargo.Project (projectName)
import Ecluse.Core.Registry.Cargo.Publish (cargoPublish)
import Ecluse.Core.Registry.Cargo.Publish.Body qualified as CargoBody
import Ecluse.Core.Registry.Npm.Adapter (npmAdapter, npmPublish)
import Ecluse.Core.Registry.Npm.Publish qualified as NpmPublish
import Ecluse.Core.Registry.PyPI.Adapter (pypiAdapter, pypiPublish)
import Ecluse.Core.Registry.PyPI.Project (canonicalName)
import Ecluse.Core.Registry.PyPI.Publish.Multipart (pypiFileUploadOnly)
import Ecluse.Core.Registry.Request (nativePresentation)
import Ecluse.Core.Security (defaultLimits, maxMetadataBytes, maxPublishRequestBytes)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Admission.Bytes (ByteAdmission, newByteAdmission, newByteAdmissionTuned)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (ChargeFactors))
import Ecluse.Core.Server.Context (PackumentDeps (pdMountBaseUrl), PublishDeps (..))
import Ecluse.Runtime.Server (application, mkServerConfig)
import Ecluse.Runtime.Test.Support (newTestEnv)
import Ecluse.Service (mountBindingFor)
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Registry.Cargo.Fixtures (CargoRelease (releaseArchive), cargoUpload, cargoUploadMetadata, cargoUploadWithMetadata)
import Ecluse.Test.Registry.Cargo.Publish (cargoForeignDevArchive, cargoForeignDevMetadata, cargoForeignRegistryArchive, cargoForeignRegistryMetadata, cargoForeignRegistryRefusal, cargoPublicationFixture, cargoResolverAgreements, cargoResolverArchive, cargoResolverContradictions, cargoResolverDependencyField)
import Ecluse.Test.Registry.Npm.Publish (npmDependentRelease, npmDeprecateDocument, npmExternalDependencyRefusal, npmMisfiledVersion, npmReleaseDocument, npmRepeatedAttachments, npmTaggedRelease)
import Ecluse.Test.Server.Mount (inertDepsFor, inertPackumentDeps)
import Ecluse.Test.Stub (Captured (capBody, capHeaders, capMethod, capPath), Stub, allCaptured, headerValue, stubBaseUrl, stubPort, withRoutedStub, withStubHeaders)
import Ecluse.Test.Support (expectRight)
import Ecluse.Test.Wai (selfBaseUrlOf, status)

withTarget :: Int -> LByteString -> (Int -> Stub -> IO a) -> IO a
withTarget code body k =
    withStubHeaders (mkStatus code "OK") [(hContentType, "application/json")] body $ \stub ->
        k (stubPort stub) stub

targetSaw :: Stub -> IO [(Maybe ByteString, ByteString)]
targetSaw stub = map (\cap -> (headerValue "Authorization" cap, capBody cap)) <$> allCaptured stub

publishDepsAt :: Int -> Maybe Secret -> ByteAdmission -> PublishDeps
publishDepsAt targetPort staticToken bodyBudget =
    PublishDeps
        { pubReadUrl = loopbackRegistryUrl ("http://127.0.0.1:" <> show targetPort)
        , pubTargetUrl = loopbackRegistryUrl ("http://127.0.0.1:" <> show targetPort)
        , pubAllowed = NpmPublish.npmPublishAllowed [mkScope "acme"]
        , pubStaticCredential = bareCredential <$> staticToken
        , pubStaticPresentation = nativePresentation
        , pubMaxRequestBytes = 26214400
        , pubInboundToken = Nothing
        , pubLimits = defaultLimits{maxPublishRequestBytes = 26214400}
        , pubBodyBudget = bodyBudget
        , pubHelp = Nothing
        , pubProjectName = adapterProjectName npmAdapter
        , pubAdapter = npmPublish
        }

proxyOver :: Deadlines -> Maybe PublishDeps -> IO Application
proxyOver deadlines publishDeps = do
    env <- newTestEnv deadlines
    let cfg = mkServerConfig (maybeToList (mountBindingFor Npm inertPackumentDeps publishDeps))
    pure (application cfg env)

proxyWith :: Deadlines -> Maybe (ByteAdmission -> PublishDeps) -> IO Application
proxyWith deadlines mkPublishDeps =
    forM mkPublishDeps (\mk -> mk <$> newByteAdmission deadlines (128 * 1024 * 1024)) >>= proxyOver deadlines

-- The body cap fires before relay, so the target port is an unconnectable placeholder.
cappedProxyWith :: Deadlines -> Int -> IO Application
cappedProxyWith deadlines cap =
    proxyWith deadlines (Just (\bodyBudget -> (publishDepsAt 1 Nothing bodyBudget){pubMaxRequestBytes = cap}))

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

-- An npm 11 release whose @_id@, @name@, and version @name@ all agree with the @\@acme\/widget@ URL.
publishBody :: LByteString
publishBody = npmReleaseDocument widget widget widget

-- Releases whose declared identity disagrees with the in-scope URL name on one field: the
-- anti-shadowing bypass a crafted body attempts.
mismatchedIdBody :: LByteString
mismatchedIdBody = npmReleaseDocument "@victim/target" widget widget

mismatchedNameBody :: LByteString
mismatchedNameBody = npmReleaseDocument widget "@victim/target" widget

mismatchedVersionNameBody :: LByteString
mismatchedVersionNameBody = npmReleaseDocument widget widget "@victim/target"

widget :: Text
widget = "@acme/widget"

-- | Publish guards and the target-visible request and response contract.
spec :: Spec
spec = do
    cargoPublishSpec
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
        testWithApplication (pure blockingApp) $ \targetPort -> withSweeper $ \deadlines -> do
            tightBudget <- newByteAdmissionTuned deadlines 1 0 50_000
            app <- proxyOver deadlines (Just (publishDepsAt targetPort Nothing tightBudget))
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

    it (caseLabel "integration/publication/native-002" "answers an over-cap chunked publish with the documented 413, not the perimeter's neutral 500 (issue #849)") $ withSweeper $ \deadlines -> do
        -- A chunked body declares no length, so the counted bounded read enforces the cap as a
        -- value: a fail-closed 413, never a throw through the perimeter's neutral 500.
        app <- cappedProxyWith deadlines 8
        resp <- putPublish "/npm/@acme/widget" (Just "publisher-token") publishBody app
        status resp `shouldBe` 413

    it (caseLabel "integration/publication/native-003" "answers an over-cap known-length publish with the documented 413 (Content-Length fast-fail)") $ withSweeper $ \deadlines -> do
        -- A declared Content-Length over the cap fails closed before the route reads a byte.
        -- Both over-cap shapes render the same 413 through the route contract.
        app <- cappedProxyWith deadlines 8
        resp <- putPublishKnownLength "/npm/@acme/widget" (Just "publisher-token") publishBody app
        status resp `shouldBe` 413

    it (caseLabel "integration/publication/native-004" "relays an in-scope publish with the publisher's forwarded credential and returns the target's response") $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> withSweeper $ \deadlines -> do
            app <- proxyWith deadlines (Just (publishDepsAt targetPort Nothing))
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
                withTarget 201 "{\"success\":true}" $ \targetPort target -> withSweeper $ \deadlines -> do
                    app <- proxyWith deadlines (Just (\budget -> (publishDepsAt targetPort (mkSecret <$> staticToken) budget){pubInboundToken = mkSecret <$> edgeToken}))
                    resp <- putPublish "/npm/@acme/widget" presented publishBody app
                    status resp `shouldBe` 201
                    targetSaw target `shouldReturn` [(expected, LBS.toStrict publishBody)]

    forM_ [Nothing, Just (mkSecret "publish-token")] $ \staticToken ->
        forM_ [Nothing, Just "wrong-token"] $ \presented ->
            it (caseLabel ("integration/publication/native-006/" <> (if isJust staticToken then "static" else "forwarded") <> "/" <> (if isJust presented then "wrong" else "absent")) (toText ("refuses an unauthenticated closed-edge publish before relay: " <> show (staticToken, presented) :: String))) $
                withTarget 201 "{}" $ \targetPort target -> withSweeper $ \deadlines -> do
                    app <- proxyWith deadlines (Just (\budget -> (publishDepsAt targetPort staticToken budget){pubInboundToken = Just (mkSecret "edge-token")}))
                    resp <- putPublish "/npm/@acme/widget" presented publishBody app
                    status resp `shouldBe` 401
                    targetSaw target `shouldReturn` []

    it (caseLabel "integration/publication/native-007" "refuses Basic authentication on the npm publish route before relay") $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> withSweeper $ \deadlines -> do
            app <- proxyWith deadlines (Just (\budget -> (publishDepsAt targetPort (Just (mkSecret "publish-token")) budget){pubInboundToken = Just (mkSecret "edge-token")}))
            let req = setPath defaultRequest{requestMethod = methodPut, requestHeaders = [(hAuthorization, "Basic dXNlcjplZGdlLXRva2Vu")]} "/npm/@acme/widget"
            resp <- runSession (srequest (SRequest req publishBody)) app
            status resp `shouldBe` 401
            targetSaw target `shouldReturn` []

    it (caseLabel "integration/publication/native-008" "refuses an out-of-scope publish with 403 BEFORE any upstream write (anti-shadowing guard)") $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> withSweeper $ \deadlines -> do
            app <- proxyWith deadlines (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/@other/widget" (Just "publisher-token") publishBody app
            status resp `shouldBe` 403
            targetSaw target `shouldReturn` []

    it (caseLabel "integration/publication/native-009" "refuses an unscoped publish with 403 (an unscoped name is within no scope)") $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> withSweeper $ \deadlines -> do
            app <- proxyWith deadlines (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/widget" (Just "publisher-token") publishBody app
            status resp `shouldBe` 403
            targetSaw target `shouldReturn` []

    it (caseLabel "integration/publication/native-010" "refuses a scope that only prefixes an allowed one (@acme-evil vs the allowed @acme) -- exact match") $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> withSweeper $ \deadlines -> do
            app <- proxyWith deadlines (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/@acme-evil/widget" (Just "publisher-token") publishBody app
            status resp `shouldBe` 403
            targetSaw target `shouldReturn` []

    it (caseLabel "integration/publication/native-011" "sends no Authorization header for an anonymous in-scope publish without a static token") $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> withSweeper $ \deadlines -> do
            app <- proxyWith deadlines (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/@acme/widget" Nothing publishBody app
            status resp `shouldBe` 201
            seen <- targetSaw target
            map fst seen `shouldBe` [Nothing]

    it (caseLabel "integration/publication/native-012" "405s a publish when no publication target is configured (the opt-in is off)") $
        withTarget 201 "{\"success\":true}" $ \_targetPort target -> withSweeper $ \deadlines -> do
            app <- proxyWith deadlines Nothing
            resp <- putPublish "/npm/@acme/widget" (Just "publisher-token") publishBody app
            status resp `shouldBe` 405
            -- The refusal names the missing setting and the publisher's way forward, in
            -- terms no one ecosystem's client owns.
            let body = decodeUtf8 (LBS.toStrict (simpleBody resp)) :: Text
            body `shouldSatisfy` T.isInfixOf "no publication target is configured"
            body `shouldSatisfy` T.isInfixOf "publish directly to the registry you intend to publish to"
            targetSaw target `shouldReturn` []

    it (caseLabel "integration/publication/native-013" "relays the publication target's own error status (e.g. a 409 the registry returns) to the client") $
        withTarget 409 "{\"error\":\"version already exists\"}" $ \targetPort _target -> withSweeper $ \deadlines -> do
            app <- proxyWith deadlines (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/@acme/widget" (Just "publisher-token") publishBody app
            status resp `shouldBe` 409
            simpleBody resp `shouldBe` "{\"error\":\"version already exists\"}"

    -- An unformable target URL is this proxy's own misconfiguration (500). A target that
    -- never answers is an upstream outage (502). Both legs of that split are pinned here.
    it (caseLabel "integration/publication/native-014" "500s a publish whose publication target URL cannot be formed (a misconfiguration, not an outage)") $ withSweeper $ \deadlines -> do
        app <- proxyWith deadlines (Just (\budget -> (publishDepsAt 1 Nothing budget){pubTargetUrl = loopbackRegistryUrl ""}))
        resp <- putPublish "/npm/@acme/widget" (Just "publisher-token") publishBody app
        status resp `shouldBe` 500

    it (caseLabel "integration/publication/native-015" "502s a publish the publication target never answers (an outage, not a misconfiguration)") $ withSweeper $ \deadlines -> do
        -- Port 1 is an unconnectable placeholder, so the relay reports a transport fault.
        app <- proxyWith deadlines (Just (publishDepsAt 1 Nothing))
        resp <- putPublish "/npm/@acme/widget" (Just "publisher-token") publishBody app
        status resp `shouldBe` 502

    -- An in-scope URL whose body declares a different package would publish a name the scope
    -- guard never authorised, so the guard checks @_id@, @name@, and @versions[].name@ first.
    it (caseLabel "integration/publication/native-016" "refuses a publish whose body _id disagrees with the in-scope URL name (403 before any relay)") $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> withSweeper $ \deadlines -> do
            app <- proxyWith deadlines (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/@acme/widget" (Just "publisher-token") mismatchedIdBody app
            status resp `shouldBe` 403
            targetSaw target `shouldReturn` []

    it (caseLabel "integration/publication/native-017" "refuses a publish whose body top-level name disagrees with the in-scope URL name (403 before any relay)") $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> withSweeper $ \deadlines -> do
            app <- proxyWith deadlines (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/@acme/widget" (Just "publisher-token") mismatchedNameBody app
            status resp `shouldBe` 403
            targetSaw target `shouldReturn` []

    it (caseLabel "integration/publication/native-018" "refuses a publish whose body versions[].name disagrees with the in-scope URL name (403 before any relay)") $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> withSweeper $ \deadlines -> do
            app <- proxyWith deadlines (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/@acme/widget" (Just "publisher-token") mismatchedVersionNameBody app
            status resp `shouldBe` 403
            targetSaw target `shouldReturn` []

    it (caseLabel "integration/publication/native-019" "relays an in-scope publish whose body _id / name / versions[].name all agree with the URL name") $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> withSweeper $ \deadlines -> do
            app <- proxyWith deadlines (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/@acme/widget" (Just "publisher-token") publishBody app
            status resp `shouldBe` 201
            seen <- targetSaw target
            map fst seen `shouldBe` [Just "Bearer publisher-token"]

    it (caseLabel "integration/publication/native-020" "refuses an npm deprecate body with 501 in npm's error shape before any relay") $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> withSweeper $ \deadlines -> do
            app <- proxyWith deadlines (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/@acme/widget" (Just "publisher-token") (npmDeprecateDocument "use 1.1.0") app
            status resp `shouldBe` 501
            Aeson.decode (simpleBody resp) `shouldBe` Just (Aeson.object ["error" .= NpmPublish.npmNewReleaseOnly])
            targetSaw target `shouldReturn` []

    it (caseLabel "integration/publication/native-021" "refuses an undecodable npm publish body with 400 before any relay") $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> withSweeper $ \deadlines -> do
            app <- proxyWith deadlines (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/@acme/widget" (Just "publisher-token") "{\"_id\":" app
            status resp `shouldBe` 400
            targetSaw target `shouldReturn` []

    it (caseLabel "integration/publication/native-022" "refuses an npm publish body that repeats _attachments with 400 before any relay") $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> withSweeper $ \deadlines -> do
            app <- proxyWith deadlines (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/@acme/widget" (Just "publisher-token") npmRepeatedAttachments app
            status resp `shouldBe` 400
            targetSaw target `shouldReturn` []

    it (caseLabel "integration/publication/native-023" "refuses an npm release that moves latest to another version with 501 before any relay") $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> withSweeper $ \deadlines -> do
            app <- proxyWith deadlines (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/@acme/widget" (Just "publisher-token") (npmTaggedRelease (Aeson.object ["latest" .= ("1.0.0" :: Text)])) app
            status resp `shouldBe` 501
            targetSaw target `shouldReturn` []

    it (caseLabel "integration/publication/native-024" "refuses an npm release filed under a key that differs from its version field with 400 before any relay") $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> withSweeper $ \deadlines -> do
            app <- proxyWith deadlines (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/@acme/widget" (Just "publisher-token") npmMisfiledVersion app
            status resp `shouldBe` 400
            targetSaw target `shouldReturn` []

    it (caseLabel "integration/publication/native-025" "refuses an npm release whose dependency names a git source with 400 naming the dependency before any relay") $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> withSweeper $ \deadlines -> do
            app <- proxyWith deadlines (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/@acme/widget" (Just "publisher-token") (npmDependentRelease ["dependencies" .= Aeson.object ["left-pad" .= ("github:acme/left-pad" :: Text)]]) app
            status resp `shouldBe` 400
            Aeson.decode (simpleBody resp) `shouldBe` Just (Aeson.object ["error" .= npmExternalDependencyRefusal "left-pad" "dependencies" "a git source"])
            targetSaw target `shouldReturn` []

    it (caseLabel "integration/publication/native-026" "refuses an npm release whose optional dependency names a remote URL with 400 before any relay") $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> withSweeper $ \deadlines -> do
            app <- proxyWith deadlines (Just (publishDepsAt targetPort Nothing))
            resp <- putPublish "/npm/@acme/widget" (Just "publisher-token") (npmDependentRelease ["optionalDependencies" .= Aeson.object ["left-pad" .= ("https://example.test/left-pad-1.0.0.tgz" :: Text)]]) app
            status resp `shouldBe` 400
            Aeson.decode (simpleBody resp) `shouldBe` Just (Aeson.object ["error" .= npmExternalDependencyRefusal "left-pad" "optionalDependencies" "a remote URL"])
            targetSaw target `shouldReturn` []

    it (caseLabel "integration/publication/native-027" "relays an npm release whose git source sits only in devDependencies") $
        withTarget 201 "{\"success\":true}" $ \targetPort target -> withSweeper $ \deadlines -> do
            app <- proxyWith deadlines (Just (publishDepsAt targetPort Nothing))
            let body = npmDependentRelease ["devDependencies" .= Aeson.object ["left-pad" .= ("github:acme/left-pad" :: Text)]]
            resp <- putPublish "/npm/@acme/widget" (Just "publisher-token") body app
            status resp `shouldBe` 201
            targetSaw target `shouldReturn` [(Just "Bearer publisher-token", LBS.toStrict body)]

pypiProxy :: Deadlines -> Maybe (ByteAdmission -> PublishDeps) -> IO Application
pypiProxy deadlines build = do
    env <- newTestEnv deadlines
    deps <- traverse (\mk -> mk <$> newByteAdmission deadlines (128 * 1024 * 1024)) build
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

-- A file_upload of one archive under the given name parts, in the order twine sends them.
multipartBody :: [ByteString] -> LByteString
multipartBody names = uploadOf names "acme_widget-1.0.tar.gz"

uploadOf :: [ByteString] -> ByteString -> LByteString
uploadOf names file = formBody ((":action", "file_upload") : map ("name",) names <> [("version", "1.0")]) (archivePart file)

-- A raw remove_pkg, which pypiserver honours and no uploader sends.
removalBody :: LByteString
removalBody = formBody [(":action", "remove_pkg"), ("name", "acme-widget"), ("version", "1.0")] ""

formBody :: [(ByteString, ByteString)] -> ByteString -> LByteString
formBody fields archive = fromStrict (foldMap field fields <> archive <> "--relay-boundary--\r\n")
  where
    field (name, value) = "--relay-boundary\r\nContent-Disposition: form-data; name=\"" <> name <> "\"\r\n\r\n" <> value <> "\r\n"

archivePart :: ByteString -> ByteString
archivePart file = archiveOf file "original archive bytes"

archiveOf :: ByteString -> ByteString -> ByteString
archiveOf file bytes = "--relay-boundary\r\nContent-Disposition: form-data; name=\"content\"; filename=\"" <> file <> "\"\r\nContent-Type: application/octet-stream\r\n\r\n" <> bytes <> "\r\n"

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
    it (caseLabel "integration/publication/native-pypi-001" "returns 405 and POST Allow when publication is disabled") $ withSweeper $ \deadlines -> do
        app <- pypiProxy deadlines Nothing
        response <- postUpload ChunkedBody [] "" app
        status response `shouldBe` 405
        lookup "Allow" (simpleHeaders response) `shouldBe` Just "POST"

    for_ [(201, "accepted"), (400, "File already exists."), (401, "private refusal"), (409, "conflict")] $ \(code, responseBody) ->
        it (caseLabel ("integration/publication/native-pypi-002/" <> toText (show code :: String)) ("preserves the original upload and native response " <> toText (show code :: String))) $
            withTarget code responseBody $ \port target -> withSweeper $ \deadlines -> do
                app <- pypiProxy deadlines (Just (pypiDeps port))
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

    for_ [("absent", []), ("empty", [""]), ("invalid", ["@invalid/name"]), ("conflict-first", ["acme-widget", "victim"]), ("conflict-last", ["victim", "acme-widget"])] $ \(nameCase, names) ->
        it (caseLabel ("integration/publication/native-pypi-003/" <> nameCase) ("refuses absent, invalid, or conflicting names " <> toText (show names :: String))) $
            withTarget 201 "accepted" $ \port target -> withSweeper $ \deadlines -> do
                app <- pypiProxy deadlines (Just (pypiDeps port))
                response <- postUpload ChunkedBody [("Content-Type", multipartType)] (multipartBody names) app
                status response `shouldBe` 400
                targetSaw target `shouldReturn` []

    for_ [("missing", Nothing), ("wrong", Just "multipart/form-data; boundary=wrong"), ("ambiguous", Just "multipart/form-data; boundary=relay-boundary; boundary=other")] $ \(framingCase, contentType) ->
        it (caseLabel ("integration/publication/native-pypi-004/" <> framingCase) ("refuses missing or ambiguous framing " <> toText (show contentType :: String))) $
            withTarget 201 "accepted" $ \port target -> withSweeper $ \deadlines -> do
                app <- pypiProxy deadlines (Just (pypiDeps port))
                response <- postUpload ChunkedBody (maybeToList (("Content-Type",) <$> contentType)) (multipartBody ["acme-widget"]) app
                status response `shouldBe` 400
                targetSaw target `shouldReturn` []

    for_ [("trailing", "trailing bytes"), ("forbidden-name", "--relay-boundary\r\nContent-Disposition: form-data; name=\"name\"\r\n\r\nvictim\r\n--relay-boundary--\r\n")] $ \(suffixCase, suffix) ->
        it (caseLabel ("integration/publication/native-pypi-005/" <> suffixCase) "refuses trailing data or a forbidden name after the closing delimiter") $
            withTarget 201 "accepted" $ \port target -> withSweeper $ \deadlines -> do
                app <- pypiProxy deadlines (Just (pypiDeps port))
                response <- postUpload ChunkedBody [("Content-Type", multipartType)] (multipartBody ["acme-widget"] <> suffix) app
                status response `shouldBe` 400
                targetSaw target `shouldReturn` []

    it (caseLabel "integration/publication/native-pypi-006" "refuses an agreed name outside the first-party declaration") $
        withTarget 201 "accepted" $ \port target -> withSweeper $ \deadlines -> do
            app <- pypiProxy deadlines (Just (pypiDeps port))
            response <- postUpload ChunkedBody [("Content-Type", multipartType)] (uploadOf ["victim"] "victim-1.0.tar.gz") app
            status response `shouldBe` 403
            targetSaw target `shouldReturn` []

    it (caseLabel "integration/publication/native-pypi-007" "authenticates before parsing malformed multipart content") $
        withTarget 201 "accepted" $ \port target -> withSweeper $ \deadlines -> do
            app <- pypiProxy deadlines (Just (\budget -> (pypiDeps port budget){pubInboundToken = Just (mkSecret "edge-token")}))
            response <- postUpload ChunkedBody [] "malformed" app
            status response `shouldBe` 401
            targetSaw target `shouldReturn` []

    for_ [("chunked", ChunkedBody), ("known", KnownLength 4096)] $ \(lengthCase, bodyLength) ->
        it (caseLabel ("integration/publication/native-pypi-008/" <> lengthCase) ("enforces the request limit before relay for " <> toText (show bodyLength :: String))) $
            withTarget 201 "accepted" $ \port target -> withSweeper $ \deadlines -> do
                app <- pypiProxy deadlines (Just (\budget -> (pypiDeps port budget){pubMaxRequestBytes = 8}))
                response <- postUpload bodyLength [("Content-Type", multipartType)] (multipartBody ["acme-widget"]) app
                status response `shouldBe` 413
                targetSaw target `shouldReturn` []

    it (caseLabel "integration/publication/native-pypi-009" "bounds the target's raw response body") $
        withTarget 201 (LBS.replicate 256 120) $ \port _ -> withSweeper $ \deadlines -> do
            app <- pypiProxy deadlines (Just (\budget -> (pypiDeps port budget){pubLimits = defaultLimits{maxMetadataBytes = 128}}))
            response <- postUpload ChunkedBody [("Content-Type", multipartType)] (multipartBody ["acme-widget"]) app
            status response `shouldBe` 502

    it (caseLabel "integration/publication/native-pypi-010" "keeps the Basic username while authenticating the password at the edge") $
        withTarget 201 "accepted" $ \port target -> withSweeper $ \deadlines -> do
            app <- pypiProxy deadlines (Just (\budget -> (pypiDeps port budget){pubInboundToken = Just (mkSecret "edge-token")}))
            let body = multipartBody ["acme-widget"]
                auth = "Basic dXNlcjplZGdlLXRva2Vu"
            response <- postUpload ChunkedBody [("Content-Type", multipartType), ("Authorization", auth)] body app
            status response `shouldBe` 201
            targetSaw target `shouldReturn` [(Just auth, LBS.toStrict body)]

    it (caseLabel "integration/publication/native-pypi-011" "uses the declared static Basic pair only after edge authentication") $
        withTarget 201 "accepted" $ \port target -> withSweeper $ \deadlines -> do
            app <-
                pypiProxy
                    deadlines
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

    it (caseLabel "integration/publication/native-pypi-012" "holds byte admission until the publication response completes") $ do
        release <- newEmptyMVar
        entered <- newEmptyMVar
        let blockingApp request respond = do
                _ <- consumeRequestBodyStrict request
                putMVar entered ()
                takeMVar release
                respond (responseLBS (mkStatus 201 "") [] "accepted")
        testWithApplication (pure blockingApp) $ \port -> withSweeper $ \deadlines -> do
            budget <- newByteAdmissionTuned deadlines 1 0 50_000
            app <- pypiProxy deadlines (Just (const (pypiDeps port budget)))
            let send = postUpload ChunkedBody [("Content-Type", multipartType)] (multipartBody ["acme-widget"]) app
            heldRequest <- async send
            arrival <- timeout 5_000_000 (takeMVar entered)
            arrival `shouldBe` Just ()
            shedResponse <- send
            status shedResponse `shouldBe` 503
            putMVar release ()
            completed <- wait heldRequest
            status completed `shouldBe` 201

    it (caseLabel "integration/publication/native-pypi-013" "refuses a remove_pkg with 501 before any relay") $
        withTarget 201 "accepted" $ \port target -> withSweeper $ \deadlines -> do
            app <- pypiProxy deadlines (Just (pypiDeps port))
            response <- postUpload ChunkedBody [("Content-Type", multipartType)] removalBody app
            status response `shouldBe` 501
            simpleBody response `shouldBe` fromStrict (encodeUtf8 pypiFileUploadOnly)
            targetSaw target `shouldReturn` []

    it (caseLabel "integration/publication/native-pypi-014" "refuses an upload with no :action with 400 before any relay") $
        withTarget 201 "accepted" $ \port target -> withSweeper $ \deadlines -> do
            app <- pypiProxy deadlines (Just (pypiDeps port))
            response <- postUpload ChunkedBody [("Content-Type", multipartType)] (formBody [("name", "acme-widget")] (archivePart "acme_widget-1.0.tar.gz")) app
            status response `shouldBe` 400
            targetSaw target `shouldReturn` []

    it (caseLabel "integration/publication/native-pypi-015" "refuses a file name for another project with 400 before any relay") $
        withTarget 201 "accepted" $ \port target -> withSweeper $ \deadlines -> do
            app <- pypiProxy deadlines (Just (pypiDeps port))
            response <- postUpload ChunkedBody [("Content-Type", multipartType)] (uploadOf ["acme-widget"] "victim_target-99.0.tar.gz") app
            status response `shouldBe` 400
            targetSaw target `shouldReturn` []

    it (caseLabel "integration/publication/native-pypi-016" "refuses a content part hidden after a bare LF with 400 before any relay") $
        withTarget 201 "accepted" $ \port target -> withSweeper $ \deadlines -> do
            app <- pypiProxy deadlines (Just (pypiDeps port))
            let hidden = "original archive bytes\n--relay-boundary\nContent-Disposition: form-data; name=\"content\"; filename=\"victim_target-99.0.tar.gz\"\n\nvictim bytes"
                body = formBody [(":action", "file_upload"), ("name", "acme-widget"), ("version", "1.0")] (archiveOf "acme_widget-1.0.tar.gz" hidden)
            response <- postUpload ChunkedBody [("Content-Type", multipartType)] body app
            status response `shouldBe` 400
            targetSaw target `shouldReturn` []

cargoPublishSpec :: Spec
cargoPublishSpec = describe "first-party Cargo publication relay" $ do
    it (caseLabel "integration/publication/cargo-001" "relays exact native bytes after credential-scoped discovery") $ do
        archive <- cargoResolverArchive
        for_ cargoResolverAgreements $ \metadata -> do
            bytes <- expectRight (cargoUploadWithMetadata metadata archive)
            withRoutedStub cargoTargetReply $ \stub -> withSweeper $ \deadlines -> do
                app <- cargoProxy deadlines stub
                reply <- cargoPut (fromStrict bytes) app
                status reply `shouldBe` 200
                simpleBody reply `shouldBe` "{\"warnings\":{\"other\":[\"owned warning\"]}}"
                captures <- allCaptured stub
                map capPath captures `shouldBe` ["/index/config.json", "/registry/api/v1/crates/new"]
                map (headerValue "Authorization") captures `shouldBe` [Just "cargo-client", Just "cargo-client"]
                map capBody (filter ((== "PUT") . capMethod) captures) `shouldBe` [bytes]
    it (caseLabel "integration/publication/cargo-002" "refuses identity and resolver contradictions before any upstream request") $ do
        release <- cargoPublicationFixture
        withRoutedStub cargoTargetReply $ \stub -> withSweeper $ \deadlines -> do
            app <- cargoProxy deadlines stub
            case cargoUploadMetadata release of
                Object fields -> for_ [("name", "victim"), ("vers", "1.0.0+changed")] $ \(key, value) -> do
                    bytes <- expectRight (CargoBody.encodeUpload (Object (KeyMap.insert key (String value) fields)) (releaseArchive release))
                    reply <- cargoPut bytes app
                    status reply `shouldBe` 400
                _ -> expectationFailure "fixture upload metadata is not an object"
            archive <- cargoResolverArchive
            for_ cargoResolverContradictions $ \metadata -> do
                bytes <- expectRight (cargoUploadWithMetadata metadata archive)
                CargoBody.decodeUpload defaultLimits bytes `shouldSatisfy` isRight
                reply <- cargoPut (fromStrict bytes) app
                status reply `shouldBe` 400
            allCaptured stub `shouldReturn` []
    it (caseLabel "integration/publication/cargo-003" "preserves native authentication challenge when target discovery refuses access") $
        withStubHeaders (mkStatus 401 "Unauthorized") [] "denied" $ \stub -> withSweeper $ \deadlines -> do
            release <- cargoPublicationFixture
            bytes <- expectRight (cargoUpload release)
            app <- cargoProxy deadlines stub
            reply <- cargoPut (fromStrict bytes) app
            status reply `shouldBe` 401
            lookup "WWW-Authenticate" (simpleHeaders reply) `shouldBe` Just "Cargo"
            captures <- allCaptured stub
            map capPath captures `shouldBe` ["/index/config.json"]
    it (caseLabel "integration/publication/cargo-004" "preserves native authentication challenge and body when the upload refuses access") $
        withRoutedStub uploadRefusal $ \stub -> withSweeper $ \deadlines -> do
            release <- cargoPublicationFixture
            bytes <- expectRight (cargoUpload release)
            app <- cargoProxy deadlines stub
            reply <- cargoPut (fromStrict bytes) app
            status reply `shouldBe` 401
            simpleBody reply `shouldBe` "native upload denied"
            lookup "WWW-Authenticate" (simpleHeaders reply) `shouldBe` Just "Cargo"
            captures <- allCaptured stub
            map capPath captures `shouldBe` ["/index/config.json", "/registry/api/v1/crates/new"]
    it (caseLabel "integration/publication/cargo-005" "refuses unstable dependency fields with 400 before any upstream request") $ do
        archive <- cargoResolverArchive
        withRoutedStub cargoTargetReply $ \stub -> withSweeper $ \deadlines -> do
            app <- cargoProxy deadlines stub
            for_ ["artifact", "bindep_target", "lib", "public"] $ \key -> do
                bytes <- expectRight (cargoUploadWithMetadata (cargoResolverDependencyField key (Aeson.Bool True)) archive)
                reply <- cargoPut (fromStrict bytes) app
                status reply `shouldBe` 400
                decodeUtf8 (simpleBody reply) `shouldSatisfy` T.isInfixOf "Cargo upload dependency 'tool' carries an unstable artifact, bindep_target, lib or public field"
            allCaptured stub `shouldReturn` []
    it (caseLabel "integration/publication/cargo-006" "refuses a dependency on another registry with 400 naming it and the fix before any upstream request") $ do
        archive <- cargoForeignRegistryArchive
        bytes <- expectRight (cargoUploadWithMetadata cargoForeignRegistryMetadata archive)
        withRoutedStub cargoTargetReply $ \stub -> withSweeper $ \deadlines -> do
            app <- cargoProxy deadlines stub
            reply <- cargoPut (fromStrict bytes) app
            status reply `shouldBe` 400
            decodeUtf8 (simpleBody reply) `shouldBe` cargoForeignRegistryRefusal
            allCaptured stub `shouldReturn` []
    it (caseLabel "integration/publication/cargo-007" "relays a crate whose only dependency on another registry is a dev-dependency") $ do
        archive <- cargoForeignDevArchive
        bytes <- expectRight (cargoUploadWithMetadata cargoForeignDevMetadata archive)
        withRoutedStub cargoTargetReply $ \stub -> withSweeper $ \deadlines -> do
            app <- cargoProxy deadlines stub
            reply <- cargoPut (fromStrict bytes) app
            status reply `shouldBe` 200
            captures <- allCaptured stub
            map capPath captures `shouldBe` ["/index/config.json", "/registry/api/v1/crates/new"]
            map capBody (filter ((== "PUT") . capMethod) captures) `shouldBe` [bytes]
  where
    uploadRefusal captured
        | capMethod captured == "PUT" = (mkStatus 401 "Unauthorized", [], "native upload denied")
        | otherwise = cargoTargetReply captured

cargoProxy :: Deadlines -> Stub -> IO Application
cargoProxy deadlines stub = do
    env <- newTestEnv deadlines
    budget <- newByteAdmission deadlines (128 * 1024 * 1024)
    let factors = ChargeFactors 1000 1000 1000
        adapter = cargoAdapter factors
        deps = (inertDepsFor adapter){pdMountBaseUrl = "https://proxy.test/cargo"}
        publication =
            (publishDepsAt (stubPort stub) Nothing budget)
                { pubReadUrl = loopbackRegistryUrl (stubBaseUrl stub <> "/index")
                , pubTargetUrl = loopbackRegistryUrl (stubBaseUrl stub <> "/registry")
                , pubProjectName = rightToMaybe . projectName
                , pubAllowed = \name -> projectName "fixture" == Right name
                , pubAdapter = cargoPublish factors
                }
    pure (application (mkServerConfig (maybeToList (mountBindingFor Cargo deps (Just publication)))) env)

cargoPut :: LByteString -> Application -> IO SResponse
cargoPut bytes = runSession (srequest (SRequest request bytes))
  where
    request = setPath defaultRequest{requestMethod = methodPut, requestHeaders = [(hAuthorization, "cargo-client"), (hContentType, "application/octet-stream")]} "/cargo/api/v1/crates/new"

cargoTargetReply :: Captured -> (Status, ResponseHeaders, LByteString)
cargoTargetReply captured =
    let root = selfBaseUrlOf (capHeaders captured)
        discovery = Aeson.object ["dl" .= (root <> "/files/{crate}/{version}/download"), "api" .= (root <> "/registry"), "auth-required" .= True]
     in (mkStatus 200 "OK", [(hContentType, "application/json")], if capMethod captured == "PUT" then "{\"warnings\":{\"other\":[\"owned warning\"]}}" else Aeson.encode discovery)
