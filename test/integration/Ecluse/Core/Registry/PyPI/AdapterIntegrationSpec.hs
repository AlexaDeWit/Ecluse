-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Exercise PyPI metadata and artifact routes using local upstreams.
Private index failures retain their access-refusal or fallback policy.
-}
module Ecluse.Core.Registry.PyPI.AdapterIntegrationSpec (spec) where

import Data.Aeson (Value (Array, Bool, Null, Number, Object, String), encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.List (dropWhileEnd, lookup)
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Ecluse.Test.Contract.Catalogue (caseLabel)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (Header, Status, methodGet, methodHead, status200, status302, status304, status401, status403, status404, status503, statusCode)
import Network.Wai (Application, responseLBS)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Test (SResponse (SResponse, simpleBody, simpleHeaders, simpleStatus))
import Test.Hspec
import UnliftIO (tryAny)

import Ecluse.Core.Credential (mkSecret)
import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Mirror.State (StateRead (StateAbsent), stateTarget)
import Ecluse.Core.Package (HashAlg (SHA512))
import Ecluse.Core.Package.Integrity (mkMinIntegrity, mkMinTrustedIntegrity)
import Ecluse.Core.Queue (MirrorQueue (enqueue))
import Ecluse.Core.Registry.PyPI.Mirror (pypiMirrorDocuments, pypiStateFormat)
import Ecluse.Core.Rules (prepare)
import Ecluse.Core.Rules.Types (Rule (AllowIfOlderThan, DenyIfYanked))
import Ecluse.Core.Security (Limits (progressFloor), mkProgressFloor)
import Ecluse.Core.Security.Egress (RegistryUrl)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Admission.Meter (MemoryMeter, steerMeter)
import Ecluse.Core.Server.Admission.Types (BrakeLevel (Calm))
import Ecluse.Core.Server.Context (PackumentDeps (..))
import Ecluse.Core.Server.Response (mkHelpMessage)
import Ecluse.Core.Server.Upstream (MirrorServePlan (MirrorOnAdmit, NoMirrorWrite))
import Ecluse.Runtime.Env (Env (envMemoryMeter))
import Ecluse.Runtime.Telemetry (telemetryDisabled)
import Ecluse.Runtime.Test.Support (newTestEnvWith)
import Ecluse.Server.Pipeline.ServingDriver (pypiServingDriver)
import Ecluse.Server.Pipeline.TestSupport (getPath, getPathWith, postPath, proxyApplication, requestAt)
import Ecluse.Test.Contract.Serving (servingContract)
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Json (asObject, fieldAt, textAtPath, withKeys)
import Ecluse.Test.Mirror.State (mirrorViewWith)
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Rules (admissionOnly, atDefaultPrecedence, inertRuleDeps)
import Ecluse.Test.Server.Mount (pypiServeDeps, withMirrorPlan)
import Ecluse.Test.Wai (closedResponse, decodedBody, localhost, lookupAuth, pacedBody, selfBaseUrl)

spec :: Spec
spec = do
    servingContract "pypi" pypiServingDriver
    indexSpec
    availabilitySpec
    conditionalFloorSpec
    artifactSpec
    sidecarSpec
    sidecarCollisionSpec
    negotiationSpec
    refusalBodySpec
    privateLegSpec
    privateAdmissionSpec

indexSpec :: Spec
indexSpec = describe "the served Simple index" $ do
    it (caseLabel "integration/pypi/native-001" "serves the PEP 691 JSON form under its own media type") $
        withPyPIProxy publicIndex $ \app -> do
            resp <- getIndex app
            simpleStatus resp `shouldBe` status200
            lookup "Content-Type" (simpleHeaders resp) `shouldBe` Just "application/vnd.pypi.simple.v1+json"

    it (caseLabel "integration/pypi/native-002" "names the surviving releases in the PEP 700 versions array") $
        withPyPIProxy publicIndex $ \app -> do
            resp <- getIndex app
            servedVersions resp `shouldBe` ["2.34.2"]

    it (caseLabel "integration/pypi/native-003" "rebases every served file onto this mount, so the bytes come back through the gate") $
        withPyPIProxy publicIndex $ \app -> do
            resp <- getIndex app
            servedUrls resp
                `shouldBe` [ "http://ecluse.test/pypi/simple/requests/requests-2.34.2-py3-none-any.whl"
                           , "http://ecluse.test/pypi/simple/requests/requests-2.34.2.tar.gz"
                           ]

    it (caseLabel "integration/pypi/native-004" "retains canonical companion digests and omits legacy aliases") $
        withPyPIProxy publicIndex $ \app -> do
            resp <- getIndex app
            for_ sidecarKeys (`excludesSidecar` resp)
            mapMaybe (textAtPath ["core-metadata", "sha256"]) (servedFiles resp) `shouldBe` [metadataDigest, metadataDigest]
            let expectedFiles = map (withoutSidecars . fileOn "http://ecluse.test/pypi") ["requests-2.34.2-py3-none-any.whl", "requests-2.34.2.tar.gz"]
            servedFiles resp `shouldBe` expectedFiles

    for_ sidecarKeys $ \key ->
        it (caseLabel ("integration/pypi/native-005/" <> key) (toText ("recognises " <> toString key <> " and rejects its retained-response control" :: String))) $
            withPyPIProxy publicIndex $ \app -> do
                resp <- getIndex app
                let input = resp{simpleBody = encode (publicIndex "http://upstream.test")}
                    retained = resp{simpleBody = encode (withKeys ["files" .= map (withKeys [(Key.fromText key, object ["sha256" .= sha256Digest])]) (servedFiles resp)] (decodedBody resp))}
                servedKeys input `shouldContain` [key]
                servedKeys retained `shouldContain` [key]
                excludesSidecar key retained `shouldThrow` namedKeyFailure key

    it (caseLabel "integration/pypi/native-006" "preserves the digest a client verifies the download against") $
        withPyPIProxy publicIndex $ \app -> do
            resp <- getIndex app
            servedDigests resp `shouldBe` [sha256Digest, sha256Digest]

    it (caseLabel "integration/pypi/native-007" "answers the same index with a trailing slash, which the router collapses") $
        withPyPIProxy publicIndex $ \app -> do
            resp <- getPath "/pypi/simple/requests/" app
            simpleStatus resp `shouldBe` status200
            servedVersions resp `shouldBe` ["2.34.2"]

    it (caseLabel "integration/pypi/native-008" "denies a non-canonical project name with the structural 404, never a redirect") $
        withPyPIProxy publicIndex $ \app -> do
            resp <- getPath "/pypi/simple/Requests" app
            simpleStatus resp `shouldBe` status404
            simpleBody resp `shouldBe` ""

conditionalFloorSpec :: Spec
conditionalFloorSpec = describe "listing validators across configured integrity floors" $
    for_ [("public", publicPypiDeps), ("trusted", privatePypiDeps)] $ \(origin, depsFor) ->
        it (caseLabel ("integration/pypi/native-009/" <> toText origin) (toText ("returns the changed " <> origin <> " listing before accepting its new validator" :: String))) $ do
            publicFloor <- either (fail . toString) pure (mkMinIntegrity SHA512)
            trustedFloor <- either (fail . toString) pure (mkMinTrustedIntegrity SHA512)
            withPyPIProxyOver mixedIntegrityIndex depsFor $ \proxy -> do
                let strictApp = proxyWithDeps proxy (\deps -> deps{pdMinIntegrity = publicFloor, pdMinTrustedIntegrity = trustedFloor})
                    request headers = getPathWith (clientCredential : headers) "/pypi/simple/requests"
                    validator resp = maybe (fail "no ETag on the listing response") pure (lookup "ETag" (simpleHeaders resp))
                initial <- request [] (proxyApp proxy)
                statusOf initial `shouldBe` 200
                servedVersions initial `shouldBe` ["2.34.2"]
                mapMaybe (fieldAt "size") (servedFiles initial) `shouldBe` map Number [1, 2, 2, 3]
                oldTag <- validator initial
                unchanged <- request [("If-None-Match", oldTag)] (proxyApp proxy)
                statusOf unchanged `shouldBe` 304
                changed <- request [("If-None-Match", oldTag)] strictApp
                statusOf changed `shouldBe` 200
                servedVersions changed `shouldBe` servedVersions initial
                mapMaybe (fieldAt "size") (servedFiles changed) `shouldBe` map Number [2, 2, 3]
                simpleBody changed `shouldNotBe` simpleBody initial
                newTag <- validator changed
                newTag `shouldNotBe` oldTag
                stable <- request [] strictApp
                simpleBody stable `shouldBe` simpleBody changed
                validator stable `shouldReturn` newTag
                matched <- request [("If-None-Match", newTag)] strictApp
                statusOf matched `shouldBe` 304
                simpleBody matched `shouldBe` ""
                hits <- privateHits proxy
                map snd hits `shouldSatisfy` all (== if origin == "trusted" then Just basicCredential else Nothing)
                when (origin == "trusted") (length hits `shouldBe` 5)

mixedIntegrityIndex :: Text -> Value
mixedIntegrityIndex authority =
    object
        [ "name" .= ("requests" :: Text)
        , "meta" .= object ["api-version" .= ("1.4" :: Text)]
        , "files" .= [entry 1 False, entry 2 True, entry 2 True, entry 3 True]
        ]
  where
    entry :: Int -> Bool -> Value
    entry size strong = case fileOn authority "requests-2.34.2.tar.gz" of
        Object fields -> Object (KeyMap.insert "size" (Number (fromIntegral size)) (KeyMap.insert "hashes" hashes fields))
          where
            hashes = if strong then object ["sha512" .= T.replicate 128 "a"] else object ["sha256" .= sha256Digest]
        value -> value

artifactSpec :: Spec
artifactSpec = describe "the served distribution file" $ do
    it (caseLabel "integration/pypi/native-010" "denies a file naming another project, which is a path-confusion attempt") $
        withPyPIProxy publicIndex $ \app -> do
            resp <- getPath "/pypi/simple/requests/urllib3-2.0.0.tar.gz" app
            simpleStatus resp `shouldBe` status404

sidecarSpec :: Spec
sidecarSpec = describe "the metadata companion" $ do
    for_ [methodGet, methodHead] $ \method ->
        it (caseLabel ("integration/pypi/native-022/" <> T.toLower (decodeUtf8 method)) (toText (("relays metadata under " <> decodeUtf8 method <> " without advisory or policy reads") :: String))) $
            withPyPIProxyOver publicIndex publicPypiDeps $ \proxy -> do
                let app = proxyWithDeps proxy (withMirrorPlan (MirrorOnAdmit (loopbackRegistryUrl "http://localhost:1")) . (\deps -> deps{pdRules = [], pdAdvisoryEtag = fail "sidecar read advisories"}))
                response <- requestAt method (sdistPath <> ".metadata") Wai.defaultRequest app
                statusOf response `shouldBe` 200
                simpleBody response `shouldBe` if method == methodHead then "" else metadataBytes
                hits <- privateHits proxy
                map fst hits `shouldContain` [sdistPath <> ".metadata"]
                map snd hits `shouldSatisfy` all isNothing

    it (caseLabel "integration/pypi/native-023" "rejects a missing edge credential before reading metadata") $
        withPyPIProxyOver publicIndex publicPypiDeps $ \proxy -> do
            let app = proxyWithDeps proxy (\deps -> deps{pdInboundToken = Just (mkSecret "edge-secret")})
            response <- getPath (sdistPath <> ".metadata") app
            statusOf response `shouldBe` 401
            privateHits proxy `shouldReturn` []

    it (caseLabel "integration/pypi/native-024" "does not fetch public metadata for a first-party private absence") $
        withPyPIProxyOver publicIndex publicPypiDeps $ \proxy -> do
            let app = proxyWithDeps proxy (\deps -> deps{pdFirstParty = const True})
            response <- getPath (sdistPath <> ".metadata") app
            statusOf response `shouldBe` 404
            privateHits proxy `shouldReturn` []

    it (caseLabel "integration/pypi/native-025" "reports an unavailable public index without fetching companion bytes") $
        withPyPIProxyResponding (const (\_ respond -> respond (responseLBS status503 [] "unavailable"))) publicIndex publicPypiDeps $ \proxy -> do
            response <- getPath (sdistPath <> ".metadata") (proxyApp proxy)
            statusOf response `shouldBe` 503

    it (caseLabel "integration/pypi/native-026" "refuses a mismatched project identity") $
        withPyPIProxyOver (withKeys ["name" .= ("other-project" :: Text)] . publicIndex) publicPypiDeps $ \proxy -> do
            response <- getPath (sdistPath <> ".metadata") (proxyApp proxy)
            statusOf response `shouldBe` 404
            hits <- privateHits proxy
            map fst hits `shouldNotContain` [sdistPath <> ".metadata"]

    it (caseLabel "integration/pypi/native-027" "serves bytes with the advertised companion digest") $
        withPyPIProxy publicIndex $ \app -> do
            index <- getIndex app
            response <- getPath (sdistPath <> ".metadata") app
            mapMaybe (textAtPath ["core-metadata", "sha256"]) (servedFiles index) `shouldSatisfy` all (== hexSha256Of (LBS.toStrict (simpleBody response)))
            metadataDigest `shouldNotBe` sha256Digest

    for_ [("absent", Nothing), ("false", Just (Bool False)), ("empty", Just (object [])), ("null", Just Null), ("invalid-digest", Just (object ["sha256" .= True]))] $ \(declarationId, declaration) ->
        it (caseLabel ("integration/pypi/native-028/" <> declarationId) (toText (("does not fetch an absent or unusable companion " <> show declaration) :: String))) $
            withPyPIProxyOver (alterDeclarations declaration) publicPypiDeps $ \proxy -> do
                response <- getPath (sdistPath <> ".metadata") (proxyApp proxy)
                statusOf response `shouldBe` 404
                hits <- privateHits proxy
                map fst hits `shouldNotContain` [sdistPath <> ".metadata"]

    it (caseLabel "integration/pypi/native-029" "enforces the parent's public and trusted integrity floors") $
        for_ [(False, publicPypiDeps), (True, privatePypiDeps)] $ \(private, depsFor) ->
            withPyPIProxyOver publicIndex depsFor $ \proxy -> do
                publicFloor <- either (fail . toString) pure (mkMinIntegrity SHA512)
                trustedFloor <- either (fail . toString) pure (mkMinTrustedIntegrity SHA512)
                let app = proxyWithDeps proxy (\deps -> deps{pdMinIntegrity = publicFloor, pdMinTrustedIntegrity = trustedFloor, pdFirstParty = const private})
                response <- getPathWith [clientCredential] (sdistPath <> ".metadata") app
                statusOf response `shouldBe` 404
                hits <- privateHits proxy
                map fst hits `shouldNotContain` [sdistPath <> ".metadata"]

    it (caseLabel "integration/pypi/native-030/404" "falls back anonymously after private metadata HTTP 404") $
        withPrivateStatus status404 $ \proxy -> do
            response <- getPathWith [clientCredential] (sdistPath <> ".metadata") (proxyApp proxy)
            statusOf response `shouldBe` 200
            simpleBody response `shouldBe` metadataBytes
            hits <- privateHits proxy
            map snd hits `shouldSatisfy` all isNothing

    it (caseLabel "integration/pypi/native-030/503" "refuses the companion after private metadata HTTP 503") $
        withPrivateStatus status503 $ \proxy -> do
            response <- getPathWith [clientCredential] (sdistPath <> ".metadata") (proxyApp proxy)
            statusOf response `shouldBe` 503
            privateHits proxy `shouldReturn` []

    it (caseLabel "integration/pypi/native-031" "reports a companion transport failure before committing bytes") $
        withPyPIProxyResponding (onCompanion (\_ respond -> respond closedResponse)) publicIndex publicPypiDeps $ \proxy -> do
            response <- getPath (sdistPath <> ".metadata") (proxyApp proxy)
            statusOf response `shouldBe` 503

    it (caseLabel "integration/pypi/native-032" "stops a streamed companion when upstream progress falls below the floor") $ do
        progress <- either (fail . show) pure (mkProgressFloor 30 0.5 1024)
        withPyPIProxyResponding (onCompanion (pacedBody [(0, "partial"), (20_000_000, "rest")])) publicIndex publicPypiDeps $ \proxy -> do
            let app = proxyWithDeps proxy (\deps -> deps{pdLimits = (pdLimits deps){progressFloor = progress}})
            outcome <- tryAny (getPath (sdistPath <> ".metadata") app)
            outcome `shouldSatisfy` isLeft

    it (caseLabel "integration/pypi/native-033" "keeps private credentials on the private companion authority") $
        withPrivatePyPIProxy privateIndexOnItself $ \proxy -> do
            response <- getPathWith [clientCredential] (sdistPath <> ".metadata") (proxyApp proxy)
            statusOf response `shouldBe` 200
            hits <- privateHits proxy
            lookup (sdistPath <> ".metadata") hits `shouldBe` Just (Just basicCredential)

    for_ [status401, status403] $ \refused ->
        it (caseLabel ("integration/pypi/native-034/" <> show (statusCode refused)) (toText (("stops public fallback after companion HTTP " <> show (statusCode refused)) :: String))) $
            withPyPIProxyResponding (companionResponse refused) publicIndex privatePypiDeps $ \proxy -> do
                response <- getPathWith [clientCredential] (sdistPath <> ".metadata") (proxyApp proxy)
                statusOf response `shouldBe` 403
                simpleBody response `shouldBe` ""

    for_ [status304, status302, status503] $ \upstreamStatus ->
        it (caseLabel ("integration/pypi/native-035/" <> show (statusCode upstreamStatus)) (toText (("relays companion HTTP " <> show (statusCode upstreamStatus) <> " without following redirects") :: String))) $
            withPyPIProxyResponding (companionResponse upstreamStatus) publicIndex publicPypiDeps $ \proxy -> do
                response <- getPathWith [("If-None-Match", "*")] (sdistPath <> ".metadata") (proxyApp proxy)
                simpleStatus response `shouldBe` upstreamStatus
                when (upstreamStatus == status304) (simpleBody response `shouldBe` "")

    it (caseLabel "integration/pypi/native-036" "refuses a foreign parent without fetching its companion") $
        withPrivatePyPIProxy (privateIndexOn "http://files.example.invalid") $ \proxy -> do
            let app = proxyWithDeps proxy (\deps -> deps{pdFirstParty = const True})
            response <- getPathWith [clientCredential] (sdistPath <> ".metadata") app
            statusOf response `shouldBe` 404
            hits <- privateHits proxy
            map fst hits `shouldNotContain` [sdistPath <> ".metadata"]

sidecarCollisionSpec :: Spec
sidecarCollisionSpec = describe "companions keep the winning source" $ do
    it (caseLabel "integration/pypi/native-037" "serves the private bytes whose digest the winning index advertised") $
        withCollision (privateCollisionIndex id) id $ \proxy -> do
            index <- getIndex (proxyApp proxy)
            mapMaybe (textAtPath ["core-metadata", "sha256"]) (servedFiles index) `shouldBe` [privateMetadataDigest, privateMetadataDigest]
            publicBefore <- privateHits proxy
            response <- getPath (sdistPath <> ".metadata") (proxyApp proxy)
            statusOf response `shouldBe` 200
            hexSha256Of (LBS.toStrict (simpleBody response)) `shouldBe` privateMetadataDigest
            privateHits proxy `shouldReturn` publicBefore

    for_
        [ ("absent", "absent declaration", KeyMap.delete "core-metadata", True)
        , ("false", "false declaration", KeyMap.insert "core-metadata" (Bool False), True)
        , ("empty", "empty declaration", KeyMap.insert "core-metadata" (object []), True)
        , ("weak", "weak parent", KeyMap.insert "hashes" (object ["md5" .= ("0123456789abcdef0123456789abcdef" :: Text)]), False)
        , ("foreign", "foreign parent", KeyMap.insert "url" (String "http://foreign.invalid/requests-2.34.2.tar.gz"), False)
        ]
        $ \(caseId, label, change, retainsParent) ->
            it (caseLabel ("integration/pypi/native-038/" <> caseId) (toText (("keeps a private sibling authoritative after an " <> label) :: String))) $
                withCollision (privateCollisionIndex (changeParent change)) id $ \proxy -> do
                    index <- getIndex (proxyApp proxy)
                    statusOf index `shouldBe` 200
                    mapMaybe (fieldAt "size") (servedFiles index) `shouldSatisfy` all (== Number 17)
                    let parent = find ((== Just "requests-2.34.2.tar.gz") . textAtPath ["filename"]) (servedFiles index)
                    isJust parent `shouldBe` retainsParent
                    length (servedFiles index) `shouldBe` if retainsParent then 2 else 1
                    when retainsParent $ (parent >>= fieldAt "core-metadata") `shouldBe` KeyMap.lookup "core-metadata" (change (KeyMap.singleton "core-metadata" (object ["sha256" .= privateMetadataDigest])))
                    publicBefore <- privateHits proxy
                    response <- getPathWith [clientCredential] (sdistPath <> ".metadata") (proxyApp proxy)
                    statusOf response `shouldBe` 404
                    privateHits proxy `shouldReturn` publicBefore

    it (caseLabel "integration/pypi/native-039" "keeps a private version authoritative when it lacks the requested filename") $
        withCollision (privateCollisionIndex (filter ((/= Just "requests-2.34.2.tar.gz") . textAtPath ["filename"]))) id $ \proxy -> do
            index <- getIndex (proxyApp proxy)
            length (servedFiles index) `shouldBe` 1
            mapMaybe (fieldAt "size") (servedFiles index) `shouldBe` [Number 17]
            publicBefore <- privateHits proxy
            response <- getPath (sdistPath <> ".metadata") (proxyApp proxy)
            statusOf response `shouldBe` 404
            privateHits proxy `shouldReturn` publicBefore

    for_ [status404, status503] $ \privateStatus ->
        for_ [methodGet, methodHead] $ \method ->
            it (caseLabel ("integration/pypi/native-040/" <> show (statusCode privateStatus) <> "/" <> T.toLower (decodeUtf8 method)) (toText (("does not replace an advertised private companion after HTTP " <> show (statusCode privateStatus) <> " under " <> decodeUtf8 method) :: String))) $
                withCollision (privateCollisionIndex id) (companionResponse privateStatus) $ \proxy -> do
                    index <- getIndex (proxyApp proxy)
                    mapMaybe (textAtPath ["core-metadata", "sha256"]) (servedFiles index) `shouldBe` [privateMetadataDigest, privateMetadataDigest]
                    publicBefore <- privateHits proxy
                    response <- requestAt method (sdistPath <> ".metadata") Wai.defaultRequest (proxyApp proxy)
                    simpleStatus response `shouldBe` privateStatus
                    privateHits proxy `shouldReturn` publicBefore

    it (caseLabel "integration/pypi/native-041" "does not replace a private companion after a transport failure") $
        withCollision (privateCollisionIndex id) (onCompanion (\_ respond -> respond closedResponse)) $ \proxy -> do
            index <- getIndex (proxyApp proxy)
            mapMaybe (fieldAt "size") (servedFiles index) `shouldBe` [Number 17, Number 17]
            publicBefore <- privateHits proxy
            response <- getPath (sdistPath <> ".metadata") (proxyApp proxy)
            statusOf response `shouldBe` 503
            privateHits proxy `shouldReturn` publicBefore

    for_
        [ ("weak", map (withKeys ["hashes" .= object ["md5" .= ("0123456789abcdef0123456789abcdef" :: Text)]]))
        , ("foreign", map (withKeys ["url" .= ("http://foreign.invalid/requests-2.34.2.tar.gz" :: Text)]))
        , ("absent", const [])
        ]
        $ \(label, change) ->
            it (caseLabel ("integration/pypi/native-042/" <> toText label) (toText (("serves the public winner when every private entry is " <> label) :: String))) $
                withCollision (privateCollisionIndex change) id $ \proxy -> do
                    index <- getIndex (proxyApp proxy)
                    mapMaybe (fieldAt "size") (servedFiles index) `shouldBe` []
                    mapMaybe (textAtPath ["core-metadata", "sha256"]) (servedFiles index) `shouldBe` [metadataDigest, metadataDigest]
                    publicBefore <- privateHits proxy
                    response <- getPathWith [clientCredential] (sdistPath <> ".metadata") (proxyApp proxy)
                    simpleBody response `shouldBe` metadataBytes
                    statusOf response `shouldBe` 200
                    publicAfter <- privateHits proxy
                    let newPublicReads = drop (length publicBefore) publicAfter
                    map fst newPublicReads `shouldContain` [sdistPath <> ".metadata"]
                    map snd newPublicReads `shouldSatisfy` all isNothing

-- A private origin that answers every request with one status, beside the public index.
withPrivateStatus :: Status -> (PyPIProxy -> IO a) -> IO a
withPrivateStatus privateStatus use =
    testWithApplication (pure (\_ respond -> respond (responseLBS privateStatus [] "private answer"))) $ \privatePort ->
        withPyPIProxyOver publicIndex (pypiDeps (Just (loopbackRegistryUrl (localhost privatePort))) . loopbackRegistryUrl . localhost) use

withCollision :: (Text -> Value) -> (Application -> Application) -> (PyPIProxy -> IO a) -> IO a
withCollision privateIndex transform use =
    testWithApplication (pure (transform privateApp)) $ \privatePort ->
        withPyPIProxyOver publicIndex (pypiDeps (Just (loopbackRegistryUrl (localhost privatePort))) . loopbackRegistryUrl . localhost) use
  where
    privateApp = onCompanion (\_ respond -> respond (responseLBS status200 [] privateMetadataBytes)) (upstreamApp privateIndex (const pass))

privateCollisionIndex :: ([Value] -> [Value]) -> Text -> Value
privateCollisionIndex change authority =
    withKeys ["files" .= change files] (publicIndex authority)
  where
    files = map (withKeys ["size" .= (17 :: Int), "core-metadata" .= object ["sha256" .= privateMetadataDigest]] . fileOn authority) ["requests-2.34.2-py3-none-any.whl", "requests-2.34.2.tar.gz"]

changeParent :: (KeyMap.KeyMap Value -> KeyMap.KeyMap Value) -> [Value] -> [Value]
changeParent change = map $ \file ->
    if textAtPath ["filename"] file == Just "requests-2.34.2.tar.gz" then Object (change (asObject file)) else file

privateMetadataDigest :: Text
privateMetadataDigest = hexSha256Of (LBS.toStrict privateMetadataBytes)

privateMetadataBytes :: LByteString
privateMetadataBytes = "private metadata"

alterDeclarations :: Maybe Value -> Text -> Value
alterDeclarations declaration authority = withKeys ["files" .= map change (servedFiles response)] (publicIndex authority)
  where
    response = SResponse status200 [] (encode (publicIndex authority))
    change = Object . maybe (KeyMap.delete "core-metadata") (KeyMap.insert "core-metadata") declaration . asObject

onCompanion :: Application -> Application -> Application
onCompanion companion original request respond
    | ".metadata" `T.isSuffixOf` decodeUtf8 (Wai.rawPathInfo request) = companion request respond
    | otherwise = original request respond

companionResponse :: Status -> Application -> Application
companionResponse status = onCompanion $ \request respond -> do
    when (status == status304) (lookup "If-None-Match" (Wai.requestHeaders request) `shouldBe` Just "*")
    respond (responseLBS status [("Location", "http://foreign.invalid/file")] "")

negotiationSpec :: Spec
negotiationSpec = describe "content negotiation, decided from the route record" $ do
    it (caseLabel "integration/pypi/native-011" "serves a client that admits the JSON form") $
        withPyPIProxy publicIndex $ \app -> do
            resp <- getPathWith [("Accept", "application/vnd.pypi.simple.v1+json, text/html;q=0.01")] "/pypi/simple/requests" app
            simpleStatus resp `shouldBe` status200

    it (caseLabel "integration/pypi/native-012" "answers 406 to a client that requires HTML, before any upstream work") $
        withPyPIProxy publicIndex $ \app -> do
            resp <- getPathWith [("Accept", "text/html")] "/pypi/simple/requests" app
            statusOf resp `shouldBe` 406
            simpleBody resp `shouldBe` ""

refusalBodySpec :: Spec
refusalBodySpec = describe "the operator help message on a refusal the route table decides" $ do
    it (caseLabel "integration/pypi/native-013" "carries it on the 406 a client admitting no JSON takes") $
        withHelpfulPyPIProxy $ \app -> do
            resp <- getPathWith [("Accept", "text/html")] "/pypi/simple/requests" app
            statusOf resp `shouldBe` 406
            simpleBody resp `shouldBe` helpBytes

    it (caseLabel "integration/pypi/native-014" "carries it on the 405 an upload takes, because this mount publishes nothing") $ do
        withPyPIProxy publicIndex $ \app -> do
            resp <- postPath "/pypi/legacy" app
            statusOf resp `shouldBe` 405
            simpleBody resp `shouldBe` ""
        withHelpfulPyPIProxy $ \app -> do
            resp <- postPath "/pypi/legacy" app
            statusOf resp `shouldBe` 405
            simpleBody resp `shouldBe` helpBytes

    it (caseLabel "integration/pypi/native-015" "carries it on the structural 404 a path no route claims takes") $
        withHelpfulPyPIProxy $ \app -> do
            resp <- getPath "/pypi/pypi/requests/json" app
            statusOf resp `shouldBe` 404
            simpleBody resp `shouldBe` helpBytes

privateLegSpec :: Spec
privateLegSpec = describe "the private artifact leg, addressed by the index's own location" $ do
    it (caseLabel "integration/pypi/native-016/404" "keeps public artifact fallback after private index HTTP 404") $
        withPrivateStatus status404 $ \proxy -> do
            response <- getPathWith [clientCredential] sdistPath (proxyApp proxy)
            statusOf response `shouldBe` 200
            simpleBody response `shouldBe` artifactBytes
            hits <- privateHits proxy
            map snd hits `shouldSatisfy` all isNothing

    it (caseLabel "integration/pypi/native-016/503" "refuses the artifact after private index HTTP 503") $
        withPrivateStatus status503 $ \proxy -> do
            response <- getPathWith [clientCredential] sdistPath (proxyApp proxy)
            statusOf response `shouldBe` 503
            privateHits proxy `shouldReturn` []

    for_ [status401, status403] $ \upstreamStatus ->
        it (caseLabel ("integration/pypi/native-017/" <> show (statusCode upstreamStatus)) (toText ("refuses an index HTTP " <> show (statusCode upstreamStatus) <> " on metadata and indexed artifacts" :: String))) $
            withPyPIProxyResponding (const (\_ respond -> respond (responseLBS upstreamStatus [("WWW-Authenticate", "private-secret")] "private-secret"))) publicIndex privatePypiDeps $ \proxy ->
                for_ ["/pypi/simple/requests", sdistPath, sdistPath <> ".metadata"] $ \path ->
                    for_ [methodGet, methodHead] $ \method -> do
                        let req = Wai.defaultRequest{Wai.requestHeaders = [clientCredential, ("If-None-Match", "*")]}
                        response <- requestAt method path req (proxyApp proxy)
                        statusOf response `shouldBe` 403
                        lookup "WWW-Authenticate" (simpleHeaders response) `shouldBe` Nothing
                        decodeUtf8 (LBS.toStrict (simpleBody response)) `shouldSatisfy` (not . T.isInfixOf "private-secret")
                        when (method == methodHead) (simpleBody response `shouldBe` "")

    it (caseLabel "integration/pypi/native-018" "resolves the file through the private index and carries the credential there") $
        -- A private index names each file's location itself, so the leg reads that index rather
        -- than probing a conventional path the backend may not spell.
        withPrivatePyPIProxy privateIndexOnItself $ \proxy -> do
            resp <- getPathWith [clientCredential] sdistPath (proxyApp proxy)
            simpleStatus resp `shouldBe` status200
            simpleBody resp `shouldBe` artifactBytes
            hits <- privateHits proxy
            lookup sdistPath hits `shouldBe` Just (Just basicCredential)

    it (caseLabel "integration/pypi/native-019" "drops a file the private index puts on a foreign host, never fetching it") $
        -- The projection and the download gate read one authority definition, so a location the
        -- served listing would not carry is a location this leg does not fetch either.
        withPrivatePyPIProxy (privateIndexOn "http://files.example.invalid") $ \proxy -> do
            resp <- getPathWith [clientCredential] sdistPath (proxyApp proxy)
            statusOf resp `shouldNotBe` 200
            hits <- privateHits proxy
            map fst hits `shouldNotContain` [sdistPath]

privateAdmissionSpec :: Spec
privateAdmissionSpec = describe "private index admission scope" $ do
    it (caseLabel "integration/pypi/native-043" "serves an ordinary private artifact while metadata admission is closed") $
        withPrivatePyPIProxy privateIndexOnItself $ \proxy -> do
            steerMeter (proxyMemory proxy) 0 Calm
            response <- getPathWith [clientCredential] sdistPath (proxyApp proxy)
            simpleStatus response `shouldBe` status200
            simpleBody response `shouldBe` artifactBytes
            privateHits proxy
                `shouldReturn` [("/pypi/simple/requests", Just basicCredential), (sdistPath, Just basicCredential)]

    it (caseLabel "integration/pypi/native-044" "reads an ordinary private miss before refusing public admission") $ do
        privateCredentials <- newIORef []
        let privateApp request respond = do
                modifyIORef' privateCredentials (<> [lookupAuth (Wai.requestHeaders request)])
                respond (responseLBS status404 [] "")
        testWithApplication (pure privateApp) $ \privatePort ->
            withPyPIProxyOver publicIndex (pypiDeps (Just (loopbackRegistryUrl (localhost privatePort))) . loopbackRegistryUrl . localhost) $ \proxy -> do
                steerMeter (proxyMemory proxy) 0 Calm
                response <- getPathWith [clientCredential] sdistPath (proxyApp proxy)
                simpleStatus response `shouldBe` status503
                readIORef privateCredentials `shouldReturn` [Just basicCredential]
                privateHits proxy `shouldReturn` []

    it (caseLabel "integration/pypi/native-045" "refuses managed selection before reading the private index or mirror state") $
        withPrivatePyPIProxy privateIndexOnItself $ \proxy -> do
            stateReads <- newIORef (0 :: Int)
            mirror <- mirrorViewWith pypiStateFormat pypiMirrorDocuments (stateTarget "https://private.example.test") (\_ _ -> modifyIORef' stateReads (+ 1) $> Right StateAbsent)
            steerMeter (proxyMemory proxy) 0 Calm
            response <- getPathWith [clientCredential] sdistPath (proxyWithDeps proxy (\deps -> deps{pdMirrorState = Just mirror}))
            simpleStatus response `shouldBe` status503
            privateHits proxy `shouldReturn` []
            readIORef stateReads `shouldReturn` 0

getIndex :: Application -> IO SResponse
getIndex = getPath "/pypi/simple/requests"

sdistPath :: ByteString
sdistPath = "/pypi/simple/requests/requests-2.34.2.tar.gz"

withPyPIProxy :: (Text -> Value) -> (Application -> IO a) -> IO a
withPyPIProxy indexFor k = withPyPIProxyOver indexFor publicPypiDeps (k . proxyApp)

withHelpfulPyPIProxy :: (Application -> IO a) -> IO a
withHelpfulPyPIProxy k =
    withPyPIProxyOver publicIndex helpfulDeps (k . proxyApp)
  where
    helpfulDeps port = (\d -> d{pdHelp = Just (mkHelpMessage helpMessage)}) <$> publicPypiDeps port

withPrivatePyPIProxy :: (Text -> Value) -> (PyPIProxy -> IO a) -> IO a
withPrivatePyPIProxy indexFor = withPyPIProxyOver indexFor privatePypiDeps

data PyPIProxy = PyPIProxy
    { proxyApp :: Application
    -- ^ The proxy under test.
    , proxyWithDeps :: (PackumentDeps -> PackumentDeps) -> Application
    -- ^ A separately configured application sharing the upstream and runtime.
    , privateHits :: IO [(ByteString, Maybe ByteString)]
    -- ^ Each upstream request as its mount-relative path and its @Authorization@ value.
    , proxyMemory :: MemoryMeter
    -- ^ Admission control for the shared runtime.
    }

withPyPIProxyOver :: (Text -> Value) -> (Int -> IO PackumentDeps) -> (PyPIProxy -> IO a) -> IO a
withPyPIProxyOver = withPyPIProxyResponding id

withPyPIProxyResponding :: (Application -> Application) -> (Text -> Value) -> (Int -> IO PackumentDeps) -> (PyPIProxy -> IO a) -> IO a
withPyPIProxyResponding transform indexFor depsFor k = withSweeper $ \deadlines -> do
    queue <- newTestMemoryQueue
    manager <- newManager defaultManagerSettings
    recorded <- newIORef []
    testWithApplication (pure (transform (upstreamApp indexFor (record recorded)))) $ \upstreamPort -> do
        env <- newTestEnvWith deadlines queue{enqueue = \_ -> fail "unexpected mirror enqueue"} (manager, manager) telemetryDisabled
        deps <- depsFor upstreamPort
        let configured change = proxyApplication PyPI env (change deps)
        k (PyPIProxy (configured id) configured (readIORef recorded) (envMemoryMeter env))
  where
    record ref request =
        atomicModifyIORef' ref (\hits -> (hits <> [(mountPathOf request, lookupAuth (Wai.requestHeaders request))], ()))

    mountPathOf request = "/pypi/" <> encodeUtf8 (T.intercalate "/" (dropWhileEnd T.null (Wai.pathInfo request)))

upstreamApp :: (Text -> Value) -> (Wai.Request -> IO ()) -> Application
upstreamApp indexFor observe request respond = do
    observe request
    case dropWhileEnd T.null (Wai.pathInfo request) of
        -- The index read carries its trailing slash, because a real index redirects a request
        -- without one and no data-plane request follows a redirect.
        ["simple", _project] -> respond (responseLBS status200 [("Content-Type", "application/vnd.pypi.simple.v1+json")] (encode (indexFor (selfBaseUrl request))))
        ["simple", _, file] | ".metadata" `T.isSuffixOf` file -> respond (responseLBS status200 [("Content-Type", "text/plain")] metadataBytes)
        ["simple", _, _] -> respond (responseLBS status200 [("Content-Type", "application/octet-stream")] artifactBytes)
        _ -> respond (responseLBS status404 [] "")

publicPypiDeps :: Int -> IO PackumentDeps
publicPypiDeps upstreamPort = pypiDeps Nothing (loopbackRegistryUrl (localhost upstreamPort))

privatePypiDeps :: Int -> IO PackumentDeps
privatePypiDeps upstreamPort =
    pypiDeps (Just (loopbackRegistryUrl (localhost upstreamPort))) (loopbackRegistryUrl "http://localhost:1")

pypiDeps :: Maybe RegistryUrl -> RegistryUrl -> IO PackumentDeps
pypiDeps privateBase publicBase = do
    prepared <- prepare inertRuleDeps [atDefaultPrecedence (AllowIfOlderThan 0)]
    pure
        (pypiServeDeps privateBase publicBase NoMirrorWrite prepared (pure servedAt))
            { pdMountBaseUrl = "http://ecluse.test/pypi"
            }

servedAt :: UTCTime
servedAt = UTCTime (fromGregorian 2026 6 20) 0

publicIndex :: Text -> Value
publicIndex authority =
    object
        [ "name" .= ("requests" :: Text)
        , "meta" .= object ["api-version" .= ("1.4" :: Text), "_last-serial" .= (37059094 :: Int)]
        , "files" .= map (fileOn authority) ["requests-2.34.2-py3-none-any.whl", "requests-2.34.2.tar.gz"]
        ]

privateIndexOnItself :: Text -> Value
privateIndexOnItself = publicIndex

privateIndexOn :: Text -> Text -> Value
privateIndexOn foreign_ _authority = publicIndex foreign_

fileOn :: Text -> Text -> Value
fileOn authority filename =
    object
        [ "filename" .= filename
        , "url" .= (authority <> "/simple/requests/" <> filename)
        , "hashes" .= object ["sha256" .= sha256Digest]
        , "requires-python" .= (">=3.10" :: Text)
        , "upload-time" .= ("2026-01-01T00:00:00Z" :: Text)
        , "core-metadata" .= object ["sha256" .= metadataDigest]
        , "dist-info-metadata" .= object ["sha256" .= sha256Digest]
        , "data-dist-info-metadata" .= object ["sha256" .= sha256Digest]
        ]

sha256Digest :: Text
sha256Digest = hexSha256Of (LBS.toStrict artifactBytes)

artifactBytes :: LByteString
artifactBytes = "python distribution bytes"

metadataBytes :: LByteString
metadataBytes = "Metadata-Version: 2.1\nName: requests\nVersion: 2.34.2\n"

metadataDigest :: Text
metadataDigest = hexSha256Of (LBS.toStrict metadataBytes)

clientCredential :: Header
clientCredential = ("Authorization", basicCredential)

basicCredential :: ByteString
basicCredential = "Basic X190b2tlbl9fOnByaXZhdGUtdG9rZW4="

helpMessage :: Text
helpMessage = "ask #platform for access"

helpBytes :: LByteString
helpBytes = LBS.fromStrict (encodeUtf8 helpMessage)

servedVersions :: SResponse -> [Text]
servedVersions resp = case fieldAt "versions" (decodedBody resp) of
    Just (Array versions) -> [v | String v <- toList versions]
    _ -> []

servedUrls :: SResponse -> [Text]
servedUrls = mapMaybe (textAtPath ["url"]) . servedFiles

servedDigests :: SResponse -> [Text]
servedDigests = mapMaybe digestOf . servedFiles
  where
    digestOf = textAtPath ["hashes", "sha256"]

servedKeys :: SResponse -> [Text]
servedKeys = concatMap (map Key.toText . KeyMap.keys . asObject) . servedFiles

servedFiles :: SResponse -> [Value]
servedFiles resp = case fieldAt "files" (decodedBody resp) of
    Just (Array files) -> toList files
    _ -> []

statusOf :: SResponse -> Int
statusOf = statusCode . simpleStatus

sidecarKeys :: [Text]
sidecarKeys = ["dist-info-metadata", "data-dist-info-metadata"]

excludesSidecar :: Text -> SResponse -> Expectation
excludesSidecar key resp = servedKeys resp `shouldNotContain` [key]

withoutSidecars :: Value -> Value
withoutSidecars = Object . (\fields -> foldr (KeyMap.delete . Key.fromText) fields sidecarKeys) . asObject

namedKeyFailure :: Text -> SomeException -> Bool
namedKeyFailure key = T.isInfixOf key . toText . displayException

availabilitySpec :: Spec
availabilitySpec = describe "admission-only yank policy" $ do
    for_ [(False, status200, status200), (True, status404, status403)] $ \(allYanked, listed, downloaded) ->
        it (caseLabel ("integration/pypi/native-020/" <> (if allYanked then "all-yanked" else "available")) (toText ("gates listings and distribution files with all files yanked=" <> show allYanked :: String))) $ do
            rules <- prepare inertRuleDeps [admissionOnly (atDefaultPrecedence DenyIfYanked), atDefaultPrecedence (AllowIfOlderThan 0)]
            withPyPIProxyOver (yankedIndex allYanked) publicPypiDeps $ \proxy -> do
                let app = proxyWithDeps proxy (\deps -> deps{pdRules = rules})
                listing <- getIndex app
                simpleStatus listing `shouldBe` listed
                artifact <- getPath sdistPath app
                simpleStatus artifact `shouldBe` downloaded
                when allYanked $ for_ [listing, artifact] $ \response ->
                    decodeUtf8 (LBS.toStrict (simpleBody response)) `shouldSatisfy` (not . T.isInfixOf "withdrawn notice must not enter refusals")
    it (caseLabel "integration/pypi/native-021" "keeps a previously mirrored yanked release retrievable from trusted metadata") $ do
        rules <- prepare inertRuleDeps [admissionOnly (atDefaultPrecedence DenyIfYanked), atDefaultPrecedence (AllowIfOlderThan 0)]
        withPrivatePyPIProxy (yankedIndex True) $ \proxy -> do
            let app = proxyWithDeps proxy (\deps -> deps{pdRules = rules})
            simpleStatus <$> getIndex app `shouldReturn` status200
            simpleStatus <$> getPath sdistPath app `shouldReturn` status200

yankedIndex :: Bool -> Text -> Value
yankedIndex allYanked authority = withKeys ["files" .= files] (publicIndex authority)
  where
    files =
        [ withKeys ["yanked" .= allYanked] (fileOn authority "requests-2.34.2-py3-none-any.whl")
        , withKeys ["yanked" .= ("withdrawn notice must not enter refusals" :: Text)] (fileOn authority "requests-2.34.2.tar.gz")
        ]
