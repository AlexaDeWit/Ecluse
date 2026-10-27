-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

module Ecluse.Core.Registry.RubyGems.HandlersSpec (spec) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Network.HTTP.Types (Header, Method, status200, status400, status401, status404, status502)
import Network.Wai (Request (..), defaultRequest, pathInfo, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Test (SRequest (SRequest), SResponse (..), runSession, setPath, srequest)
import Test.Hspec

import Ecluse.Core.Credential (mkSecret)
import Ecluse.Core.Registry.RubyGems.Adapter (rubyGemsAdapter)
import Ecluse.Core.Registry.RubyGems.Credential (rubyReadCredential)
import Ecluse.Core.Registry.RubyGems.Handlers (parseBatchQuery)
import Ecluse.Core.Registry.RubyGems.Route (rubyGemsRouter)
import Ecluse.Core.Security (Limits (..), defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Admission (newServeAdmissionTuned)
import Ecluse.Core.Server.Admission.Types (ChargeFactors (ChargeFactors))
import Ecluse.Core.Server.Context (MountBinding (..), PackumentDeps (..), ReadSourceSelection (PrivateOnly), RequestCtx (RequestCtx), ResponseAction (..), RouteAction (RouteAction), ServeRuntime, runHandler)
import Ecluse.Core.Server.Contract (responseToWai)
import Ecluse.Core.Server.Upstream (MirrorServePlan (NoMirrorWrite))
import Ecluse.Test.Deadline (withSweeper)
import Ecluse.Test.Log (newTestLogEnv)
import Ecluse.Test.Port (noopMetricsPort)
import Ecluse.Test.Server.Handler (testServeRuntimeWith)
import Ecluse.Test.Server.Mount (serveDepsFor)

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

withNative :: (ServeRuntime -> MountBinding -> IORef [([Text], [Header])] -> IO a) -> IO a
withNative use = withSweeper $ \deadlines -> do
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
        admission <- newServeAdmissionTuned deadlines 1 0 0
        runtime <- testServeRuntimeWith deadlines admission noopMetricsPort
        let origin = loopbackRegistryUrl ("http://localhost:" <> show port)
            adapter = rubyGemsAdapter (ChargeFactors 1000 1000 1000)
            deps = (serveDepsFor adapter (Just origin) origin NoMirrorWrite [] (pure (UTCTime (fromGregorian 2030 1 1) 0))){pdReadSourceSelection = PrivateOnly}
            binding = MountBinding ("rubygems" :| []) rubyGemsRouter rubyReadCredential deps Nothing
        use runtime binding hits

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
