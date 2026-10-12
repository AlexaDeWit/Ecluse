-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Managed mirrors preserve private authorisation and complete visibility through HTTP routes.
module Ecluse.Core.Server.MirrorViewIntegrationSpec (spec) where

import Data.Aeson (Value (Array, Object), encode)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (dropWhileEnd)
import Data.Text qualified as T
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (Status, methodHead, status200, status304, status403, status503)
import Network.Wai (Application, defaultRequest, pathInfo, requestHeaders, responseLBS)
import Network.Wai.Handler.Warp (testWithApplication)
import Network.Wai.Test (SResponse (simpleBody, simpleHeaders, simpleStatus))
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Mirror.State
import Ecluse.Core.Mirror.State.Internal (StateRevision (StateRevision), VerifiedCompanionBytes (VerifiedCompanionBytes))
import Ecluse.Core.Queue (MirrorQueue (enqueue))
import Ecluse.Core.Registry.MirrorUpload (MirrorReadData (..), inventoryFiles, inventoryReadData)
import Ecluse.Core.Registry.PyPI.Document (simpleEnvelope, simpleFiles)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Server.Cache.Backend.Local (newLocalPool)
import Ecluse.Core.Server.Cache.Types (StoreBudget (StoreBudget))
import Ecluse.Core.Server.Context (PackumentDeps (..))
import Ecluse.Core.Server.MirrorView
import Ecluse.Core.Server.Upstream (MirrorServePlan (NoMirrorWrite))
import Ecluse.Runtime.Server (application, mkServerConfig)
import Ecluse.Server.Pipeline.TestSupport (getPath, getPathWith, newTestEnvWithQueue, requestAt)
import Ecluse.Service (mountBindingFor)
import Ecluse.Test.Mirror.State
import Ecluse.Test.Queue (newTestMemoryQueue)
import Ecluse.Test.Server.Mount (pypiServeDeps)
import Ecluse.Test.Support (expectRight)
import Ecluse.Test.Wai (localhost, lookupAuth, selfBaseUrl)

spec :: Spec
spec = describe "managed PyPI mirror reads" $ do
    it "refuses private access before consulting retained source fields" $ do
        calls <- newIORef (0 :: Int)
        view <- testView (modifyIORef' calls (+ 1) $> Right StateAbsent)
        withManagedProxy view False (native status403 [1]) (native status200 [1]) $ \app -> do
            response <- getPath "/pypi/simple/demo/" app
            simpleStatus response `shouldBe` status403
            artifact <- getPath artifactPath app
            simpleStatus artifact `shouldBe` status403
        readIORef calls `shouldReturn` 0

    it "bypasses state entirely for first-party private names" $ do
        calls <- newIORef (0 :: Int)
        view <- testView (modifyIORef' calls (+ 1) $> Left (StateInvalid "unavailable"))
        withManagedProxy view True (native status200 [1]) (native status503 []) $ \app -> do
            response <- getPath "/pypi/simple/demo/" app
            simpleStatus response `shouldBe` status200
            artifact <- getPath artifactPath app
            simpleStatus artifact `shouldBe` status200
        readIORef calls `shouldReturn` 0

    it "preserves unmanaged private handling on conclusive project absence" $ do
        view <- testView (pure (Right StateAbsent))
        withManagedProxy view False (native status200 [1]) (native status503 []) $ \app -> do
            response <- getPath "/pypi/simple/demo/" app
            simpleStatus response `shouldBe` status200
            artifact <- getPath artifactPath app
            simpleStatus artifact `shouldBe` status200

    it "cannot serve A and B while unread state may retain expected C" $ do
        view <- testView (pure (Left (StateInvalid "state outage")))
        withManagedProxy view False (native status200 [1, 2]) (native status200 [1, 2]) $ \app -> do
            response <- getPath "/pypi/simple/demo/" app
            simpleStatus response `shouldBe` status503
            artifact <- getPath artifactPath app
            simpleStatus artifact `shouldBe` status503

    it "serves completed artifacts without public reads, policy evaluation, or enqueue after fresh proxy construction" $ do
        inventory <- mirrorSample [1, 2, 3] False "https://public.test/" "original"
        record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
        calls <- newIORef (0 :: Int)
        publicCalls <- newIORef (0 :: Int)
        view <- testView (modifyIORef' calls (+ 1) $> Right (StateLoaded revision record))
        let public request respond = modifyIORef' publicCalls (+ 1) >> native status503 [] request respond
        withManagedProxy view False (native status200 [1, 2, 3]) public $ \app -> do
            for_ [1, 2, 3] $ \number -> do
                response <- getPath ("/pypi/simple/demo/" <> encodeUtf8 (mirrorFilename number)) app
                simpleStatus response `shouldBe` status200
                simpleBody response `shouldBe` "wheel"
        readIORef calls `shouldReturn` 3
        readIORef publicCalls `shouldReturn` 0

    it "does not reuse a successful caller's authorisation for a refused credential" $ do
        calls <- newIORef (0 :: Int)
        view <- testView (modifyIORef' calls (+ 1) $> Right StateAbsent)
        let private request = native (if lookupAuth (requestHeaders request) == Just "Basic dXNlcjpnb29k" then status200 else status403) [1] request
        withManagedProxy view False private (native status503 []) $ \app -> do
            accepted <- getPathWith [("Authorization", "Basic dXNlcjpnb29k")] artifactPath app
            simpleStatus accepted `shouldBe` status200
            refused <- getPathWith [("Authorization", "Basic dXNlcjpiYWQ=")] artifactPath app
            simpleStatus refused `shouldBe` status403
        readIORef calls `shouldReturn` 1

    it "serves retained companions with HEAD and validators after the native target drops the declaration" $ do
        inventory <- mirrorSample [1] True "https://public.test/" "source"
        initial <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
        let expected :| _ = inventoryFiles inventory
        verified <- expectRight (verifyCompanionBytes defaultLimits expected "retained metadata")
        let VerifiedCompanionBytes _ reference _ = verified
        record <- expectRight (retainCompanionReference mirrorVersion verified reference initial)
        view <- testViewWith (pure (Right (StateLoaded revision record))) (\_ -> pure (Right "retained metadata"))
        publicCalls <- newIORef (0 :: Int)
        let public request respond = modifyIORef' publicCalls (+ 1) >> native status503 [] request respond
            path = artifactPath <> ".metadata"
        withManagedProxy view False (native status200 [1]) public $ \app -> do
            first <- getPath path app
            simpleStatus first `shouldBe` status200
            simpleBody first `shouldBe` "retained metadata"
            headed <- requestAt methodHead path defaultRequest app
            simpleStatus headed `shouldBe` status200
            simpleBody headed `shouldBe` ""
            etag <- maybe (fail "missing companion validator") pure (lookup "ETag" (simpleHeaders first))
            unchanged <- getPathWith [("If-None-Match", etag)] path app
            simpleStatus unchanged `shouldBe` status304
        readIORef publicCalls `shouldReturn` 0

    it "returns explicit unavailability when a native file disappears" $ do
        inventory <- mirrorSample [1, 2, 3] False "https://public.test/" "original"
        record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
        view <- testView (pure (Right (StateLoaded revision record)))
        withManagedProxy view False (native status200 [1, 2]) (native status503 []) $ \app -> do
            response <- getPath "/pypi/simple/demo/" app
            simpleStatus response `shouldBe` status503
            artifact <- getPath artifactPath app
            simpleStatus artifact `shouldBe` status503

native :: Status -> [Int] -> Application
native status [] _ respond = respond (responseLBS status [] "{}")
native status numbers request respond = case dropWhileEnd T.null (pathInfo request) of
    ["simple", _] -> do
        inventory <- mirrorSample numbers False (selfBaseUrl request <> "/files/") "native"
        let body = case inventoryReadData inventory of
                PyPIMirrorRead document -> encode (Object (KeyMap.insert "files" (fromListValue (map snd (simpleFiles document))) (simpleEnvelope document)))
                NpmMirrorRead fields -> encode (Object fields)
        respond (responseLBS status [("Content-Type", "application/vnd.pypi.simple.v1+json")] body)
    _ -> respond (responseLBS status [] "wheel")

fromListValue :: [Value] -> Value
fromListValue = Array . fromList

artifactPath :: ByteString
artifactPath = "/pypi/simple/demo/" <> encodeUtf8 (mirrorFilename 1)

target :: StateTarget
target = stateTarget "https://private.test/"

revision :: StateRevision
revision = StateRevision target mirrorPackage "one"

testView :: IO (Either MirrorStateFault StateRead) -> IO MirrorView
testView readState = testViewWith readState (\_ -> pure (Left StateMissingCompanion))

testViewWith :: IO (Either MirrorStateFault StateRead) -> (CompanionRef -> IO (Either MirrorStateFault ByteString)) -> IO MirrorView
testViewWith readState companion = do
    pool <- newLocalPool 20 10_000_000
    cache <- newMirrorRetention pool 60 (StoreBudget 0 0)
    let store = MirrorState (\_ _ _ -> readState) (\_ _ _ _ -> pure (Left (StateInvalid "unexpected state write"))) (\_ _ -> pure (Left StateMissingCompanion)) (const companion)
    pure (newMirrorView defaultLimits target store cache)

withManagedProxy :: MirrorView -> Bool -> Application -> Application -> (Application -> IO a) -> IO a
withManagedProxy view firstParty private public action = do
    queue <- newTestMemoryQueue
    manager <- newManager defaultManagerSettings
    testWithApplication (pure private) $ \privatePort ->
        testWithApplication (pure public) $ \publicPort -> do
            env <- newTestEnvWithQueue queue{enqueue = \_ -> fail "unexpected mirror enqueue"} manager
            let deps =
                    (pypiServeDeps (Just (loopbackRegistryUrl (localhost privatePort))) (loopbackRegistryUrl (localhost publicPort)) NoMirrorWrite [] (pure (UTCTime (fromGregorian 2026 10 11) 0)))
                        { pdMirrorState = Just view
                        , pdFirstParty = const firstParty
                        , pdMountBaseUrl = "http://ecluse.test/pypi"
                        }
            action (application (mkServerConfig (maybeToList (mountBindingFor PyPI deps Nothing))) env)
