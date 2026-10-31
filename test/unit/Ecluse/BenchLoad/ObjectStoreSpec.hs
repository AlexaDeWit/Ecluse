-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The bench's object reads answer as S3 does: stored bytes under a revision, 304 on a match, NoSuchKey when absent.
module Ecluse.BenchLoad.ObjectStoreSpec (spec) where

import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (Header, status200, status304, status404)
import Network.HTTP.Types.Header (hETag, hIfNoneMatch)
import Network.Wai (rawPathInfo)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec

import Ecluse.BenchLoad.ObjectStore (conditionalObjectRead, objectRevision)
import Ecluse.BenchLoad.ProxyProcess (mirrorStateBucket)
import Ecluse.Core.Mirror.State (stateTarget)
import Ecluse.Core.Mirror.State.Internal (projectKey)
import Ecluse.Test.Mirror.Read (ReadFixture (fixtureOld, fixtureTarget), makeReadFixtureFor)
import Ecluse.Test.Mirror.State (mirrorPackage)

spec :: Spec
spec = describe "conditionalObjectRead" $
    it "answers a stored object under its revision, a matching revision with an empty 304, and a missing key as NoSuchKey" $ do
        input <- makeReadFixtureFor (stateTarget "https://private.fixture") 1
        let nativePath = encodeUtf8 ("/" <> mirrorStateBucket <> "/" <> projectKey (fixtureTarget input) mirrorPackage <> "/project.json")
            objects = Map.singleton nativePath (fixtureOld input)
            application request respond = respond (conditionalObjectRead request ((\body -> (objectRevision body, body)) <$> Map.lookup (rawPathInfo request) objects))
        testWithApplication (pure application) $ \port -> do
            manager <- HTTP.newManager HTTP.defaultManagerSettings
            let fetchObject :: ByteString -> [Header] -> IO (HTTP.Response LByteString)
                fetchObject path headers = do
                    request <- HTTP.parseRequest ("http://127.0.0.1:" <> show port <> toString (decodeUtf8 path :: Text))
                    HTTP.httpLbs request{HTTP.requestHeaders = headers, HTTP.checkResponse = \_ _ -> pass} manager
            stored <- fetchObject nativePath []
            HTTP.responseStatus stored `shouldBe` status200
            toStrict (HTTP.responseBody stored) `shouldBe` fixtureOld input
            case lookup hETag (HTTP.responseHeaders stored) of
                Nothing -> expectationFailure "the stored object has no revision"
                Just revision -> do
                    unchanged <- fetchObject nativePath [(hIfNoneMatch, revision)]
                    HTTP.responseStatus unchanged `shouldBe` status304
                    HTTP.responseBody unchanged `shouldBe` ""
                    lookup hETag (HTTP.responseHeaders unchanged) `shouldBe` Just revision
            missing <- fetchObject (encodeUtf8 ("/" <> mirrorStateBucket <> "/missing/project.json")) []
            HTTP.responseStatus missing `shouldBe` status404
            HTTP.responseBody missing `shouldBe` "<Error><Code>NoSuchKey</Code></Error>"
