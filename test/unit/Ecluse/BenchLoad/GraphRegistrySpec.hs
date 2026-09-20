-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.BenchLoad.GraphRegistrySpec (spec) where

import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Network.HTTP.Types (status200, status401, status404, status405)
import Network.Wai (defaultRequest, requestHeaders, requestMethod)
import Network.Wai.Test (request, runSession, setPath, simpleBody, simpleStatus)
import System.FilePath (takeDirectory)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Ecluse.BenchLoad.GraphRegistry
import Ecluse.Test.Package (hexSha256Of)

spec :: Spec
spec = describe "frozen graph response provenance" $ do
    it "keeps scoped artifact probes attached to their listing" $
        packageKey "@scope/name/-/name-1.0.0.tgz" `shouldBe` "@scope/name"
    it "cannot turn an HTTP path into a filesystem escape" $
        takeDirectory (capturePath "/capture" "../../outside") `shouldBe` "/capture"
    it "reports an uncaptured key as absent" $
        withSystemTempDirectory "graph" $
            \root -> loadCapture root "absent" `shouldReturn` Nothing
    it "rejects changed bytes against the saved digest" $
        withSystemTempDirectory "graph" $ \root -> do
            let key = "package"
                provenance = Capture key "https://registry.npmjs.org/package" "2026-09-20T00:00:00Z" (hexSha256Of "original") 8 200 "application/json" []
                base = capturePath root key
            LBS.writeFile (base <> ".json") (encode provenance)
            LBS.writeFile (base <> ".body") "replaced"
            loadCapture root key `shouldThrow` anyException
    it "retains upstream refusal bodies and status as evidence" $
        withSystemTempDirectory "graph" $ \root -> do
            let key = "missing"
                provenance = Capture key "https://registry.npmjs.org/missing" "2026-09-20T00:00:00Z" (hexSha256Of "{}") 2 404 "application/json" []
                base = capturePath root key
            LBS.writeFile (base <> ".json") (encode provenance)
            LBS.writeFile (base <> ".body") "{}"
            loadCapture root key `shouldReturn` Just (provenance, "{}")
    it "redacts unknown, cookie, and authentication header values" $
        map snd (traceHeaders [("Authorization", "secret"), ("Cookie", "secret"), ("X-Api-Key", "secret"), ("Accept", "application/json")])
            `shouldBe` ["[redacted]", "[redacted]", "[redacted]", "application/json"]
    it "captures once, rewrites metadata authority, and reads the frozen bytes next time" $
        withSystemTempDirectory "graph" $ \root -> do
            fetched <- newIORef (0 :: Int)
            let body = "{\"name\":\"@scope/name\",\"versions\":{},\"readme\":\"https://registry.npmjs.org/example\"}"
                fetch key = do
                    modifyIORef' fetched (+ 1)
                    saveCapture root key body
            app <- graphRegistryWithFetch (Just fetch) root 0
            let query = (setPath defaultRequest "/@scope%2Fname"){requestHeaders = [("Host", "frozen.test")]}
            firstResponse <- runSession (request query) app
            secondResponse <- runSession (request query) app
            simpleStatus firstResponse `shouldBe` status200
            simpleBody firstResponse `shouldBe` "{\"name\":\"@scope/name\",\"versions\":{},\"readme\":\"http://frozen.test/example\"}"
            simpleBody secondResponse `shouldBe` simpleBody firstResponse
            readIORef fetched `shouldReturn` 1
    it "serves exact artifact bytes and refuses missing, credential, method, and query requests" $
        withSystemTempDirectory "graph" $ \root -> do
            _ <- saveCapture root "package/-/package-1.0.0.tgz" "\NUL\255artifact"
            app <- graphRegistry False root 0
            artifact <- runSession (request (setPath defaultRequest "/package/-/package-1.0.0.tgz")) app
            simpleBody artifact `shouldBe` "\NUL\255artifact"
            for_ [(setPath defaultRequest "/missing", status404), ((setPath defaultRequest "/package"){requestMethod = "POST"}, status405), (setPath defaultRequest "/package?token=hidden", status405), ((setPath defaultRequest "/package"){requestHeaders = [("Authorization", "secret")]}, status401)] $ \(query, expected) ->
                simpleStatus <$> runSession (request query) app `shouldReturn` expected
            counters <- runSession (request (setPath defaultRequest "/_bench/counters")) app
            let counts = eitherDecode (simpleBody counters) :: Either String (Map Text Integer)
            fmap (Map.lookup "artifact.200") counts `shouldBe` Right (Just 1)
            fmap (Map.lookup "artifact.bodyBytes") counts `shouldBe` Right (Just 10)
            fmap (Map.lookup "metadata.405") counts `shouldBe` Right (Just 2)
            fmap (Map.lookup "metadata.200") counts `shouldBe` Right Nothing

saveCapture :: FilePath -> Text -> LByteString -> IO (Capture, ByteString)
saveCapture root key bytes = do
    let body = LBS.toStrict bytes
        provenance = Capture key ("https://registry.npmjs.org/" <> key) "2026-09-20T00:00:00Z" (hexSha256Of body) (fromIntegral (LBS.length bytes)) 200 "application/json" []
        base = capturePath root key
    LBS.writeFile (base <> ".json") (encode provenance)
    LBS.writeFile (base <> ".body") bytes
    pure (provenance, body)
