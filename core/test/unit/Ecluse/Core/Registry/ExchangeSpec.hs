-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Destructive requests are not replayed, and digesting reads cover every consumed chunk.
module Ecluse.Core.Registry.ExchangeSpec (spec) where

import Data.Aeson (Value (String))
import Data.ByteString qualified as BS
import Data.JsonStream.Parser qualified as J
import Network.HTTP.Client (Request (method), defaultManagerSettings, httpLbs, newManager, parseRequest)
import Network.HTTP.Types (status200)
import Network.Wai (responseLBS, responseRaw)
import Network.Wai.Handler.Warp (testWithApplication)
import Test.Hspec
import UnliftIO.Exception (tryAny)

import Ecluse.Core.Registry.Exchange (digestingRead, singleAttemptSettings)
import Ecluse.Core.Registry.JsonStream (StreamResult (..), readJsonStream, retainedValue)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (BodyTooLarge))
import Ecluse.Core.Snapshot (ContentDigest)
import Ecluse.Test.Snapshot (digestOf)
import Ecluse.Test.Support (expectRightIO)

spec :: Spec
spec = do
    singleAttemptSpec
    digestingReadSpec

singleAttemptSpec :: Spec
singleAttemptSpec = describe "singleAttemptSettings" $
    it "returns a lost destructive response without resending on a fresh connection" $ do
        requests <- newIORef (0 :: Int)
        let application _ respond = do
                attempt <- atomicModifyIORef' requests (\n -> (n + 1, n + 1))
                respond $
                    if attempt == 1
                        then responseLBS status200 [] "warm connection"
                        else responseRaw (\_ _ -> pure ()) (responseLBS status200 [] "")
        testWithApplication (pure application) $ \port -> do
            manager <- newManager (singleAttemptSettings defaultManagerSettings)
            request <- parseRequest ("http://127.0.0.1:" <> show port <> "/")
            void (httpLbs request manager)
            result <- tryAny (httpLbs request{method = "DELETE"} manager)
            result `shouldSatisfy` isLeft
            readIORef requests `shouldReturn` 2

digestingReadSpec :: Spec
digestingReadSpec = describe "digestingRead" $ do
    it "digests every chunk at every split, including bytes after the extracted value" $ do
        let body = "{\"keep\":\"yes\",\"ignored\":[1,2,3]} trailing bytes"
        forM_ [1 .. BS.length body - 1] $ \position -> do
            (streamed, digest) <- expectRightIO (readDigested (MetadataBodyLimit 1024) [BS.take position body, BS.drop position body])
            streamValue streamed `shouldBe` Right (Just (String "yes"))
            streamBytes streamed `shouldBe` BS.length body
            digest `shouldBe` digestOf body

    it "digests omitted fields even when the extracted value stays equal" $ do
        (firstRead, firstDigest) <- expectRightIO (readDigested (MetadataBodyLimit 1024) ["{\"keep\":\"yes\",\"ignored\":\"one\"}"])
        (secondRead, secondDigest) <- expectRightIO (readDigested (MetadataBodyLimit 1024) ["{\"keep\":\"yes\",\"ignored\":\"two\"}"])
        streamValue firstRead `shouldBe` streamValue secondRead
        firstDigest `shouldNotBe` secondDigest

    it "returns the consumer's refusal without a digest" $
        readDigested (MetadataBodyLimit 2) ["{}", "x"] `shouldReturn` Left (BodyTooLarge (MetadataBodyLimit 2))

readDigested :: BodyLimit -> [ByteString] -> IO (Either LimitError (StreamResult (Maybe Value), ContentDigest))
readDigested bound chunks = do
    remaining <- newIORef chunks
    let next = atomicModifyIORef' remaining $ \case
            [] -> ([], BS.empty)
            chunk : rest -> (rest, chunk)
    digestingRead (readJsonStream bound (J.objectWithKey "keep" (retainedValue 1)) (\_ value -> Right (Just value)) Nothing) next
