-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.BenchLoad.ValkeySpec (spec) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Map.Strict qualified as Map
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (status200)
import Network.Socket (accept, close, socketToHandle)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp (openFreePort, testWithApplication)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO (hClose)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import UnliftIO (bracket, timeout, withAsync)
import UnliftIO.Exception (throwIO)

import Ecluse.BenchLoad.Valkey (Valkey, ValkeyConfig (..), externalFetch, responseLength, withValkey)
import Ecluse.Core.Credential (bareCredential, mkSecret)
import Ecluse.Core.Registry (RegistryResponse (responseBody))
import Ecluse.Core.Registry.Origin (OriginClient, originClient)
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Test.Package (unscopedNpm)
import Ecluse.Test.Wai (localhost)

spec :: Spec
spec = describe "bounded external cache replies" $ do
    it "accepts a bulk value at the body limit" $
        responseLength 1024 "$1024" `shouldBe` Right 1024
    it "refuses an oversized value before reading its body" $
        responseLength 1024 "$1025" `shouldSatisfy` isLeft
    it "refuses overflow, negative sizes, and protocol errors" $
        for_ ["$99999999999999999999999999999", "$-2", "-OOM command not allowed", "+unexpected"] $ \header ->
            responseLength 1024 header `shouldSatisfy` isLeft
    it "keeps private source and credential-bearing reads outside the external cache" $
        withSystemTempDirectory "valkey" $ \root ->
            testWithApplication (pure (\_ respond -> respond (responseLBS status200 [] "private"))) $ \port -> do
                manager <- newManager defaultManagerSettings
                let source = localhost port
                    config = ValkeyConfig 1 1 1000 1000 "test" source (root </> "events.jsonl") True
                withValkey config $ \client ->
                    for_ [(source <> "/private", Nothing), (source, Just (bareCredential (mkSecret "fixture-token")))] $ \(url, credential) -> do
                        response <- externalFetch client (originClient defaultLimits manager (loopbackRegistryUrl url) credential) (unscopedNpm "fixture")
                        fmap responseBody response `shouldBe` Right "private"
                doesFileExist (root </> "events.jsonl") `shouldReturn` False
    it "returns an external hit without an origin request" $
        withExternalFixture (const (pure "$6\r\ncached\r\n")) $ \client origin calls -> do
            fmap responseBody <$> externalFetch client origin (unscopedNpm "fixture") `shouldReturn` Right "cached"
            readIORef calls `shouldReturn` 0
    it "fills a miss synchronously and reuses the raw value on the next read" $ do
        stored <- newIORef Map.empty
        let respond args = case args of
                ["GET", key] -> do
                    value <- Map.lookup key <$> readIORef stored
                    pure (maybe "$-1\r\n" (\bytes -> "$" <> encodeUtf8 (show (BS.length bytes) :: Text) <> "\r\n" <> bytes <> "\r\n") value)
                ["SET", key, value, "PX", _] -> modifyIORef' stored (Map.insert key value) $> "+OK\r\n"
                _ -> throwIO MockProtocolFailure
        withExternalFixture respond $ \client origin calls -> do
            fmap responseBody <$> externalFetch client origin (unscopedNpm "fixture") `shouldReturn` Right "origin"
            fmap responseBody <$> externalFetch client origin (unscopedNpm "fixture") `shouldReturn` Right "origin"
            readIORef calls `shouldReturn` 1
    it "falls back when the cache accepts connections but never responds" $ do
        blocked <- newEmptyMVar
        result <- timeout 5_000_000 $ withExternalFixture (const (takeMVar blocked)) $ \client origin calls -> do
            fmap responseBody <$> externalFetch client origin (unscopedNpm "fixture") `shouldReturn` Right "origin"
            readIORef calls `shouldReturn` 1
        result `shouldBe` Just ()

data MockProtocolFailure = MockProtocolFailure
    deriving stock (Show)
instance Exception MockProtocolFailure

withExternalFixture :: ([ByteString] -> IO ByteString) -> (Valkey -> OriginClient -> IORef Int -> IO a) -> IO a
withExternalFixture respond use = withSystemTempDirectory "valkey" $ \root -> do
    calls <- newIORef 0
    testWithApplication (pure (\_ reply -> modifyIORef' calls (+ 1) >> reply (responseLBS status200 [] "origin"))) $ \originPort ->
        bracket openFreePort (close . snd) $ \(cachePort, listener) ->
            withAsync
                ( bracket (accept listener >>= (`socketToHandle` ReadWriteMode) . fst) hClose $ \handle -> do
                    hSetBuffering handle NoBuffering
                    forever (readCommand handle >>= respond >>= BS.hPut handle)
                )
                ( \_ -> do
                    manager <- newManager defaultManagerSettings
                    let source = localhost originPort
                        origin = originClient defaultLimits manager (loopbackRegistryUrl source) Nothing
                        config = ValkeyConfig cachePort 1 100_000 1000 "test" source (root </> "events.jsonl") False
                    withValkey config (\client -> use client origin calls)
                )

readCommand :: Handle -> IO [ByteString]
readCommand handle = do
    header <- BS.takeWhile (/= 13) <$> BSC.hGetLine handle
    count <- either (const (throwIO MockProtocolFailure)) pure (responseLength 10 ("$" <> BS.drop 1 header))
    replicateM count $ do
        lengthHeader <- BS.takeWhile (/= 13) <$> BSC.hGetLine handle
        size <- either (const (throwIO MockProtocolFailure)) pure (responseLength 1024 lengthHeader)
        bytes <- BS.hGet handle size
        ending <- BS.hGet handle 2
        unless (ending == "\r\n") (throwIO MockProtocolFailure)
        pure bytes
