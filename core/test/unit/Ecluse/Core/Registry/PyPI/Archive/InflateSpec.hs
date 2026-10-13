-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Scoped zlib decoding checks framing and closes its pull action on every callback exit.
module Ecluse.Core.Registry.PyPI.Archive.InflateSpec (spec) where

import Codec.Compression.GZip qualified as GZip
import Codec.Compression.Zlib.Raw qualified as Raw
import Control.Concurrent.MVar qualified as MVar
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Test.Hspec
import UnliftIO.Async (cancel, race_, wait, withAsync)
import UnliftIO.Exception (throwIO)

import Ecluse.Core.Registry (ParseError)
import Ecluse.Core.Registry.PyPI.Archive.Inflate
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "withInflatedChunks" $ do
    it "decodes raw deflate and gzip with bounded output chunks" $ do
        let body = BS.replicate 100000 97
        for_ [(RawDeflate, Raw.compress), (GzipMembers 1, GZip.compress)] $ \(format, compress) -> do
            result <- withInflatedChunks format (const (pure ())) (BSL.toStrict (compress (BSL.fromStrict body))) collect
            BS.concat <$> expectRight result `shouldReturn` body
    it "checks every gzip member and rejects trailing junk or an exceeded count" $ do
        let member = BSL.toStrict (GZip.compress "one")
        withInflatedChunks (GzipMembers 2) (const (pure ())) (member <> member) collect `shouldReturn` Right ["one", "one"]
        for_ [(GzipMembers 1, member <> member), (GzipMembers 2, member <> "junk"), (GzipMembers 1, BS.take (BS.length member - 1) member)] $ \(format, bytes) ->
            withInflatedChunks format (const (pure ())) bytes collect >>= (`shouldSatisfy` isLeft)
    it "closes an active inflater when its consumer returns early" $ do
        let bytes = BSL.toStrict (Raw.compress (BSL.replicate 100000 97))
        pull <- withInflatedChunks RawDeflate (const (pure ())) bytes $ \readChunk -> do
            initialChunk <- readChunk >>= expectRight
            BS.length initialChunk `shouldBe` 32768
            pure readChunk
        pull `shouldReturn` Right BS.empty
    it "closes an active inflater before a consumer exception escapes" $ do
        saved <- newIORef (pure (Right BS.empty))
        let bytes = BSL.toStrict (Raw.compress (BSL.replicate 100000 97))
        withInflatedChunks RawDeflate (const (pure ())) bytes (\pull -> void pull >> writeIORef saved pull >> throwIO DecoderStopped) `shouldThrow` (== DecoderStopped)
        join (readIORef saved) `shouldReturn` Right BS.empty
    it "closes an active inflater when another thread cancels its consumer" $ do
        ready <- MVar.newEmptyMVar
        blocked <- MVar.newEmptyMVar
        saved <- newIORef (pure (Right BS.empty))
        let bytes = BSL.toStrict (Raw.compress (BSL.replicate 100000 97))
            consume pull = void (pull >>= expectRight) >> writeIORef saved pull >> MVar.putMVar ready () >> MVar.takeMVar blocked
        withAsync (withInflatedChunks RawDeflate (const (pure ())) bytes consume) $ \worker -> do
            race_ (MVar.takeMVar ready) (wait worker)
            cancel worker
        join (readIORef saved) `shouldReturn` Right BS.empty

collect :: IO (Either ParseError ByteString) -> IO (Either ParseError [ByteString])
collect pull =
    pull >>= \case
        Left fault -> pure (Left fault)
        Right bytes | BS.null bytes -> pure (Right [])
        Right bytes -> do
            BS.length bytes `shouldSatisfy` (<= 32768)
            fmap (bytes :) <$> collect pull

data DecoderStopped = DecoderStopped deriving stock (Eq, Show)
instance Exception DecoderStopped
