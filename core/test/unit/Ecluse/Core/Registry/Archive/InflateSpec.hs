-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | Scoped zlib decoding checks framing and closes its pull action on every callback exit.
module Ecluse.Core.Registry.Archive.InflateSpec (spec) where

import Codec.Compression.GZip qualified as GZip
import Codec.Compression.Zlib qualified as Zlib
import Codec.Compression.Zlib.Raw qualified as Raw
import Control.Concurrent.MVar qualified as MVar
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Test.Hspec
import UnliftIO.Async (cancel, race_, wait, withAsync)
import UnliftIO.Exception (throwIO)

import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.Archive.Inflate
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = do
    chunkSpec
    inflateBoundedSpec

chunkSpec :: Spec
chunkSpec = describe "withInflatedChunks" $ do
    it "decodes raw deflate and gzip with bounded output chunks" $ do
        let body = BS.replicate 100000 97
        for_ [(RawDeflate, Raw.compress), (ZlibStream, Zlib.compress), (GzipMembers 1, GZip.compress)] $ \(format, compress) -> do
            result <- withInflatedChunks format (const (pure ())) (BSL.toStrict (compress (BSL.fromStrict body))) collect
            BS.concat <$> expectRight result `shouldReturn` body
    it "checks zlib checksum, truncation, framing and exact consumption" $ do
        let bytes = BSL.toStrict (Zlib.compress "one")
        for_ [bytes <> "junk", bytes <> bytes, BS.take (BS.length bytes - 1) bytes, BS.init bytes <> "x", BSL.toStrict (Raw.compress "one"), BSL.toStrict (GZip.compress "one")] $ \malformed -> do
            result <- withInflatedChunks ZlibStream (const (pure ())) malformed collect
            result `shouldSatisfy` isLeft
    it "checks every gzip member and rejects trailing junk or an exceeded count" $ do
        let member = BSL.toStrict (GZip.compress "one")
        withInflatedChunks (GzipMembers 2) (const (pure ())) (member <> member) collect `shouldReturn` Right ["one", "one"]
        for_ [(GzipMembers 1, member <> member, "compressed member count exceeds its bound"), (GzipMembers 2, member <> "junk", "malformed or truncated compressed stream"), (GzipMembers 1, BS.take (BS.length member - 1) member, "malformed or truncated compressed stream")] $ \(format, bytes, reason) ->
            withInflatedChunks format (const (pure ())) bytes collect `shouldReturn` Left (ParseError reason)
    it "refuses trailing raw-deflate bytes and a zero-member allowance" $ do
        let raw = BSL.toStrict (Raw.compress "one")
            gzip = BSL.toStrict (GZip.compress "one")
        withInflatedChunks RawDeflate (const (pure ())) raw collect `shouldReturn` Right ["one"]
        withInflatedChunks RawDeflate (const (pure ())) (raw <> "junk") collect `shouldReturn` Left (ParseError "deflate member has trailing compressed bytes")
        withInflatedChunks (GzipMembers 1) (const (pure ())) gzip collect `shouldReturn` Right ["one"]
        withInflatedChunks (GzipMembers 0) (const (pure ())) gzip collect `shouldReturn` Left (ParseError "compressed member count exceeds its bound")
    it "retains a typed failure inside the callback and closes the saved pull afterwards" $ do
        let raw = BSL.toStrict (Raw.compress "one") <> "junk"
            fault = ParseError "deflate member has trailing compressed bytes"
        saved <- withInflatedChunks RawDeflate (const (pure ())) raw $ \pull -> do
            collect pull `shouldReturn` Left fault
            pull `shouldReturn` Left fault
            pure pull
        saved `shouldReturn` Right BS.empty
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

inflateBoundedSpec :: Spec
inflateBoundedSpec = describe "inflateBounded" $ do
    it "collects the payload, paying for the decoder and then for each kept chunk" $ do
        paid <- newIORef []
        let body = BS.replicate 70000 97
        result <- inflateBounded ZlibStream "payload exceeds its inflated byte bound" 70000 (\n -> modifyIORef' paid (<> [n])) (BSL.toStrict (Zlib.compress (BSL.fromStrict body)))
        result `shouldBe` Right body
        readIORef paid `shouldReturn` [32768 + 7168 + 32768, 73728 + 3 * 32768, 73728 + 3 * 65536, 73728 + 3 * 70000]
    it "refuses a payload past its inflated bound with the caller's reason" $
        inflateBounded (GzipMembers 1) "payload exceeds its inflated byte bound" 2 (const pass) (BSL.toStrict (GZip.compress "one"))
            `shouldReturn` Left (ParseError "payload exceeds its inflated byte bound")

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
