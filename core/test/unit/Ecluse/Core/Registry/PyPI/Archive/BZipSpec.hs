-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | BZIP2 framing, block declarations, and consumer failures share the production resource scope.
module Ecluse.Core.Registry.PyPI.Archive.BZipSpec (spec) where

import Codec.Compression.BZip qualified as BZip
import Conduit
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Test.Hspec
import UnliftIO.Exception (throwIO)

import Ecluse.Core.Registry.PyPI.Archive.BZip (withBzipChunks)

spec :: Spec
spec = describe "withBzipChunks" $ do
    it "decodes concatenated members within the declared count" $ do
        let bytes = BSL.toStrict (BZip.compress "one")
        withBzipChunks 2 (const (pure ())) (bytes <> bytes) (runExceptT . runConduit . (.| foldC)) `shouldReturn` Right "oneone"
        withBzipChunks 1 (const (pure ())) (bytes <> bytes) (runExceptT . runConduit . (.| foldC)) >>= (`shouldSatisfy` isLeft)
    it "rejects malformed, truncated, and trailing compressed input" $ do
        let bytes = BSL.toStrict (BZip.compress "one")
        for_ ["", "BZh0bad", BS.take (BS.length bytes - 1) bytes, bytes <> "junk"] $ \input ->
            withBzipChunks 1 (const (pure ())) input (runExceptT . runConduit . (.| sinkNull)) >>= (`shouldSatisfy` isLeft)
    it "propagates a consumer failure after native output" $ do
        let bytes = BSL.toStrict (BZip.compress (BSL.replicate 100000 97))
        withBzipChunks 1 (const (pure ())) bytes (\source -> runExceptT (runConduit (source .| (await >> liftIO (throwIO BzipStopped))))) `shouldThrow` (== BzipStopped)
    it "charges before opening the decoder" $
        withBzipChunks 1 (const (throwIO BzipStopped)) "BZh9" (runExceptT . runConduit . (.| sinkNull)) `shouldThrow` (== BzipStopped)

data BzipStopped = BzipStopped deriving stock (Eq, Show)
instance Exception BzipStopped
