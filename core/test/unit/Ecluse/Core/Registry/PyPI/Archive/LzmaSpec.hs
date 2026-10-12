-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PackageImports #-}

-- | Managed LZMA decoding retains its dictionary charge through early consumer exits.
module Ecluse.Core.Registry.PyPI.Archive.LzmaSpec (spec) where

import Conduit
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Test.Hspec
import UnliftIO.Exception (throwIO)
import "lzma" Codec.Compression.Lzma qualified as Lzma

import Ecluse.Core.Registry.PyPI.Archive.Lzma

spec :: Spec
spec = describe "withLzmaChunks" $ do
    it "decodes chunked and concatenated XZ members" $ do
        let bytes = BSL.toStrict (Lzma.compress "one")
        withLzmaChunks XzArchive (const (pure ())) (map BS.singleton (BS.unpack (bytes <> bytes))) (runExceptT . runConduit . (.| foldC)) `shouldReturn` Right "oneone"
    it "rejects truncated input and non-padding compressed tails" $ do
        let bytes = BSL.toStrict (Lzma.compress "one")
        for_ [BS.take (BS.length bytes - 1) bytes, bytes <> "junk"] $ \input ->
            withLzmaChunks XzArchive (const (pure ())) [input] (runExceptT . runConduit . (.| sinkNull)) >>= (`shouldSatisfy` isLeft)
    it "propagates a consumer failure after native output" $ do
        let bytes = BSL.toStrict (Lzma.compress (BSL.replicate 100000 97))
        withLzmaChunks XzArchive (const (pure ())) [bytes] (\source -> runExceptT (runConduit (source .| (await >> liftIO (throwIO LzmaStopped))))) `shouldThrow` (== LzmaStopped)
    it "charges the enforced native limit before decoder allocation" $ do
        requested <- newIORef 0
        let charge amount = writeIORef requested amount >> throwIO LzmaStopped
        withLzmaChunks XzArchive charge [] (runExceptT . runConduit . (.| sinkNull)) `shouldThrow` (== LzmaStopped)
        readIORef requested `shouldReturn` (64 * 1024 * 1024 + 32752)

data LzmaStopped = LzmaStopped deriving stock (Eq, Show)
instance Exception LzmaStopped
