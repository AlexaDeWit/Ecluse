-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | PAX framing and bounded in-archive metadata link resolution.
module Ecluse.Core.Registry.PyPI.Archive.TarSpec (spec) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Conduit (liftIO, yield, yieldMany)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Ecluse.Core.Registry (ParseError)
import Numeric (showOct)
import Test.Hspec
import UnliftIO.Exception (throwIO)

import Ecluse.Core.Registry.PyPI.Archive.Tar (extractTar)
import Ecluse.Core.Security.Limits (Limits (..), defaultLimits)
import Ecluse.Test.Package (pypiVersion, unscopedPyPI)
import Ecluse.Test.Registry.PyPI.Archive (metadata, paxRecord, tarBytes)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "extractTar" $ do
    it "accepts unrelated internal symbolic and hard links" $ do
        info <- regular "example-1/PKG-INFO" metadata
        target <- regular "example-1/module.py" "source"
        symbolic <- link True "example-1/symbolic.py" "module.py"
        hard <- link False "example-1/hard.py" "example-1/module.py"
        extract defaultLimits [info, target, symbolic, hard] `shouldReturn` Right metadata
    for_ [True, False] $ \symbolic ->
        it ("resolves linked PKG-INFO before or after its target: " <> show symbolic) $ do
            target <- regular "example-1/metadata.txt" metadata
            alias <- link symbolic "example-1/PKG-INFO" (if symbolic then "metadata.txt" else "example-1/metadata.txt")
            extract defaultLimits [alias, target] `shouldReturn` Right metadata
            extract defaultLimits [target, alias] `shouldReturn` Right metadata
    it "accepts a link to an implicit directory without a directory header" $ do
        info <- regular "example-1/PKG-INFO" metadata
        child <- regular "example-1/src/module.py" "source"
        alias <- link True "example-1/alias" "src"
        extract defaultLimits [info, child, alias] `shouldReturn` Right metadata
    it "accepts relative parent references that remain inside the archive" $ do
        info <- regular "example-1/PKG-INFO" metadata
        target <- regular "example-1/data" "data"
        alias <- link True "example-1/sub/alias" "../data"
        extract defaultLimits [info, target, alias] `shouldReturn` Right metadata
    it "rejects escapes, dangling targets, cycles, and excessive link depth" $ do
        info <- regular "example-1/PKG-INFO" metadata
        for_ ["../../outside", "/outside", "C:/outside", "missing"] $ \target -> do
            alias <- link True "example-1/alias" target
            extract defaultLimits [info, alias] >>= (`shouldSatisfy` isLeft)
        a <- link True "example-1/a" "b"
        b <- link True "example-1/b" "a"
        extract defaultLimits [info, a, b] >>= (`shouldSatisfy` isLeft)
        alias <- link True "example-1/PKG-INFO" "metadata.txt"
        target <- regular "example-1/metadata.txt" metadata
        extract defaultLimits{maxNestingDepth = 1} [alias, target] >>= (`shouldSatisfy` isLeft)
    it "uses PAX size for framing when the legacy header reports zero" $ do
        extended <- pax 'x' (paxRecord ("size=" <> encodeUtf8 (show (BS.length metadata) :: Text) <> "\n") <> paxRecord "SCHILY.xattr.user.note=ignored\n")
        info <- regular "example-1/PKG-INFO" metadata
        let bytes = BSL.toStrict (Tar.write [extended, info])
            header = BS.take 512 (BS.drop 1024 bytes)
            patched = zeroSizeHeader header
            archive = BS.take 1024 bytes <> patched <> BS.drop 1536 bytes
        readChunks defaultLimits (map BS.singleton (BS.unpack archive)) `shouldReturn` Right metadata
    it "applies PAX path and linkpath and ignores unrelated extension metadata" $ do
        extended <- pax 'x' (paxRecord "path=example-1/PKG-INFO\n" <> paxRecord "linkpath=metadata.txt\n" <> paxRecord "VENDOR.annotation=opaque\n")
        alias <- link True "placeholder" "unused"
        target <- regular "example-1/metadata.txt" metadata
        extract defaultLimits [extended, alias, target] `shouldReturn` Right metadata
    it "pays for an indexed path before reading its metadata payload" $ do
        tar <- BSL.toStrict <$> tarBytes [("example-1/PKG-INFO", metadata)]
        readPayload <- newIORef False
        let source = yield (BS.take 512 tar) >> liftIO (writeIORef readPayload True) >> yield (BS.drop 512 tar)
            charge requested = when (requested > 65536) (throwIO TarChargeStopped)
        extractTar defaultLimits charge (unscopedPyPI "example") (pypiVersion "1") ($ source) `shouldThrow` (== TarChargeStopped)
        readIORef readPayload `shouldReturn` False
    it "rejects duplicate framing keys, invalid size, and sparse framing extensions" $ do
        info <- regular "example-1/PKG-INFO" metadata
        for_ [paxRecord "size=10\n" <> paxRecord "size=11\n", paxRecord "size=-1\n", paxRecord "GNU.sparse.size=10\n"] $ \attributes -> do
            extended <- pax 'x' attributes
            extract defaultLimits [extended, info] >>= (`shouldSatisfy` isLeft)
  where
    extract limits = readChunks limits . BSL.toChunks . Tar.write

readChunks :: Limits -> [ByteString] -> IO (Either ParseError ByteString)
readChunks limits chunks = extractTar limits (const (pure ())) (unscopedPyPI "example") (pypiVersion "1") ($ yieldMany chunks)

regular :: FilePath -> ByteString -> IO Tar.Entry
regular path bytes = do
    name <- expectRight (Tar.toTarPath False path)
    pure (Tar.fileEntry name (BSL.fromStrict bytes))

link :: Bool -> FilePath -> FilePath -> IO Tar.Entry
link symbolic path target = do
    name <- expectRight (Tar.toTarPath False path)
    destination <- maybe (fail "invalid fixture link") pure (Tar.toLinkTarget target)
    pure (Tar.simpleEntry name (if symbolic then Tar.SymbolicLink destination else Tar.HardLink destination))

pax :: Char -> BSL.ByteString -> IO Tar.Entry
pax code bytes = do
    path <- expectRight (Tar.toTarPath False "PaxHeaders/entry")
    pure (Tar.simpleEntry path (Tar.OtherEntryType code bytes (BSL.length bytes)))

zeroSizeHeader :: ByteString -> ByteString
zeroSizeHeader header = BS.take 148 changed <> checksum <> BS.drop 156 changed
  where
    changed = BS.take 124 header <> "00000000000\NUL" <> BS.take 12 (BS.drop 136 header) <> BS.replicate 8 32 <> BS.drop 156 header
    digits = toText (showOct (BS.foldl' (\total byte -> total + fromIntegral byte) (0 :: Integer) changed) "")
    checksum = encodeUtf8 (T.replicate (6 - T.length digits) "0" <> digits) <> "\NUL "

data TarChargeStopped = TarChargeStopped
    deriving stock (Eq, Show)

instance Exception TarChargeStopped
