-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

-- | PAX framing and bounded in-archive metadata link resolution.
module Ecluse.Core.Registry.PyPI.Archive.TarSpec (spec) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Conduit (yield, yieldMany)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Ecluse.Core.Registry (ParseError (..))
import Numeric (showOct)
import Test.Hspec
import UnliftIO.Exception (throwIO)

import Ecluse.Core.Registry.PyPI.Archive.Tar (extractTar)
import Ecluse.Core.Security.Limits (Limits (..), defaultLimits)
import Ecluse.Test.Package (pypiVersion, unscopedPyPI)
import Ecluse.Test.Registry.Archive (paxRecord, tarBytes)
import Ecluse.Test.Registry.JsonStream (byteChunks)
import Ecluse.Test.Registry.PyPI.Archive (metadata)
import Ecluse.Test.Support (expectRight)

spec :: Spec
spec = describe "extractTar" $ do
    it "accepts unrelated internal symbolic and hard links" $ do
        info <- regular "example-1/PKG-INFO" metadata
        target <- regular "example-1/module.py" "source"
        symbolic <- link True "example-1/symbolic.py" "module.py"
        hard <- link False "example-1/hard.py" "example-1/module.py"
        extract defaultLimits [info, target, symbolic, hard] `shouldReturn` Right metadata
    it "refuses a valid archive without Core Metadata" $ do
        source <- regular "example-1/module.py" "source"
        extract defaultLimits [source] `shouldReturn` Left (ParseError "archive has no Core Metadata")
    it "reads a long USTAR prefix when resolving a metadata link" $ do
        let directory = "example-1/" <> replicate 110 'a'
            path = directory <> "/metadata.txt"
        target <- regular path metadata
        alias <- link True "example-1/PKG-INFO" "placeholder"
        let longLink = Tar.longSymLinkEntry (replicate 110 'a' <> "/metadata.txt")
        extract defaultLimits [longLink, alias, target] `shouldReturn` Right metadata
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
        aliasEntry <- link True "example-1/alias" "placeholder"
        let bytes = BSL.toStrict (Tar.write [aliasEntry, info])
        for_
            [ ("../../outside", "tar link escapes its archive")
            , ("/outside", "unsafe tar link target")
            , ("C:/outside", "unsafe tar link target")
            , ("missing", "tar link target is absent")
            ]
            $ \(target, reason) -> do
                let header = linkTargetHeader target (BS.take 512 bytes)
                readChunks defaultLimits [header <> BS.drop 512 bytes] `shouldReturn` Left (ParseError reason)
        a <- link True "example-1/a" "b"
        b <- link True "example-1/b" "a"
        extract defaultLimits [info, a, b] `shouldReturn` Left (ParseError "cyclic tar link")
        let limits = defaultLimits{maxNestingDepth = 2}
        alias <- link True "example-1/PKG-INFO" "metadata.txt"
        target <- regular "example-1/metadata.txt" metadata
        extract limits [alias, target] `shouldReturn` Right metadata
        indirect <- link True "example-1/PKG-INFO" "next.txt"
        intermediate <- link True "example-1/next.txt" "metadata.txt"
        extract limits [indirect, intermediate, target] `shouldReturn` Left (ParseError "tar link chain exceeds its bound")
    it "uses PAX size for framing when the legacy header reports zero" $ do
        extended <- pax 'x' (paxRecord ("size=" <> encodeUtf8 (show (BS.length metadata) :: Text) <> "\n") <> paxRecord "SCHILY.xattr.user.note=ignored\n")
        info <- regular "example-1/PKG-INFO" metadata
        let bytes = BSL.toStrict (Tar.write [extended, info])
            header = BS.take 512 (BS.drop 1024 bytes)
            patched = zeroSizeHeader header
            archive = BS.take 1024 bytes <> patched <> BS.drop 1536 bytes
        readChunks defaultLimits (byteChunks archive) `shouldReturn` Right metadata
    it "applies PAX path and linkpath and ignores unrelated extension metadata" $ do
        extended <- pax 'x' (paxRecord "path=example-1/PKG-INFO\n" <> paxRecord "linkpath=metadata.txt\n" <> paxRecord "VENDOR.annotation=opaque\n")
        alias <- link True "placeholder" "unused"
        target <- regular "example-1/metadata.txt" metadata
        extract defaultLimits [extended, alias, target] `shouldReturn` Right metadata
    it "applies a local PAX path once and removes a deleted global path" $ do
        global <- pax 'g' (paxRecord "path=example-1/global.txt\n")
        localHeader <- pax 'x' (paxRecord "path=example-1/PKG-INFO\n")
        info <- regular "placeholder" metadata
        inherited <- regular "another-placeholder" "global content"
        deleteGlobal <- pax 'g' (paxRecord "path=\n")
        ordinary <- regular "example-1/ordinary.txt" "ordinary content"
        alias <- link True "example-1/global-alias" "global.txt"
        extract defaultLimits [global, localHeader, info, inherited, deleteGlobal, ordinary, alias] `shouldReturn` Right metadata
        extract defaultLimits [global, localHeader, info, deleteGlobal, ordinary, alias] `shouldReturn` Left (ParseError "tar link target is absent")
    it "lets an empty local PAX value suppress a global path for one entry" $ do
        global <- pax 'g' (paxRecord "path=example-1/inherited.txt\n")
        localHeader <- pax 'x' (paxRecord "path=\n")
        info <- regular "example-1/PKG-INFO" metadata
        inherited <- regular "placeholder" "data"
        deleteGlobal <- pax 'g' (paxRecord "path=\n")
        alias <- link True "example-1/inherited-alias" "inherited.txt"
        extract defaultLimits [global, localHeader, info, inherited, deleteGlobal, alias] `shouldReturn` Right metadata
        extract defaultLimits [global, localHeader, info, deleteGlobal, alias] `shouldReturn` Left (ParseError "tar link target is absent")
    it "reads GNU long-name and long-link records" $ do
        let longName = Tar.longLinkEntry "example-1/PKG-INFO"
            longLink = Tar.longSymLinkEntry "metadata.txt"
        alias <- link True "placeholder" "unused"
        target <- regular "example-1/metadata.txt" metadata
        extract defaultLimits [longName, longLink, alias, target] `shouldReturn` Right metadata
    it "rejects orphan and conflicting extended headers" $ do
        info <- regular "example-1/PKG-INFO" metadata
        extended <- pax 'x' (paxRecord "path=example-1/data\n")
        let longName = Tar.longLinkEntry "example-1/data"
        extract defaultLimits [info, extended] `shouldReturn` Left (ParseError "orphan extended tar header")
        extract defaultLimits [extended, longName, info] `shouldReturn` Left (ParseError "ambiguous extended tar headers")
    for_
        [ ("99 path=x\n", "truncated pax record")
        , ("x path=x\n", "malformed pax decimal")
        , ("9 path=x!", "malformed pax record")
        , (paxRecord "bad key=x\n", "malformed pax record")
        , (paxRecord "hdrcharset=BINARY\n", "unsupported pax header charset")
        , (paxRecord "SCHILY.realsize=10\n", "unsupported pax framing extension")
        ]
        $ \(attributes, reason) ->
            it ("classifies invalid PAX framing: " <> toString reason <> " " <> show attributes) $ do
                extended <- pax 'x' attributes
                info <- regular "example-1/PKG-INFO" metadata
                extract defaultLimits [extended, info] `shouldReturn` Left (ParseError reason)
    it "accepts an explicit UTF-8 PAX charset and rejects a non-UTF-8 path" $ do
        charset <- pax 'x' (paxRecord "hdrcharset=UTF-8\n")
        invalidPath <- pax 'x' (paxRecord ("path=example-1/" <> BS.singleton 255 <> "\n"))
        info <- regular "example-1/PKG-INFO" metadata
        extract defaultLimits [charset, info] `shouldReturn` Right metadata
        extract defaultLimits [invalidPath, info] `shouldReturn` Left (ParseError "archive path is not UTF-8")
    it "refuses metadata that resolves to an explicit or implicit directory" $ do
        path <- expectRight (Tar.toTarPath True "example-1/PKG-INFO")
        let directory = Tar.simpleEntry path Tar.Directory
        extract defaultLimits [directory] `shouldReturn` Left (ParseError "Core Metadata link does not name a regular file")
        alias <- link True "example-1/PKG-INFO" "src"
        child <- regular "example-1/src/module.py" "source"
        extract defaultLimits [alias, child] `shouldReturn` Left (ParseError "Core Metadata link does not name a regular file")
    it "refuses a member beneath a link even when its target remains in the archive" $ do
        info <- regular "example-1/PKG-INFO" metadata
        alias <- link True "example-1/alias" "src"
        target <- regular "example-1/src/module.py" "source"
        child <- regular "example-1/alias/extra.py" "extra"
        extract defaultLimits [info, alias, target, child] `shouldReturn` Left (ParseError "archive member traverses a link")
    it "rejects duplicate paths after PAX overrides and multiple metadata identities" $ do
        info <- regular "example-1/PKG-INFO" metadata
        extended <- pax 'x' (paxRecord "path=example-1/PKG-INFO\n")
        duplicate <- regular "placeholder" metadata
        equivalent <- regular "example-1.0/PKG-INFO" metadata
        extract defaultLimits [info, extended, duplicate] `shouldReturn` Left (ParseError "duplicate archive entry")
        extract defaultLimits [info, equivalent] `shouldReturn` Left (ParseError "ambiguous archive Core Metadata")
    it "refuses a PAX payload size on a link" $ do
        extended <- pax 'x' (paxRecord "size=1\n")
        alias <- link True "example-1/PKG-INFO" "metadata.txt"
        target <- regular "example-1/metadata.txt" metadata
        extract defaultLimits [extended, alias, target] `shouldReturn` Left (ParseError "non-file tar entry carries a payload")
    it "refuses a FIFO after valid metadata" $ do
        info <- regular "example-1/PKG-INFO" metadata
        path <- expectRight (Tar.toTarPath False "example-1/pipe")
        let pipeEntry = Tar.simpleEntry path Tar.NamedPipe
        extract defaultLimits [info, pipeEntry] `shouldReturn` Left (ParseError "unsupported tar device or framing extension")
    it "refuses an oversized entry before reading its payload" $ do
        info <- regular "example-1/PKG-INFO" metadata
        let bytes = BSL.toStrict (Tar.write [info])
            limits = defaultLimits{maxMirrorArtifactBytes = BS.length bytes}
            digits = toText (showOct (BS.length bytes + 1) "")
            size = encodeUtf8 (T.replicate (11 - T.length digits) "0" <> digits) <> "\NUL"
            oversized = checksumHeader (BS.take 124 bytes <> size <> BS.drop 136 (BS.take 512 bytes))
        readChunks limits [BS.take 512 bytes, BS.drop 512 bytes] `shouldReturn` Right metadata
        readChunks limits [oversized, BS.drop 512 bytes] `shouldReturn` Left (ParseError "tar entry size exceeds its bound")
    it "refuses payload size on a directory header and accepts a zero-size directory" $ do
        info <- regular "example-1/PKG-INFO" metadata
        path <- expectRight (Tar.toTarPath True "example-1/data")
        let directory = Tar.simpleEntry path Tar.Directory
            bytes = BSL.toStrict (Tar.write [directory, info])
            header = BS.take 512 bytes
            sized = checksumHeader (BS.take 124 header <> "00000000001\NUL" <> BS.drop 136 header)
        readChunks defaultLimits [bytes] `shouldReturn` Right metadata
        readChunks defaultLimits [sized <> BS.drop 512 bytes] `shouldReturn` Left (ParseError "non-file tar entry carries a payload")
    it "pays for an indexed path before reading its metadata payload" $ do
        tar <- BSL.toStrict <$> tarBytes [("example-1/PKG-INFO", metadata)]
        readPayload <- newIORef False
        let source = yield (BS.take 512 tar) >> liftIO (writeIORef readPayload True) >> yield (BS.drop 512 tar)
            charge requested = when (requested > 65536) (throwIO TarChargeStopped)
        extractTar defaultLimits charge (unscopedPyPI "example") (pypiVersion "1") ($ source) `shouldThrow` (== TarChargeStopped)
        readIORef readPayload `shouldReturn` False
    it "rejects duplicate framing keys, invalid size, and sparse framing extensions" $ do
        info <- regular "example-1/PKG-INFO" metadata
        for_ [(paxRecord "size=10\n" <> paxRecord "size=11\n", "duplicate pax framing attribute"), (paxRecord "size=-1\n", "malformed pax decimal"), (paxRecord "GNU.sparse.size=10\n", "unsupported pax framing extension")] $ \(attributes, reason) -> do
            extended <- pax 'x' attributes
            extract defaultLimits [extended, info] `shouldReturn` Left (ParseError reason)
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
zeroSizeHeader header = checksumHeader (BS.take 124 header <> "00000000000\NUL" <> BS.drop 136 header)

-- The native link constructor rejects absolute targets before the extractor can inspect them.
linkTargetHeader :: ByteString -> ByteString -> ByteString
linkTargetHeader target header = checksumHeader (BS.take 157 header <> target <> BS.replicate (100 - BS.length target) 0 <> BS.drop 257 header)

checksumHeader :: ByteString -> ByteString
checksumHeader header = BS.take 148 changed <> checksum <> BS.drop 156 changed
  where
    changed = BS.take 148 header <> BS.replicate 8 32 <> BS.drop 156 header
    digits = toText (showOct (BS.foldl' (\total byte -> total + fromIntegral byte) (0 :: Integer) changed) "")
    checksum = encodeUtf8 (T.replicate (6 - T.length digits) "0" <> digits) <> "\NUL "

data TarChargeStopped = TarChargeStopped
    deriving stock (Eq, Show)

instance Exception TarChargeStopped
