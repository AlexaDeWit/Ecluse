-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Candidate source digests for issue #1520. Each capture passes through a digesting source in
pieces of 32 KiB, the size an inflated registry response arrives in. The largest capture of each
ecosystem also runs the production walk under each candidate. A measurement, not for @main@.
-}
module Ecluse.Core.DigestBench (benchmarks) where

import Crypto.Hash (Blake2b_256, Context, HashAlgorithm, SHA512, hashFinalize, hashInit, hashUpdate)
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import OpenSSL (withOpenSSL)
import OpenSSL.EVP.Digest (Digest, getDigestByName)
import OpenSSL.EVP.Internal (digestFinalBS, digestStrictly, digestUpdateBS)
import Test.Tasty.Bench (Benchmark, bench, bgroup, whnfAppIO)
import Test.Tasty.HUnit (assertFailure, (@?=))
import UnliftIO (tryIO)

import Ecluse.Bench.Corpus (LoadedEntry, entryName)
import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems), ecosystemName)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.Exchange (digestingRead)
import Ecluse.Core.Registry.JsonStream (StreamResult (streamValue))
import Ecluse.Core.Registry.Npm.Metadata (readNpmFull)
import Ecluse.Core.Registry.PyPI.Metadata (readPyPIIndex)
import Ecluse.Core.Registry.PyPI.Streaming (PyPIRead (FullRead))
import Ecluse.Core.Security (LimitError, Limits (maxMetadataBytes), defaultLimits)
import Ecluse.Core.Snapshot (digestBytes)
import Ecluse.Test.Corpus (CaptureUpstream (upstreamOrigin), cpPackage)
import Ecluse.Test.EcosystemBench (EcosystemBench (..))
import Ecluse.Test.Registry.JsonStream (heldChunks)
import Ecluse.Test.Snapshot (digestOf)

-- | What reads the source's chunks, as 'digestingRead' takes it. The flag says the read gave a value.
type Consumer = IO ByteString -> IO (Either LimitError Bool)

-- | A digesting source in the shape of 'digestingRead', with the digest as raw bytes.
type Digesting = Consumer -> IO ByteString -> IO (Either LimitError (Bool, ByteString))

data Candidate = Candidate
    { candidateLabel :: String
    , candidateBytes :: Int
    -- ^ The digest's length.
    , candidateKeepsDigest :: Bool
    -- ^ Whether its digest must equal the shipped SHA-256, so no ETag would change.
    , candidateDigesting :: Digesting
    }

-- | One group per ecosystem. The host's processor lines go to stderr first, for the run's log.
benchmarks :: [EcosystemBench] -> IO [Benchmark]
benchmarks ecosystems = do
    reportProcessor
    sha256 <- withOpenSSL (getDigestByName "sha256") >>= maybe (assertFailure "digest bench: OpenSSL offers no sha256") pure
    traverse (ecosystemGroup (candidates sha256)) ecosystems

candidates :: Digest -> [Candidate]
candidates sha256 =
    [ Candidate "sha256 crypton (shipped)" 32 True (\consume next -> fmap (second digestBytes) <$> digestingRead consume next)
    , Candidate "sha256 openssl" 32 True (opensslDigesting sha256)
    , Candidate "blake2b-256 crypton" 32 False (cryptonDigesting (hashInit :: Context Blake2b_256))
    , Candidate "sha512 crypton" 64 False (cryptonDigesting (hashInit :: Context SHA512))
    ]

ecosystemGroup :: [Candidate] -> EcosystemBench -> IO Benchmark
ecosystemGroup digests ecosystem = do
    entries <- traverse (captureGroup digests ecosystem largest) (ebCorpus ecosystem)
    pure (bgroup ("ecosystem: " <> toString (ecosystemName (ebEcosystem ecosystem))) [bgroup "source digest candidates (32 KiB chunks)" entries])
  where
    largest = foldl' max 0 [BS.length raw | (_, raw, _, _) <- ebCorpus ecosystem]

-- | A capture's digest rows, and the walk rows when it is the ecosystem's largest capture.
captureGroup :: [Candidate] -> EcosystemBench -> Int -> LoadedEntry -> IO Benchmark
captureGroup digests ecosystem largest entry@(package, raw, _, _) = do
    for_ digests $ \candidate -> do
        digest <- digested (candidateDigesting candidate) drain chunks
        BS.length digest @?= candidateBytes candidate
        when (candidateKeepsDigest candidate) (digest @?= digestBytes (digestOf raw))
    pure (bgroup (entryName entry) (bgroup "digest only" (rows drain) : walks))
  where
    chunks = sourceChunks raw
    rows consume = [bench (candidateLabel candidate) (whnfAppIO (digested (candidateDigesting candidate) consume) chunks) | candidate <- digests]
    walks = case fullWalk ecosystem (cpPackage package) (BS.length raw) of
        Just walk | BS.length raw == largest -> [bgroup "full walk" (bench "no digest" (whnfAppIO (digested undigested walk) chunks) : rows walk)]
        _ -> []

-- | Run a consumer over the chunks under a digest, and give the digest of a read that gave a value.
digested :: Digesting -> Consumer -> [ByteString] -> IO ByteString
digested digesting consume chunks = do
    next <- heldChunks chunks
    digesting consume next >>= \case
        Right (True, digest) -> pure digest
        _ -> assertFailure "digest bench: the read failed"

-- | The pieces an inflated response body arrives in.
sourceChunks :: ByteString -> [ByteString]
sourceChunks bytes
    | BS.null bytes = []
    | otherwise = let (piece, rest) = BS.splitAt 32768 bytes in piece : sourceChunks rest

-- | Read the source to its end and keep nothing, so a row times the digest alone.
drain :: Consumer
drain next = next >>= \chunk -> if BS.null chunk then pure (Right True) else drain next

-- | The production full-read walk over a chunk source, under a body limit that admits the capture.
fullWalk :: EcosystemBench -> PackageName -> Int -> Maybe Consumer
fullWalk ecosystem name size = case ebEcosystem ecosystem of
    Npm -> Just (fmap gaveValue . readNpmFull held name (upstreamOrigin (ebUpstream ecosystem)))
    PyPI -> Just (fmap gaveValue . readPyPIIndex held name FullRead)
    RubyGems -> Nothing
  where
    held = defaultLimits{maxMetadataBytes = max (maxMetadataBytes defaultLimits) size}
    gaveValue :: Either LimitError (StreamResult a) -> Either LimitError Bool
    gaveValue = fmap (isRight . streamValue)

undigested :: Digesting
undigested consume next = fmap (,BS.empty) <$> consume next

-- | 'digestingRead' over any hash crypton holds, starting from the given context.
cryptonDigesting :: (HashAlgorithm algorithm) => Context algorithm -> Digesting
cryptonDigesting initial consume readChunk = do
    context <- newIORef initial
    let next = do
            chunk <- readChunk
            modifyIORef' context (`hashUpdate` chunk)
            pure chunk
    consume next >>= traverse (\result -> (result,) . BA.convert . hashFinalize <$> readIORef context)

-- | 'digestingRead' over an OpenSSL digest context, which the binding updates in place.
opensslDigesting :: Digest -> Digesting
opensslDigesting algorithm consume readChunk = do
    context <- digestStrictly algorithm BS.empty
    let next = do
            chunk <- readChunk
            digestUpdateBS context chunk
            pure chunk
    consume next >>= traverse (\result -> (result,) <$> digestFinalBS context)

-- | Name the processor and its instruction set extensions, which decide the path a library takes.
reportProcessor :: IO ()
reportProcessor = do
    cpuinfo <- tryIO (readFileBS "/proc/cpuinfo")
    for_ cpuinfo $ \bytes ->
        traverse_ (TIO.hPutStrLn stderr . ("digest bench: " <>)) (ordNub (filter described (lines (decodeUtf8 bytes))))
  where
    described line = any (`T.isPrefixOf` line) ["model name", "flags", "Features", "CPU implementer", "CPU part"]
