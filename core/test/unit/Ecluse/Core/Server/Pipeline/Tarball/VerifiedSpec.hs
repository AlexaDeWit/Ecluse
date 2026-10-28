-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE ImportQualifiedPost #-}

module Ecluse.Core.Server.Pipeline.Tarball.VerifiedSpec (spec) where

import Data.ByteString qualified as BS
import Data.List (lookup)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (status200, status304, status401)
import Test.Hspec

import Ecluse.Core.Package (Artifact (..), HashAlg (SHA256), mkHash)
import Ecluse.Core.Package.Entry (EntryKey (SingletonEntry))
import Ecluse.Core.Security (BodyLimit (VerifiedArtifactBodyLimit), LimitError (BodyTooLarge))
import Ecluse.Core.Server.MemoryModel (verifiedArtifactCharge)
import Ecluse.Core.Server.Pipeline.Tarball.Verified
import Ecluse.Core.Server.Pipeline.Tarball.Verified.Internal (VerifiedArtifactBody (VerifiedArtifactBody))
import Ecluse.Test.Registry.JsonStream (heldChunks)
import Ecluse.Test.Support (expectRight, parseRequestOrFail)

spec :: Spec
spec = describe "complete verified artifact bodies" $ do
    it "verifies the genuine native archive across ordinary and one-byte chunks" $ do
        (artifact, bytes) <- fixture
        for_ [[bytes], map BS.singleton (BS.unpack bytes)] $ \chunks -> do
            next <- heldChunks chunks
            paid <- newIORef []
            VerifiedArtifactBody checked _ <- expectRight =<< readVerifiedArtifactWith (const pass) (BS.length bytes) (\amount -> modifyIORef' paid (<> [amount])) artifact next
            checked `shouldBe` bytes
            charges <- readIORef paid
            charges `shouldSatisfy` (not . null)
            all (== verifiedArtifactCharge (BS.length bytes)) charges `shouldBe` True
    it "reserves the full unknown-size destination and compacts under the same charge" $ do
        (artifact, bytes) <- fixture
        paid <- newIORef []
        next <- heldChunks [bytes]
        let cap = 2 * BS.length bytes
        VerifiedArtifactBody checked _ <- expectRight =<< readVerifiedArtifactWith (const pass) cap (\amount -> modifyIORef' paid (<> [amount])) artifact{artSize = Nothing} next
        checked `shouldBe` bytes
        readIORef paid `shouldReturn` replicate 3 (verifiedArtifactCharge cap)
    it "refuses a known cap before reading and rejects one byte past an unknown cap" $ do
        (artifact, bytes) <- fixture
        calls <- newIORef (0 :: Int)
        known <- readVerifiedArtifactWith (const pass) (BS.length bytes - 1) (const pass) artifact (modifyIORef' calls (+ 1) $> bytes)
        isLeft known `shouldBe` True
        readIORef calls `shouldReturn` 0
        next <- heldChunks [bytes, "x"]
        unknown <- readVerifiedArtifactWith (const pass) (BS.length bytes) (const pass) artifact{artSize = Nothing} next
        case unknown of
            Left (VerifiedArtifactBound bound) -> bound `shouldBe` BodyTooLarge (VerifiedArtifactBodyLimit (BS.length bytes))
            _ -> expectationFailure "an over-cap body did not return its typed bound"
    it "rejects an early EOF and changed native bytes" $ do
        (artifact, bytes) <- fixture
        for_ [BS.take (BS.length bytes - 1) bytes, "x" <> BS.drop 1 bytes] $ \changed -> do
            next <- heldChunks [changed]
            result <- readVerifiedArtifactWith (const pass) (BS.length bytes) (const pass) artifact next
            isLeft result `shouldBe` True
    it "observes completed mismatch hashing separately from hash entry and verified success" $ do
        (artifact, bytes) <- fixture
        phases <- newIORef []
        next <- heldChunks ["x" <> BS.drop 1 bytes]
        result <- readVerifiedArtifactWith (\phase -> modifyIORef' phases (<> [phase])) (BS.length bytes) (const pass) artifact next
        isLeft result `shouldBe` True
        readIORef phases `shouldReturn` [VerifyingArtifact, ArtifactHashCompleted]
    it "charges fixed zero-cap work and refuses invalid digest evidence before a read" $ do
        (artifact, _) <- fixture
        paid <- newIORef []
        next <- heldChunks []
        result <- readVerifiedArtifactWith (const pass) 0 (\amount -> modifyIORef' paid (<> [amount])) artifact{artSize = Nothing} next
        isLeft result `shouldBe` True
        charges <- readIORef paid
        all (> 0) charges `shouldBe` True
        calls <- newIORef (0 :: Int)
        missing <- readVerifiedArtifactWith (const pass) 0 (const pass) artifact{artHashes = [], artSize = Nothing} (modifyIORef' calls (+ 1) $> "")
        isLeft missing `shouldBe` True
        readIORef calls `shouldReturn` 0
    it "strips conditional and range bypasses while retaining identity bytes" $ do
        request <- parseRequestOrFail "https://source.test/archive.gem"
        let shaped = unconditionalArtifactRequest request{HTTP.requestHeaders = [("If-None-Match", "tag"), ("If-Modified-Since", "date"), ("Range", "bytes=1-2"), ("Accept-Encoding", "gzip")]}
        HTTP.method shaped `shouldBe` "GET"
        map (\name -> lookup name (HTTP.requestHeaders shaped)) ["If-None-Match", "If-Modified-Since", "Range"] `shouldBe` replicate 3 Nothing
        lookup "Accept-Encoding" (HTTP.requestHeaders shaped) `shouldBe` Just "identity"
        HTTP.decompress shaped "application/octet-stream" `shouldBe` False
    it "evaluates a local HEAD validator after source availability without hashing a body" $ do
        (artifact, bytes) <- fixture
        let (status, headers) = verifiedHeadResponse (BS.length bytes) artifact [] status200 [("ETag", "arbitrary-source-tag"), ("Content-Length", show (BS.length bytes))]
        status `shouldBe` status200
        tag <- maybe (expectationFailure "local validator absent" >> fail "local validator absent") pure (lookup "ETag" headers)
        tag `shouldNotBe` "arbitrary-source-tag"
        fst (verifiedHeadResponse (BS.length bytes) artifact [("If-None-Match", tag)] status200 headers) `shouldBe` status304
        fst (verifiedHeadResponse (BS.length bytes) artifact [("If-None-Match", tag)] status401 []) `shouldBe` status401
  where
    fixture = do
        bytes <- BS.readFile "test/fixtures/rubygems/gems/ecluse-fixture-root-1.0.0.gem"
        digest <- expectRight (mkHash SHA256 "646c0b16ba8b286beb737cc2e18ceb32b9cd42f7c7903ca3adc9b2896f489885")
        pure (Artifact SingletonEntry "ecluse-fixture-root-1.0.0.gem" "https://public.test/gems/ecluse-fixture-root-1.0.0.gem" [digest] Nothing (Just (BS.length bytes)), bytes)
