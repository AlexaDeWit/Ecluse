-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Capture authentication rejects changed bytes before harness measurements.
module Ecluse.Test.CorpusSpec (spec) where

import System.FilePath (makeRelative, (</>))
import Test.Hspec
import UnliftIO.Temporary (withTempDirectory)

import Ecluse.Test.Corpus (CorpusPackage (..), CorpusTier (Medium), FilePin (..), readCapture, readPinned)
import Ecluse.Test.Package (unscopedNpm)

-- | Check pins against an independent SHA-256 vector and refuse missing capture identities.
spec :: Spec
spec = do
    describe "readPinned" $ do
        it "returns bytes that match the committed size and SHA-256" $
            withPinnedSample $
                \pin -> readPinned pin `shouldReturn` "abc"
        it "refuses a different size" $
            withPinnedSample $
                \pin -> readPinned pin{fpBytes = 4} `shouldThrow` anyIOException
        it "refuses an equal-length body with a different digest" $
            withPinnedSample $ \pin -> do
                writeFileBS ("bench/corpus" </> fpPath pin) "abd"
                readPinned pin `shouldThrow` anyIOException
    describe "readCapture" $ do
        it "refuses a package without a pin" $
            readCapture (CorpusPackage (unscopedNpm "unrecorded-capture") "unused.json" Medium 1) `shouldThrow` anyIOException
        it "refuses a capture path that differs from its pin" $
            readCapture (CorpusPackage (unscopedNpm "lodash") "unused.json" Medium 1) `shouldThrow` anyIOException

withPinnedSample :: (FilePin -> IO a) -> IO a
withPinnedSample action = withTempDirectory "bench/corpus" "pin-reader" $ \directory -> do
    let path = directory </> "sample"
        pin = FilePin (makeRelative "bench/corpus" path) 3 "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    writeFileBS path "abc"
    action pin
