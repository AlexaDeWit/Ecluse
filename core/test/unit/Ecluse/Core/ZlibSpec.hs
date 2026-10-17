-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Core.ZlibSpec (spec) where

import Data.Streaming.Zlib.Lowlevel qualified as Z
import Foreign.Marshal.Alloc (allocaBytes)
import Test.Hspec
import UnliftIO.Exception (bracket)

import Ecluse.Core.Zlib (ZlibFault (ZlibInitialisationFailed), advanceStream, initialiseDeflate, openStream, releaseDeflate)

spec :: Spec
spec = do
    describe "openStream" $
        it "throws the initialiser's status when zlib refuses its parameters" $
            -- Level 42 is outside zlib's range, so deflateInit2 answers Z_STREAM_ERROR.
            openStream (\native -> initialiseDeflate native 42 31 8 0) `shouldThrow` (== ZlibInitialisationFailed (-2))
    describe "advanceStream" $
        it "reports the input consumed and the output written within the room it was given" $
            bracket (openStream (\native -> initialiseDeflate native 1 31 8 0)) releaseDeflate $ \native ->
                allocaBytes 16 $ \destination -> do
                    (status, consumed, written) <- advanceStream Z.c_call_deflate_finish native "metadata body" destination 16
                    status `shouldBe` 0
                    consumed `shouldBe` 13
                    written `shouldBe` 16
