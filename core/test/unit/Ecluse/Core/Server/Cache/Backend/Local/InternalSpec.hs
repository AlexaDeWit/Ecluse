-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

-- | Recency publication remains monotonic when callers publish in reverse allocation order.
module Ecluse.Core.Server.Cache.Backend.Local.InternalSpec (spec) where

import Test.Hspec
import UnliftIO (timeout, wait, withAsync)

import Ecluse.Core.Server.Cache.Backend.Local.Internal (publishAccessStamp)

spec :: Spec
spec = describe "publishAccessStamp" $
    it "keeps the newer access when an older caller publishes after it" $ do
        outcome <- timeout 1000000 $ do
            clock <- newIORef (0 :: Word64)
            published <- newIORef (0 :: Word64)
            allocated <- newEmptyMVar
            release <- newEmptyMVar
            let allocate = atomicModifyIORef' clock (\held -> (held + 1, held + 1))
            withAsync (do stamp <- allocate; putMVar allocated (); takeMVar release; publishAccessStamp published stamp) $ \older -> do
                takeMVar allocated
                newer <- allocate
                publishAccessStamp published newer
                putMVar release ()
                wait older
                readIORef published `shouldReturn` newer
        outcome `shouldBe` Just ()
