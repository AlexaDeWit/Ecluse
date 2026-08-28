-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Test.Container.ImageSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Test.Container.Image (mkPinnedImageRef, renderPinnedImageRef)

{- | The validator behind 'PinnedImageRef' and the only way to build one, so a pull site
never receives a mutable tag. A harness resolves its raw image literals here at startup, and
an unpinned literal aborts the suite before it pulls anything.
-}
spec :: Spec
spec =
    describe "mkPinnedImageRef" $ do
        it "accepts a name@sha256:<64 lowercase hex> reference and round-trips it" $
            (renderPinnedImageRef <$> mkPinnedImageRef pinned) `shouldBe` Right pinned

        it "rejects a bare mutable tag" $
            mkPinnedImageRef "verdaccio/verdaccio:5" `shouldSatisfy` isLeft

        it "rejects a bare repository name with no digest at all" $
            mkPinnedImageRef "nginx" `shouldSatisfy` isLeft

        it "rejects a well-formed digest with no repository name before it" $
            mkPinnedImageRef ("@sha256:" <> hex) `shouldSatisfy` isLeft

        it "rejects a short digest" $
            mkPinnedImageRef ("nginx@sha256:" <> T.take 40 hex) `shouldSatisfy` isLeft

        it "rejects an over-long digest" $
            mkPinnedImageRef ("nginx@sha256:" <> hex <> "ab") `shouldSatisfy` isLeft

        it "rejects an upper-cased digest" $
            mkPinnedImageRef ("nginx@sha256:" <> T.toUpper hex) `shouldSatisfy` isLeft

-- A real 64-character lowercase sha256 digest (Verdaccio's), reused across the cases.
hex :: Text
hex = "9d622d256378c6e7ae09f384774ee2f0f8ac67a66c066db55921a0b7218abc4c"

-- A well-formed pinned reference built from that digest.
pinned :: Text
pinned = "verdaccio/verdaccio@sha256:" <> hex
