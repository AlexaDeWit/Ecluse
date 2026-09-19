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

        -- The refusal names which of the three things is wrong, so a harness author reads the
        -- fault off the message rather than rereading the literal.
        for_ refusals $ \(label, raw, detail) ->
            it ("rejects " <> label) $
                mkPinnedImageRef raw `shouldBe` Left (raw <> detail)

-- | Each way a raw image literal can fail to name an immutable image, and what it is told.
refusals :: [(String, Text, Text)]
refusals =
    [ ("a bare mutable tag", "verdaccio/verdaccio:5", noDigest)
    , ("a bare repository name with no digest at all", "nginx", noDigest)
    , ("a well-formed digest with no repository name before it", "@sha256:" <> hex, noName)
    , ("a short digest", "nginx@sha256:" <> T.take 40 hex, malformed)
    , ("an over-long digest", "nginx@sha256:" <> hex <> "ab", malformed)
    , ("an upper-cased digest", "nginx@sha256:" <> T.toUpper hex, malformed)
    ]
  where
    noDigest = " is not pinned to an @sha256: digest; a mutable tag must never reach a pull site"
    noName = " has an empty repository name before the @sha256: digest"
    malformed = " has a malformed sha256 digest: expected exactly 64 lowercase hex characters"

-- A real 64-character lowercase sha256 digest (Verdaccio's), reused across the cases.
hex :: Text
hex = "9d622d256378c6e7ae09f384774ee2f0f8ac67a66c066db55921a0b7218abc4c"

-- A well-formed pinned reference built from that digest.
pinned :: Text
pinned = "verdaccio/verdaccio@sha256:" <> hex
