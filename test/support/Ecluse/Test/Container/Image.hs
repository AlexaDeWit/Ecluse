-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Digest-pinned container image references for the integration and end-to-end
suites.

Écluse is a supply-chain policy proxy. Every image its test harness pulls, runs, or
builds @FROM@ must therefore name an immutable @\@sha256:@ digest, never a mutable tag.
An attacker can re-point a tag at a poisoned image between one pull and the next. A
content-addressed digest verifies the bytes on every pull. This module makes an unpinned
reference /unrepresentable/ at a pull site, rather than scanning the harness for stray
tags after the fact. The only way to obtain a 'PinnedImageRef' is the validating
'mkPinnedImageRef', which rejects a bare tag. A pull site accepts only a
'PinnedImageRef', so no unpinned reference can reach it. A harness resolves its raw
literals through 'mkPinnedImageRef' at startup and fails loudly on a 'Left'. An unpinned
literal therefore aborts the suite before it pulls anything.

'ImageRef' then distinguishes the two kinds of image a harness names. A 'PinnedExternal'
image comes from a registry and /must/ be pinned. The run produces a 'LocallyBuilt' image
itself: built each run, never pulled, so never pinned. The sum type turns that
distinction into a type-checked fact, instead of a special case at the @docker run@ site.
-}
module Ecluse.Test.Container.Image (
    -- * A pinned reference
    PinnedImageRef,
    mkPinnedImageRef,
    renderPinnedImageRef,

    -- * Either pinned-external or locally-built
    ImageRef (PinnedExternal, LocallyBuilt),
    renderImageRef,
) where

import Data.Char (isDigit)
import Data.Text qualified as T

{- | A container image reference nailed to an immutable digest: @\<name\>\@sha256:\<64
lowercase hex\>@. The constructor is hidden, so the only way to build one is
'mkPinnedImageRef', which rejects a bare tag. A value of this type therefore already
carries the "is pinned" invariant, so no unpinned reference can reach a pull site.
-}
newtype PinnedImageRef = PinnedImageRef Text
    deriving stock (Eq, Show)

{- | Validate a raw reference as @\<name\>\@sha256:\<64 lowercase hex\>@, returning the
'PinnedImageRef' or a reason. Rejects a bare tag (no digest at all) and an empty
repository name. It also rejects a digest that is not exactly 64 lowercase hexadecimal
characters: short, long, or upper-cased.
-}
mkPinnedImageRef :: Text -> Either Text PinnedImageRef
mkPinnedImageRef raw =
    case T.breakOn digestMarker raw of
        (name, marked)
            | T.null marked ->
                Left (raw <> " is not pinned to an @sha256: digest; a mutable tag must never reach a pull site")
            | T.null name ->
                Left (raw <> " has an empty repository name before the @sha256: digest")
            | not (isDigest (T.drop (T.length digestMarker) marked)) ->
                Left (raw <> " has a malformed sha256 digest: expected exactly 64 lowercase hex characters")
            | otherwise -> Right (PinnedImageRef raw)
  where
    digestMarker = "@sha256:"

-- | Exactly 64 lowercase hexadecimal characters: the shape of a @sha256@ digest.
isDigest :: Text -> Bool
isDigest digest = T.length digest == 64 && T.all isLowerHex digest

-- | A lowercase hexadecimal character (@0-9@ or @a-f@).
isLowerHex :: Char -> Bool
isLowerHex c = isDigit c || (c >= 'a' && c <= 'f')

-- | The canonical wire form of a pinned reference: the @\<name\>\@sha256:...@ string.
renderPinnedImageRef :: PinnedImageRef -> Text
renderPinnedImageRef (PinnedImageRef ref) = ref

{- | An image a harness names at a @docker run@ or @docker build FROM@ site. A
'PinnedExternal' image comes from a registry and must carry a digest. The run produces a
'LocallyBuilt' image itself, each run and never pulled, so it carries no digest and takes
its plain tag as its name.
-}
data ImageRef
    = -- | An external image pulled from a registry, digest-pinned by construction.
      PinnedExternal PinnedImageRef
    | -- | An image built by the run itself, named by its plain local tag.
      LocallyBuilt Text
    deriving stock (Eq, Show)

-- | The reference string to hand @docker@, whichever kind of image it names.
renderImageRef :: ImageRef -> Text
renderImageRef = \case
    PinnedExternal ref -> renderPinnedImageRef ref
    LocallyBuilt name -> name
