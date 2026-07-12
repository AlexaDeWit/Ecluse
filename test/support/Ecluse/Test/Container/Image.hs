-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Digest-pinned container image references for the integration and end-to-end
suites.

Écluse is a supply-chain-security tool, so every image its test harness pulls, runs,
or builds @FROM@ must be nailed to an immutable @\@sha256:@ digest and never a mutable
tag: a tag can be re-pointed at a poisoned image between one pull and the next, while a
content-addressed digest verifies the bytes on every pull. Rather than scan the harness
for stray tags after the fact, this module makes an unpinned reference /unrepresentable/
at a pull site: the only way to obtain a 'PinnedImageRef' is through the validating
'mkPinnedImageRef', which rejects a bare tag, so a pull site (which accepts only a
'PinnedImageRef') can never be handed one. A harness resolves its raw literals through
'mkPinnedImageRef' at startup and fails loudly on a 'Left', so an unpinned literal aborts
the suite before it pulls anything.

'ImageRef' then distinguishes the two kinds of image a harness names: a 'PinnedExternal'
image pulled from a registry (which /must/ be pinned) and a 'LocallyBuilt' image
produced by the run itself (built each run, never pulled, so never pinned). Making that a
sum type turns the distinction into a type-checked fact instead of a special case at the
@docker run@ site.
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
carries the "is pinned" invariant, and no pull site can be handed an unpinned reference.
-}
newtype PinnedImageRef = PinnedImageRef Text
    deriving stock (Eq, Show)

{- | Validate a raw reference as @\<name\>\@sha256:\<64 lowercase hex\>@, returning the
'PinnedImageRef' or a reason. Rejects a bare tag (no digest at all), an empty repository
name, and a digest that is not exactly 64 lowercase hexadecimal characters (a short,
long, or upper-cased digest).
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

{- | An image a harness names at a @docker run@ or @docker build FROM@ site: either a
'PinnedExternal' image pulled from a registry (which must carry a digest) or a
'LocallyBuilt' image the run produced itself (built each run, never pulled, so it carries
no digest and is named by its plain tag).
-}
data ImageRef
    = -- | An external image pulled from a registry; digest-pinned by construction.
      PinnedExternal PinnedImageRef
    | -- | An image built by the run itself, named by its plain local tag.
      LocallyBuilt Text
    deriving stock (Eq, Show)

-- | The reference string to hand @docker@, whichever kind of image it names.
renderImageRef :: ImageRef -> Text
renderImageRef = \case
    PinnedExternal ref -> renderPinnedImageRef ref
    LocallyBuilt name -> name
