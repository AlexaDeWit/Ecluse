-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Digest-pinned image references for the container test tiers, plus the pins themselves.

An attacker can re-point a mutable tag at a poisoned image between two pulls, so every
image the harness pulls or builds @FROM@ names an immutable @\@sha256:@ digest.
'mkPinnedImageRef' is the only way to obtain a 'PinnedImageRef' and a pull site accepts
nothing else, so an unpinned reference cannot reach one. 'ImageRef' separates an image the
run pulls, which must be pinned, from one the run builds, which has no digest to pin.
-}
module Ecluse.Test.Container.Image (
    -- * A pinned reference
    PinnedImageRef,
    mkPinnedImageRef,
    renderPinnedImageRef,

    -- * Either pinned-external or locally-built
    ImageRef (PinnedExternal, LocallyBuilt),
    renderImageRef,

    -- * The pins the container tiers pull
    collectorImage,
    ministackImage,
    nginxImage,
    verdaccioImage,
) where

import Data.Char (isDigit)
import Data.Text qualified as T

{- | A container image reference nailed to an immutable digest: @\<name\>\@sha256:\<64 lowercase
hex\>@. The constructor is hidden and 'mkPinnedImageRef' rejects a bare tag, so no unpinned
reference can reach a pull site.
-}
newtype PinnedImageRef = PinnedImageRef Text
    deriving stock (Eq, Show)

{- | Validate a raw reference as @\<name\>\@sha256:\<64 lowercase hex\>@. Rejects a bare tag, an
empty repository name, and a digest that is not exactly 64 lowercase hexadecimal characters.
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

{- | An image a harness names at a @docker run@ or @docker build FROM@ site. A locally built image
carries no digest because the run produces it itself and never pulls it.
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

{- | The OTLP Collector, version 0.119.0, by its multi-arch index digest. The specs assert on
this image's exact @debug@ exporter output and readiness line, so the reference cannot move.
-}
collectorImage :: Either Text PinnedImageRef
collectorImage = mkPinnedImageRef "otel/opentelemetry-collector@sha256:3805724e26351df55a45032a793c9b64a2117ac9a58f13f070674a9723fab373"

-- | @ministack@, tag @1.3-full@: the SQS and S3 emulator both container tiers run against.
ministackImage :: Either Text PinnedImageRef
ministackImage = mkPinnedImageRef "ministackorg/ministack@sha256:5164592def36af01b8ac76364028e27c5ecd8f1494c8a53d5fcd811cc7dfb594"

-- | The nginx the e2e run terminates TLS with, in front of both registry stubs.
nginxImage :: Either Text PinnedImageRef
nginxImage = mkPinnedImageRef "nginx@sha256:54f2a904c251d5a34adf545a72d32515a15e08418dae0266e23be2e18c66fefa"

-- | The Verdaccio the e2e run uses as its private upstream and mirror target.
verdaccioImage :: Either Text PinnedImageRef
verdaccioImage = mkPinnedImageRef "verdaccio/verdaccio@sha256:9d622d256378c6e7ae09f384774ee2f0f8ac67a66c066db55921a0b7218abc4c"
