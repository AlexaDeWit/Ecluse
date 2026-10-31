-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Digest-pinned image references for the container test tiers, plus the pins themselves.

An attacker can re-point a mutable tag at a poisoned image between two pulls, so every
image the harness pulls or builds @FROM@ names an immutable @\@sha256:@ digest. The CI
pre-pull steps in @.github/workflows/ci.yml@ repeat each pin, so a bump edits both files.
-}
module Ecluse.Test.Container.Image (
    -- * A pinned reference
    PinnedImageRef,
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

-- | An image reference fixed to an immutable digest. The constructor is hidden, so a pull site receives only what 'mkPinnedImageRef' accepted.
newtype PinnedImageRef = PinnedImageRef Text
    deriving stock (Eq, Show)

-- Validate a raw reference as @\<name\>\@sha256:\<64 lowercase hex\>@, refusing a bare tag, an empty name, or a malformed digest.
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

-- | An image a harness names at a @docker run@ or @docker build FROM@ site. A pulled image must be pinned, and one the run builds has no digest.
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

-- | The OTLP Collector 0.162.0. The specs parse the text its @debug@ exporter prints, and the e2e run waits for its readiness log line.
collectorImage :: Either Text PinnedImageRef
collectorImage = mkPinnedImageRef "otel/opentelemetry-collector@sha256:310a800ad69ee430e7c541796852a242c9c7db97aaad4daa5ccf843c525fbdb2"

-- | @ministack@ 1.5.24-full: the SQS and S3 emulator both container tiers run against.
ministackImage :: Either Text PinnedImageRef
ministackImage = mkPinnedImageRef "ministackorg/ministack@sha256:8862a8488307284427c831f8ac3940c8d19e8fe4d023d8057b668e3eb4eb4ee1"

-- | nginx 1.31.2-alpine: the e2e stub that answers over TLS for every registry name the product dials.
nginxImage :: Either Text PinnedImageRef
nginxImage = mkPinnedImageRef "nginx@sha256:54f2a904c251d5a34adf545a72d32515a15e08418dae0266e23be2e18c66fefa"

-- | Verdaccio 5.33.0: the e2e run's private upstream, mirror target, and Dredger private cache.
verdaccioImage :: Either Text PinnedImageRef
verdaccioImage = mkPinnedImageRef "verdaccio/verdaccio@sha256:9d622d256378c6e7ae09f384774ee2f0f8ac67a66c066db55921a0b7218abc4c"
