-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The drop-tracking vocabulary a registry projection records a malformed entry in.

'mkInvalidEntry' is the only builder, because the record reaches an operator log line and an
upstream-supplied value can carry a credential. It reduces every URL-shaped string to its
authority ('Ecluse.Core.Security.Authority.authorityLabel'), so no userinfo and no signed
query string survives into the entry. Hiding the constructor is what makes that
unrepresentable to bypass.

Each ecosystem's projection contributes its own 'InvalidEntryKind' arms. The neutral serve
path never branches on them: it buckets through 'dropCountsByKind', whose labels come from
'renderInvalidEntryKind' beside the arms, so a new ecosystem's kinds reach the operator log
without a case anywhere else.
-}
module Ecluse.Core.Package.InvalidEntry (
    -- * A dropped entry
    InvalidEntry (invalidKind, invalidKey, invalidValue, invalidReason),
    mkInvalidEntry,

    -- * Which kind of entry dropped
    InvalidEntryKind (..),
    renderInvalidEntryKind,
    dropCountsByKind,
) where

import Data.Aeson (Value (Array, Object, String))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Vector qualified as V

import Ecluse.Core.Security.Authority (authorityLabel)

{- | A single registry-document entry a projection __dropped__ as malformed rather than
failing the entire document. It is kept so the drop is observable: an operator can see that
an upstream served a malformed entry, and which one.
-}
data InvalidEntry = InvalidEntry
    { invalidKind :: InvalidEntryKind
    -- ^ Which kind of document entry the projection dropped.
    , invalidKey :: Text
    {- ^ The key the dropped entry sat under: the raw version string for a version manifest
    or publish time, the tag name for a dist-tag, the file name for an index file.
    -}
    , invalidValue :: Value
    {- ^ The __offending value__, so an operator can see what the upstream sent rather than
    only a reason string, with every URL reduced to its authority by 'mkInvalidEntry'. Render
    it at log time, truncating if it is large.
    -}
    , invalidReason :: Text
    -- ^ Why the entry could not be projected (the decode error), for the operator log.
    }
    deriving stock (Eq, Show)

{- | Record a dropped entry, reducing every URL in the key and the value to its authority.
An upstream-supplied @dist.tarball@ or PEP 691 @url@ can carry a credential in its userinfo
or a signature in its query string, and this record reaches a log line.
-}
mkInvalidEntry :: InvalidEntryKind -> Text -> Value -> Text -> InvalidEntry
mkInvalidEntry kind key value reason =
    InvalidEntry
        { invalidKind = kind
        , invalidKey = redactUrlText key
        , invalidValue = redactUrls value
        , invalidReason = reason
        }

-- Reduce every URL-shaped string in a decoded value to its authority, walking containers.
redactUrls :: Value -> Value
redactUrls = \case
    String s -> String (redactUrlText s)
    Object o -> Object (KeyMap.map redactUrls o)
    Array xs -> Array (V.map redactUrls xs)
    scalar -> scalar

{- The scheme separator is what makes a string a URL that can carry userinfo, so it is the
one shape reduced. Anything else is recorded as the upstream wrote it. -}
redactUrlText :: Text -> Text
redactUrlText raw
    | "://" `T.isInfixOf` raw = authorityLabel raw
    | otherwise = raw

{- | Which kind of registry-document entry a dropped 'InvalidEntry' came from. A dropped
manifest or index file removes a serve candidate, fail-closed for that one version or file.
The advisory kinds lose only their own datum, and the version still resolves.

The arms are per-ecosystem by construction, so a new ecosystem adds its own beside these.
-}
data InvalidEntryKind
    = -- | A @versions@ entry whose manifest did not project (no @dist@\/@tarball@, an unusable @version@).
      InvalidVersionManifest
    | -- | A @dist-tags@ entry whose target was not a usable version string.
      InvalidDistTag
    | -- | A @time@ entry, keyed by a present version, that was not a decodable instant.
      InvalidPublishTime
    | -- | A Simple-index file entry that did not project (no name or location, an unusable digest).
      InvalidIndexFile
    | -- | A versions-listing entry that was not a usable version string.
      InvalidVersionListing
    deriving stock (Eq, Ord, Show)

{- | The operator-facing label for a drop kind. It is the bucket key an operator filters on, so
it is held stable as this text rather than the constructor name.
-}
renderInvalidEntryKind :: InvalidEntryKind -> Text
renderInvalidEntryKind = \case
    InvalidVersionManifest -> "version-manifest"
    InvalidDistTag -> "dist-tag"
    InvalidPublishTime -> "publish-time"
    InvalidIndexFile -> "index-file"
    InvalidVersionListing -> "version-listing"

{- | How many entries dropped under each kind's label. Only the kinds actually seen appear, so
a document's drop profile carries no bucket its ecosystem has no entries for.
-}
dropCountsByKind :: [InvalidEntry] -> Map Text Int
dropCountsByKind entries =
    Map.fromListWith (+) [(renderInvalidEntryKind (invalidKind e), 1) | e <- entries]
