-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The two pure transforms an npm packument needs before Écluse serves it:
rewrite the embedded artifact URLs under the mount's prefix, and assemble the
served document from a cross-upstream 'MergePlan' and the raw source documents.

Both transforms operate __structurally over the raw @aeson@ 'Value'__, never by
re-serialising a typed model. This is load-bearing: the served packument is an
__open__ document -- its schema is @additionalProperties: true@ (see
@docs\/architecture\/api-surface.md@ → "The synthesized-packument schema") -- so
any field Écluse does not model (author keys, registry bookkeeping, per-version
extras) must be __relayed unchanged__. Building the served body from the raw
@Value@s keeps every unmodelled key; rebuilding it from "Ecluse.Core.Package"
would silently drop them.

== The decision\/replay split

/Which/ versions survive, which source wins each one, where @dist-tags.latest@
resolves, and each surviving version's publish instant are the ecosystem-agnostic
decisions, taken over the typed 'Ecluse.Core.Package.PackageInfo' by
"Ecluse.Core.Package.Filter" and "Ecluse.Core.Package.Merge" and handed here as a
'MergePlan'. This module owns the __npm wire-shape assembly__: rebuilding
@versions@\/@dist-tags@\/@time@ onto the base document from the plan, and the
tarball-URL rewrite over the raw upstream bytes. The npm wire knowledge lives
here; the decision logic does not (it is reused by every ecosystem). See
@docs\/architecture\/registry-model.md@ → "Decision surface vs served surface".

== URL rewriting

'rewriteVersion' rewrites one version object's @dist.tarball@ to
@{mount-base}\/{pkg}\/-\/{file}@, so a client resolving metadata /through/ the
proxy also downloads the bytes through it rather than going straight to upstream
and bypassing the gate (see @docs\/architecture\/web-layer.md@ → "Multi-ecosystem
mounts", whose URL rewriting is load-bearing). Keeping artifacts same-host also keeps npm's auth
flowing, which a separate artifact host would silently drop. The
@{mount-base}\/{pkg}@ prefix is __supplied by the caller__;
'assembleMergedPackument' derives it from the mount base and the document's own
safety-gated @name@ as it places each surviving version. The transform performs
no IO. It is __idempotent__: re-deriving @{file}@ from an already-rewritten URL
yields the same URL, so applying it more than once is safe.

== Assembling the served document

'assembleMergedPackument' replays a 'MergePlan' onto the raw source @Value@s in
__one pass__: each surviving version's object is taken from the raw document of the
source that won it (so the served bytes are the winning upstream's, unmodelled keys
and all) with its @dist.tarball@ rewritten under the mount base as it is placed;
@dist-tags@ and @time@ are rebuilt from the plan's reconciled decisions (the times
as normalised ISO-8601, with the base document's @created@\/@modified@ bookkeeping
retained); every other top-level key is relayed from the base document. A version
not in the plan's survivors is simply never taken, so a client's resolver only ever
sees admitted versions (presence in the packument /is/ availability -- see
@docs\/research\/reverse-engineering\/npm.md@ §8).

The fused single pass is deliberate: restricting, assembling, and rewriting as
separate whole-document edits would rebuild a many-version packument several times
per request, and this transform sits on the serve path's hot loop (see
@docs\/architecture\/performance.md@). The rewrite gates the interpolated name: the
base document's own @name@ is validated component-wise by the shared gate
('Ecluse.Core.Registry.Assembly.safeMountPrefix', over npm's 'npmNameComponents')
before it is interpolated, and a document with no usable name has no URLs rewritten.
-}
module Ecluse.Core.Registry.Npm.Filter (
    -- * URL rewriting
    rewriteVersion,

    -- * Assembling the served document
    assembleMergedPackument,

    -- * The served-document boundary (npm's 'CachedDoc' capabilities)
    assembleMergedDocument,
    serialiseMergedDocument,
) where

import Data.Aeson (Value (Object, String), encode)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime)

import Lens.Micro ((%~), (^?))
import Lens.Micro.Aeson (key, _String)

import Ecluse.Core.Package.Merge (MergePlan (mpDistTags, mpSurvivors, mpTime), SourceId)
import Ecluse.Core.Registry.Assembly (rebaseArtifactUrl, rebaseHook, replaySurvivors)
import Ecluse.Core.Registry.CachedDocument (CachedDoc, npmCached)
import Ecluse.Core.Text (renderIso8601Utc)
import Ecluse.Core.Version (renderVersion)

{- | npm's __name grammar__, the one protocol fact the shared safety gate
('Ecluse.Core.Registry.Assembly.safeMountPrefix') needs from npm: an @\@scope\/base@
decomposes into its scope and base name, any other name is a single component.
Splitting on the scope separator first means a legitimate @\@scope\/name@'s own
@\'\/\'@ is not itself judged unsafe, while a slash anywhere else (a traversal, a
path injection) leaves a component the gate rejects.
-}
npmNameComponents :: Text -> [Text]
npmNameComponents name = case T.stripPrefix "@" name of
    Just scopeAndBase ->
        let (scope, base) = T.breakOn "/" scopeAndBase
         in if T.null base then [name] else [scope, T.drop 1 base]
    Nothing -> [name]

{- | Rewrite one version object's @dist.tarball@ to @{prefix}\/-\/{file}@, so the
artifact is fetched back through this mount rather than directly from upstream.

@prefix@ is the mount's @{base}\/{pkg}@ -- the externally-visible base URL joined
with the package's URL form -- supplied by the caller. @{file}@ is the existing
tarball URL's last path segment (the artifact filename), preserved verbatim so
the bytes a client integrity-checks are unchanged.

Total and lossless: a version with no @dist@ object, no @tarball@ string, or a
@tarball@ with no filename segment is left untouched; every unmodelled key is
relayed unchanged. Rewriting is __idempotent__ -- a second pass derives the same
@{file}@ and so produces the same URL.

A @{pkg}@ read from a document's own @name@ is __upstream-controlled__, so it
must be gated component-wise through "Ecluse.Core.Server.Route.isSafeComponent"
before it reaches the prefix: 'assembleMergedPackument' performs that gate as it
places each surviving version, and a caller building its own prefix owns it.
-}
rewriteVersion :: Text -> Value -> Value
rewriteVersion prefix v = v & tarballUrl %~ rebaseArtifactUrl (npmArtifactPath prefix)
  where
    -- npm's locator: where a version object keeps its artifact URL. A version with
    -- no @dist@ object, no @tarball@, or a non-string @tarball@ has no target here,
    -- so the traversal leaves it untouched.
    tarballUrl = key "dist" . key "tarball" . _String

-- | npm's artifact path convention: the mount prefix, the @\/-\/@ infix, the filename.
npmArtifactPath :: Text -> Text -> Text
npmArtifactPath prefix file = prefix <> "/-/" <> file

{- | Assemble the served packument from a 'MergePlan' and the raw source documents:
rebuild @versions@, @dist-tags@, and @time@ from the plan onto the base document,
rewriting each surviving version's @dist.tarball@ under @mountBase@ in the same
pass. Other top-level keys are inherited from the base document.

The plan was decided over the projected 'Ecluse.Core.Package.PackageInfo's (the
typed views of the /same/ documents), but the assembly reads the raw @Value@s, so
unmodelled fields survive (see the module header). Each surviving version's object
is taken from the source that won its key ('mpSurvivors'); a survivor whose source
object is missing is dropped rather than fabricated, so coherence with the plan is
preserved by construction. @dist-tags@ is the plan's reconciled map ('mpDistTags':
@latest@ resolved, absent-target tags dropped); @time@ is the plan's
surviving-version instants ('mpTime', rendered as normalised ISO-8601) plus the
base document's non-version @created@\/@modified@ bookkeeping.

The tarball rewrite applies 'rewriteVersion' to each surviving version as it is
placed, so the versions object is built once rather than rebuilt by a second
whole-document pass; the interpolated prefix is gated on the base document's own
@name@ through the shared 'Ecluse.Core.Registry.Assembly.rebaseHook', with no rewrite
when the name is unusable or refuses the gate.

The caller decides what to do with an empty plan; an empty 'mpSurvivors' simply
assembles an empty @versions@ object. A non-object base document contributes no
top-level keys and no bookkeeping (the plan-owned keys are still assembled), so the
result is always an object.
-}
assembleMergedPackument :: Text -> Map SourceId Value -> MergePlan -> Value -> Value
assembleMergedPackument mountBase bySource plan base =
    -- The plan-owned keys win; every other key is relayed from the base document.
    -- 'KeyMap.union' is left-biased, so naming the rebuilt keys first is what makes
    -- them override the base's own @versions@\/@dist-tags@\/@time@.
    Object
        ( KeyMap.fromList
            [ ("versions", Object survivingVersions)
            , ("dist-tags", Object distTags)
            , ("time", Object reconciledTime)
            ]
            `KeyMap.union` baseObject
        )
  where
    baseObject :: KeyMap Value
    baseObject = case base of
        Object o -> o
        _ -> mempty

    -- The per-version tarball rewrite, resolved once for the whole assembly: the
    -- shared gate turns the base document's upstream-controlled self-reported @name@
    -- into a validated @{base}/{pkg}@ prefix, and refuses (no rewrite) when it cannot.
    rewriteSurvivor :: Value -> Value
    rewriteSurvivor =
        rebaseHook npmNameComponents rewriteVersion mountBase (base ^? key "name" . _String)

    -- Each surviving version's object, taken from the raw @Value@ of the source
    -- that won its key (so the served bytes are the winning upstream's, unmodelled
    -- keys and all), rewritten as placed. The fold and the drop-if-missing rule are
    -- the shared skeleton's ('replaySurvivors').
    survivingVersions :: KeyMap Value
    survivingVersions = replaySurvivors rewriteSurvivor versionsBySource (mpSurvivors plan)

    -- Each source's raw @versions@ object, extracted once per source, so each
    -- survivor the skeleton places costs a single inner keymap lookup rather than
    -- re-extracting the object. ('bySource' holds one entry per upstream.)
    versionsBySource :: Map SourceId (KeyMap Value)
    versionsBySource = Map.mapMaybe versionsObjectOf bySource

    -- @dist-tags@ rebuilt from the plan's reconciled tags (each a rendered version
    -- string). The plan has already resolved @latest@ and dropped absent-target
    -- tags over the union.
    distTags :: KeyMap Value
    distTags =
        KeyMap.fromList
            [ (Key.fromText tag, String (renderVersion v))
            | (tag, v) <- Map.toList (mpDistTags plan)
            ]

    -- @time@ rebuilt from the plan's surviving-version times, with the base
    -- document's non-version bookkeeping keys (@created@\/@modified@) retained.
    reconciledTime :: KeyMap Value
    reconciledTime =
        bookkeepingTime
            <> KeyMap.fromList
                [ (Key.fromText version, String (renderTime t))
                | (version, t) <- Map.toList (mpTime plan)
                ]

    -- The base @time@ map carries one entry per published version (up to the
    -- packument's version cap) plus the @created@\/@modified@ bookkeeping keys.
    -- Look those two keys up directly rather than filtering the whole map, so this
    -- is a pair of lookups, not a full traversal of every version's publish time.
    bookkeepingTime :: KeyMap Value
    bookkeepingTime = case KeyMap.lookup "time" baseObject of
        Just (Object timeObject) ->
            KeyMap.fromList
                [ (k, value)
                | name <- timeBookkeepingKeys
                , let k = Key.fromText name
                , Just value <- [KeyMap.lookup k timeObject]
                ]
        _ -> mempty

{- | npm's served-document __assemble__ capability
('Ecluse.Core.Registry.Adapter.Types.metadataAssemble'): project each per-source
'CachedDoc' and the precedence-winning base document into npm's 'Value', replay the plan
through 'assembleMergedPackument', and inject the assembled 'Value' back. The neutral
pipeline threads the documents opaquely; the projection\/injection is npm's boundary.
-}
assembleMergedDocument :: Text -> Map SourceId CachedDoc -> MergePlan -> Maybe CachedDoc -> CachedDoc
assembleMergedDocument mountBase bySource plan base =
    fst npmCached (assembleMergedPackument mountBase (Map.map npmValue bySource) plan (maybe (Object mempty) npmValue base))

{- | npm's served-document __serialise__ capability
('Ecluse.Core.Registry.Adapter.Types.metadataSerialise'): project the assembled
'CachedDoc' to npm's 'Value' and encode it compactly to the wire bytes.
-}
serialiseMergedDocument :: CachedDoc -> LByteString
serialiseMergedDocument = encode . npmValue

-- Project a served document back to npm's 'Value'. The single disposition for the
-- projection boundary: a document npm did not inject falls back to the empty object (a
-- benign miss that contributes no keys and no versions). npm is the only injector, so
-- this default is never taken in practice.
npmValue :: CachedDoc -> Value
npmValue = fromMaybe (Object mempty) . snd npmCached

-- A source document's raw @versions@ object, when the document carries one.
versionsObjectOf :: Value -> Maybe (KeyMap Value)
versionsObjectOf = \case
    Object o | Just (Object vs) <- KeyMap.lookup "versions" o -> Just vs
    _ -> Nothing

-- The non-version keys an npm @time@ object carries that must be relayed unchanged.
timeBookkeepingKeys :: [Text]
timeBookkeepingKeys = ["created", "modified"]

-- Render a publish time as the ISO-8601 instant npm serves in its @time@ map --
-- through the hot-path renderer (byte-for-byte 'iso8601Show' parity), since this
-- runs once per surviving version per request.
renderTime :: UTCTime -> Text
renderTime = renderIso8601Utc
