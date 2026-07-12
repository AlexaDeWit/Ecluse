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
@docs\/architecture\/performance.md@). The rewrite gates the interpolated name:
the base document's own @name@ is validated component-wise ('safeName') before it
is interpolated, and a document with no usable name has no URLs rewritten.
-}
module Ecluse.Core.Registry.Npm.Filter (
    -- * URL rewriting
    rewriteVersion,

    -- * Assembling the served document
    assembleMergedPackument,
) where

import Data.Aeson (Value (Object, String))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime)

import Ecluse.Core.Package.Merge (MergePlan (mpDistTags, mpSurvivors, mpTime), SourceId)
import Ecluse.Core.Server.Route (isSafeComponent)
import Ecluse.Core.Text (renderIso8601Utc)
import Ecluse.Core.Version (renderVersion)

{- | Whether an upstream-controlled packument @name@ is safe to interpolate into a
rewritten @dist.tarball@ path: every structural component (the scope and base name
either side of an @\@scope\/@ prefix, or the whole name when unscoped) must pass
"Ecluse.Core.Server.Route.isSafeComponent". Splitting on the scope separator first means
a legitimate @\@scope\/name@'s own @\'\/\'@ is not itself judged unsafe, while a
slash anywhere else (a traversal, a path injection) is caught.
-}
safeName :: Text -> Bool
safeName name = all isSafeComponent components
  where
    components = case T.stripPrefix "@" name of
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
rewriteVersion prefix = \case
    Object vo -> Object (adjustObject "dist" (rewriteDist prefix) vo)
    other -> other

{- | Rewrite a @dist@ object's @tarball@ to @{prefix}\/-\/{file}@, where @file@ is
the existing URL's last path segment. A @dist@ with no string @tarball@, or a
tarball with no filename segment, is left unchanged.
-}
rewriteDist :: Text -> Value -> Value
rewriteDist prefix = \case
    Object dist
        | Just url <- stringField "tarball" dist
        , Just file <- tarballFile url ->
            Object (KeyMap.insert "tarball" (String (prefix <> "/-/" <> file)) dist)
    other -> other

{- | The artifact filename of a tarball URL: the path segment after the last
@\'\/\'@. 'Nothing' when that segment is empty (a URL ending in a slash), so
the caller leaves such a URL untouched rather than forming a fileless path.
-}
tarballFile :: Text -> Maybe Text
tarballFile url =
    let afterLastSlash = snd (T.breakOnEnd "/" url)
     in if T.null afterLastSlash then Nothing else Just afterLastSlash

{- | Join a base URL and a path segment with a single @\'\/\'@, ignoring a
trailing slash already on the base.
-}
joinUrl :: Text -> Text -> Text
joinUrl base seg = T.dropWhileEnd (== '/') base <> "/" <> seg

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
@name@ (validated by 'safeName'), with no rewrite when the name is unusable.

The caller decides what to do with an empty plan; an empty 'mpSurvivors' simply
assembles an empty @versions@ object. A non-object base document contributes no
top-level keys and no bookkeeping (the plan-owned keys are still assembled), so the
result is always an object.
-}
assembleMergedPackument :: Text -> Map SourceId Value -> MergePlan -> Value -> Value
assembleMergedPackument mountBase bySource plan base =
    Object rebuilt
  where
    rebuilt :: KeyMap Value
    rebuilt =
        baseObject
            & KeyMap.insert "versions" (Object survivingVersions)
            & KeyMap.insert "dist-tags" (Object distTags)
            & KeyMap.insert "time" (Object reconciledTime)

    baseObject :: KeyMap Value
    baseObject = case base of
        Object o -> o
        _ -> mempty

    -- The per-version tarball rewrite, resolved once for the whole assembly:
    -- 'rewriteVersion' under the @{base}/{pkg}@ prefix, over the base document's
    -- safe-name-gated self-reported @name@. No usable or safe name -> no rewrite.
    rewriteSurvivor :: Value -> Value
    rewriteSurvivor = case stringField "name" baseObject of
        Just pkg | safeName pkg -> rewriteVersion (joinUrl mountBase pkg)
        _ -> id

    -- Each surviving version's object, taken from the raw @Value@ of the source
    -- that won the key (so the served bytes are the winning upstream's, unmodelled
    -- keys and all), rewritten as it is placed. A survivor whose source object is
    -- missing is dropped rather than fabricated.
    survivingVersions :: KeyMap Value
    survivingVersions =
        KeyMap.fromList
            [ (Key.fromText version, rewriteSurvivor object)
            | (version, sid) <- Map.toList (mpSurvivors plan)
            , Just object <- [versionObjectFrom sid version]
            ]

    -- Each source's raw @versions@ object, extracted once per source.
    -- 'versionObjectFrom' runs once per surviving version (up to the packument's
    -- version cap), so resolving the source's @versions@ object inside it would
    -- re-extract the same object on every version; hoisting it here leaves each
    -- survivor a single inner lookup. ('bySource' holds one entry per upstream.)
    versionsBySource :: Map SourceId (KeyMap Value)
    versionsBySource = Map.mapMaybe versionsObjectOf bySource

    versionObjectFrom :: SourceId -> Text -> Maybe Value
    versionObjectFrom sid version =
        Map.lookup sid versionsBySource >>= KeyMap.lookup (Key.fromText version)

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

{- | Apply a function to the value at @key@ in an object, only when that key is
present. A missing key is left absent (no key is fabricated), preserving lossless
passthrough; the function itself decides what to do with a non-object value.
-}
adjustObject :: Key.Key -> (Value -> Value) -> KeyMap Value -> KeyMap Value
adjustObject key f o = case KeyMap.lookup key o of
    Just v -> KeyMap.insert key (f v) o
    Nothing -> o

-- | The 'Text' at @key@ in an object, if present and a JSON string.
stringField :: Key.Key -> KeyMap Value -> Maybe Text
stringField key o = case KeyMap.lookup key o of
    Just (String s) -> Just s
    _ -> Nothing
