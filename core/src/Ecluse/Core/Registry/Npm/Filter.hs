{- | The two pure transforms a __single public-upstream__ npm packument needs
before Écluse serves it: rewrite the embedded artifact URLs under the mount's
prefix, and replay a 'FilterPlan'\'s verdicts across every version.

Both transforms operate __structurally over the raw @aeson@ 'Value'__, never by
re-serialising a typed model. This is load-bearing: the served packument is an
__open__ document -- its schema is @additionalProperties: true@ (see
@docs\/architecture\/api-surface.md@ → "The synthesized-packument schema") -- so
any field Écluse does not model (author keys, registry bookkeeping, per-version
extras) must be __relayed unchanged__. Editing the @Value@ in place removes denied
versions and rewrites @dist.tarball@ while leaving every unmodelled key untouched;
rebuilding the body from "Ecluse.Core.Package" would silently drop them.

== The decision\/replay split

/Which/ versions survive, where @dist-tags.latest@ resolves, and each version's
denial 'Decision' is the ecosystem-agnostic filtering decision, taken over the
typed 'Ecluse.Core.Package.PackageInfo' by "Ecluse.Core.Package.Filter" and handed here as a
'Ecluse.Core.Package.Filter.FilterPlan'. This module owns the __npm wire-shape
transforms__: the plan replay (restrict @versions@\/@time@ to the surviving keys and
rebuild @dist-tags@) and the tarball-URL rewrite over the raw upstream bytes. The npm
wire knowledge lives here; the decision logic does not (it is reused by every
ecosystem). See
@docs\/architecture\/registry-model.md@ → "Decision surface vs served surface".

== URL rewriting

'rewriteTarballUrls' rewrites each version's @dist.tarball@ to
@{mount-base}\/{pkg}\/-\/{file}@, so a client resolving metadata /through/ the
proxy also downloads the bytes through it rather than going straight to upstream
and bypassing the gate (see @docs\/architecture\/hosting.md@ → "The load-bearing
requirement: URL rewriting"). Keeping artifacts same-host also keeps npm's auth
flowing, which a separate artifact host would silently drop. The mount's
externally-visible base URL is __supplied by the caller__; this
transform performs no IO. It is __idempotent__: re-deriving @{pkg}@ and @{file}@ from
an already-rewritten URL yields the same URL, so applying it more than once is safe.

== Replaying the filter plan

'applyFilterPlan' replays a 'FilterPlan' onto the raw @Value@: a version not in the
plan's survivors is removed from both @versions@ and @time@, so a client's resolver
only ever sees admitted versions (presence in the packument /is/ availability -- see
@docs\/research\/reverse-engineering\/npm.md@ §8). @dist-tags.latest@ is repointed
at the plan's resolved @latest@, and any other tag whose target did not survive is
__dropped__, never repointed. The replay does __not__ rewrite tarball URLs -- that is
'rewriteTarballUrls', applied once to the assembled body. The replay's result is
coherent: @dist-tags.latest@ is always a key of @versions@, and @time@ has an entry
for exactly the surviving versions.

When the plan has __no survivors__, the replay returns 'NoSurvivors' carrying the
plan's per-version denial 'Decision's; the serve layer maps that to a status, which
this module deliberately does not choose. A body that is not even a JSON object is
not a packument we can replay onto -- it carries no versions to serve, so it yields
'NoSurvivors' with no decisions.
-}
module Ecluse.Core.Registry.Npm.Filter (
    -- * URL rewriting
    rewriteTarballUrls,

    -- * Filtering
    applyFilterPlan,
    FilterResult (..),
) where

import Data.Aeson (Value (Object, String))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Set qualified as Set
import Data.Text qualified as T

import Ecluse.Core.Package.Filter (FilterPlan (fpDecisions, fpLatest, fpSurvivors), FilterResult (..))

import Ecluse.Core.Server.Route (isSafeComponent)
import Ecluse.Core.Version (unVersion)

{- | Rewrite every version's @dist.tarball@ to @{base}\/{pkg}\/-\/{file}@, so the
artifact is fetched back through this mount rather than directly from upstream.

@base@ is the mount's externally-visible base URL (including any path prefix),
supplied by the caller; a trailing slash on it is ignored. @{pkg}@ is the
packument's own @name@ (the scoped @\@scope\/name@ form npm uses in URLs), read
from the document so the transform is self-contained. @{file}@ is the upstream
tarball URL's last path segment -- the artifact filename -- preserved verbatim so
the bytes a client integrity-checks are unchanged.

Total and lossless: a version with no @dist@ object, no @tarball@ string, or a
@tarball@ with no filename segment is left untouched, as is a document with no
usable @name@; every unmodelled key is relayed unchanged. Rewriting is
__idempotent__ -- a second pass derives the same @{pkg}@ and @{file}@ and so
produces the same URL.

The @name@ is __upstream-controlled__ (it is the packument's own field), so each
of its structural components -- the scope and base name either side of a @\@scope\/@
prefix -- is gated through "Ecluse.Core.Server.Route.isSafeComponent" before it is
interpolated. A name carrying a traversal, an embedded separator, or a control
character is rejected and the document is left untouched rather than emit a
@dist.tarball@ that aims a client outside the package's own path.
-}
rewriteTarballUrls :: Text -> Value -> Value
rewriteTarballUrls base = \case
    Object o
        | Just pkg <- stringField "name" o
        , safeName pkg ->
            Object (adjustObject "versions" (mapValues (rewriteVersion (joinUrl base pkg))) o)
    other -> other

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

{- | Rewrite one version object's @dist.tarball@ under the given @{base}\/{pkg}@
prefix, leaving the object untouched if it carries no rewritable tarball.
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

{- | The outcome of replaying a 'FilterPlan' onto a packument.

A 'Filtered' body still has at least one admitted version and is internally
coherent. 'NoSurvivors' means every version was rejected; it carries each
version's 'Decision' so the serve layer can render the denial and choose the
status (403 for an all-policy denial, 503 for a transient or undecidable cause).
Choosing that status is __not__ this module's job.
-}

{- | Replay a 'FilterPlan' onto the raw packument @Value@, removing every
non-surviving version and repairing cross-field coherence.

The plan was decided over the projected 'Ecluse.Core.Package.PackageInfo' (the typed
view of the /same/ document), but the edits land on the raw 'Value', so unmodelled
fields survive (see the module header). A version key is kept iff it is in the
plan's survivors.

When survivors remain the body is returned 'Filtered' with:

* @versions@ and @time@ restricted to the surviving version keys (@time@ is pruned
  by /removal/ of the denied keys, so its unmodelled @created@\/@modified@
  bookkeeping is relayed);
* @dist-tags.latest@ pointed at the plan's resolved @latest@ ('fpLatest') -- the
  kept upstream @latest@, or its downward repoint when the upstream @latest@ was
  denied;
* every other @dist-tags@ entry whose target did not survive __dropped__ (never
  repointed -- repointing @beta@ at a stable release would misrepresent it).

Surviving versions' @dist.tarball@ URLs are __not__ rewritten here -- they are
relayed as the upstream bytes. Rewriting them under the mount base is
'rewriteTarballUrls', applied once to the assembled body uniformly across every
contributing source, so the replay carries no base URL.

When the plan has no survivors, 'NoSurvivors' carries its per-version decisions. A
non-object body is not a packument we can replay onto; with no versions it has no
survivors and no decisions to report.
-}
applyFilterPlan :: FilterPlan -> Value -> FilterResult
applyFilterPlan plan = \case
    Object o
        | Set.null (fpSurvivors plan) -> NoSurvivors (fpDecisions plan)
        | otherwise -> Filtered (Object (repairTags plan (restrict plan o)))
    -- A non-object body is not a packument we can replay onto; with no versions to
    -- serve it has no survivors and no decisions to report.
    _ -> NoSurvivors []

{- | Restrict @versions@ to the surviving keys, and drop the denied versions from
@time@. @time@ is pruned by /removal/, not /retention/, because it also carries
non-version bookkeeping keys (@created@, @modified@) that Écluse does not model and
must relay; keeping only the survivor keys would drop them. The denied keys are the
raw @versions@ keys absent from the plan's survivors -- derived from the document so
the replay needs no extra plan field. (@versions@ has only version keys, so
retention and removal coincide there.)
-}
restrict :: FilterPlan -> KeyMap Value -> KeyMap Value
restrict plan o =
    adjustObject "versions" (keepKeys survivors)
        . adjustObject "time" (dropKeys deniedVersions)
        $ o
  where
    survivors = fpSurvivors plan
    deniedVersions = Set.difference (versionKeys o) survivors

{- | Resolve @dist-tags.latest@ to the plan's resolved @latest@ and drop any other
tag pointing at a removed version. A @dist-tags@ that is absent /or/
present-but-malformed -- most commonly JSON @null@, which the projection reads as
"absent" yet the raw body still carries -- is treated as empty, so the coherence
promise (a resolvable @latest@) holds even for that malformed-upstream edge (see
npm.md §8); a well-formed object is rebuilt in place, preserving its unmodelled
tags.
-}
repairTags :: FilterPlan -> KeyMap Value -> KeyMap Value
repairTags plan o =
    let resolved = unVersion <$> fpLatest plan
        existing = case KeyMap.lookup "dist-tags" o of
            Just tags@(Object _) -> tags
            _ -> Object mempty
     in KeyMap.insert "dist-tags" (rebuildTags (fpSurvivors plan) resolved existing) o

{- | The raw @versions@ object's keys, as a 'Set' of version strings; empty when
@versions@ is absent or not an object. These are exactly the projected
'Ecluse.Core.Package.infoVersions' keys, so subtracting the survivors yields the denied
version keys the @time@ prune removes.
-}
versionKeys :: KeyMap Value -> Set Text
versionKeys o = case KeyMap.lookup "versions" o of
    Just (Object vs) -> Set.fromList (map Key.toText (KeyMap.keys vs))
    _ -> mempty

{- | Rebuild a @dist-tags@ object: point @latest@ at @resolved@ (the raw version
string the plan resolved -- the kept upstream @latest@, or its downward repoint) and
keep every other tag only if its target version still survives. A tag dropped here
is one that pointed at a removed version -- repointing @beta@ at a stable release
would misrepresent it.
-}
rebuildTags :: Set Text -> Maybe Text -> Value -> Value
rebuildTags survivors resolved = \case
    Object tags ->
        Object
            ( KeyMap.filterWithKey keepTag tags
                & maybe id (KeyMap.insert "latest" . String) resolved
            )
    -- @dist-tags@ should be an object; an unexpected shape is left as-is rather
    -- than fabricated, so nothing unmodelled is dropped.
    other -> other
  where
    keepTag :: Key.Key -> Value -> Bool
    keepTag k v
        | k == "latest" = False -- always re-inserted, pointed at the survivor
        | otherwise = case v of
            String target -> target `Set.member` survivors
            _ -> True -- a non-string tag value is unmodelled; relay it

{- | Apply a function to the value at @key@ in an object, only when that key is
present. A missing key is left absent (no key is fabricated), preserving lossless
passthrough; the function itself decides what to do with a non-object value.
-}
adjustObject :: Key.Key -> (Value -> Value) -> KeyMap Value -> KeyMap Value
adjustObject key f o = case KeyMap.lookup key o of
    Just v -> KeyMap.insert key (f v) o
    Nothing -> o

-- | Map a function over every value of an 'Object', leaving a non-object as-is.
mapValues :: (Value -> Value) -> Value -> Value
mapValues f = \case
    Object o -> Object (fmap f o)
    other -> other

-- | Keep only the object entries whose key is in the surviving set.
keepKeys :: Set Text -> Value -> Value
keepKeys survivors = \case
    Object o -> Object (KeyMap.filterWithKey (\k _ -> Key.toText k `Set.member` survivors) o)
    other -> other

{- | Drop the object entries whose key is in the given set, leaving every other
entry -- surviving versions and unmodelled bookkeeping keys (@created@,
@modified@) alike -- untouched.
-}
dropKeys :: Set Text -> Value -> Value
dropKeys keys = \case
    Object o -> Object (KeyMap.filterWithKey (\k _ -> Key.toText k `Set.notMember` keys) o)
    other -> other

-- | The 'Text' at @key@ in an object, if present and a JSON string.
stringField :: Key.Key -> KeyMap Value -> Maybe Text
stringField key o = case KeyMap.lookup key o of
    Just (String s) -> Just s
    _ -> Nothing
