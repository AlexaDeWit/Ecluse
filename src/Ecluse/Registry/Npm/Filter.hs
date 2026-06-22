{- | The two pure transforms a __single public-upstream__ npm packument needs
before Écluse serves it: rewrite the embedded artifact URLs under the mount's
prefix, and apply the rules engine's verdicts across every version.

Both transforms operate __structurally over the raw @aeson@ 'Value'__, never by
re-serialising a typed model. This is load-bearing: the served packument is an
__open__ document — its schema is @additionalProperties: true@ (see
@docs\/architecture\/api-surface.md@ → "The synthesized-packument schema") — so
any field Écluse does not model (author keys, registry bookkeeping, per-version
extras) must be __relayed unchanged__. Editing the @Value@ in place removes denied
versions and rewrites @dist.tarball@ while leaving every unmodelled key untouched;
rebuilding the body from "Ecluse.Package" would silently drop them. The typed
model is consulted only to /decide/ which versions survive; the bytes that ship
are the edited upstream ones.

== URL rewriting

'rewriteTarballUrls' rewrites each version's @dist.tarball@ to
@{mount-base}\/{pkg}\/-\/{file}@, so a client resolving metadata /through/ the
proxy also downloads the bytes through it rather than going straight to upstream
and bypassing the gate (see @docs\/architecture\/hosting.md@ → "The load-bearing
requirement: URL rewriting"). Keeping artifacts same-host also keeps npm's auth
flowing, which a separate artifact host would silently drop. The mount's
externally-visible base URL is __supplied by the caller__ (config, S03); this
transform performs no IO.

== Filtering

'filterPackument' applies the rules engine across the @versions@ map: a version
that is not approved is removed from both @versions@ and @time@, so a client's
resolver only ever sees admitted versions (presence in the packument /is/
availability — see @docs\/research\/reverse-engineering\/npm.md@ §8). It then
resolves @dist-tags.latest@ with the shared __keep-unless-denied,
stable-preferring__ rule ('Ecluse.Version.selectLatest'): the upstream @latest@
is kept untouched as long as it survives, and only repointed — to the highest
/stable/ survivor — when it was itself denied. Any other tag that pointed at a
removed version is __dropped__, never repointed. The result is coherent:
@dist-tags.latest@ is always a key of @versions@, and @time@ has an entry for
exactly the surviving versions.

When __no version survives__, filtering returns 'NoSurvivors' carrying each
version's denial 'Decision'; the serve layer maps that to a status (S11\/S14),
which this slice deliberately does not choose.

This filters a __single public packument__ (the gated set). Combining it with the
trusted /private/ set is the cross-upstream merge, a separate slice; the @latest@
resolved here is over the survivors /within the public set/, not the final
@latest@ over the merged union.
-}
module Ecluse.Registry.Npm.Filter (
    -- * URL rewriting
    rewriteTarballUrls,

    -- * Filtering
    filterPackument,
    FilterResult (..),
) where

import Data.Aeson (Value (Object, String))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T

import Ecluse.Package (PackageInfo (infoDistTags, infoVersions), pkgVersion)
import Ecluse.Rules (evalRules)
import Ecluse.Rules.Types (Decision (Approved), EvalContext, PrecededRule)
import Ecluse.Version (Version, selectLatest, unVersion)

-- ── URL rewriting ─────────────────────────────────────────────────────────────

{- | Rewrite every version's @dist.tarball@ to @{base}\/{pkg}\/-\/{file}@, so the
artifact is fetched back through this mount rather than directly from upstream.

@base@ is the mount's externally-visible base URL (including any path prefix),
supplied by the caller; a trailing slash on it is ignored. @{pkg}@ is the
packument's own @name@ (the scoped @\@scope\/name@ form npm uses in URLs), read
from the document so the transform is self-contained. @{file}@ is the upstream
tarball URL's last path segment — the artifact filename — preserved verbatim so
the bytes a client integrity-checks are unchanged.

Total and lossless: a version with no @dist@ object, no @tarball@ string, or a
@tarball@ with no filename segment is left untouched, as is a document with no
usable @name@; every unmodelled key is relayed unchanged. Rewriting is
__idempotent__ — a second pass derives the same @{pkg}@ and @{file}@ and so
produces the same URL.
-}
rewriteTarballUrls :: Text -> Value -> Value
rewriteTarballUrls base = \case
    Object o
        | Just pkg <- stringField "name" o ->
            Object (adjustObject "versions" (mapValues (rewriteVersion (joinUrl base pkg))) o)
    other -> other

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

-- ── filtering ─────────────────────────────────────────────────────────────────

{- | The outcome of filtering a packument against a rule set.

A 'Filtered' body still has at least one admitted version and is internally
coherent. 'NoSurvivors' means every version was rejected; it carries each
version's 'Decision' so the serve layer can render the denial and choose the
status (403 for an all-policy denial; 503 once the undecidable path lands).
Choosing that status is __not__ this slice's job (S11\/S14).
-}
data FilterResult
    = -- | At least one version survived; the coherent, filtered packument body.
      Filtered Value
    | {- | No version survived; each rejected version's decision, for the serve
      layer to map to a status and a denial body.
      -}
      NoSurvivors [Decision]
    deriving stock (Eq, Show)

{- | Apply the rules engine across a packument's versions, removing every
non-approved version and repairing cross-field coherence.

The decisions are taken from the projected 'PackageInfo' (the typed view of the
/same/ document), but the edits land on the raw 'Value', so unmodelled fields
survive (see the module header). A version is kept iff 'evalRules' 'Approved' it;
a denied — or, once S21 lands, undecidable — version is dropped. (The undecidable
path is currently __stubbed to the deny path__: a version that is not approved is
simply removed, exactly like a denial, and no transient status is fabricated.)

When survivors remain the body is returned 'Filtered' with:

* @versions@ and @time@ restricted to the surviving version keys;
* @dist-tags.latest@ resolved by 'Ecluse.Version.selectLatest' — kept as the
  upstream maintainer published it while it survives, and only repointed (to the
  highest /stable/ survivor) when that chosen @latest@ was itself denied. A
  surviving @latest@ is never /promoted/ to a higher survivor; repointing a
  denied @latest@ downward is the deliberate downgrade (a not-yet-cleared release
  does not silently remain the default install while older admitted versions
  exist);
* every other @dist-tags@ entry whose target did not survive __dropped__.

When nothing survives, 'NoSurvivors' carries the per-version decisions.
-}
filterPackument :: EvalContext -> [PrecededRule] -> PackageInfo -> Value -> FilterResult
filterPackument ctx rules info = \case
    Object o ->
        let decisions = Map.map (evalRules ctx rules) (infoVersions info)
            survivors = Map.keysSet (Map.filter isApproved decisions)
         in if null survivors
                then NoSurvivors (Map.elems decisions)
                else Filtered (Object (repairTags survivors (restrict survivors o)))
    -- A non-object body is not a packument we can filter; with no versions to
    -- evaluate it has no survivors and no decisions to report.
    _ -> NoSurvivors []
  where
    -- A version key survives only on an explicit approval; every other outcome
    -- (deny, deny-by-default, and the stubbed undecidable path) drops it.
    isApproved :: Decision -> Bool
    isApproved = \case
        Approved{} -> True
        _ -> False

    -- The parsed 'Version' a raw key projects to, if that key is present in the
    -- packument (used both to map surviving keys to 'Version's and to resolve the
    -- upstream @latest@ tag's target).
    versionOf :: Text -> Maybe Version
    versionOf raw = pkgVersion <$> Map.lookup raw (infoVersions info)

    -- Restrict @versions@ to the surviving keys, and drop the denied versions
    -- from @time@. @time@ is pruned by /removal/, not /retention/, because it
    -- also carries non-version bookkeeping keys (@created@, @modified@) that
    -- Écluse does not model and must relay (PR #23); keeping only the survivor
    -- keys would drop them. (@versions@ has only version keys, so retention and
    -- removal coincide there.)
    restrict :: Set Text -> KeyMap Value -> KeyMap Value
    restrict survivors =
        adjustObject "versions" (keepKeys survivors)
            . adjustObject "time" (dropKeys deniedVersions)
      where
        deniedVersions = Set.difference (Map.keysSet (infoVersions info)) survivors

    -- Resolve @dist-tags.latest@ and drop any other tag pointing at a removed
    -- version. A @dist-tags@ that is absent /or/ present-but-malformed — most
    -- commonly JSON @null@, which the projection reads as "absent" yet the raw
    -- body still carries — is treated as empty, so the coherence promise (a
    -- resolvable @latest@) holds even for that malformed-upstream edge (see
    -- npm.md §8); a well-formed object is rebuilt in place, preserving its
    -- unmodelled tags.
    repairTags :: Set Text -> KeyMap Value -> KeyMap Value
    repairTags survivors o =
        let resolved = unVersion <$> selectLatest chosen (survivingVersions survivors)
            existing = case KeyMap.lookup "dist-tags" o of
                Just tags@(Object _) -> tags
                _ -> Object mempty
         in KeyMap.insert "dist-tags" (rebuildTags survivors resolved existing) o

    -- The upstream @latest@ tag's target as a 'Version': the @latest@ tag's raw
    -- string (from the projected dist-tags) looked up in @versions@, so a tag
    -- aimed at a version absent from the packument contributes nothing. This is
    -- 'selectLatest'\'s @chosen@; it decides /survival/ itself, so this only
    -- needs the version to be present, not surviving.
    chosen :: Maybe Version
    chosen = Map.lookup "latest" (infoDistTags info) >>= versionOf . unVersion

    -- The surviving versions' parsed 'Version's — 'selectLatest'\'s @survivors@.
    survivingVersions :: Set Text -> [Version]
    survivingVersions = mapMaybe versionOf . toList

{- | Rebuild a @dist-tags@ object: point @latest@ at @resolved@ (the raw version
string 'selectLatest' resolved — the kept upstream @latest@, or its downward
repoint) and keep every other tag only if its target version still survives. A
tag dropped here is one that pointed at a removed version — repointing @beta@ at a
stable release would misrepresent it.
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

-- ── structural @Value@ helpers ────────────────────────────────────────────────

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
entry — surviving versions and unmodelled bookkeeping keys (@created@,
@modified@) alike — untouched.
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
