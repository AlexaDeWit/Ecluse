-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The plan-replay and artifact-URL rebase skeleton a __JSON__ registry adapter
instantiates to assemble the document it serves.

The @Json@ in the name is a real constraint, not decoration: every function here works
over an @aeson@ 'Value', so only an ecosystem whose served document /is/ JSON (npm now)
can instantiate it. An ecosystem served as another representation cannot, and would
instantiate its own replay over its own type. It sits under @Registry@ rather than in
"Ecluse.Core.Json.Selective"'s namespace because, unlike those pure-aeson engines, it
reasons over registry concepts (a 'Ecluse.Core.Package.Merge.MergePlan'\'s survivors,
the mount's safety gate).

The served document is an __open__ document: any key Écluse does not model must be
relayed unchanged, so a JSON adapter assembles the served body structurally over the
raw 'Value's rather than re-serialising a lossy typed model. Everything that recurs
across JSON ecosystems lives here; an ecosystem supplies only its own protocol facts,
never the security discipline.

* 'replaySurvivors' folds a plan's surviving versions over the raw per-source
  documents, taking each survivor's object from the source that won its key.
* 'plannedKeysOver' overlays the plan-rebuilt top-level keys onto the base document,
  fixing the precedence so the plan always wins.
* 'safeMountPrefix' is the __safety gate__: an upstream-controlled self-reported name
  becomes a served URL prefix only after every structural component passes
  'Ecluse.Core.Server.Path.isSafeComponent'.
* 'rebaseArtifactUrl' is the __rebase discipline__: derive the artifact filename from
  the upstream URL, hand it to the ecosystem's path convention, and leave a URL with
  no derivable filename untouched.
* 'rebaseHook' ties the gate and the rebase together into the per-version hook
  'replaySurvivors' takes, defaulting to no rewrite when the gate refuses.

An ecosystem therefore supplies only __where its artifact URL lives__ (a traversal into
its version object), __its own path convention__ (filename to served path), and __its
name grammar__ (how a name decomposes into structural components). It re-derives none
of the gate, the filename extraction, the leave-unchanged rule, the idempotence, or the
plan-key precedence.

The skeleton is strictly 'Value'-side: it does not import
"Ecluse.Core.Registry.CachedDocument" at all, so the neutral pipeline's opaque carrier
cannot leak into it. Crossing that boundary is
"Ecluse.Core.Registry.Json.Boundary"'s job.
-}
module Ecluse.Core.Registry.Json.Assembly (
    -- * Replaying a merge plan
    replaySurvivors,
    plannedKeysOver,

    -- * Rebasing artifact URLs under the mount
    safeMountPrefix,
    rebaseArtifactUrl,
    rebaseHook,
) where

import Data.Aeson (Value (Object))
import Data.Aeson.Key (Key)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map

import Ecluse.Core.Package.Merge (SourceId)
import Ecluse.Core.Server.Path (isSafeComponent)
import Ecluse.Core.Text (joinUrlPath, lastPathSegment)

{- | Replay a plan's surviving versions over the raw per-source documents. For each
@(version, source)@ survivor, take the version's object from that source's
version-keyed map (@versionsBySource@) and apply @rewrite@ as it is placed. A
survivor whose winning source carries no object for it is dropped rather than
fabricated, so the result stays coherent with the plan by construction.

The rebuilt versions map is @O(survivors)@ inner lookups: each source's version map
is extracted once by the caller, so a survivor costs one map lookup plus one keymap
lookup.
-}
replaySurvivors ::
    -- | The per-version rebase hook, applied to each survivor as placed.
    (Value -> Value) ->
    -- | Each source's version-keyed object map, extracted once per source.
    Map SourceId (KeyMap Value) ->
    -- | The plan's survivors: which source won each surviving version key.
    Map Text SourceId ->
    KeyMap Value
replaySurvivors rewrite versionsBySource survivors =
    KeyMap.fromList
        [ (key, rewrite object)
        | (version, sid) <- Map.toList survivors
        , let key = Key.fromText version
        , Just object <- [Map.lookup sid versionsBySource >>= KeyMap.lookup key]
        ]

{- | Assemble the served object by overlaying the keys the merge plan rebuilt onto the
base document's own keys. Every key the plan does not name is relayed from the base
unchanged, so the open document keeps the fields Écluse does not model.

__The planned keys always win, and that precedence is fixed here rather than at the
call site.__ The overlay is a left-biased 'KeyMap.union', so an adapter assembling the
two operands in the other order would serve the /upstream's/ own version map over the
plan's: a version the plan denied would reach the client, silently, with no type error
and no failing round-trip. Taking the planned keys as a distinct argument makes that
order unrepresentable, which is the same reason the name gate lives in
'safeMountPrefix' rather than in each adapter.
-}
plannedKeysOver ::
    -- | The top-level keys the merge plan rebuilt. These win.
    [(Key, Value)] ->
    -- | The base document's own keys, relayed wherever the plan does not name them.
    KeyMap Value ->
    Value
plannedKeysOver planned base = Object (KeyMap.fromList planned `KeyMap.union` base)

{- | The validated URL prefix a rebase interpolates a document's self-reported name
into: @{mountBase}\/{name}@, or 'Nothing' when the name is unusable.

__This is a security gate, not a formatting step.__ A document's name is
upstream-controlled and flows into a URL Écluse serves, so every structural component
of it must pass 'Ecluse.Core.Server.Path.isSafeComponent' (no empty, @.@, @..@, path
separator, or control character) before interpolation; otherwise a hostile name is a
path injection into the served surface. A refusal yields 'Nothing' and the caller
rewrites nothing.

The ecosystem supplies only its __name grammar__ (@components@), because only it knows
whether a separator is structural: npm's @\@scope\/base@ legitimately carries one
@\'\/\'@, so it decomposes to two components, while a slash anywhere else is caught.
The validate-every-component-or-refuse rule is shared, so no ecosystem re-derives it.
-}
safeMountPrefix ::
    -- | The ecosystem's name grammar: a name's structural components.
    (Text -> [Text]) ->
    -- | The mount's externally-visible base URL.
    Text ->
    -- | The document's self-reported name.
    Text ->
    Maybe Text
safeMountPrefix components mountBase name
    | all isSafeComponent (components name) = Just (joinUrlPath mountBase name)
    | otherwise = Nothing

{- | Rebase one upstream artifact URL under the mount: derive the artifact filename
from the URL's last path segment and hand it to the ecosystem's path convention.

Total and lossless: a URL from which no filename can be derived is returned
__unchanged__, so a rebase never fabricates a location. The filename is carried over
verbatim, so the bytes a client integrity-checks are untouched by the rewrite.

__Idempotent__ whenever @toServedPath@ ends in the filename it is given (as a
@{prefix}\/-\/{file}@ or @{prefix}\/files\/{file}@ convention does): re-deriving the
last segment of an already-rebased URL yields the same filename and so the same URL,
which is what makes applying the rebase more than once safe.
-}
rebaseArtifactUrl ::
    -- | The ecosystem's path convention: artifact filename to served path.
    (Text -> Text) ->
    -- | The upstream artifact URL.
    Text ->
    Text
rebaseArtifactUrl toServedPath url = maybe url toServedPath (lastPathSegment url)

{- | The per-version rebase hook for one document, as 'replaySurvivors' takes it: gate
the document's self-reported name into a validated mount prefix ('safeMountPrefix') and
build the ecosystem's per-version rewrite from it.

A document with no readable name, or one whose name refuses the gate, yields 'id': no
version's URL is rewritten. Defaulting to __no rewrite__ rather than to an ungated
prefix is the fail-closed half of the gate, kept here so no ecosystem re-derives it.
-}
rebaseHook ::
    -- | The ecosystem's name grammar, for the gate.
    (Text -> [Text]) ->
    -- | The ecosystem's per-version rewrite, given a validated prefix.
    (Text -> Value -> Value) ->
    -- | The mount's externally-visible base URL.
    Text ->
    -- | The document's self-reported name, when it has a readable one.
    Maybe Text ->
    Value ->
    Value
rebaseHook components rewrite mountBase name =
    maybe id rewrite (name >>= safeMountPrefix components mountBase)
