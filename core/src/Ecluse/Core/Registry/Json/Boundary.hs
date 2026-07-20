-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The served-document boundary a __JSON__ registry adapter crosses: the two
capabilities the neutral pipeline injects
('Ecluse.Core.Registry.Adapter.Types.metadataAssemble' and
'Ecluse.Core.Registry.Adapter.Types.metadataSerialise'), built once from an ecosystem's
inject\/project pair.

The neutral pipeline threads the served document as an opaque
'Ecluse.Core.Registry.CachedDocument.CachedDoc' and never reads it; an ecosystem
projects it to its own representation at its capabilities and injects the result back.
For every JSON ecosystem that projection is the /same/ shape, parameterised by nothing
but the boundary pair, so it is built here rather than re-authored per adapter.

This module is deliberately separate from
"Ecluse.Core.Registry.Json.Assembly": that skeleton is strictly 'Value'-side and does
not import 'CachedDoc' at all, which is what keeps the opaque carrier from leaking into
the assembly logic. The 'CachedDoc' knowledge is confined here, at the boundary itself.
-}
module Ecluse.Core.Registry.Json.Boundary (
    projectedDocument,
    assembleThroughBoundary,
    serialiseThroughBoundary,
) where

import Data.Aeson (Value (Object), encode)
import Data.Map.Strict qualified as Map

import Ecluse.Core.Package.Merge (MergePlan, SourceId)
import Ecluse.Core.Registry.CachedDocument (CachedDoc)

{- | Project a served document to the ecosystem's 'Value'.

__The empty-object fallback is a shared policy, not a protocol fact.__ A 'CachedDoc'
projects to 'Nothing' only when it was injected by a /different/ ecosystem, which cannot
happen on a mount: an ecosystem is the sole injector of every document it later
projects, so the fallback is unreachable by construction. It is defined once, here, so
every JSON adapter takes the same disposition for that unreachable case: treat it as a
__benign miss__ (an empty object contributes no keys and no versions) rather than
fabricating content or failing the request. Sharing it also means the choice is stated
in one place instead of being re-decided, differently, per adapter.
-}
projectedDocument :: (CachedDoc -> Maybe Value) -> CachedDoc -> Value
projectedDocument project = fromMaybe (Object mempty) . project

{- | Build an ecosystem's __assemble__ capability from its boundary pair and its own
'Value'-based assembly: project every per-source document and the precedence-winning
base document, run the assembly, and inject the result back.

A 'Nothing' base (no source won precedence) projects to the empty object, which the
assembly reads as a document contributing no top-level keys and no bookkeeping.
-}
assembleThroughBoundary ::
    -- | The ecosystem's boundary pair: inject, and project.
    (Value -> CachedDoc, CachedDoc -> Maybe Value) ->
    -- | The ecosystem's 'Value'-based assembly.
    (Text -> Map SourceId Value -> MergePlan -> Value -> Value) ->
    Text ->
    Map SourceId CachedDoc ->
    MergePlan ->
    Maybe CachedDoc ->
    CachedDoc
assembleThroughBoundary (inject, project) assemble mountBase bySource plan base =
    inject (assemble mountBase (Map.map value bySource) plan (maybe (Object mempty) value base))
  where
    value = projectedDocument project

{- | Build an ecosystem's __serialise__ capability from its projection: project the
assembled document and encode it compactly to the wire bytes.
-}
serialiseThroughBoundary :: (CachedDoc -> Maybe Value) -> CachedDoc -> LByteString
serialiseThroughBoundary project = encode . projectedDocument project
