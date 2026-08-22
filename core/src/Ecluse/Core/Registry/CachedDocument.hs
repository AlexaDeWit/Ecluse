-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The cached and served raw document as an __opaque carrier with a private constructor__.

The serve path caches an upstream packument's raw document and rebuilds the served body
from it. The neutral pipeline and the metadata cache must /hold/ that document without
/reading/ it. Every structural operation over it is an injected adapter capability: the
fetch that produces it, the assembly that merges it, the serialisation that encodes it.
The compiler enforces that opacity, so the neutral code does not have to keep it by
discipline: the constructor of 'CachedDoc' is __not exported__. A module outside this one
can cache, thread, and hand back a 'CachedDoc', but it cannot inspect what the document
carries.

An ecosystem works in its own representation and crosses this boundary through an
inject/project pair ('npmCached' for npm, whose representation is a JSON
'Data.Aeson.Value'). It injects on the way into the cache and pipeline, and projects on
the way out, at its own capabilities. The cache weighs a held document through
'weighCachedDoc' without projecting it.
-}
module Ecluse.Core.Registry.CachedDocument (
    CachedDoc,
    weighCachedDoc,
    npmCached,
) where

import Data.Aeson (Value, encode)
import Data.ByteString.Lazy qualified as BSL

{- | A raw document the metadata cache holds and the neutral serve pipeline threads. The
representation: an npm packument as a JSON 'Value'. Exported __abstract__, with a private
constructor, so only this module's boundary pairs cross it. The neutral core cannot read
the document a 'CachedDoc' carries. The derived 'Show' and 'Eq' are a debug and test
affordance (a failure message, a round-trip assertion), not a projection. They give the
neutral core no way to recover the underlying document to assemble or serialise it.
-}
newtype CachedDoc = CachedNpm Value
    deriving stock (Eq, Show)

{- | A held document's resident-size estimate: the byte length of its compact encoding,
the figure the metadata cache weighs an entry by. Computed here, where the arm is known,
so the cache need not project the document back to an ecosystem representation to size
it.
-}
weighCachedDoc :: CachedDoc -> Int64
weighCachedDoc (CachedNpm v) = BSL.length (encode v)

{- | npm's boundary pair: inject a packument 'Value' into a 'CachedDoc', and project one
back to the 'Value' npm assembles and serialises over. The npm adapter injects every
document it later projects, so a document it injected always projects back ('Just'). The
'Maybe' is the projection's type, matched at npm's own capabilities.
-}
npmCached :: (Value -> CachedDoc, CachedDoc -> Maybe Value)
npmCached = (CachedNpm, \case CachedNpm v -> Just v)
