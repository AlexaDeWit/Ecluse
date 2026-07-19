-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | A __selective__ decode of an npm packument: pull __one version's__ pieces out of
the document bytes without materialising the other versions.

The whole-packument decode (@aeson@'s @eitherDecodeStrict@) builds a 'Value' for /every/
version -- and on a heavy packument (thousands of versions, multiple megabytes) that
decode dominates the serve-path cost. But the tarball gate consults a __single__
version: it needs that version's manifest object, its @time[version]@ publish stamp, and
the document's self-reported @name@ -- nothing of the other versions. This module walks
the registry's own JSON token stream (@aeson@'s @Data.Aeson.Decoding@, no new
dependency) and materialises a 'Value' only for those few pieces, __skipping every other
version's tokens without allocating them__. The win is on the /parse/, not the fetch:
the full bytes are still read (npm carries @time@ only in the full document), but they
are parsed selectively -- O(1 version) work and residency rather than O(N).

The generic bounded token-walk engine this decode drives lives in
"Ecluse.Core.Json.Selective"; this module adds npm's packument key selection on top.

== Faithful to the whole-document decode

The skip is not a shortcut past validation. The walk consumes the __entire__ token
stream, so:

  * malformed JSON __anywhere__ surfaces as 'SelectiveUndecodable' -- the lexer reaches
    the offending bytes whether or not they sit in the requested version (matching
    @eitherDecodeStrict@ failing the whole body);
  * trailing non-whitespace after the top-level object is rejected likewise (the same
    end-of-input check @eitherDecodeStrict@ applies);
  * every value is depth-bounded at the same budget
    'Ecluse.Core.Security.checkNestingDepth' would apply to it, so a deeply-nested
    sub-tree __anywhere__ is a 'SelectiveTooDeeplyNested' breach, not a serve.

The two pieces it /does/ build -- the requested version object and the document @name@ --
are produced by the same @aeson@ 'Value' decoder the whole-document path uses, so
projecting them yields a byte-for-byte identical 'Ecluse.Core.Package.PackageDetails'
(the projection is "Ecluse.Core.Registry.Npm.Project.projectVersionEntry", run over the
same 'Value').

== What it deliberately does not re-validate

The selective walk reaches only the requested version's @time@ entry: a structurally
malformed-JSON one anywhere is still 'SelectiveUndecodable' (the lexer reaches it), but a
__schema-invalid__ sibling (a non-ISO @time@ string for /another/ version, a non-string
@dist-tags@ value) is __skipped unallocated__ and never inspected. The whole-document
decode degrades the same way: it drops a malformed @time@\/@dist-tags@ entry per-entry
(graceful per-entry degradation) rather than failing the document, so neither path
refuses a sound version over an unrelated sibling malformation. The two paths agree on
__what is served__ (the one sound version, identically projected) and differ only in
__tracking__: the whole-document projection records each dropped sibling as an
'Ecluse.Core.Package.InvalidEntry' for the serve-path log, while this walk, skipping the
siblings unallocated, cannot report them (the degenerate tracking a single-version read
inherently has). The requested version's /own/ schema-invalid stamp folds, on both paths,
to a version with no known publish time (the projecting caller's lenient parse), never a
document failure.
-}
module Ecluse.Core.Registry.Npm.SelectiveDecode (
    -- * The selective decode
    SelectedVersion (..),
    SelectiveError (..),
    selectVersionFromPackument,
) where

import Data.Aeson (Value)
import Data.Aeson.Decoding.ByteString (bsToTokens)
import Data.Aeson.Decoding.Tokens (TkRecord (..), Tokens (TkRecordOpen))
import Data.Aeson.Key qualified as Key

import Ecluse.Core.Json.Selective (
    SelectiveError (..),
    findInRecord,
    materialiseWithinBudget,
    skipValue,
    trailingWhitespace,
    withRecord,
 )
import Ecluse.Core.Version (Version, renderVersion)

{- | The pieces a selective decode pulls out of a packument for one requested version:
the document's self-reported @name@, the requested version's manifest object and publish
stamp (each as the raw 'Value' the same projection the whole-document path uses then
consumes), and the __raw__ number of entries in the @versions@ object.

Each value field is 'Nothing' when its key is absent from the document, so the caller
reproduces the whole-document outcome: an absent @name@ is the empty-name decode failure,
an absent version object is a genuine miss, an absent @time@ entry is a version with no
known publish stamp. The 'svVersionCount' is the count the caller bounds against
'Ecluse.Core.Security.maxVersionCount'.

When a key appears more than once -- a duplicate top-level @name@, @versions@ or @time@, or
a duplicate version key inside @versions@ -- the __first__ occurrence is kept and the later
ones are consumed only for validation, matching @aeson@'s duplicate-key resolution (the
first of a duplicate wins), so neither the chosen value nor the count diverges from the
whole-document decode.
-}
data SelectedVersion = SelectedVersion
    { svName :: Maybe Value
    -- ^ The top-level @name@ value, if the key was present (else 'Nothing').
    , svVersion :: Maybe Value
    -- ^ The requested version's object from @versions@, if that key was present.
    , svTime :: Maybe Value
    -- ^ The requested version's @time[version]@ value, if that key was present.
    , svVersionCount :: Int
    -- ^ The number of entries in the @versions@ object (@0@ when @versions@ is absent).
    }
    deriving stock (Eq, Show)

{- | Selectively decode a packument's bytes for one version: walk the token stream,
extracting the document @name@, the requested version's object and @time@ entry, and the
@versions@ count, while skipping every other version's tokens unallocated and bounding
every value at @maxDepth@ levels (the 'Ecluse.Core.Security.maxNestingDepth' budget, so
the depth bound matches 'Ecluse.Core.Security.checkNestingDepth' over the whole
document).

The body must be a well-formed JSON object with nothing but whitespace after it, or the
result is 'SelectiveUndecodable' -- exactly as @eitherDecodeStrict@ would fail it.
-}
selectVersionFromPackument :: Int -> Version -> ByteString -> Either SelectiveError SelectedVersion
selectVersionFromPackument maxDepth version body
    -- The top-level value is itself a container occupying one level, so a zero (or
    -- negative) budget refuses it before the walk -- mirroring @within cap@ requiring
    -- @cap >= 1@ for the document object.
    | maxDepth < 1 = Left SelectiveTooDeeplyNested
    | otherwise = case bsToTokens body of
        TkRecordOpen rec -> walkTop (maxDepth - 1) (renderVersion version) rec
        -- A well-formed non-object body decodes but never projects to a packument, and a
        -- malformed body never decodes; the whole-document path renders both as the same
        -- "unobtainable metadata", so neither is distinguished here.
        _ -> Left SelectiveUndecodable

-- The starting accumulator: nothing found, no versions counted.
emptySelection :: SelectedVersion
emptySelection = SelectedVersion Nothing Nothing Nothing 0

{- The walk's threaded state: the selection built so far, plus whether each captured
top-level key has already been seen. @aeson@ keeps the __first__ occurrence of a duplicate
key, so once a @name@, @versions@ or @time@ key is captured a later duplicate is consumed
for validation but never overwrites the first. The flags carry that "already captured"
signal, which the selection alone cannot: a captured @versions@\/@time@ whose target was
absent leaves its value field 'Nothing', indistinguishable from "not yet seen". -}
data WalkState = WalkState
    { wsSelection :: SelectedVersion
    , wsSeenName :: Bool
    , wsSeenVersions :: Bool
    , wsSeenTime :: Bool
    }

initialWalk :: WalkState
initialWalk = WalkState emptySelection False False False

{- Walk the top-level packument record to its end, threading the walk state. @childBudget@
is the depth budget each top-level value sits at (one below the document object's own
budget). @name@, @versions@ and @time@ are each captured at their first occurrence (the
requested version and the count come from that first @versions@ object); every other value
is skipped unallocated. The trailing bytes after the record must be whitespace only. -}
walkTop :: Int -> Text -> TkRecord ByteString String -> Either SelectiveError SelectedVersion
walkTop childBudget target = fmap wsSelection . go initialWalk
  where
    go st = \case
        TkRecordEnd leftover
            | trailingWhitespace leftover -> Right st
            | otherwise -> Left SelectiveUndecodable
        TkRecordErr _ -> Left SelectiveUndecodable
        TkPair key valueToks -> case Key.toText key of
            "versions" -> adoptFirst wsSeenVersions captureVersions st valueToks
            "time" -> adoptFirst wsSeenTime captureTime st valueToks
            "name" -> adoptFirst wsSeenName captureName st valueToks
            _ -> skipValue childBudget valueToks >>= go st

    {- Adopt a captured top-level key at its first occurrence, or skip a later duplicate:
    @aeson@ keeps the first of a duplicate key, so once captured a repeat must not overwrite
    it. Either branch still walks the value to its end (its tokens consumed, depth-bounded),
    so a malformed or over-deep sibling anywhere still breaches; a skipped value is never
    materialised. Continues the walk from the value's continuation. -}
    adoptFirst captured capture st valueToks
        | captured st = skipValue childBudget valueToks >>= go st
        | otherwise = capture st valueToks >>= uncurry go

    -- Capture the first @versions@ object: the requested version (first-wins within the
    -- object) and its raw entry count, then mark @versions@ seen.
    captureVersions st valueToks =
        withRecord childBudget valueToks $ \versionsRec -> do
            (found, count, cont) <- findInRecord (childBudget - 1) target versionsRec
            pure (st{wsSelection = (wsSelection st){svVersion = found, svVersionCount = count}, wsSeenVersions = True}, cont)

    -- Capture the first @time@ object: the requested version's publish stamp (first-wins),
    -- then mark @time@ seen. The entry count is the version count's concern, not @time@'s.
    captureTime st valueToks =
        withRecord childBudget valueToks $ \timeRec -> do
            (found, _count, cont) <- findInRecord (childBudget - 1) target timeRec
            pure (st{wsSelection = (wsSelection st){svTime = found}, wsSeenTime = True}, cont)

    -- Capture the first top-level @name@ value, then mark @name@ seen.
    captureName st valueToks = do
        (nameValue, cont) <- materialiseWithinBudget childBudget valueToks
        pure (st{wsSelection = (wsSelection st){svName = Just nameValue}, wsSeenName = True}, cont)
