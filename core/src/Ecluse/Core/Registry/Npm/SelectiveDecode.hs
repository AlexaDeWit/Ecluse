-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | A __selective__ decode of an npm packument: pull __one version's__ pieces out of the
document bytes without materialising the other versions.

The whole-packument decode builds a 'Value' for every version, which dominates the serve-path
cost on a packument of thousands of versions. The tarball gate consults a single version. This
module walks the registry's own JSON token stream (@aeson@'s @Data.Aeson.Decoding@, no new
dependency) and skips every other version's tokens without allocating them. The generic bounded
token-walk engine is "Ecluse.Core.Json.Selective". The win is on the parse, not the fetch: the
proxy still reads the full bytes, because npm carries @time@ only in the full document.

== Faithful to the whole-document decode

The walk consumes the __entire__ token stream, so malformed JSON anywhere, and trailing
non-whitespace after the top-level object, surface as 'SelectiveUndecodable' exactly as
@eitherDecodeStrict@ fails them. Every value is depth-bounded at the
'Ecluse.Core.Security.checkNestingDepth' budget that would apply to it.

It materialises three pieces: the requested version's object, the document @name@, and the
@dist-tags.latest@ target. The same @aeson@ 'Value' decoder the whole-document path uses
produces them, so projecting them yields an identical
'Ecluse.Core.Package.PackageDetails'.

== What it deliberately does not re-validate

The walk __skips a schema-invalid sibling unallocated__ and never inspects it: a non-ISO @time@
string for another version, a non-string @dist-tags@ value other than @latest@. The
whole-document decode drops such an entry per-entry rather than failing the document. The two
paths agree on __what is served__ and differ only in __tracking__: the whole-document projection
records each dropped sibling as an 'Ecluse.Core.Package.InvalidEntry', and this walk cannot,
having never allocated them. The requested version's own malformed stamp folds to a version with
no known publish time on both paths, never to a document failure.
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

{- | The raw 'Value' pieces a selective decode pulls out of a packument for one requested
version. A field is 'Nothing' when its key is absent, so the caller reproduces the
whole-document outcome, and an absent @name@ is the empty-name decode failure.

A duplicate key keeps its __first__ occurrence, matching @aeson@'s own resolution, so
neither the chosen value nor the count diverges from the whole-document decode. The caller
bounds 'svVersionCount' against 'Ecluse.Core.Security.maxVersionCount'.
-}
data SelectedVersion = SelectedVersion
    { svName :: Maybe Value
    -- ^ The top-level @name@ value, if the key was present (else 'Nothing').
    , svVersion :: Maybe Value
    -- ^ The requested version's object from @versions@, if that key was present.
    , svTime :: Maybe Value
    -- ^ The requested version's @time[version]@ value, if that key was present.
    , svDistTagLatest :: Maybe Value
    -- ^ The @dist-tags.latest@ value, if both keys were present.
    , svVersionCount :: Int
    -- ^ The number of entries in the @versions@ object (@0@ when @versions@ is absent).
    }
    deriving stock (Eq, Show)

{- | Selectively decode a packument's bytes for one version, skipping every other version's
tokens unallocated. Each value is bounded at @maxDepth@ levels, the
'Ecluse.Core.Security.maxNestingDepth' budget, so the bound matches
'Ecluse.Core.Security.checkNestingDepth' over the whole document.

The body must be a well-formed JSON object with nothing but whitespace after it. Anything
else is 'SelectiveUndecodable', exactly as @eitherDecodeStrict@ would fail it.
-}
selectVersionFromPackument :: Int -> Version -> ByteString -> Either SelectiveError SelectedVersion
selectVersionFromPackument maxDepth version body
    -- The document object itself occupies one level, so a budget below 1 refuses it before the
    -- walk, matching @within cap@, which requires @cap >= 1@ for the document object.
    | maxDepth < 1 = Left SelectiveTooDeeplyNested
    | otherwise = case bsToTokens body of
        TkRecordOpen rec -> walkTop (maxDepth - 1) (renderVersion version) rec
        -- The whole-document path renders a malformed body and a well-formed non-object alike as
        -- unobtainable metadata, so this walk does not distinguish them either.
        _ -> Left SelectiveUndecodable

-- The starting accumulator: nothing found, no versions counted.
emptySelection :: SelectedVersion
emptySelection = SelectedVersion Nothing Nothing Nothing Nothing 0

{- The walk's threaded state. A flag marks each captured top-level key so a later duplicate
never overwrites the first, as @aeson@ resolves it. The selection alone cannot carry that: a
captured key whose target was absent leaves 'Nothing', and so does "not yet seen". -}
data WalkState = WalkState
    { wsSelection :: SelectedVersion
    , wsSeenName :: Bool
    , wsSeenVersions :: Bool
    , wsSeenTime :: Bool
    , wsSeenDistTags :: Bool
    }

initialWalk :: WalkState
initialWalk = WalkState emptySelection False False False False

{- Walk the top-level packument record to its end, threading the walk state. Each top-level
value sits at @childBudget@, one level below the document object's own budget. -}
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
            "dist-tags" -> adoptFirst wsSeenDistTags captureDistTags st valueToks
            _ -> skipValue childBudget valueToks >>= go st

    {- Adopt a captured top-level key at its first occurrence, or skip a later duplicate, since
    @aeson@ keeps the first. Either branch still walks the value to its end, depth-bounded and
    never materialised, so a malformed or over-deep sibling anywhere still breaches. -}
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

    -- Capture the first @dist-tags@ object's @latest@ target (first-wins), then mark
    -- @dist-tags@ seen. A non-string target projects to no known latest, never a failure.
    captureDistTags st valueToks =
        withRecord childBudget valueToks $ \tagsRec -> do
            (found, _count, cont) <- findInRecord (childBudget - 1) "latest" tagsRec
            pure (st{wsSelection = (wsSelection st){svDistTagLatest = found}, wsSeenDistTags = True}, cont)

    -- Capture the first top-level @name@ value, then mark @name@ seen.
    captureName st valueToks = do
        (nameValue, cont) <- materialiseWithinBudget childBudget valueToks
        pure (st{wsSelection = (wsSelection st){svName = Just nameValue}, wsSeenName = True}, cont)
