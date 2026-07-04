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
import Data.Aeson.Decoding (toEitherValue)
import Data.Aeson.Decoding.ByteString (bsToTokens)
import Data.Aeson.Decoding.Tokens (TkArray (..), TkRecord (..), Tokens (..))
import Data.Aeson.Key qualified as Key
import Data.ByteString qualified as BS

import Ecluse.Core.Security (withinNestingBudget)
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

{- | Why a selective decode could not yield a 'SelectedVersion' -- the two refusal causes
the whole-document decode would also raise, so the caller maps them onto the same
'Ecluse.Core.Registry.Metadata.MetadataError' the full path does.
-}
data SelectiveError
    = -- | The body was not a well-formed JSON object (or carried trailing non-whitespace).
      SelectiveUndecodable
    | -- | Some value nested deeper than the depth budget allowed.
      SelectiveTooDeeplyNested
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
        TkRecordOpen rec -> walkTop (maxDepth - 1) (renderVersion version) emptySelection rec
        -- A well-formed non-object body decodes but never projects to a packument, and a
        -- malformed body never decodes; the whole-document path renders both as the same
        -- "unobtainable metadata", so neither is distinguished here.
        _ -> Left SelectiveUndecodable

-- The starting accumulator: nothing found, no versions counted.
emptySelection :: SelectedVersion
emptySelection = SelectedVersion Nothing Nothing Nothing 0

{- Walk the top-level packument record to its end, threading the accumulated selection.
@childBudget@ is the depth budget each top-level value sits at (one below the document
object's own budget). The requested version object and the @name@ value are materialised;
the @versions@ count is tallied; every other value is skipped unallocated. The trailing
bytes after the record must be whitespace only. -}
walkTop :: Int -> Text -> SelectedVersion -> TkRecord ByteString String -> Either SelectiveError SelectedVersion
walkTop childBudget target = go
  where
    go acc = \case
        TkRecordEnd leftover
            | trailingWhitespace leftover -> Right acc
            | otherwise -> Left SelectiveUndecodable
        TkRecordErr _ -> Left SelectiveUndecodable
        TkPair key valueToks -> case Key.toText key of
            "versions" -> withRecord childBudget valueToks $ \versionsRec -> do
                (found, count, cont) <- findInRecord (childBudget - 1) target versionsRec
                go acc{svVersion = found, svVersionCount = svVersionCount acc + count} cont
            "time" -> withRecord childBudget valueToks $ \timeRec -> do
                (found, _count, cont) <- findInRecord (childBudget - 1) target timeRec
                go acc{svTime = found} cont
            "name" -> do
                (nameValue, cont) <- materialiseWithinBudget childBudget valueToks
                go acc{svName = Just nameValue} cont
            _ -> skipValue childBudget valueToks >>= go acc

{- Find one key in a record, materialising __only__ that key's value (a 'Value', the last
occurrence winning as @aeson@'s object decode does) and skipping every other entry's
tokens unallocated. Returns the found value (if any), the number of entries scanned, and
the record's continuation. @childBudget@ is the depth budget the record's values sit at;
each is depth-bounded there so a deeply-nested sibling still breaches. The scan runs to
the record's end (it never stops early), so a later duplicate key, a malformed entry, or
an over-deep sibling is still seen. -}
findInRecord :: Int -> Text -> TkRecord k String -> Either SelectiveError (Maybe Value, Int, k)
findInRecord childBudget target = go Nothing 0
  where
    go found !count = \case
        TkRecordEnd cont -> Right (found, count, cont)
        TkRecordErr _ -> Left SelectiveUndecodable
        TkPair key valueToks
            | Key.toText key == target -> do
                (value, cont) <- materialiseWithinBudget childBudget valueToks
                go (Just value) (count + 1) cont
            | otherwise -> skipValue childBudget valueToks >>= go found (count + 1)

{- Materialise one value from its tokens -- the same 'Value' decode the whole-document
path uses -- bounded at @budget@: malformed tokens are 'SelectiveUndecodable', a value
past the depth budget is 'SelectiveTooDeeplyNested'. The single materialisation point of
the walk, so every built 'Value' passes the same depth gate. -}
materialiseWithinBudget :: Int -> Tokens k String -> Either SelectiveError (Value, k)
materialiseWithinBudget budget toks = case toEitherValue toks of
    Left _ -> Left SelectiveUndecodable
    Right (value, cont)
        | withinNestingBudget budget value -> Right (value, cont)
        | otherwise -> Left SelectiveTooDeeplyNested

{- Run @k@ on a record token, refusing a non-record (the @versions@\/@time@ value must be
an object, exactly as the whole-document decode reads each as a @Map@) and refusing the
container outright when the depth budget is already spent (a record is itself one level).
-}
withRecord :: Int -> Tokens k String -> (TkRecord k String -> Either SelectiveError a) -> Either SelectiveError a
withRecord budget toks k
    | budget < 1 = Left SelectiveTooDeeplyNested
    | otherwise = case toks of
        TkRecordOpen rec -> k rec
        TkErr _ -> Left SelectiveUndecodable
        _ -> Left SelectiveUndecodable

{- Consume one value's tokens without allocating a 'Value', returning the continuation.
Bounds nesting at @budget@ levels exactly as 'Ecluse.Core.Security.withinNestingBudget'
does over a built 'Value' -- a value occupies one level (refused at @budget < 1@) and a
container's children are bounded one level deeper -- so skipping reproduces the depth check
the whole-document path runs. Malformed tokens are 'SelectiveUndecodable'. -}
skipValue :: Int -> Tokens k String -> Either SelectiveError k
skipValue budget toks
    | budget < 1 = Left SelectiveTooDeeplyNested
    | otherwise = case toks of
        TkLit _ cont -> Right cont
        TkText _ cont -> Right cont
        TkNumber _ cont -> Right cont
        TkArrayOpen arr -> skipArray (budget - 1) arr
        TkRecordOpen rec -> skipRecord (budget - 1) rec
        TkErr _ -> Left SelectiveUndecodable

-- Skip an array's items (each at @budget@), returning the continuation after its end.
skipArray :: Int -> TkArray k String -> Either SelectiveError k
skipArray budget = \case
    TkItem toks -> skipValue budget toks >>= skipArray budget
    TkArrayEnd cont -> Right cont
    TkArrayErr _ -> Left SelectiveUndecodable

-- Skip a record's values (each at @budget@), returning the continuation after its end.
skipRecord :: Int -> TkRecord k String -> Either SelectiveError k
skipRecord budget = \case
    TkPair _ toks -> skipValue budget toks >>= skipRecord budget
    TkRecordEnd cont -> Right cont
    TkRecordErr _ -> Left SelectiveUndecodable

{- Whether the bytes after the top-level value are JSON whitespace only -- the
end-of-input check @eitherDecodeStrict@ applies, so a body with trailing non-whitespace is
refused identically (space, tab, newline, carriage return are the four JSON whitespace
bytes). -}
trailingWhitespace :: ByteString -> Bool
trailingWhitespace = BS.all isJsonSpace
  where
    isJsonSpace :: Word8 -> Bool
    isJsonSpace w = w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09
