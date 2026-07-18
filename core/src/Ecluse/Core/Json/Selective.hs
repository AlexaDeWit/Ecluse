-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | A memory-bounded __selective decode__ over a JSON document's token stream: the reusable
engine that materialises only the values a caller picks out and skips every other value's tokens
unallocated, while depth-bounding every value it walks.

A whole-document decode (@aeson@'s @eitherDecodeStrict@) builds a 'Value' for /every/ member of a
large object. When a caller needs only a few members out of a multi-megabyte document, that decode
dominates the cost. This engine walks a document's JSON token stream (@aeson@'s
@Data.Aeson.Decoding@, no new dependency) and materialises a 'Value' only for the picked members,
skipping the rest without allocating them. The win is on the /parse/, not the fetch: the full bytes
are still read, but they are parsed selectively, @O(picked)@ work and residency rather than @O(N)@.

== Faithful to the whole-document decode

Bounded selective decode is a memory-bounding defence on a size-unbounded, attacker-influenced
document, so the walk is faithful to the whole-document decode rather than a shortcut past it:

  * it consumes the __entire__ token stream, so malformed JSON __anywhere__ surfaces as
    'SelectiveUndecodable', matching @eitherDecodeStrict@ failing the whole body;
  * every value is depth-bounded at the caller's budget, so a value nested past it __anywhere__ is
    a 'SelectiveTooDeeplyNested' breach ('Ecluse.Core.Security.withinNestingBudget' is the same
    bound applied to a built 'Value');
  * within one object a key's __first__ occurrence wins: a later duplicate is walked for the
    malformed and over-deep checks but never re-materialised, matching @aeson@'s duplicate-key
    resolution.

The engine names @aeson@'s token types and a depth budget only, no registry or package concept, so
each JSON ecosystem layers its own key-selection walk on top. The npm packument selector is
"Ecluse.Core.Registry.Npm.SelectiveDecode".
-}
module Ecluse.Core.Json.Selective (
    -- * Refusal vocabulary
    SelectiveError (..),

    -- * Bounded token-walk primitives
    findInRecord,
    materialiseWithinBudget,
    withRecord,
    skipValue,
    trailingWhitespace,
) where

import Data.Aeson (Value)
import Data.Aeson.Decoding (toEitherValue)
import Data.Aeson.Decoding.Tokens (TkArray (..), TkRecord (..), Tokens (..))
import Data.Aeson.Key qualified as Key
import Data.ByteString qualified as BS

import Ecluse.Core.Security (withinNestingBudget)

{- | Why a selective decode could not yield a value: the two refusal causes a whole-document
decode would also raise, so a caller maps them onto its own error vocabulary.
-}
data SelectiveError
    = {- | The token stream was not well-formed JSON: malformed bytes anywhere, or trailing
      non-whitespace after the top-level value.
      -}
      SelectiveUndecodable
    | -- | Some value nested deeper than the depth budget allowed.
      SelectiveTooDeeplyNested
    deriving stock (Eq, Show)

{- | Find one key in a record, materialising __only__ the __first__ occurrence of that key's value
(a 'Value') and skipping every other entry's tokens unallocated. @aeson@'s object decode keeps the
first of duplicate keys, so a later duplicate of the target is walked (for the malformed and
over-deep checks) but not re-materialised. Returns the found value (if any), the __raw__ number of
entries scanned, and the record's continuation. @childBudget@ is the depth budget the record's
values sit at, so a deeply-nested sibling still breaches. The scan runs to the record's end, never
stopping early, so a later duplicate key, a malformed entry, or an over-deep sibling is still seen.
-}
findInRecord :: Int -> Text -> TkRecord k String -> Either SelectiveError (Maybe Value, Int, k)
findInRecord childBudget target = go Nothing 0
  where
    go found !count = \case
        TkRecordEnd cont -> Right (found, count, cont)
        TkRecordErr _ -> Left SelectiveUndecodable
        TkPair key valueToks
            | Key.toText key == target
            , Nothing <- found -> do
                (value, cont) <- materialiseWithinBudget childBudget valueToks
                go (Just value) (count + 1) cont
            | otherwise -> skipValue childBudget valueToks >>= go found (count + 1)

{- | Materialise one value from its tokens, the same 'Value' decode a whole-document path uses,
bounded at @budget@: malformed tokens are 'SelectiveUndecodable', a value past the depth budget is
'SelectiveTooDeeplyNested'. Route every 'Value' a selective walk builds through here, so each
passes the same depth gate.
-}
materialiseWithinBudget :: Int -> Tokens k String -> Either SelectiveError (Value, k)
materialiseWithinBudget budget toks = case toEitherValue toks of
    Left _ -> Left SelectiveUndecodable
    Right (value, cont)
        | withinNestingBudget budget value -> Right (value, cont)
        | otherwise -> Left SelectiveTooDeeplyNested

{- | Run @k@ on a record token, refusing a non-record value and refusing the container outright
when the depth budget is already spent (a record is itself one level).
-}
withRecord :: Int -> Tokens k String -> (TkRecord k String -> Either SelectiveError a) -> Either SelectiveError a
withRecord budget toks k
    | budget < 1 = Left SelectiveTooDeeplyNested
    | otherwise = case toks of
        TkRecordOpen rec -> k rec
        TkErr _ -> Left SelectiveUndecodable
        _ -> Left SelectiveUndecodable

{- | Consume one value's tokens without allocating a 'Value', returning the continuation. Bounds
nesting at @budget@ levels exactly as 'Ecluse.Core.Security.withinNestingBudget' does over a built
'Value': a value occupies one level (refused at @budget < 1@) and a container's children are
bounded one level deeper. Malformed tokens are 'SelectiveUndecodable'.
-}
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

{- | Whether the bytes after the top-level value are JSON whitespace only: the end-of-input check
@eitherDecodeStrict@ applies, so a body with trailing non-whitespace is refused identically (space,
tab, newline, carriage return are the four JSON whitespace bytes).
-}
trailingWhitespace :: ByteString -> Bool
trailingWhitespace = BS.all isJsonSpace
  where
    isJsonSpace :: Word8 -> Bool
    isJsonSpace w = w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09
