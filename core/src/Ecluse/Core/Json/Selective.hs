-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Bounded JSON token selection shared by registry decoders.
Unselected values remain unmaterialised, while the walk validates their syntax and nesting.
-}
module Ecluse.Core.Json.Selective (
    -- * Refusal vocabulary
    SelectiveError (..),

    -- * Bounded selection
    findInRecord,
    collectFromArray,
    selectFromArray,
    selectIndexedFromArray,
    materialiseWithinBudget,

    -- * Container guards
    withRecord,
    withArray,

    -- * Bounded skips
    skipValue,
    skipArray,
    skipRecord,

    -- * End of input
    trailingWhitespace,
) where

import Data.Aeson (Value)
import Data.Aeson.Decoding (toEitherValue)
import Data.Aeson.Decoding.Tokens (TkArray (..), TkRecord (..), Tokens (..))
import Data.Aeson.Key qualified as Key
import Data.ByteString qualified as BS

import Ecluse.Core.Security (withinNestingBudget)

{- | Why a selective decode could not yield a value. These are the two refusal causes a
whole-document decode would also raise, so a caller maps them onto its own error vocabulary.
-}
data SelectiveError
    = {- | The token stream was not well-formed JSON: malformed bytes anywhere, or trailing
      non-whitespace after the top-level value.
      -}
      SelectiveUndecodable
    | -- | Some value nested deeper than the depth budget allowed.
      SelectiveTooDeeplyNested
    deriving stock (Eq, Show)

-- | Find the first occurrence of a record key, returning its value, the raw entry count, and continuation.
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

{- | Collect the picked items out of an array, deciding by position so a rejected item's tokens
are skipped unallocated. The scan runs to the end, so a malformed unpicked item still refuses.
-}
collectFromArray :: Int -> (Int -> Bool) -> TkArray k String -> Either SelectiveError ([Value], Int, k)
collectFromArray budget pick = selectFromArray budget (\position _ -> Right (pick position))

{- | Collect the items a probe accepts, deciding from an item's own lazy tokens so reading one
discriminating member costs no materialised value. The scan still runs to the array's end.
-}
selectFromArray ::
    Int ->
    -- | The probe: an item's position and its own tokens, which continue into the rest of the array.
    (Int -> Tokens (TkArray k String) String -> Either SelectiveError Bool) ->
    TkArray k String ->
    Either SelectiveError ([Value], Int, k)
selectFromArray budget probe = fmap (\(entries, count, cont) -> (map snd entries, count, cont)) . selectIndexedFromArray budget probe

-- | Select array entries while retaining their positions, including gaps left by skipped entries.
selectIndexedFromArray ::
    Int ->
    (Int -> Tokens (TkArray k String) String -> Either SelectiveError Bool) ->
    TkArray k String ->
    Either SelectiveError ([(Int, Value)], Int, k)
selectIndexedFromArray budget probe = go [] 0
  where
    go picked !count = \case
        TkArrayEnd cont -> Right (reverse picked, count, cont)
        TkArrayErr _ -> Left SelectiveUndecodable
        TkItem valueToks -> do
            wanted <- probe count valueToks
            if wanted
                then do
                    (value, cont) <- materialiseWithinBudget budget valueToks
                    go ((count, value) : picked) (count + 1) cont
                else skipValue budget valueToks >>= go picked (count + 1)

-- | Decode one value within the shared nesting budget, returning its token continuation.
materialiseWithinBudget :: Int -> Tokens k String -> Either SelectiveError (Value, k)
materialiseWithinBudget budget toks = case toEitherValue toks of
    Left _ -> Left SelectiveUndecodable
    Right (value, cont)
        | withinNestingBudget budget value -> Right (value, cont)
        | otherwise -> Left SelectiveTooDeeplyNested

{- | Run @k@ on a record token. Refuse a non-record value, and refuse the container outright when
the depth budget is already spent, because a record is itself one level.
-}
withRecord :: Int -> Tokens k String -> (TkRecord k String -> Either SelectiveError a) -> Either SelectiveError a
withRecord budget toks k
    | budget < 1 = Left SelectiveTooDeeplyNested
    | otherwise = case toks of
        TkRecordOpen rec -> k rec
        _ -> Left SelectiveUndecodable

{- | Run @k@ on an array token. Refuse a non-array value, and refuse the container outright when
the depth budget is already spent, because an array is itself one level.
-}
withArray :: Int -> Tokens k String -> (TkArray k String -> Either SelectiveError a) -> Either SelectiveError a
withArray budget toks k
    | budget < 1 = Left SelectiveTooDeeplyNested
    | otherwise = case toks of
        TkArrayOpen arr -> k arr
        _ -> Left SelectiveUndecodable

-- | Consume a value without materialising it, applying the shared nesting budget.
skipValue :: Int -> Tokens k String -> Either SelectiveError k
skipValue budget toks
    | budget < 1 = Left SelectiveTooDeeplyNested
    | otherwise = case toks of
        TkLit _ cont -> Right cont
        TkText _ cont -> Right cont
        TkNumber _ cont -> Right cont
        TkArrayOpen{} -> withArray budget toks (skipArray (budget - 1))
        TkRecordOpen rec -> skipRecord (budget - 1) rec
        TkErr _ -> Left SelectiveUndecodable

{- | Skip an array's items (each at @budget@), returning the continuation after its end. It is
'collectFromArray' picking none of them.
-}
skipArray :: Int -> TkArray k String -> Either SelectiveError k
skipArray budget arr = (\(_, _, cont) -> cont) <$> collectFromArray budget (const False) arr

-- | Skip a record's values (each at @budget@), returning the continuation after its end.
skipRecord :: Int -> TkRecord k String -> Either SelectiveError k
skipRecord budget = \case
    TkPair _ toks -> skipValue budget toks >>= skipRecord budget
    TkRecordEnd cont -> Right cont
    TkRecordErr _ -> Left SelectiveUndecodable

{- | Whether the bytes after the top-level value are JSON whitespace only. It is the end-of-input
check @eitherDecodeStrict@ applies, so a body with trailing non-whitespace fails identically.
-}
trailingWhitespace :: ByteString -> Bool
trailingWhitespace = BS.all isJsonSpace
  where
    isJsonSpace :: Word8 -> Bool
    isJsonSpace w = w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09
