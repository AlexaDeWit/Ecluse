-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Select one PyPI release without materialising unrelated files.
The bounded token walk retains original array positions and checks the entire JSON stream.
-}
module Ecluse.Core.Registry.PyPI.SelectiveDecode (
    SelectedFiles (..),
    SelectiveError (..),
    selectFilesFromIndex,
) where

import Data.Aeson (Value (String))
import Data.Aeson.Decoding.ByteString (bsToTokens)
import Data.Aeson.Decoding.Tokens (TkRecord (..), Tokens (TkRecordOpen))
import Data.Aeson.Key qualified as Key

import Ecluse.Core.Json.Selective (
    SelectiveError (..),
    findInRecord,
    materialiseWithinBudget,
    selectIndexedFromArray,
    skipValue,
    trailingWhitespace,
    withArray,
    withRecord,
 )

{- | The raw 'Value' pieces a selective decode pulls out for one release. A field is 'Nothing' when
its key is absent, so the caller reproduces the whole-document outcome.
-}
data SelectedFiles = SelectedFiles
    { sfName :: Maybe Value
    -- ^ The top-level @name@ value, if the key was present (else 'Nothing').
    , sfFiles :: [(Int, Value)]
    -- ^ The requested release's file entries, in index order.
    , sfFileCount :: Int
    -- ^ The number of entries in the @files@ array (@0@ when @files@ is absent).
    }
    deriving stock (Eq, Show)

{- | Selectively decode a Simple index's bytes for one release. @belongsTo@ reads a release out of a
distribution name, PyPI's grammar rather than this walk's, and @maxDepth@ bounds each value.
-}
selectFilesFromIndex :: Int -> (Text -> Bool) -> ByteString -> Either SelectiveError SelectedFiles
selectFilesFromIndex maxDepth belongsTo body
    -- The document object itself occupies one level, so a budget below 1 refuses it before the
    -- walk, matching a whole-document check, which requires a cap of at least 1 for the object.
    | maxDepth < 1 = Left SelectiveTooDeeplyNested
    | otherwise = case bsToTokens body of
        TkRecordOpen rec -> walkTop (maxDepth - 1) belongsTo rec
        -- The whole-document path renders a malformed body and a well-formed non-object alike
        -- as unobtainable metadata, so this walk does not distinguish them either.
        _ -> Left SelectiveUndecodable

-- The starting accumulator: nothing found, no files counted.
emptySelection :: SelectedFiles
emptySelection = SelectedFiles Nothing [] 0

-- The flags mark a captured @name@ or @files@ so a later duplicate never overwrites the first,
-- as @aeson@ resolves it. The selection alone cannot carry that: absent and unseen look alike.
data WalkState = WalkState
    { wsSelection :: SelectedFiles
    , wsSeenName :: Bool
    , wsSeenFiles :: Bool
    }

initialWalk :: WalkState
initialWalk = WalkState emptySelection False False

{- Walk the top-level index record to its end, threading the walk state. Each top-level value
sits at @childBudget@, one level below the document object's own budget. -}
walkTop :: Int -> (Text -> Bool) -> TkRecord ByteString String -> Either SelectiveError SelectedFiles
walkTop childBudget belongsTo = fmap wsSelection . go initialWalk
  where
    go st = \case
        TkRecordEnd leftover
            | trailingWhitespace leftover -> Right st
            | otherwise -> Left SelectiveUndecodable
        TkRecordErr _ -> Left SelectiveUndecodable
        TkPair key valueToks -> case Key.toText key of
            "files" -> adoptFirst wsSeenFiles captureFiles st valueToks
            "name" -> adoptFirst wsSeenName captureName st valueToks
            _ -> skipValue childBudget valueToks >>= go st

    -- @aeson@ keeps the first occurrence. Either branch still walks the value to its end, so a
    -- malformed or over-deep sibling anywhere still breaches.
    adoptFirst captured capture st valueToks
        | captured st = skipValue childBudget valueToks >>= go st
        | otherwise = capture st valueToks >>= uncurry go

    -- Capture the first @files@ array: the requested release's entries and the raw entry count,
    -- then mark @files@ seen.
    captureFiles st valueToks =
        withArray childBudget valueToks $ \files -> do
            (picked, count, cont) <- selectIndexedFromArray (childBudget - 1) (const (belongsToRelease (childBudget - 1))) files
            pure (st{wsSelection = (wsSelection st){sfFiles = picked, sfFileCount = count}, wsSeenFiles = True}, cont)

    -- Capture the first top-level @name@ value, then mark @name@ seen.
    captureName st valueToks = do
        (nameValue, cont) <- materialiseWithinBudget childBudget valueToks
        pure (st{wsSelection = (wsSelection st){sfName = Just nameValue}, wsSeenName = True}, cont)

    -- Read from the entry's own @filename@ alone, so a file of another release costs one
    -- materialised string. An entry declaring no readable name belongs to no release.
    belongsToRelease budget entryToks@TkRecordOpen{} =
        case withRecord budget entryToks (findInRecord (budget - 1) "filename") of
            Left err -> Left err
            Right (found, _count, _cont) -> Right (maybe False (belongsTo . renderName) found)
    belongsToRelease _ _ = Right False

-- A @filename@ value as text, or the empty name for a value that is not a string.
renderName :: Value -> Text
renderName = \case
    String name -> name
    _ -> ""
