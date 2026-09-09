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

import Data.Aeson (Value (Array, String), object)
import Data.Aeson.Decoding.ByteString (bsToTokens)
import Data.Aeson.Decoding.Tokens (TkRecord (..), Tokens (TkArrayOpen, TkRecordOpen))
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

-- | Selected values retain absent fields and original file positions for shared projection.
data SelectedFiles = SelectedFiles
    { sfName :: Maybe Value
    -- ^ The top-level @name@ value, if the key was present (else 'Nothing').
    , sfMeta :: Maybe Value
    -- ^ Only @api-version@ survives an object. Other shapes retain their decoder outcome.
    , sfFiles :: [(Int, Value)]
    -- ^ The requested release's file entries, in index order.
    , sfFileCount :: Int
    -- ^ The number of entries in the @files@ array (@0@ when @files@ is absent).
    }
    deriving stock (Eq, Show)

-- | Select one release and its protocol envelope, checking syntax and depth throughout the index.
selectFilesFromIndex :: Int -> (Text -> Bool) -> ByteString -> Either SelectiveError SelectedFiles
selectFilesFromIndex maxDepth belongsTo body
    | maxDepth < 1 = Left SelectiveTooDeeplyNested
    | otherwise = case bsToTokens body of
        TkRecordOpen rec -> walkTop (maxDepth - 1) belongsTo rec
        _ -> Left SelectiveUndecodable

emptySelection :: SelectedFiles
emptySelection = SelectedFiles Nothing Nothing [] 0

-- Duplicate keys keep their first occurrence, matching aeson.
data WalkState = WalkState
    { wsSelection :: SelectedFiles
    , wsSeenName :: Bool
    , wsSeenMeta :: Bool
    , wsSeenFiles :: Bool
    }

initialWalk :: WalkState
initialWalk = WalkState emptySelection False False False

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
            "meta" -> adoptFirst wsSeenMeta captureMeta st valueToks
            _ -> skipValue childBudget valueToks >>= go st

    adoptFirst captured capture st valueToks
        | captured st = skipValue childBudget valueToks >>= go st
        | otherwise = capture st valueToks >>= uncurry go

    captureFiles st valueToks =
        withArray childBudget valueToks $ \files -> do
            (picked, count, cont) <- selectIndexedFromArray (childBudget - 1) (const (belongsToRelease (childBudget - 1))) files
            pure (st{wsSelection = (wsSelection st){sfFiles = picked, sfFileCount = count}, wsSeenFiles = True}, cont)

    captureName st valueToks = do
        (nameValue, cont) <- materialiseWithinBudget childBudget valueToks
        pure (st{wsSelection = (wsSelection st){sfName = Just nameValue}, wsSeenName = True}, cont)

    captureMeta st valueToks = do
        (metaValue, cont) <- case valueToks of
            TkRecordOpen{} -> withRecord childBudget valueToks $ \record -> do
                (declared, _, rest) <- findInRecord (childBudget - 1) "api-version" record
                pure (object (maybe [] (\value -> [("api-version", value)]) declared), rest)
            -- Arrays always fail the shared object parser, regardless of their contents.
            TkArrayOpen{} -> do
                rest <- skipValue childBudget valueToks
                pure (Array mempty, rest)
            _ -> materialiseWithinBudget childBudget valueToks
        pure (st{wsSelection = (wsSelection st){sfMeta = Just metaValue}, wsSeenMeta = True}, cont)

    -- Read from the entry's own @filename@ alone, so a file of another release costs one
    -- materialised string. An entry declaring no readable name belongs to no release.
    belongsToRelease budget entryToks@TkRecordOpen{} =
        case withRecord budget entryToks (findInRecord (budget - 1) "filename") of
            Left err -> Left err
            Right (found, _count, _cont) -> Right (maybe False (belongsTo . renderName) found)
    belongsToRelease _ _ = Right False

renderName :: Value -> Text
renderName = \case
    String name -> name
    _ -> ""
