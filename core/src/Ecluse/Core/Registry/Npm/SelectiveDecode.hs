-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Select one npm version without materialising sibling values.
The bounded token walk validates the whole document. Optional containers treat null as absent,
and duplicate keys keep their first value, matching the full npm projection.
-}
module Ecluse.Core.Registry.Npm.SelectiveDecode (
    -- * The selective decode
    SelectedVersion (..),
    SelectiveError (..),
    selectVersionFromPackument,
) where

import Data.Aeson (Value)
import Data.Aeson.Decoding.ByteString (bsToTokens)
import Data.Aeson.Decoding.Tokens (Lit (LitNull), TkRecord (..), Tokens (TkLit, TkRecordOpen))
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

-- | Selected fields and the first versions container's count. Absent or null containers yield no fields.
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

-- | Decode one version, rejecting malformed JSON or values outside the whole document's nesting budget.
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

emptySelection :: SelectedVersion
emptySelection = SelectedVersion Nothing Nothing Nothing Nothing 0

-- A null first container must stay distinct from an unseen key when a duplicate follows.
data WalkState = WalkState
    { wsSelection :: SelectedVersion
    , wsSeenName :: Bool
    , wsSeenVersions :: Bool
    , wsSeenTime :: Bool
    , wsSeenDistTags :: Bool
    }

initialWalk :: WalkState
initialWalk = WalkState emptySelection False False False False

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

    adoptFirst captured capture st valueToks
        | captured st = skipValue childBudget valueToks >>= go st
        | otherwise = capture st valueToks >>= uncurry go

    captureVersions st valueToks = do
        (found, count, cont) <- findInOptionalRecord childBudget target valueToks
        pure (st{wsSelection = (wsSelection st){svVersion = found, svVersionCount = count}, wsSeenVersions = True}, cont)

    captureTime st valueToks = do
        (found, _count, cont) <- findInOptionalRecord childBudget target valueToks
        pure (st{wsSelection = (wsSelection st){svTime = found}, wsSeenTime = True}, cont)

    captureDistTags st valueToks = do
        (found, _count, cont) <- findInOptionalRecord childBudget "latest" valueToks
        pure (st{wsSelection = (wsSelection st){svDistTagLatest = found}, wsSeenDistTags = True}, cont)

    captureName st valueToks = do
        (nameValue, cont) <- materialiseWithinBudget childBudget valueToks
        pure (st{wsSelection = (wsSelection st){svName = Just nameValue}, wsSeenName = True}, cont)

findInOptionalRecord :: Int -> Text -> Tokens k String -> Either SelectiveError (Maybe Value, Int, k)
findInOptionalRecord budget target toks = case toks of
    TkLit LitNull _ -> do
        cont <- skipValue budget toks
        pure (Nothing, 0, cont)
    _ -> withRecord budget toks (findInRecord (budget - 1) target)
