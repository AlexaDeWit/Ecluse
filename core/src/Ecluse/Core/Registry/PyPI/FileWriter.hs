-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TypeFamilies #-}

-- | Pack supported file members while reducing their typed facts. Only diagnostics decode a file again.
module Ecluse.Core.Registry.PyPI.FileWriter (
    FileWriter,
    fileWriter,
    FileValue (..),
) where

import Control.Monad.ST (ST)
import Data.Aeson (Value (..), parseJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict qualified as Map
import Data.Primitive.PrimVar (PrimVar, newPrimVar, readPrimVar, writePrimVar)
import Data.Set qualified as Set
import Data.Time (UTCTime)

import Ecluse.Core.Package.Entry (EntryKey (SingletonEntry))
import Ecluse.Core.Registry.Json.Intern (entryString, entryText, nameText)
import Ecluse.Core.Registry.Json.Shape (Build (..), MemberKey (..))
import Ecluse.Core.Registry.Json.Walk (Steps)
import Ecluse.Core.Registry.Json.Writer (Frame, Writer)
import Ecluse.Core.Registry.PyPI.Wire (IndexFile (..), YankState (FileOffered), yankState)
import Ecluse.Core.Registry.WireSupport (parsePublishTime)

-- | A writer with the object depth needed to distinguish file fields from digest members.
data FileWriter st = FileWriter (Writer st) (PrimVar st Int)

-- | Attach typed reduction to a read's existing scratch writer.
fileWriter :: Writer st -> ST st (FileWriter st)
fileWriter writer = FileWriter writer <$> newPrimVar 0

-- | A completed scalar, digest object, or file. A failed typed decode keeps no error tree.
data FileValue
    = -- | A scalar used by one field reducer, including the empty-array container fallback.
      ScalarValue Value
    | -- | Digest text by algorithm, or a failed digest value.
      HashValue (Maybe (Map Text Text))
    | -- | A decoded file, or a failure whose diagnostic needs the packed payload.
      FileValue (Maybe IndexFile)

data FileFields
    = FileFields Frame FileFacts (Set Text)
    | HashFields Frame (Map Text (Maybe Text))

data FileFacts = FileFacts
    { ffFilename :: Maybe Text
    , ffUrl :: Maybe Text
    , ffHashes :: Maybe (Map Text Text)
    , ffRequiresPython :: Maybe Text
    , ffSize :: Maybe Int
    , ffUploadTime :: Maybe UTCTime
    , ffYanked :: YankState
    , ffProvenance :: Maybe Text
    , ffValid :: Bool
    }

emptyFacts :: FileFacts
emptyFacts = FileFacts Nothing Nothing (Just mempty) Nothing Nothing Nothing FileOffered Nothing True

instance Build (FileWriter st) (ST st (Steps (ST st) s)) where
    type Built (FileWriter st) = FileValue
    type Fields (FileWriter st) = FileFields
    type Items (FileWriter st) = Int
    sharedString (FileWriter writer _) entry next = sharedString writer entry (\() -> next (ScalarValue (entryString entry)))
    ownString (FileWriter writer _) name next = ownString writer name (\() -> next (ScalarValue (String (nameText name))))
    integer (FileWriter writer _) number next = integer writer number (\() -> next (ScalarValue (Number (fromIntegral number))))
    whole (FileWriter writer _) value next = whole writer value (\() -> next (ScalarValue value))
    emptyContainer (FileWriter writer _) next = emptyContainer writer (\() -> next (ScalarValue (Array mempty)))
    openObject (FileWriter writer depth) next = do
        level <- readPrimVar depth
        writePrimVar depth (level + 1)
        openObject writer (\frame -> next (if level == 0 then FileFields frame emptyFacts mempty else HashFields frame mempty))
    beginMember (FileWriter writer _) key fields next = beginMember writer key (frameOf fields) $ \repeated ->
        next (repeated || seenMember (keyText key) fields)
    addMember (FileWriter writer _) key value fields next = addMember writer key () (frameOf fields) $ \frame ->
        next $! case fields of
            FileFields _ facts seen -> FileFields frame (reduceMember (keyText key) value facts) (Set.insert (keyText key) seen)
            HashFields _ hashes -> HashFields frame (Map.insert (keyText key) (scalarText value) hashes)
    dropValue (FileWriter writer _) _ = dropValue writer ()
    closeObject (FileWriter writer depth) fields next = do
        level <- readPrimVar depth
        writePrimVar depth (level - 1)
        closeObject writer (frameOf fields) $ \() ->
            next $! case fields of
                FileFields _ facts _ -> FileValue (finishFacts facts)
                HashFields _ hashes -> HashValue (sequenceA hashes)
    openArray (FileWriter writer _) = openArray writer
    addItem (FileWriter writer _) _ = addItem writer ()
    closeArray (FileWriter writer _) count start next = closeArray writer count start (\() -> next (ScalarValue (Array mempty)))
    {-# INLINE sharedString #-}
    {-# INLINE ownString #-}
    {-# INLINE integer #-}
    {-# INLINE whole #-}
    {-# INLINE emptyContainer #-}
    {-# INLINE openObject #-}
    {-# INLINE beginMember #-}
    {-# INLINE addMember #-}
    {-# INLINE dropValue #-}
    {-# INLINE closeObject #-}
    {-# INLINE openArray #-}
    {-# INLINE addItem #-}
    {-# INLINE closeArray #-}

frameOf :: FileFields -> Frame
frameOf = \case
    FileFields frame _ _ -> frame
    HashFields frame _ -> frame

keyText :: MemberKey -> Text
keyText = \case
    SharedKey entry -> entryText entry
    OwnKey key -> Key.toText key

seenMember :: Text -> FileFields -> Bool
seenMember key = \case
    FileFields _ _ seen -> Set.member key seen
    HashFields _ hashes -> Map.member key hashes

scalarText :: FileValue -> Maybe Text
scalarText = \case
    ScalarValue (String text) -> Just text
    _ -> Nothing

reduceMember :: Text -> FileValue -> FileFacts -> FileFacts
reduceMember key value facts = case (key, value) of
    ("filename", _) -> facts{ffFilename = scalarText value}
    ("url", _) -> facts{ffUrl = scalarText value}
    ("hashes", HashValue hashes) -> facts{ffHashes = hashes}
    ("hashes", ScalarValue Null) -> facts{ffHashes = Just mempty}
    ("hashes", _) -> facts{ffHashes = Nothing}
    ("requires-python", ScalarValue scalar) -> updateOptionalFact (\text -> facts{ffRequiresPython = text}) scalar
    ("provenance", ScalarValue scalar) -> updateOptionalFact (\text -> facts{ffProvenance = text}) scalar
    ("size", ScalarValue scalar) -> facts{ffSize = force (parseMaybe parseJSON scalar)}
    ("upload-time", ScalarValue scalar) -> facts{ffUploadTime = force (parseMaybe parsePublishTime scalar)}
    ("yanked", ScalarValue scalar) -> facts{ffYanked = yankState (Just scalar)}
    _ -> facts
  where
    updateOptionalFact update scalar = maybe facts{ffValid = False} update (parseMaybe parseJSON scalar)

finishFacts :: FileFacts -> Maybe IndexFile
finishFacts facts = do
    guard (ffValid facts)
    name <- ffFilename facts
    url <- ffUrl facts
    hashes <- ffHashes facts
    pure
        IndexFile
            { ifEntryKey = SingletonEntry
            , ifFilename = name
            , ifUrl = url
            , ifHashes = hashes
            , ifRequiresPython = ffRequiresPython facts
            , ifSize = ffSize facts
            , ifUploadTime = ffUploadTime facts
            , ifYanked = ffYanked facts
            , ifProvenance = ffProvenance facts
            }
