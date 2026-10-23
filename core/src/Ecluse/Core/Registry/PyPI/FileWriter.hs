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
import Data.Aeson.Types (parseMaybe)
import Data.Bits ((.&.), (.|.))
import Data.Map.Strict qualified as Map
import Data.Primitive.MutVar (MutVar, newMutVar, readMutVar, writeMutVar)
import Data.Primitive.PrimVar (PrimVar, newPrimVar, readPrimVar, writePrimVar)
import Data.Time (UTCTime)

import Ecluse.Core.Package (CoreMetadata (..))
import Ecluse.Core.Package.Entry (EntryKey (SingletonEntry))
import Ecluse.Core.Registry.Json.Intern (entryText, nameText)
import Ecluse.Core.Registry.Json.Shape (Build (..), MemberKey (..), memberText)
import Ecluse.Core.Registry.Json.Walk (Steps)
import Ecluse.Core.Registry.Json.Writer (Frame, Writer)
import Ecluse.Core.Registry.PyPI.Wire (IndexFile (..), YankState (FileOffered), parseCoreMetadata, yankState)
import Ecluse.Core.Registry.WireSupport (parsePublishTime)

-- | A writer with the object depth that tells file fields from digest members, and the open file's facts.
data FileWriter st = FileWriter (Writer st) (PrimVar st Int) (FactSlots st)

-- | Attach typed reduction to a read's existing scratch writer.
fileWriter :: Writer st -> ST st (FileWriter st)
fileWriter writer = FileWriter writer <$> newPrimVar 0 <*> newFactSlots

-- | Completed facts keep no error tree after a failed typed decode.
data FileValue
    = -- | Text already decoded by the walk.
      TextValue Text
    | -- | An integer the walk already represents as 'Int'.
      IntegerValue Int
    | -- | Other scalars and the empty-array container fallback.
      ScalarValue Value
    | -- | Digest text by algorithm, or a failed digest value.
      HashValue (Maybe (Map Text Text))
    | -- | A decoded file, or a failure whose diagnostic needs the packed payload.
      FileValue (Maybe IndexFile)

-- Failed digests reserve their keys so a duplicate cannot repair the first value.
data FileFields
    = FileFields Frame {-# UNPACK #-} !Word16
    | HashFields Frame (Map Text Text) Bool

-- One slot per fact. A file object resets them as it opens, each member writes one, and the close reads them.
data FactSlots st = FactSlots
    { slotFilename :: MutVar st (Maybe Text)
    , slotUrl :: MutVar st (Maybe Text)
    , slotHashes :: MutVar st (Maybe (Map Text Text))
    , slotRequiresPython :: MutVar st (Maybe Text)
    , slotSize :: MutVar st (Maybe Int)
    , slotUploadTime :: MutVar st (Maybe UTCTime)
    , slotYanked :: MutVar st YankState
    , slotProvenance :: MutVar st (Maybe Text)
    , slotCoreMetadata :: MutVar st (Maybe CoreMetadata)
    , slotValid :: MutVar st Bool
    }

-- Slots start invalid, so they yield no file before the first reset.
newFactSlots :: ST st (FactSlots st)
newFactSlots =
    FactSlots
        <$> newMutVar Nothing
        <*> newMutVar Nothing
        <*> newMutVar Nothing
        <*> newMutVar Nothing
        <*> newMutVar Nothing
        <*> newMutVar Nothing
        <*> newMutVar FileOffered
        <*> newMutVar Nothing
        <*> newMutVar Nothing
        <*> newMutVar False

resetFacts :: FactSlots st -> ST st ()
resetFacts slots = do
    writeMutVar (slotFilename slots) Nothing
    writeMutVar (slotUrl slots) Nothing
    writeMutVar (slotHashes slots) (Just mempty)
    writeMutVar (slotRequiresPython slots) Nothing
    writeMutVar (slotSize slots) Nothing
    writeMutVar (slotUploadTime slots) Nothing
    writeMutVar (slotYanked slots) FileOffered
    writeMutVar (slotProvenance slots) Nothing
    writeMutVar (slotCoreMetadata slots) Nothing
    writeMutVar (slotValid slots) True

-- A slot holds an evaluated fact, never a thunk over the member's value.
setFact :: MutVar st a -> a -> ST st ()
setFact slot !fact = writeMutVar slot fact

instance Build (FileWriter st) (ST st (Steps (ST st) s)) where
    type Built (FileWriter st) = FileValue
    type Fields (FileWriter st) = FileFields
    type Items (FileWriter st) = Int
    sharedString (FileWriter writer _ _) entry next = sharedString writer entry (\() -> next (TextValue (entryText entry)))
    ownString (FileWriter writer _ _) name next = ownString writer name (\() -> next (TextValue (nameText name)))
    integer (FileWriter writer _ _) number next = integer writer number (\() -> next (IntegerValue number))
    whole (FileWriter writer _ _) value next = whole writer value (\() -> next (ScalarValue value))
    emptyContainer (FileWriter writer _ _) next = emptyContainer writer (\() -> next (ScalarValue (Array mempty)))
    openObject (FileWriter writer depth slots) next = do
        level <- readPrimVar depth
        writePrimVar depth (level + 1)
        if level == 0
            then resetFacts slots >> openObject writer (\frame -> next (FileFields frame 0))
            else openObject writer (\frame -> next (HashFields frame mempty True))
    beginMember (FileWriter writer _ _) key fields next = beginMember writer key (frameOf fields) $ \repeated ->
        next (repeated || seenMember key fields)
    addMember (FileWriter writer _ slots) key value fields next = addMember writer key () (frameOf fields) $ \frame -> case fields of
        FileFields _ seen -> do
            reduceMember slots (memberText key) value
            next $! FileFields frame $ case key of
                OwnKey _ -> seen .|. fileFieldBit (memberText key)
                SharedKey _ -> seen
        HashFields _ hashes valid ->
            let text = scalarText value
             in next $! HashFields frame (Map.insert (memberText key) (fromMaybe "" text) hashes) (valid && isJust text)
    dropValue (FileWriter writer _ _) _ = dropValue writer ()
    closeObject (FileWriter writer depth slots) fields next = do
        level <- readPrimVar depth
        writePrimVar depth (level - 1)
        closeObject writer (frameOf fields) $ \() -> case fields of
            FileFields{} -> finishFacts slots >>= \file -> next $! FileValue file
            HashFields _ hashes valid -> next $! HashValue (if valid then Just hashes else Nothing)
    openArray (FileWriter writer _ _) = openArray writer
    addItem (FileWriter writer _ _) _ = addItem writer ()
    closeArray (FileWriter writer _ _) count start next = closeArray writer count start (\() -> next (ScalarValue (Array mempty)))
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
    FileFields frame _ -> frame
    HashFields frame _ _ -> frame

seenMember :: MemberKey -> FileFields -> Bool
seenMember key fields = case (key, fields) of
    (OwnKey _, FileFields _ seen) -> seen .&. fileFieldBit (memberText key) /= 0
    (_, HashFields _ hashes _) -> Map.member (memberText key) hashes
    _ -> False

fileFieldBit :: Text -> Word16
fileFieldBit = \case
    "filename" -> 0x01
    "url" -> 0x02
    "hashes" -> 0x04
    "requires-python" -> 0x08
    "size" -> 0x10
    "upload-time" -> 0x20
    "yanked" -> 0x40
    "provenance" -> 0x80
    "core-metadata" -> 0x100
    _ -> 0

scalarText :: FileValue -> Maybe Text
scalarText = \case
    TextValue text -> Just text
    ScalarValue (String text) -> Just text
    _ -> Nothing

reduceMember :: FactSlots st -> Text -> FileValue -> ST st ()
reduceMember slots key value = case (key, value) of
    ("filename", _) -> setFact (slotFilename slots) (scalarText value)
    ("url", _) -> setFact (slotUrl slots) (scalarText value)
    ("hashes", HashValue hashes) -> setFact (slotHashes slots) hashes
    ("hashes", ScalarValue Null) -> setFact (slotHashes slots) (Just mempty)
    ("hashes", _) -> setFact (slotHashes slots) Nothing
    ("core-metadata", HashValue (Just hashes)) -> setFact (slotCoreMetadata slots) (Just (MetadataDigests hashes))
    ("core-metadata", ScalarValue scalar) -> maybe invalid (setFact (slotCoreMetadata slots) . Just) (parseMaybe parseCoreMetadata scalar)
    ("core-metadata", _) -> invalid
    ("requires-python", _) -> optionalFact (slotRequiresPython slots)
    ("provenance", _) -> optionalFact (slotProvenance slots)
    ("size", IntegerValue number) -> setFact (slotSize slots) (Just number)
    ("size", _) -> withScalar (setFact (slotSize slots) . force . parseMaybe parseJSON)
    ("upload-time", _) -> withScalar (setFact (slotUploadTime slots) . force . parseMaybe parsePublishTime)
    ("yanked", _) -> withScalar (setFact (slotYanked slots) . yankState . Just)
    _ -> pass
  where
    invalid = setFact (slotValid slots) False
    withScalar update = case value of
        TextValue text -> update (String text)
        IntegerValue number -> update (Number (fromIntegral number))
        ScalarValue scalar -> update scalar
        _ -> pass
    optionalFact slot = case value of
        ScalarValue Null -> setFact slot Nothing
        HashValue{} -> pass
        FileValue{} -> pass
        _ -> maybe invalid (setFact slot . Just) (scalarText value)

finishFacts :: FactSlots st -> ST st (Maybe IndexFile)
finishFacts slots = do
    valid <- readMutVar (slotValid slots)
    required <- (,,) <$> readMutVar (slotFilename slots) <*> readMutVar (slotUrl slots) <*> readMutVar (slotHashes slots)
    case required of
        (Just name, Just url, Just hashes) | valid -> do
            requiresPython <- readMutVar (slotRequiresPython slots)
            size <- readMutVar (slotSize slots)
            uploadTime <- readMutVar (slotUploadTime slots)
            yanked <- readMutVar (slotYanked slots)
            coreMetadata <- readMutVar (slotCoreMetadata slots)
            provenance <- readMutVar (slotProvenance slots)
            let !file =
                    IndexFile
                        { ifEntryKey = SingletonEntry
                        , ifFilename = name
                        , ifUrl = url
                        , ifHashes = hashes
                        , ifRequiresPython = requiresPython
                        , ifSize = size
                        , ifUploadTime = uploadTime
                        , ifYanked = yanked
                        , ifCoreMetadata = coreMetadata
                        , ifProvenance = provenance
                        }
            pure (Just file)
        _ -> pure Nothing
