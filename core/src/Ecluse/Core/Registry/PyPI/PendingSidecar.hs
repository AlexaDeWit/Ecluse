-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TypeFamilies #-}

-- | Delay companion values until a selected filename survives, while checking duplicate keys as read.
module Ecluse.Core.Registry.PyPI.PendingSidecar (readPendingSidecar) where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.JsonStream.Lexer (Cursor, Element (ObjectBegin, ObjectEnd, StringPlain), plainText)
import Data.JsonStream.Lexer qualified as Lexer
import Data.Vector qualified as Vector

import Ecluse.Core.Registry.Json.Intern (InternTable, entryString, nameText, plainName)
import Ecluse.Core.Registry.Json.Shape (Build (..), Members, Mode (Keep), Shape (ObjectWith, Scalar), listedMember, memberText, readShape)
import Ecluse.Core.Registry.Json.Walk (Walk)

{- | Read one plain singleton digest directly, or resume the general reader from the untouched cursor.
The members must retain every digest as a scalar with one less level of structural budget.
-}
readPendingSidecar :: (Walk r) => Int -> Members -> InternTable -> Element -> Cursor -> (Value -> InternTable -> Cursor -> r) -> r
readPendingSidecar budget members table element rest next
    | budget > 1
    , ObjectBegin <- element
    , Lexer.Token (StringPlain key) afterKey <- Lexer.next rest
    , Lexer.Token (StringPlain digest) afterDigest <- Lexer.next afterKey
    , Lexer.Token ObjectEnd afterObject <- Lexer.next afterDigest =
        let name = plainName key
            algorithm = maybe (Key.fromText (nameText name)) fst (listedMember members name)
         in next (Object (KeyMap.singleton algorithm (String (plainText digest)))) table afterObject
    | otherwise = readShape PendingSidecar (ObjectWith budget members (Scalar budget)) Keep table element rest next
{-# INLINEABLE readPendingSidecar #-}

-- | A tree builder for a scalar or digest object read in Keep mode.
data PendingSidecar = PendingSidecar

data PendingFields
    = NoFields
    | OneField Key.Key ~Value
    | ManyFields (KeyMap.KeyMap (() -> Value))

instance (Walk r) => Build PendingSidecar r where
    type Built PendingSidecar = Value
    type Fields PendingSidecar = PendingFields
    type Items PendingSidecar = [Value]
    sharedString _ entry next = next (entryString entry)
    ownString _ name next = next (String (nameText name))
    integer _ number next = next (Number (fromIntegral number))
    whole _ value next = next value
    emptyContainer _ next = next (Array mempty)
    openObject _ next = next NoFields
    beginMember _ key fields next = next (contains (Key.fromText (memberText key)) fields)
    addMember _ key value fields next = next (insertField (Key.fromText (memberText key)) value fields)
    dropValue _ _ next = next
    closeObject _ fields next = next (Object (finishFields fields))
    openArray _ next = next []
    addItem _ value values next = next (value : values)
    closeArray _ count values next = next (Array (Vector.fromListN count (reverse values)))
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

contains :: Key.Key -> PendingFields -> Bool
contains _ NoFields = False
contains key (OneField held _) = key == held
contains key (ManyFields fields) = KeyMap.member key fields

insertField :: Key.Key -> Value -> PendingFields -> PendingFields
insertField key value = \case
    NoFields -> OneField key value
    OneField held previous -> ManyFields (KeyMap.fromList [(held, const previous), (key, const value)])
    ManyFields fields -> ManyFields (KeyMap.insert key (const value) fields)

finishFields :: PendingFields -> KeyMap.KeyMap Value
finishFields = \case
    NoFields -> mempty
    OneField key value -> KeyMap.singleton key value
    ManyFields fields -> KeyMap.map ($ ()) fields
