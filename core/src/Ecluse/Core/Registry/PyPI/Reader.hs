-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TypeFamilies #-}

{- | The Simple-index walk for full and selected reads. It emits the fields of
"Ecluse.Core.Registry.PyPI.Streaming" in source order and builds each retained file once. A full
read interns each file's keys and strings as read.
-}
module Ecluse.Core.Registry.PyPI.Reader (
    pypiWalk,
    fileUniqueFields,
) where

import Data.Aeson (Value (Null, String))
import Data.Aeson.Key qualified as Key
import Data.JsonStream.TokenReader (Element (..), Tokens)

import Ecluse.Core.Registry.Json.Intern (InternTable, nameBytes, nameText)
import Ecluse.Core.Registry.Json.Shape (Members, Mode (..), Shape (..), Trees (..), knownMembers, listedMember, namedMembers, prepareMembers, readShape)
import Ecluse.Core.Registry.Json.Walk (Walk (..), Walked (..), eachItem, eachMember, pureStep, skipFrom, skipRest, tooDeep, withElement)
import Ecluse.Core.Registry.Json.Walk qualified as Walk
import Ecluse.Core.Registry.PyPI.Project (FilenameMemo, filenameMemo)
import Ecluse.Core.Registry.PyPI.Streaming (PyPIField (..), PyPIRead (..), SelectedFile (..), SelectedFileEvent (..), collectSelected, fileScalars, finishSelected, hashNames)
import Ecluse.Core.Security (LimitError)

-- | Members whose values differ in every file, so the table keeps them as read.
fileUniqueFields :: [Text]
fileUniqueFields = ["filename", "url", "hashes", "upload-time", "provenance"]

{- | Walk one project's Simple index, passing each field to the step as it completes. Only a file a
full read keeps enters the table, and only the first of each member it repeats.
-}
pypiWalk :: (Walk r, Result r ~ s) => Int -> PyPIRead -> (s -> PyPIField -> Either LimitError s) -> (s -> Bool) -> InternTable -> s -> Tokens (TokenState r) -> r
{-# INLINE pypiWalk #-}
pypiWalk depth mode step keeps table0 initial = start
  where
    start tokens
        | depth <= 0 = withElement tokens tooDeep
        | otherwise = withElement tokens $ \element rest -> case element of
            ObjectBegin -> eachMember topField (\(Walked _ acc) _ -> finish acc) (Walked table0 initial) rest
            _ -> skipFrom element rest (const (finish initial))
    full = case mode of
        FullRead -> True
        SelectedRead{} -> False
    emit = Walk.emit (pureStep step)
    topField walked@(Walked table acc) name after continue = case nameBytes name of
        "name" -> envelope "name" (Scalar (depth - 1))
        "meta" -> envelope "meta" (ObjectWith (depth - 1) metaFields (Scalar (depth - 1)))
        "project-status" | full -> envelope "project-status" (ObjectWith (depth - 1) statusFields (Scalar (depth - 1)))
        "alternate-locations" | full -> envelope "alternate-locations" (ArrayWith (depth - 1) (Scalar (depth - 2)) (Scalar (depth - 1)))
        "files" -> shaped FilesShape files walked after continue
        "versions" | full -> shaped VersionsShape (eachItem version) walked after continue
        _ -> withElement after $ \element afterKey -> skipFrom element afterKey (continue walked)
      where
        envelope key shape = withElement after $ \element afterKey ->
            readShape Trees shape Keep table element afterKey $ \value _ afterValue ->
                emit acc (EnvelopeField key value) (\acc' -> continue (Walked table acc') afterValue)

    -- The first array claims the key and its end yields an ignored field. Null claims it empty.
    shaped claim items (Walked table acc) after continue = withElement after $ \element rest -> case element of
        ArrayBegin -> emit acc (claim True) $ \claimed ->
            items (\(Walked table' acc') afterArray -> emit acc' IgnoredField (\ended -> continue (Walked table' ended) afterArray)) (Walked table claimed) rest
        JValue Null -> emit acc (claim True) (\claimed -> continue (Walked table claimed) rest)
        _ -> skipFrom element rest (\afterValue -> emit acc (claim False) (\marked -> continue (Walked table marked) afterValue))

    -- A selected read carries its filename memo from file to file of one array.
    files = case mode of
        FullRead -> eachItem file
        SelectedRead name wanted -> \done (Walked table acc) ->
            eachItem (candidate Selection{selWanted = wanted, selBudget = depth - 3, selFields = fileFields, selHashes = hashFields}) (\(Selecting table' _ acc') -> done (Walked table' acc')) (Selecting table (filenameMemo name) acc)

    file (Walked table acc) position element rest continue
        | depth <= 2 = tooDeep element rest
        | otherwise = readShape Trees (ObjectOr Null fileFields) (if keeps acc then Share else Keep) table element rest $ \payload table' afterValue ->
            emit acc (FileField position (Just payload)) (\acc' -> continue (Walked table' acc') afterValue)

    candidate selection (Selecting table memo acc) position element rest continue
        | depth <= 2 = tooDeep element rest
        | otherwise = selectedFile selection memo table element rest $ \payload memo' table' afterValue ->
            emit acc (FileField position payload) (\acc' -> continue (Selecting table' memo' acc') afterValue)

    version (Walked table acc) position element rest continue =
        readShape Trees (Scalar (depth - 2)) Keep table element rest $ \value _ afterValue ->
            let field = case value of
                    String _ -> IgnoredField
                    _ -> InvalidVersionField position value
             in emit acc field (\acc' -> continue (Walked table acc') afterValue)

    fileFields = prepare (namedMembers (("hashes", ObjectWith (depth - 3) hashFields (Scalar (depth - 3))) : [(key, Scalar (depth - 3)) | key <- fileScalars]))
    hashFields = prepare (knownMembers hashNames (Scalar (depth - 4)))
    prepare = if full then prepareMembers table0 else id
    metaFields = namedMembers ([("tracks", ArrayWith (depth - 2) (Scalar (depth - 3)) (Scalar (depth - 2))) | full] <> [("api-version", Scalar (depth - 2))] <> [("_last-serial", Scalar (depth - 2)) | full])
    statusFields = namedMembers [(key, Scalar (depth - 2)) | key <- ["status", "reason"]]

-- What a selected read fixes for every file.
data Selection = Selection
    { selWanted :: Text
    , selBudget :: Int
    , selFields :: Members
    , selHashes :: Members
    }

-- A selected read between tokens: the table, the filename memo, and the consumer's accumulator
-- between files or the file under selection within one.
data Selecting a = Selecting !InternTable !FilenameMemo a

-- A file is read up to the first name that rejects it, and its rest is skipped like any skipped value:
-- no member there is decoded, and only the lexer can fail the read. A file of the release is read whole.
selectedFile :: (Walk r) => Selection -> FilenameMemo -> InternTable -> Element -> Tokens (TokenState r) -> (Maybe Value -> FilenameMemo -> InternTable -> Tokens (TokenState r) -> r) -> r
{-# INLINEABLE selectedFile #-}
selectedFile Selection{selWanted = wanted, selBudget = budget, selFields = fields, selHashes = hashes} memo0 table0 element rest next = case element of
    ObjectBegin -> eachMember visit (\(Selecting table memo file) after -> next (finishSelected file) memo table after) (Selecting table0 memo0 (CandidateFile False [] Nothing False)) rest
    _ -> skipFrom element rest (next Nothing memo0 table0)
  where
    collect table memo file event = case collectSelected wanted memo file event of
        (collected, remembered) -> Selecting table remembered collected
    -- A budget with no level for a hash value reads a rejected file on, so its hashes meet the nesting limit.
    skipsRejected = budget > 1
    -- Listed keys come from the shapes and texts are copies, so no file enters the table.
    visit (Selecting table memo file) name after continue = withElement after $ \value afterKey -> case nameBytes name of
        "hashes"
            | budget <= 0 -> tooDeep value afterKey
            | ObjectBegin <- value ->
                eachMember
                    hashField
                    (\(Selecting table' memo' hashed) afterObject -> continue (collect table' memo' hashed HashesEnd) afterObject)
                    (collect table memo file HashesStart)
                    afterKey
            | otherwise -> readShape Trees (Scalar budget) Keep table value afterKey $ \scalar table' afterValue ->
                continue (collect table' memo file (HashesValue scalar)) afterValue
        _ -> case listedMember fields name of
            Just (key, shape) -> readShape Trees shape Keep table value afterKey $ \scalar table' afterValue ->
                case collect table' memo file (FileScalar key scalar) of
                    Selecting held remembered RejectedFile | skipsRejected -> skipRest 1 afterValue (next Nothing remembered held)
                    collected -> continue collected afterValue
            Nothing -> skipFrom value afterKey (continue (Selecting table memo file))
    hashField (Selecting table memo file) name after continue = withElement after $ \value afterKey ->
        readShape Trees (Scalar (budget - 1)) Keep table value afterKey $ \scalar table' afterValue ->
            continue (collect table' memo file (HashField (maybe (Key.fromText (nameText name)) fst (listedMember hashes name)) scalar)) afterValue
