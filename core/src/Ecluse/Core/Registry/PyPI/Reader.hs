-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The Simple-index walk for full and selected reads. It emits the fields that
"Ecluse.Core.Registry.PyPI.Streaming" emits for the same mode, in the same order, and builds each
retained file once. A full read interns each file's keys and strings as read.
-}
module Ecluse.Core.Registry.PyPI.Reader (
    pypiWalk,
    fileUniqueFields,
) where

import Data.Aeson (Value (Null, String))
import Data.Aeson.Key qualified as Key
import Data.JsonStream.TokenParser (Element (..), TokenResult)

import Ecluse.Core.Registry.Json.Intern (InternTable, nameBytes, nameText)
import Ecluse.Core.Registry.Json.Shape (Mode (..), Shape (..), knownMembers, namedMembers, readShape)
import Ecluse.Core.Registry.Json.Walk (Step (..), Walked (..), eachItem, eachMember, skipFrom, tooDeep, withElement)
import Ecluse.Core.Registry.Json.Walk qualified as Walk
import Ecluse.Core.Registry.PyPI.Project (FileProject, fileProject)
import Ecluse.Core.Registry.PyPI.Streaming (PyPIField (..), PyPIRead (..), SelectedFile (..), SelectedFileEvent (..), collectSelected, fileScalars, finishSelected, hashNames)
import Ecluse.Core.Security (LimitError)

-- | Members whose values differ in every file, so the table keeps them as read.
fileUniqueFields :: [Text]
fileUniqueFields = ["filename", "url", "hashes", "upload-time", "provenance"]

{- | Walk one project's Simple index, passing each field to the step as it completes. Before it
reads a file in a full read, the walk asks whether the consumer keeps the next file. Only a kept
file's keys and strings enter the table.
-}
pypiWalk :: Int -> PyPIRead -> (s -> PyPIField -> Either LimitError s) -> (s -> Bool) -> InternTable -> s -> TokenResult -> Step s
pypiWalk depth mode step keeps table0 initial tokens
    | depth <= 0 = withElement tokens tooDeep
    | otherwise = withElement tokens $ \element rest -> case element of
        ObjectBegin -> eachMember topField (\(Walked _ acc) _ -> Finished acc) (Walked table0 initial) rest
        _ -> skipFrom element rest (const (Finished initial))
  where
    full = case mode of
        FullRead -> True
        SelectedRead{} -> False
    emit = Walk.emit step
    topField walked@(Walked table acc) name after continue = case nameBytes name of
        "name" -> envelope "name" (Scalar (depth - 1))
        "meta" -> envelope "meta" (ObjectWith (depth - 1) metaFields (Scalar (depth - 1)))
        "project-status" | full -> envelope "project-status" (ObjectWith (depth - 1) statusFields (Scalar (depth - 1)))
        "alternate-locations" | full -> envelope "alternate-locations" (ArrayWith (depth - 1) (Scalar (depth - 2)) (Scalar (depth - 1)))
        "files" -> shaped FilesShape (eachItem file) walked after continue
        "versions" | full -> shaped VersionsShape (eachItem version) walked after continue
        _ -> withElement after $ \element afterKey -> skipFrom element afterKey (continue walked)
      where
        envelope key shape = withElement after $ \element afterKey ->
            readShape shape Keep table element afterKey $ \value _ afterValue ->
                emit acc (EnvelopeField key value) (\acc' -> continue (Walked table acc') afterValue)

    -- The first array claims the key and its end yields an ignored field. Null claims it empty.
    shaped claim items (Walked table acc) after continue = withElement after $ \element rest -> case element of
        ArrayBegin -> emit acc (claim True) $ \claimed ->
            items (\(Walked table' acc') afterArray -> emit acc' IgnoredField (\ended -> continue (Walked table' ended) afterArray)) (Walked table claimed) rest
        JValue Null -> emit acc (claim True) (\claimed -> continue (Walked table claimed) rest)
        _ -> skipFrom element rest (\afterValue -> emit acc (claim False) (\marked -> continue (Walked table marked) afterValue))

    file (Walked table acc) position element rest continue
        | depth <= 2 = tooDeep element rest
        | otherwise = case mode of
            FullRead -> readShape (ObjectOr Null fileFields) (if keeps acc then Share else Keep) table element rest (retained Just)
            SelectedRead name wanted -> selectedFile (depth - 3) (fileProject name) wanted table element rest (retained id)
      where
        retained wrap payload table' afterValue = emit acc (FileField position (wrap payload)) (\acc' -> continue (Walked table' acc') afterValue)

    version (Walked table acc) position element rest continue =
        readShape (Scalar (depth - 2)) Keep table element rest $ \value _ afterValue ->
            let field = case value of
                    String _ -> IgnoredField
                    _ -> InvalidVersionField position value
             in emit acc field (\acc' -> continue (Walked table acc') afterValue)

    fileFields = namedMembers (("hashes", ObjectWith (depth - 3) (knownMembers hashNames (Scalar (depth - 4))) (Scalar (depth - 3))) : [(key, Scalar (depth - 3)) | key <- fileScalars])
    metaFields = namedMembers ([("tracks", ArrayWith (depth - 2) (Scalar (depth - 3)) (Scalar (depth - 2))) | full] <> [("api-version", Scalar (depth - 2))] <> [("_last-serial", Scalar (depth - 2)) | full])
    statusFields = namedMembers [(key, Scalar (depth - 2)) | key <- ["status", "reason"]]

data Selecting = Selecting !InternTable SelectedFile

-- json-stream's selected-file fold: every member event reaches the fold, even after the name rejects
-- the file. The file's texts keep their own copies, so a rejected file never enters the table.
selectedFile :: Int -> FileProject -> Text -> InternTable -> Element -> TokenResult -> (Maybe Value -> InternTable -> TokenResult -> Step s) -> Step s
selectedFile budget project wanted table0 element rest next = case element of
    ObjectBegin -> eachMember visit (\(Selecting table selected) after -> next (finishSelected selected) table after) (Selecting table0 (CandidateFile False [] Nothing False)) rest
    _ -> skipFrom element rest (next Nothing table0)
  where
    collect = collectSelected project wanted
    visit (Selecting table selected) name after continue = withElement after $ \value afterKey -> case nameBytes name of
        "hashes"
            | budget <= 0 -> tooDeep value afterKey
            | ObjectBegin <- value ->
                eachMember
                    hashField
                    (\(Selecting table' hashed) afterObject -> continue (Selecting table' (collect hashed HashesEnd)) afterObject)
                    (Selecting table (collect selected HashesStart))
                    afterKey
            | otherwise -> readShape (Scalar budget) Keep table value afterKey $ \scalar table' afterValue ->
                continue (Selecting table' (collect selected (HashesValue scalar))) afterValue
        bytes
            | bytes `elem` scalarNames -> readShape (Scalar budget) Keep table value afterKey $ \scalar table' afterValue ->
                continue (Selecting table' (collect selected (FileScalar (Key.fromText (nameText name)) scalar))) afterValue
            | otherwise -> skipFrom value afterKey (continue (Selecting table selected))
    hashField (Selecting table selected) name after continue = withElement after $ \value afterKey ->
        readShape (Scalar (budget - 1)) Keep table value afterKey $ \scalar table' afterValue ->
            continue (Selecting table' (collect selected (HashField (Key.fromText (nameText name)) scalar))) afterValue
    scalarNames = map encodeUtf8 fileScalars
