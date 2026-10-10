-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Simple-index walks share source ordering and validation across tree and packed reads.
module Ecluse.Core.Registry.PyPI.Reader (
    pypiWalk,
    pypiFullWalk,
    fileUniqueFields,
) where

import Control.Monad.ST (ST)
import Data.Aeson (Value (Null, String))
import Data.Aeson.Key qualified as Key
import Data.JsonStream.TokenParser (Element (..), TokenResult)

import Ecluse.Core.Registry.Json.Intern (InternTable, nameBytes, nameText)
import Ecluse.Core.Registry.Json.Shape (Members, Mode (..), Shape (..), Trees (..), knownMembers, listedMember, namedMembers, readShape)
import Ecluse.Core.Registry.Json.Walk (FieldStep, Step, Steps (..), Walk, Walked (..), eachItem, eachMember, pureStep, skipFrom, skipRest, tooDeep, withElement)
import Ecluse.Core.Registry.Json.Walk qualified as Walk
import Ecluse.Core.Registry.PyPI.FileWriter (FileValue, FileWriter)
import Ecluse.Core.Registry.PyPI.Project (FilenameMemo, filenameMemo)
import Ecluse.Core.Registry.PyPI.Streaming (PyPIField (..), PyPIRead (..), SelectedFile (..), SelectedFileEvent (..), collectSelected, fileScalars, finishSelected, hashNames)
import Ecluse.Core.Security (LimitError)

-- | Members whose values differ in every file, so the table keeps them as read.
fileUniqueFields :: [Text]
fileUniqueFields = ["filename", "url", "hashes", "upload-time", "provenance"]

-- | Walk an index into trees, retaining the first occurrence and the selected-file skip policy.
pypiWalk :: Int -> PyPIRead -> (s -> PyPIField -> Either LimitError s) -> (s -> Bool) -> InternTable -> s -> TokenResult -> Step s
{-# INLINE pypiWalk #-}
pypiWalk depth mode step keeps = walkIndex depth mode (pureStep step) (\_ acc -> Finished acc) treeFile
  where
    treeFile table acc position element rest next =
        readShape Trees (ObjectOr Null (fileMembers depth)) (if keeps acc then Share else Keep) table element rest $ \payload held after ->
            Walk.emit (pureStep step) acc (FileField position (Just payload)) (\kept -> next kept held after)

-- | Walk a full index with typed reduction beside packing, returning the read's final table.
pypiFullWalk :: FileWriter st -> Int -> FieldStep s PyPIField (ST st (Steps (ST st) (Walked s))) -> (s -> Bool) -> (s -> Int -> FileValue -> (Either LimitError s -> ST st (Steps (ST st) (Walked s))) -> ST st (Steps (ST st) (Walked s))) -> InternTable -> s -> TokenResult -> ST st (Steps (ST st) (Walked s))
{-# INLINE pypiFullWalk #-}
pypiFullWalk writer depth step keeps fileStep = walkIndex depth FullRead step (\table acc -> pure (Finished (Walked table acc))) packedFile
  where
    packedFile table acc position element rest next =
        readShape writer (ObjectOr Null (fileMembers depth)) (if keeps acc then Share else Keep) table element rest $ \payload held after ->
            fileStep acc position payload (either Walk.refuse (\kept -> next kept held after))

walkIndex :: (Walk r) => Int -> PyPIRead -> FieldStep s PyPIField r -> (InternTable -> s -> r) -> (InternTable -> s -> Int -> Element -> TokenResult -> (s -> InternTable -> TokenResult -> r) -> r) -> InternTable -> s -> TokenResult -> r
{-# INLINE walkIndex #-}
walkIndex depth mode step finishRead readFile table0 initial = start
  where
    start tokens
        | depth <= 0 = withElement tokens tooDeep
        | otherwise = withElement tokens $ \element rest -> case element of
            ObjectBegin -> eachMember topField (\(Walked held acc) _ -> finishRead held acc) (Walked table0 initial) rest
            _ -> skipFrom element rest (const (finishRead table0 initial))
    full = case mode of
        FullRead -> True
        SelectedRead{} -> False
    emit = Walk.emit step
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
        | otherwise = readFile table acc position element rest (\kept held after -> continue (Walked held kept) after)

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

    fileFields = fileMembers depth
    hashFields = knownMembers hashNames (Scalar (depth - 4))
    metaFields = namedMembers ([("tracks", ArrayWith (depth - 2) (Scalar (depth - 3)) (Scalar (depth - 2))) | full] <> [("api-version", Scalar (depth - 2))] <> [("_last-serial", Scalar (depth - 2)) | full])
    statusFields = namedMembers [(key, Scalar (depth - 2)) | key <- ["status", "reason"]]

fileMembers :: Int -> Members
fileMembers depth = namedMembers (("hashes", ObjectWith (depth - 3) (knownMembers hashNames (Scalar (depth - 4))) (Scalar (depth - 3))) : [(key, Scalar (depth - 3)) | key <- fileScalars])

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

-- After a rejecting filename, only the lexer checks the remaining members.
selectedFile :: (Walk r) => Selection -> FilenameMemo -> InternTable -> Element -> TokenResult -> (Maybe Value -> FilenameMemo -> InternTable -> TokenResult -> r) -> r
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
