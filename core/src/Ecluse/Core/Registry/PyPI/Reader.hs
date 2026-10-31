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
import Data.Aeson (Value (Array, Null, Object, String))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.JsonStream.Lexer (Cursor, Element (..))
import Data.Vector qualified as V

import Ecluse.Core.Registry.Json.Intern (InternTable, nameBytes, nameText)
import Ecluse.Core.Registry.Json.Shape (Members, Mode (..), Shape (..), Trees (..), knownMembers, listedMember, namedMembers, prepareMembers, readShape)
import Ecluse.Core.Registry.Json.Walk (FieldStep, Step, Steps (..), Walk, Walked (..), eachItem, eachMember, pureStep, skipFrom, skipRest, tooDeep, withElement)
import Ecluse.Core.Registry.Json.Walk qualified as Walk
import Ecluse.Core.Registry.PyPI.FileWriter (FileValue, FileWriter)
import Ecluse.Core.Registry.PyPI.PendingSidecar (readPendingSidecar)
import Ecluse.Core.Registry.PyPI.Project (FilenameMemo, filenameMemo)
import Ecluse.Core.Registry.PyPI.Streaming (PyPIField (..), PyPIRead (..), SelectedFile (..), SelectedFileEvent (..), collectSelected, fileScalars, finishSelected, hashNames)
import Ecluse.Core.Security (LimitError)

-- | Members whose values differ in every file, so the table keeps them as read.
fileUniqueFields :: [Text]
fileUniqueFields = ["filename", "url", "hashes", "upload-time", "provenance", "core-metadata"]

-- | Walk an index into trees, retaining the first occurrence and the selected-file skip policy.
pypiWalk :: Int -> PyPIRead -> (s -> PyPIField -> Either LimitError s) -> (s -> Bool) -> (s -> Text -> Bool) -> InternTable -> s -> Cursor -> Step s
{-# INLINE pypiWalk #-}
pypiWalk depth mode step keeps claimed table0 = walkIndex depth mode (pureStep step) claimed (\_ acc -> Finished acc) treeFile table0
  where
    fileShape = ObjectOr Null (fileMembers depth (prepareMembers table0))
    treeFile table acc position element rest next =
        readShape Trees fileShape (if keeps acc then Share else Keep) table element rest $ \payload held after ->
            Walk.emit (pureStep step) acc (FileField position (Just payload)) (\kept -> next kept held after)

-- | Walk a full index with typed reduction beside packing, returning the read's final table.
pypiFullWalk :: FileWriter st -> Int -> FieldStep s PyPIField (ST st (Steps (ST st) (Walked s))) -> (s -> Bool) -> (s -> Text -> Bool) -> (s -> Int -> FileValue -> (Either LimitError s -> ST st (Steps (ST st) (Walked s))) -> ST st (Steps (ST st) (Walked s))) -> InternTable -> s -> Cursor -> ST st (Steps (ST st) (Walked s))
{-# INLINE pypiFullWalk #-}
pypiFullWalk writer depth step keeps claimed fileStep table0 = walkIndex depth FullRead step claimed (\table acc -> pure (Finished (Walked table acc))) packedFile table0
  where
    fileShape = ObjectOr Null (fileMembers depth (prepareMembers table0))
    packedFile table acc position element rest next =
        readShape writer fileShape (if keeps acc then Share else Keep) table element rest $ \payload held after ->
            fileStep acc position payload (either Walk.refuse (\kept -> next kept held after))

-- An envelope key the consumer has @claimed@ holds an earlier occurrence, so a repeat is read and dropped.
walkIndex :: (Walk r) => Int -> PyPIRead -> FieldStep s PyPIField r -> (s -> Text -> Bool) -> (InternTable -> s -> r) -> (InternTable -> s -> Int -> Element -> Cursor -> (s -> InternTable -> Cursor -> r) -> r) -> InternTable -> s -> Cursor -> r
{-# INLINE walkIndex #-}
walkIndex depth mode step claimed finishRead readIndexFile table0 initial = start
  where
    start tokens
        | depth <= 0 = withElement tokens tooDeep
        | otherwise = withElement tokens $ \element rest -> case element of
            ObjectBegin -> eachMember topField (\(Walked held acc) _ -> finishRead held acc) (Walked table0 initial) rest
            _ -> skipFrom element rest (const (finishRead table0 initial))
    full = case mode of
        FullRead -> True
        SelectedRead{} -> False
        MirrorRead{} -> True
    emit = Walk.emit step
    topField walked@(Walked table acc) name after continue = case nameBytes name of
        "name" -> envelope "name" (Scalar (depth - 1))
        "meta" -> withElement after $ \element afterKey -> case element of
            ObjectBegin
                | depth > 1, claimed acc "meta" -> eachMember (metaMember False table) (\(Fields held _) -> continue (Walked table held)) (Fields acc mempty) afterKey
                | depth > 1 -> eachMember (metaMember True table) (\(Fields held members) -> keep "meta" held (Object members)) (Fields acc mempty) afterKey
            _ -> envelopeValue "meta" (ObjectWith (depth - 1) metaFields (Scalar (depth - 1))) element afterKey
        "project-status" | full -> envelope "project-status" (ObjectWith (depth - 1) statusFields (Scalar (depth - 1)))
        "alternate-locations" | full -> withElement after $ \element afterKey -> case element of
            ArrayBegin
                | depth > 1, claimed acc "alternate-locations" -> dropItems (depth - 2) table afterKey (continue walked)
                | depth > 1 -> envelopeItems "alternate-locations" (depth - 2) table acc afterKey (keep "alternate-locations")
            _ -> envelopeValue "alternate-locations" (ArrayWith (depth - 1) (Scalar (depth - 2)) (Scalar (depth - 1))) element afterKey
        "files" -> shaped FilesShape files walked after continue
        "versions" | full -> shaped VersionsShape (eachItem version) walked after continue
        _ -> withElement after $ \element afterKey -> skipFrom element afterKey (continue walked)
      where
        envelope key shape = withElement after (envelopeValue key shape)
        envelopeValue key shape element afterKey =
            readShape Trees shape Keep table element afterKey $ \value _ -> keep key acc value
        keep key held value afterValue = emit held (EnvelopeField key value) (\acc' -> continue (Walked table acc') afterValue)

    -- Meta keeps its listed members, the first of each name, as an object read keeps them. A member
    -- it does not keep, every member of a meta object after the first included, is read and dropped.
    metaMember keeping table (Fields acc members) name after next = withElement after $ \element afterKey -> case listedMember metaFields name of
        Just (key, shape)
            | key == "tracks"
            , ArrayBegin <- element
            , depth > 2 ->
                if kept key
                    then envelopeItems "meta" (depth - 3) table acc afterKey $ \counted value -> next (Fields counted (KeyMap.insert key value members))
                    else dropItems (depth - 3) table afterKey (next (Fields acc members))
            | kept key -> readShape Trees shape Keep table element afterKey (\value _ -> next (Fields acc (KeyMap.insert key value members)))
            | otherwise -> readShape Trees shape Keep table element afterKey (\_ _ -> next (Fields acc members))
        Nothing -> skipFrom element afterKey (next (Fields acc members))
      where
        kept key = keeping && not (KeyMap.member key members)

    -- Each item reaches the step as it is read, so the entry floor counts it before its array closes.
    envelopeItems key budget table acc0 tokens done = eachItem item (\(Items acc values) -> done acc (Array (V.fromList (reverse values)))) (Items acc0 []) tokens
      where
        item (Items acc values) _ element rest next =
            readShape Trees (Scalar budget) Keep table element rest $ \value _ afterValue ->
                emit acc (EnvelopeItem key) (\counted -> next (Items counted (value : values)) afterValue)

    -- A repeated array's items are read as its first occurrence's are, so the nesting limit holds, and none is kept.
    dropItems budget table tokens done = eachItem item (const done) () tokens
      where
        item _ _ element rest next = readShape Trees (Scalar budget) Keep table element rest (\_ _ -> next ())

    -- The first array claims the key and its end yields an ignored field. Null claims it empty.
    shaped claim items (Walked table acc) after continue = withElement after $ \element rest -> case element of
        ArrayBegin -> emit acc (claim True) $ \claimed ->
            items (\(Walked table' acc') afterArray -> emit acc' IgnoredField (\ended -> continue (Walked table' ended) afterArray)) (Walked table claimed) rest
        JValue Null -> emit acc (claim True) (\claimed -> continue (Walked table claimed) rest)
        _ -> skipFrom element rest (\afterValue -> emit acc (claim False) (\marked -> continue (Walked table marked) afterValue))

    -- A selected read carries its filename memo from file to file of one array.
    files = case mode of
        FullRead -> eachItem file
        SelectedRead name wanted -> selected name wanted
        MirrorRead name wanted -> selected name wanted

    selected name wanted done (Walked table acc) =
        eachItem (candidate Selection{selWanted = wanted, selBudget = depth - 3, selFields = fileFields, selHashes = hashFields}) (\(Selecting table' _ acc') -> done (Walked table' acc')) (Selecting table (filenameMemo name) acc)

    file (Walked table acc) position element rest continue
        | depth <= 2 = tooDeep element rest
        | otherwise = readIndexFile table acc position element rest (\kept held after -> continue (Walked held kept) after)

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

    fileFields = fileMembers depth id
    hashFields = knownMembers hashNames (Scalar (depth - 4))
    metaFields = namedMembers ([("tracks", ArrayWith (depth - 2) (Scalar (depth - 3)) (Scalar (depth - 2))) | full] <> [("api-version", Scalar (depth - 2))] <> [("_last-serial", Scalar (depth - 2)) | full])
    statusFields = namedMembers [(key, Scalar (depth - 2)) | key <- ["status", "reason"]]

fileMembers :: Int -> (Members -> Members) -> Members
fileMembers depth prepare = prepare (namedMembers (digestMembers <> scalarMembers))
  where
    digestMembers = [(key, ObjectWith (depth - 3) hashFields (Scalar (depth - 3))) | key <- ["hashes", "core-metadata"]]
    scalarMembers = [(key, Scalar (depth - 3)) | key <- fileScalars]
    hashFields = prepare (knownMembers hashNames (Scalar (depth - 4)))

data Selection = Selection
    { selWanted :: Text
    , selBudget :: Int
    , selFields :: Members
    , selHashes :: Members
    }

-- Keep the filename memo scoped to the current files array.
data Selecting a = Selecting !InternTable !FilenameMemo a

-- The consumer's state beside an envelope object's members so far.
data Fields s = Fields s (KeyMap.KeyMap Value)

-- The consumer's state beside an envelope array's items so far, newest first.
data Items s = Items s [Value]

-- After a rejecting filename, only the lexer checks the remaining members.
selectedFile :: (Walk r) => Selection -> FilenameMemo -> InternTable -> Element -> Cursor -> (Maybe Value -> FilenameMemo -> InternTable -> Cursor -> r) -> r
{-# INLINEABLE selectedFile #-}
selectedFile Selection{selWanted = wanted, selBudget = budget, selFields = fields, selHashes = hashes} memo0 table0 element rest next = case element of
    ObjectBegin -> eachMember visit (\(Selecting table memo file) after -> next (finishSelected file) memo table after) (Selecting table0 memo0 (CandidateFile False [] Nothing False)) rest
    _ -> skipFrom element rest (next Nothing memo0 table0)
  where
    collect table memo file event = case collectSelected wanted memo file event of
        (collected, remembered) -> Selecting table remembered collected
    -- A budget with no level for a hash value reads a rejected file on, so its hashes meet the nesting limit.
    skipsRejected = budget > 1
    -- Selected files keep their own text and do not enter the intern table.
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
        "core-metadata" -> readPendingSidecar budget hashes table value afterKey $ \pending table' afterValue ->
            continue (collect table' memo file (FileScalar "core-metadata" pending)) afterValue
        _ -> case listedMember fields name of
            Just (key, shape) -> readShape Trees shape Keep table value afterKey $ \scalar table' afterValue ->
                case collect table' memo file (FileScalar key scalar) of
                    Selecting held remembered RejectedFile | skipsRejected -> skipRest 1 afterValue (next Nothing remembered held)
                    collected -> continue collected afterValue
            Nothing -> skipFrom value afterKey (continue (Selecting table memo file))
    hashField (Selecting table memo file) name after continue = withElement after $ \value afterKey ->
        readShape Trees (Scalar (budget - 1)) Keep table value afterKey $ \scalar table' afterValue ->
            continue (collect table' memo file (HashField (maybe (Key.fromText (nameText name)) fst (listedMember hashes name)) scalar)) afterValue
