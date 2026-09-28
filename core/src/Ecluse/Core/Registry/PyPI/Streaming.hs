-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Extract supported Simple-index fields without retaining unknown fields or metadata sidecars.
Selected reads retain pending fields only until the first filename excludes the requested release.
-}
module Ecluse.Core.Registry.PyPI.Streaming (
    PyPIRead (..),
    PyPIField (..),
    pypiFields,
    fileScalars,
    hashNames,
    SelectedFileEvent (..),
    SelectedFile (..),
    collectSelected,
    finishSelected,
) where

import Data.Aeson (Value (Array, Null, Object, String))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.JsonStream.Parser qualified as J
import Data.Universe.Class qualified as Universe

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Package.Hash (HashAlg (SRI), renderHashAlg)
import Ecluse.Core.Registry.JsonStream (knownMembers, namedMembers, retainedArrayWith, retainedObjectOr, retainedObjectWith, retainedScalar, retainedValue)
import Ecluse.Core.Registry.PyPI.Project (FileProject, fileProject, fileVersionKey)

-- | Select all files or one canonical release while counting every input file.
data PyPIRead = FullRead | SelectedRead PackageName Text
    deriving stock (Eq, Show)

-- | File events carry original positions, including items that yield no retained payload.
data PyPIField
    = EnvelopeField Text Value
    | FilesShape Bool
    | FileField Int (Maybe Value)
    | VersionsShape Bool
    | InvalidVersionField Int Value
    | IgnoredField
    deriving stock (Eq, Show)

-- | Read supported fields in any member order. Empty first containers still claim their keys.
pypiFields :: Int -> PyPIRead -> J.Parser PyPIField
pypiFields depth mode
    | depth <= 0 = IgnoredField <$ retainedValue 0
    | otherwise = J.objectKeyValues topField
  where
    topField "name" = EnvelopeField "name" <$> scalar (depth - 1)
    topField "meta" = EnvelopeField "meta" <$> objectOrScalar (depth - 1) metaFields
    topField "project-status" = fullOnly (EnvelopeField "project-status" <$> objectOrScalar (depth - 1) statusFields)
    topField "alternate-locations" = fullOnly (EnvelopeField "alternate-locations" <$> arrayOrScalar (depth - 1))
    topField "files" = files
    topField "versions" = versions
    topField _ = mempty
    fullOnly parser = case mode of
        FullRead -> parser
        SelectedRead{} -> mempty
    objectOrScalar budget fields
        | budget <= 0 = retainedValue 0
        | otherwise = retainedObjectWith (scalar budget) fields
    arrayOrScalar budget
        | budget <= 0 = retainedValue 0
        | otherwise = retainedArrayWith (scalar budget) (scalar (budget - 1))
    files =
        J.arrayFound (FilesShape True) IgnoredField (uncurry FileField <$> J.indexedArrayOf file)
            <|> (FilesShape True <$ J.jNull)
            <|> pure (FilesShape False)
    file
        | depth <= 2 = Nothing <$ retainedValue 0
        | otherwise = case mode of
            FullRead -> Just <$> retainedObjectOr Null fileFields
            SelectedRead name wanted -> selectedFile (depth - 3) (fileProject name) wanted
    versions = case mode of
        SelectedRead{} -> mempty
        FullRead ->
            J.arrayFound (VersionsShape True) IgnoredField (version <$> J.indexedArrayOf (scalar (depth - 2)))
                <|> (VersionsShape True <$ J.jNull)
                <|> pure (VersionsShape False)
    version (_, String _) = IgnoredField
    version (position, value) = InvalidVersionField position value
    -- Built once per read, so every file shares its field and hash algorithm names.
    fileFields = namedMembers (("hashes", objectOrScalar (depth - 3) (knownMembers hashNames (scalar (depth - 4)))) : [(key, scalar (depth - 3)) | key <- fileScalars])
    metaFields = namedMembers [("tracks", fullOnly (arrayOrScalar (depth - 2))), ("api-version", scalar (depth - 2)), ("_last-serial", fullOnly (scalar (depth - 2)))]
    statusFields = namedMembers [(key, scalar (depth - 2)) | key <- ["status", "reason"]]

scalar :: Int -> J.Parser Value
scalar budget
    | budget <= 0 = retainedValue 0
    | otherwise = retainedScalar <|> pure (Array mempty)

-- | The scalar members a file retains.
fileScalars :: [Text]
fileScalars = ["filename", "url", "requires-python", "size", "upload-time", "yanked", "provenance"]

isFileScalar :: Text -> Bool
isFileScalar key = key `elem` fileScalars

-- | The digest names a file's @hashes@ object can use. Any other name keeps its own key.
hashNames :: [Text]
hashNames = [renderHashAlg alg | alg <- Universe.universe, alg /= SRI]

-- | One member event of a file object that a selected read inspects.
data SelectedFileEvent
    = FileScalar Key.Key Value
    | HashesStart
    | HashesEnd
    | HashField Key.Key Value
    | HashesValue Value

-- | A file under selection: rejected by its name, or a candidate with its fields so far.
data SelectedFile
    = RejectedFile
    | CandidateFile Bool [(Key.Key, Value)] (Maybe Value) Bool

selectedFile :: Int -> FileProject -> Text -> J.Parser (Maybe Value)
selectedFile budget project wanted = finishSelected <$> J.foldI (collectSelected project wanted) initial (J.objectKeyValues field)
  where
    initial = CandidateFile False [] Nothing False
    field "hashes"
        | budget <= 0 = HashesValue <$> retainedValue 0
        | otherwise =
            J.objectFound HashesStart HashesEnd (J.objectKeyValues hashField)
                <|> (HashesValue <$> scalar budget)
    field key
        | isFileScalar key = FileScalar (Key.fromText key) <$> scalar budget
        | otherwise = mempty
    hashField key = HashField (Key.fromText key) <$> scalar (budget - 1)

-- | Fold one member event into a file under selection. The first of each field wins.
collectSelected :: FileProject -> Text -> SelectedFile -> SelectedFileEvent -> SelectedFile
collectSelected _ _ RejectedFile _ = RejectedFile
collectSelected project wanted current@(CandidateFile matched scalars hashes active) event = case event of
    FileScalar key value
        | any ((== key) . fst) scalars -> current
        | key == "filename" -> case value of
            String filename
                | fileVersionKey project filename == Just wanted ->
                    CandidateFile True ((key, value) : scalars) hashes active
            _ -> RejectedFile
        | otherwise -> CandidateFile matched ((key, value) : scalars) hashes active
    HashesStart ->
        CandidateFile matched scalars (hashes <|> Just (Object mempty)) (isNothing hashes)
    HashesEnd -> CandidateFile matched scalars hashes False
    HashesValue value -> CandidateFile matched scalars (hashes <|> Just value) active
    HashField key value
        | active
        , Just (Object fields) <- hashes
        , not (KeyMap.member key fields) ->
            CandidateFile matched scalars (Just (Object (KeyMap.insert key value fields))) active
        | otherwise -> current

-- | The retained object of a file the requested release names.
finishSelected :: SelectedFile -> Maybe Value
finishSelected (CandidateFile True fields hashes _) =
    Just (Object (KeyMap.fromList (maybeToList ((,) "hashes" <$> hashes) <> fields)))
finishSelected _ = Nothing
