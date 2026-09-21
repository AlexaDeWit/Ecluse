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
) where

import Data.Aeson (Value (Array, Null, Object, String))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.JsonStream.Parser qualified as J

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.JsonStream (retainedArrayWith, retainedObjectOr, retainedObjectWith, retainedScalar, retainedValue)
import Ecluse.Core.Registry.PyPI.Project (fileVersionKey)

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
    topField "meta" = EnvelopeField "meta" <$> objectOrScalar (depth - 1) metaField
    topField "project-status" = fullOnly (EnvelopeField "project-status" <$> objectOrScalar (depth - 1) statusField)
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
            FullRead -> Just <$> retainedObjectOr Null fileField
            SelectedRead name wanted -> selectedFile (depth - 3) name wanted
    versions = case mode of
        SelectedRead{} -> mempty
        FullRead ->
            J.arrayFound (VersionsShape True) IgnoredField (version <$> J.indexedArrayOf (scalar (depth - 2)))
                <|> (VersionsShape True <$ J.jNull)
                <|> pure (VersionsShape False)
    version (_, String _) = IgnoredField
    version (position, value) = InvalidVersionField position value
    fileField "hashes" = objectOrScalar (depth - 3) (const (scalar (depth - 4)))
    fileField key
        | isFileScalar key = scalar (depth - 3)
        | otherwise = mempty
    metaField "tracks" = fullOnly (arrayOrScalar (depth - 2))
    metaField key
        | key == "api-version" = scalar (depth - 2)
        | key == "_last-serial" = fullOnly (scalar (depth - 2))
        | otherwise = mempty
    statusField key
        | key `elem` ["status", "reason"] = scalar (depth - 2)
        | otherwise = mempty

scalar :: Int -> J.Parser Value
scalar budget
    | budget <= 0 = retainedValue 0
    | otherwise = retainedScalar <|> pure (Array mempty)

isFileScalar :: Text -> Bool
isFileScalar key = key `elem` ["filename", "url", "requires-python", "size", "upload-time", "yanked", "provenance"]

data SelectedFileEvent
    = FileScalar Key.Key Value
    | HashesStart
    | HashesEnd
    | HashField Key.Key Value
    | HashesValue Value

data SelectedFile
    = RejectedFile
    | CandidateFile Bool [(Key.Key, Value)] (Maybe Value) Bool

selectedFile :: Int -> PackageName -> Text -> J.Parser (Maybe Value)
selectedFile budget name wanted = finishSelected <$> J.foldI (collectSelected name wanted) initial (J.objectKeyValues field)
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

collectSelected :: PackageName -> Text -> SelectedFile -> SelectedFileEvent -> SelectedFile
collectSelected _ _ RejectedFile _ = RejectedFile
collectSelected name wanted current@(CandidateFile matched scalars hashes active) event = case event of
    FileScalar key value
        | any ((== key) . fst) scalars -> current
        | key == "filename" -> case value of
            String filename
                | fileVersionKey name filename == Just wanted ->
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

finishSelected :: SelectedFile -> Maybe Value
finishSelected (CandidateFile True fields hashes _) =
    Just (Object (KeyMap.fromList (maybeToList ((,) "hashes" <$> hashes) <> fields)))
finishSelected _ = Nothing
