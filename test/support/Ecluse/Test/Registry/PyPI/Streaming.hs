-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The json-stream field parser that Simple-index reads ran before the token walk, kept as the
reference for "Ecluse.Core.Registry.PyPI.Reader". Member and hash names keep their own keys here
and each file starts its own filename memo, which leaves every value equal. A selected read here
decodes every member of every file, where the walk skips the rest of a file its name rejects.
-}
module Ecluse.Test.Registry.PyPI.Streaming (pypiFields) where

import Data.Aeson (Value (Array, Null, String))
import Data.Aeson.Key qualified as Key
import Data.JsonStream.Parser qualified as J

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.JsonStream (everyMember, namedMembers, retainedArrayWith, retainedObjectOr, retainedObjectWith, retainedScalar, retainedValue)
import Ecluse.Core.Registry.PyPI.Project (filenameMemo)
import Ecluse.Core.Registry.PyPI.Streaming (PyPIField (..), PyPIRead (..), SelectedFile (..), SelectedFileEvent (..), collectSelected, fileScalars, finishSelected)

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
            SelectedRead name wanted -> selectedFile (depth - 3) name wanted
    versions = case mode of
        SelectedRead{} -> mempty
        FullRead ->
            J.arrayFound (VersionsShape True) IgnoredField (version <$> J.indexedArrayOf (scalar (depth - 2)))
                <|> (VersionsShape True <$ J.jNull)
                <|> pure (VersionsShape False)
    version (_, String _) = IgnoredField
    version (position, value) = InvalidVersionField position value
    fileFields = namedMembers (("hashes", objectOrScalar (depth - 3) (everyMember (scalar (depth - 4)))) : [(key, scalar (depth - 3)) | key <- fileScalars])
    metaFields = namedMembers [("tracks", fullOnly (arrayOrScalar (depth - 2))), ("api-version", scalar (depth - 2)), ("_last-serial", fullOnly (scalar (depth - 2)))]
    statusFields = namedMembers [(key, scalar (depth - 2)) | key <- ["status", "reason"]]

scalar :: Int -> J.Parser Value
scalar budget
    | budget <= 0 = retainedValue 0
    | otherwise = retainedScalar <|> pure (Array mempty)

isFileScalar :: Text -> Bool
isFileScalar key = key `elem` fileScalars

selectedFile :: Int -> PackageName -> Text -> J.Parser (Maybe Value)
selectedFile budget name wanted = finishSelected . fst <$> J.foldI collect initial (J.objectKeyValues field)
  where
    initial = (CandidateFile False [] Nothing False, filenameMemo name)
    collect (file, memo) = collectSelected wanted memo file
    field "hashes"
        | budget <= 0 = HashesValue <$> retainedValue 0
        | otherwise =
            J.objectFound HashesStart HashesEnd (J.objectKeyValues hashField)
                <|> (HashesValue <$> scalar budget)
    field key
        | isFileScalar key = FileScalar (Key.fromText key) <$> scalar budget
        | otherwise = mempty
    hashField key = HashField (Key.fromText key) <$> scalar (budget - 1)
