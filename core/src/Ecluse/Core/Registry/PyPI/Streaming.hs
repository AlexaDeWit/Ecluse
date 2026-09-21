-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Extract supported Simple-index fields without retaining unknown fields or metadata sidecars.
module Ecluse.Core.Registry.PyPI.Streaming (
    PyPIRead (..),
    PyPIField (..),
    pypiFields,
) where

import Data.Aeson (Value (Array, Null, Object, String))
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
    scalar budget
        | budget <= 0 = retainedValue 0
        | otherwise = retainedScalar <|> pure (Array mempty)
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
    file = select <$> if depth <= 2 then retainedValue 0 else retainedObjectOr Null fileField
    select raw = case mode of
        FullRead -> Just raw
        SelectedRead name wanted -> case raw of
            Object fields | Just (String filename) <- KeyMap.lookup "filename" fields, fileVersionKey name filename == Just wanted -> Just raw
            _ -> Nothing
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
        | key `elem` ["filename", "url", "requires-python", "size", "upload-time", "yanked", "provenance"] = scalar (depth - 3)
        | otherwise = mempty
    metaField "tracks" = fullOnly (arrayOrScalar (depth - 2))
    metaField key
        | key == "api-version" = scalar (depth - 2)
        | key == "_last-serial" = fullOnly (scalar (depth - 2))
        | otherwise = mempty
    statusField key
        | key `elem` ["status", "reason"] = scalar (depth - 2)
        | otherwise = mempty
