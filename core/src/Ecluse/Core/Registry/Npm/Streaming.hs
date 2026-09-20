-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Select npm installation and policy fields before constructing decoded values.
module Ecluse.Core.Registry.Npm.Streaming (
    NpmRead (..),
    NpmField (..),
    npmFields,
    versionFields,
    versionListFields,
) where

import Data.Aeson (Value (Array, Null, Number, Object, String))
import Data.JsonStream.Parser qualified as J
import Data.Text qualified as T

import Ecluse.Core.Registry.JsonStream (retainedArray, retainedObject, retainedScalar, retainedValue)

-- | Full serving data, one release, or the fields needed to recognise usable version entries.
data NpmRead = FullRead | SelectedRead Text | VersionListRead
    deriving stock (Eq, Show)

-- | Each field retains its source coordinate. Skipped releases still count towards the version cap.
data NpmField
    = IgnoredField
    | NameField Value
    | VersionField Text (Maybe Value)
    | TimeField Text Value
    | TagField Text Value
    deriving stock (Eq, Show)

-- | Extract independent maps without assuming their ordering in the source.
npmFields :: Int -> NpmRead -> J.Parser NpmField
npmFields depth mode =
    (if mode == VersionListRead then mempty else NameField <$> J.objectWithKey "name" value)
        <> J.objectWithKey "versions" (pure IgnoredField <> J.objectKeyValues release)
        <> J.objectWithKey "time" (pure IgnoredField <> J.objectKeyValues timestamp)
        <> J.objectWithKey "dist-tags" (pure IgnoredField <> J.objectKeyValues tag)
  where
    value = scalar (depth - 1)
    release key =
        VersionField (T.copy key) <$> case mode of
            SelectedRead target | key /= target -> pure Nothing
            VersionListRead -> (Just <$> retainedObject listField) <|> pure (Just Null)
            _ -> (Just <$> retainedObject (field versionFields)) <|> pure (Just Null)
    timestamp key
        | wantedTime key = TimeField (T.copy key) <$> value
        | otherwise = mempty
    tag key
        | wantedTag key = TagField (T.copy key) <$> value
        | otherwise = mempty
    wantedTime key = case mode of
        FullRead -> True
        SelectedRead target -> key == target
        VersionListRead -> False
    wantedTag key = case mode of
        FullRead -> True
        SelectedRead _ -> key == "latest"
        VersionListRead -> False
    listField key
        | key == "name" || key == "version" = witness
        | key == "dist" = retainedObject (\slot -> if slot `elem` ["tarball", "shasum", "integrity"] then witness else mempty) <|> pure Null
        | key == "scripts" = stringMapWitness <|> (Null <$ J.jNull) <|> pure (Number 0)
        | key == "deprecated" = pure Null
        | key `elem` versionListFields = field versionListFields key
        | otherwise = mempty
    witness = (String "" <$ J.string) <|> (Null <$ J.jNull) <|> pure (Number 0)
    field supported key
        | key == "_npmUser" = personValue ["name", "email", "url"]
        | key == "license" = personValue ["type", "url"]
        | key == "dist" = retainedObject distField <|> scalar (depth - 3)
        | key == "peerDependenciesMeta" = retainedObject (const (fixed ["optional"] (depth - 5))) <|> scalar (depth - 3)
        | key == "directories" = fixed ["lib", "bin", "man", "doc", "example", "test"] (depth - 4)
        | key == "devEngines" = retainedObject devEngineField <|> scalar (depth - 3)
        | key == "publishConfig" = retainedObject publishField <|> scalar (depth - 3)
        | key == "workspaces" = retainedArray (scalar (depth - 4)) <|> retainedObject workspaceField <|> scalar (depth - 3)
        | key `elem` ["dependencies", "acceptDependencies", "devDependencies", "optionalDependencies", "peerDependencies", "engines", "scripts", "bin", "browser"] =
            retainedObject (const (scalar (depth - 4))) <|> scalar (depth - 3)
        | key `elem` ["name", "version", "_hasShrinkwrap", "hasInstallScript", "deprecated", "main", "module", "type", "types", "typings", "gypfile", "preferGlobal", "packageManager", "engineStrict"] = scalar (depth - 3)
        | key `elem` supported = retainedValue (depth - 3)
        | otherwise = mempty
    personValue keys = (String . T.copy <$> J.string) <|> fixed keys (depth - 4)
    scalar budget
        | budget <= 0 = retainedValue budget
        | otherwise = retainedScalar <|> pure (Array mempty)
    fixed keys budget = retainedObject (\key -> if key `elem` keys then scalar budget else mempty) <|> scalar budget
    distField "signatures" = retainedArray (fixed ["keyid", "sig"] (depth - 6)) <|> scalar (depth - 4)
    distField "attestations" = retainedObject attestationField <|> scalar (depth - 4)
    distField key
        | key `elem` distFields = scalar (depth - 4)
        | otherwise = mempty
    attestationField "url" = scalar (depth - 5)
    attestationField "provenance" = fixed ["predicateType"] (depth - 6)
    attestationField _ = mempty
    devEngineField key
        | key `elem` ["cpu", "os", "libc", "runtime", "packageManager"] =
            retainedArray (fixed ["name", "version", "onFail"] (depth - 6)) <|> fixed ["name", "version", "onFail"] (depth - 5)
        | otherwise = mempty
    publishField key
        | key `elem` ["registry", "tag", "access", "provenance", "ignore-scripts", "directory", "linkDirectory", "executableFiles", "main", "module", "types", "typings", "exports", "imports", "bin", "browser"] = retainedValue (depth - 4)
        | otherwise = mempty
    workspaceField key
        | key `elem` ["packages", "nohoist"] = retainedArray (scalar (depth - 5)) <|> scalar (depth - 4)
        | otherwise = mempty

-- | Supported release fields for installation, runtime resolution, policy and mirrored publication.
versionFields :: [Text]
versionFields =
    versionListFields
        <> [ "dependencies"
           , "acceptDependencies"
           , "_hasShrinkwrap"
           , "devDependencies"
           , "optionalDependencies"
           , "peerDependencies"
           , "peerDependenciesMeta"
           , "bundleDependencies"
           , "bundledDependencies"
           , "engines"
           , "engineStrict"
           , "os"
           , "cpu"
           , "libc"
           , "bin"
           , "man"
           , "directories"
           , "main"
           , "module"
           , "browser"
           , "exports"
           , "imports"
           , "type"
           , "types"
           , "typings"
           , "typesVersions"
           , "files"
           , "gypfile"
           , "preferGlobal"
           , "config"
           , "workspaces"
           , "packageManager"
           , "devEngines"
           , "publishConfig"
           , "sideEffects"
           ]

-- | VersionEntry's required fields and optional discriminators, excluding installer-only fields.
versionListFields :: [Text]
versionListFields = ["name", "version", "dist", "deprecated", "hasInstallScript", "scripts", "license", "_npmUser"]

distFields :: [Text]
distFields = ["tarball", "shasum", "integrity", "unpackedSize", "fileCount", "signatures", "attestations"]

stringMapWitness :: J.Parser Value
stringMapWitness = J.catMaybeI (fmap result <$> J.foldI collect Nothing events)
  where
    events = J.objectFound True True (J.objectValues ((True <$ J.string) <|> pure False))
    collect valid next = Just (fromMaybe True valid && next)
    result True = Object mempty
    result False = Number 0
