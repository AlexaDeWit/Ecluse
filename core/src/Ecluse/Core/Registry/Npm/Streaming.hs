-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Select npm installation and policy fields before constructing decoded values.
module Ecluse.Core.Registry.Npm.Streaming (
    NpmRead (..),
    NpmField (..),
    NpmContainer (..),
    npmFields,
    versionFields,
    versionListFields,
) where

import Data.Aeson (Value (Array, Null, Number, Object, String))
import Data.JsonStream.Parser qualified as J
import Data.Text qualified as T

import Ecluse.Core.Registry.JsonStream (retainedArrayWith, retainedObjectOr, retainedObjectWith, retainedScalar, retainedValue, withinRetainedDepth)

-- | Full serving data, one release, or the fields needed to recognise usable version entries.
data NpmRead = FullRead | SelectedRead Text | VersionListRead
    deriving stock (Eq, Show)

-- | Independent top-level maps, with first-container precedence retained by their consumer.
data NpmContainer = VersionsContainer | TimeContainer | TagsContainer
    deriving stock (Eq, Ord, Show)

-- | Each field retains its source coordinate. Skipped releases still count towards the version cap.
data NpmField
    = IgnoredField
    | BeginContainer NpmContainer
    | InvalidContainer NpmContainer
    | NameField Value
    | VersionField Text (Maybe Value)
    | TimeField Text Value
    | TagField Text Value
    deriving stock (Eq, Show)

-- | Extract independent maps without assuming their ordering in the source.
npmFields :: Int -> NpmRead -> J.Parser NpmField
npmFields depth mode = withinRetainedDepth depth (J.objectKeyValues topField)
  where
    topField "name"
        | mode /= VersionListRead = NameField <$> scalar (depth - 1)
    topField "versions" = container VersionsContainer (J.objectKeyValues release)
    topField "time" = case mode of
        VersionListRead -> mempty
        SelectedRead target -> container TimeContainer (TimeField target <$> J.objectWithKey target (scalar (depth - 2)))
        FullRead -> container TimeContainer (J.objectKeyValues timestamp)
    topField "dist-tags" = case mode of
        VersionListRead -> mempty
        SelectedRead _ -> container TagsContainer (TagField "latest" <$> J.objectWithKey "latest" (scalar (depth - 2)))
        FullRead -> container TagsContainer (J.objectKeyValues tag)
    topField _ = mempty
    container slot parser =
        withinRetainedDepth (depth - 1) $
            J.objectFound (BeginContainer slot) IgnoredField parser
                <|> (BeginContainer slot <$ J.jNull)
                <|> pure (InvalidContainer slot)
    release key = case mode of
        SelectedRead target | key /= target -> pure (VersionField "" Nothing)
        VersionListRead -> VersionField (T.copy key) . Just <$> withinRetainedDepth (depth - 2) (retainedObjectOr Null listField)
        _ -> VersionField (T.copy key) . Just <$> withinRetainedDepth (depth - 2) (retainedObjectOr Null (field versionFields))
    timestamp key = TimeField (T.copy key) <$> scalar (depth - 2)
    tag key = TagField (T.copy key) <$> scalar (depth - 2)
    listField key
        | key == "name" || key == "version" = withinRetainedDepth (depth - 3) witness
        | key == "dist" = withinRetainedDepth (depth - 3) (retainedObjectOr Null (\slot -> if slot `elem` ["tarball", "shasum", "integrity"] then withinRetainedDepth (depth - 4) witness else mempty))
        | key == "scripts" = withinRetainedDepth (depth - 3) (stringMapWitness (depth - 4))
        | key == "deprecated" = withinRetainedDepth (depth - 3) (pure Null)
        | key `elem` versionListFields = field versionListFields key
        | otherwise = mempty
    witness = (String "" <$ J.string) <|> (Null <$ J.jNull) <|> pure (Number 0)
    field supported key
        | key == "_npmUser" = personValue ["name", "email", "url"] (depth - 3)
        | key == "license" = personValue ["type", "url"] (depth - 3)
        | key == "dist" = objectValue (depth - 3) distField
        | key == "peerDependenciesMeta" || key == "dependenciesMeta" = objectValue (depth - 3) (const (fixed ["optional"] (depth - 4)))
        | key == "directories" = fixed ["lib", "bin", "man", "doc", "example", "test"] (depth - 3)
        | key == "devEngines" = objectValue (depth - 3) devEngineField
        | key == "publishConfig" = objectValue (depth - 3) publishField
        | key == "workspaces" = withinRetainedDepth (depth - 3) (retainedArrayWith (objectValue (depth - 3) workspaceField) (scalar (depth - 4)))
        | key `elem` ["dependencies", "acceptDependencies", "devDependencies", "optionalDependencies", "peerDependencies", "engines", "scripts", "bin", "browser"] =
            objectValue (depth - 3) (const (scalar (depth - 4)))
        | key `elem` ["name", "version", "_hasShrinkwrap", "hasInstallScript", "deprecated", "main", "module", "type", "types", "typings", "gypfile", "preferGlobal", "packageManager", "engineStrict"] = scalar (depth - 3)
        | key `elem` supported = retainedValue (depth - 3)
        | otherwise = mempty
    personValue keys budget = withinRetainedDepth budget ((String . T.copy <$> J.string) <|> fixed keys budget)
    scalar budget = withinRetainedDepth budget (retainedScalar <|> pure (Array mempty))
    objectValue budget fields = withinRetainedDepth budget (retainedObjectWith (scalar budget) fields)
    arrayValue budget entry = withinRetainedDepth budget (retainedArrayWith (scalar budget) entry)
    fixed keys budget = objectValue budget (\key -> if key `elem` keys then scalar (budget - 1) else mempty)
    distField "signatures" = arrayValue (depth - 4) (fixed ["keyid", "sig"] (depth - 5))
    distField "attestations" = objectValue (depth - 4) attestationField
    distField key
        | key `elem` distFields = scalar (depth - 4)
        | otherwise = mempty
    attestationField "url" = scalar (depth - 5)
    attestationField "provenance" = fixed ["predicateType"] (depth - 5)
    attestationField _ = mempty
    devEngineField key
        | key `elem` ["cpu", "os", "libc", "runtime", "packageManager"] =
            withinRetainedDepth (depth - 4) (retainedArrayWith (fixed ["name", "version", "onFail"] (depth - 4)) (fixed ["name", "version", "onFail"] (depth - 5)))
        | otherwise = mempty
    publishField key
        | key `elem` ["registry", "tag", "access", "provenance", "ignore-scripts", "directory", "linkDirectory", "executableFiles", "main", "module", "types", "typings", "exports", "imports", "bin", "browser"] = retainedValue (depth - 4)
        | otherwise = mempty
    workspaceField key
        | key `elem` ["packages", "nohoist"] = arrayValue (depth - 4) (scalar (depth - 5))
        | otherwise = mempty

-- | Supported release fields for installation, runtime resolution, policy and mirrored publication.
versionFields :: [Text]
versionFields =
    versionListFields
        <> [ "dependencies"
           , "dependenciesMeta"
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

data StringMapShape = ValidStringMap | InvalidStringMap | NullStringMap

stringMapWitness :: Int -> J.Parser Value
stringMapWitness budget = result <$> J.foldI collect NullStringMap events
  where
    events =
        J.objectFound ValidStringMap ValidStringMap (J.objectValues (withinRetainedDepth budget ((ValidStringMap <$ J.string) <|> pure InvalidStringMap)))
            <|> (NullStringMap <$ J.jNull)
            <|> pure InvalidStringMap
    collect InvalidStringMap _ = InvalidStringMap
    collect _ next = next
    result ValidStringMap = Object mempty
    result InvalidStringMap = Number 0
    result NullStringMap = Null
