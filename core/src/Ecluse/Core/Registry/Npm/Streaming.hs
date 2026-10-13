-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | npm field events and retained field names shared by native reads and their reference oracle.
module Ecluse.Core.Registry.Npm.Streaming (
    NpmFieldOf (..),
    NpmField,
    NpmContainer (..),
    versionFields,
    versionListFields,
) where

import Data.Aeson (Value)

-- | Independent top-level maps, with first-container precedence retained by their consumer.
data NpmContainer = VersionsContainer | TimeContainer | TagsContainer
    deriving stock (Eq, Ord, Show)

-- | Each field retains its source coordinate. Skipped releases still count towards the version cap.
data NpmFieldOf release
    = IgnoredField
    | BeginContainer NpmContainer
    | InvalidContainer NpmContainer
    | NameField Value
    | VersionField Text (Maybe release)
    | TimeField Text Value
    | TagField Text Value
    deriving stock (Eq, Show)

-- | A field whose release is aeson's tree.
type NpmField = NpmFieldOf Value

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
