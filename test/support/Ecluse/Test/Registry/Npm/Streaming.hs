-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Independent combinator oracle for npm metadata and version inventories.
module Ecluse.Test.Registry.Npm.Streaming (NpmRead (..), npmFields) where

import Data.Aeson (Value (Array, Null, Number, Object, String))
import Data.JsonStream.Parser qualified as J

import Ecluse.Core.Registry.JsonStream (everyMember, retainedArrayWith, retainedObjectWith, retainedScalar, retainedValue, withinRetainedDepth)
import Ecluse.Core.Registry.Npm.Streaming (NpmContainer (..), NpmField, NpmFieldOf (..), versionFields, versionListFields)
import Ecluse.Test.Registry.JsonStream (namedMembers, retainedObjectOr)

-- | Full serving data, one release, or the fields needed to recognise usable version entries.
data NpmRead = FullRead | SelectedRead Text | VersionListRead
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
        VersionListRead -> VersionField key . Just <$> withinRetainedDepth (depth - 2) (retainedObjectOr Null listFields)
        _ -> VersionField key . Just <$> withinRetainedDepth (depth - 2) (retainedObjectOr Null releaseFields)
    timestamp key = TimeField key <$> scalar (depth - 2)
    tag key = TagField key <$> scalar (depth - 2)
    -- The first entry wins, so witness shapes precede generic shapes.
    listFields = namedMembers (listWitnesses <> filter ((`elem` versionListFields) . fst) releaseEntries)
    releaseFields = namedMembers releaseEntries
    releaseEntries = shapedFields <> [(key, retainedValue (depth - 3)) | key <- versionFields]
    listWitnesses =
        [ ("name", withinRetainedDepth (depth - 3) witness)
        , ("version", withinRetainedDepth (depth - 3) witness)
        , ("dist", withinRetainedDepth (depth - 3) (retainedObjectOr Null (namedMembers [(slot, withinRetainedDepth (depth - 4) witness) | slot <- ["tarball", "shasum", "integrity"]])))
        , ("scripts", withinRetainedDepth (depth - 3) (stringMapWitness (depth - 4)))
        , ("deprecated", withinRetainedDepth (depth - 3) (pure Null))
        ]
    shapedFields =
        [ ("_npmUser", personValue ["name", "email", "url"] (depth - 3))
        , ("license", personValue ["type", "url"] (depth - 3))
        , ("dist", objectValue (depth - 3) distFields)
        , ("peerDependenciesMeta", dependencyMeta)
        , ("dependenciesMeta", dependencyMeta)
        , ("directories", fixed ["lib", "bin", "man", "doc", "example", "test"] (depth - 3))
        , ("devEngines", objectValue (depth - 3) (namedMembers [(key, devEngine) | key <- ["cpu", "os", "libc", "runtime", "packageManager"]]))
        , ("publishConfig", objectValue (depth - 3) publishFields)
        , ("workspaces", withinRetainedDepth (depth - 3) (retainedArrayWith (objectValue (depth - 3) workspaceFields) (scalar (depth - 4))))
        ]
            <> [ (key, objectValue (depth - 3) (everyMember (scalar (depth - 4))))
               | key <- ["dependencies", "acceptDependencies", "devDependencies", "optionalDependencies", "peerDependencies", "engines", "scripts", "bin", "browser"]
               ]
            <> [ (key, scalar (depth - 3))
               | key <- ["name", "version", "_hasShrinkwrap", "hasInstallScript", "deprecated", "main", "module", "type", "types", "typings", "gypfile", "preferGlobal", "packageManager", "engineStrict"]
               ]
    witness = (String "" <$ J.string) <|> (Null <$ J.jNull) <|> pure (Number 0)
    personValue keys budget = withinRetainedDepth budget ((String <$> J.string) <|> fixed keys budget)
    scalar budget = withinRetainedDepth budget (retainedScalar <|> pure (Array mempty))
    objectValue budget members = withinRetainedDepth budget (retainedObjectWith (scalar budget) members)
    arrayValue budget entry = withinRetainedDepth budget (retainedArrayWith (scalar budget) entry)
    fixed keys budget = objectValue budget (namedMembers [(key, scalar (budget - 1)) | key <- keys])
    distFields =
        namedMembers
            ( [ ("signatures", arrayValue (depth - 4) (fixed ["keyid", "sig"] (depth - 5)))
              , ("attestations", objectValue (depth - 4) (namedMembers [("url", scalar (depth - 5)), ("provenance", fixed ["predicateType"] (depth - 5))]))
              ]
                <> [(key, scalar (depth - 4)) | key <- distScalars]
            )
    dependencyMeta = objectValue (depth - 3) (everyMember (fixed ["optional"] (depth - 4)))
    devEngine = withinRetainedDepth (depth - 4) (retainedArrayWith (fixed ["name", "version", "onFail"] (depth - 4)) (fixed ["name", "version", "onFail"] (depth - 5)))
    publishFields = namedMembers [(key, retainedValue (depth - 4)) | key <- ["registry", "tag", "access", "provenance", "ignore-scripts", "directory", "linkDirectory", "executableFiles", "main", "module", "types", "typings", "exports", "imports", "bin", "browser"]]
    workspaceFields = namedMembers [(key, arrayValue (depth - 4) (scalar (depth - 5))) | key <- ["packages", "nohoist"]]

distScalars :: [Text]
distScalars = ["tarball", "shasum", "integrity", "unpackedSize", "fileCount"]

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
