-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE TypeFamilies #-}

{- | The packument walk for full and selected reads. It emits the fields that
"Ecluse.Core.Registry.Npm.Streaming" emits for the same mode, in the same order, and builds each
retained release once with its keys and strings interned as read.
-}
module Ecluse.Core.Registry.Npm.Reader (
    PackumentRead (..),
    npmWalk,
    releaseUniqueFields,
) where

import Data.Aeson (Value (Null))
import Data.JsonStream.TokenParser (Element (..), TokenResult)

import Ecluse.Core.Registry.Json.Intern (InternTable, Interned (..), entryText, internName, nameBytes, nameText)
import Ecluse.Core.Registry.Json.Shape (Build (..), Members, Mode (..), Shape (..), Trees (..), everyMember, namedMembers, prepareMembers, readShape)
import Ecluse.Core.Registry.Json.Walk (FieldStep, Walk (..), Walked (..), eachMember, skipFrom, skipRest, tooDeep, withElement)
import Ecluse.Core.Registry.Json.Walk qualified as Walk
import Ecluse.Core.Registry.Npm.Streaming (NpmContainer (..), NpmFieldOf (..), versionFields)

-- | The whole packument, or one release with its timestamp and the latest tag.
data PackumentRead = WholePackument | OneRelease Text
    deriving stock (Eq, Show)

{- | Members whose values differ in every release, so the table keeps them as read. A name matches
at any depth, so a rarer member such as @_npmUser.url@ is kept as read too.
-}
releaseUniqueFields :: [Text]
releaseUniqueFields = ["tarball", "shasum", "integrity", "sig", "url"]

{- | Walk one packument into the builder, passing each field to the step as it completes. Only a
release the consumer keeps enters the table, and only the first of each member it repeats.
-}
npmWalk :: (Build b r, Result r ~ Walked s) => b -> Int -> PackumentRead -> FieldStep s (NpmFieldOf (Built b)) r -> (s -> Text -> Bool) -> InternTable -> s -> TokenResult -> r
{-# INLINE npmWalk #-}
npmWalk build depth mode step keeps table0 initial = start
  where
    start tokens
        | depth <= 0 = withElement tokens tooDeep
        | otherwise = withElement tokens $ \element rest -> case element of
            ObjectBegin -> eachMember topField (\walked _ -> finish walked) (Walked table0 initial) rest
            _ -> skipFrom element rest (const (finish (Walked table0 initial)))
    topField (Walked table acc) name after continue = case nameBytes name of
        "name" -> withElement after $ \element rest ->
            readShape Trees (Scalar (depth - 1)) Keep table element rest $ \field _ afterValue ->
                emit acc (NameField field) (\acc' -> continue (Walked table acc') afterValue)
        "versions" -> container VersionsContainer table acc after continue (releases table)
        "time" -> container TimeContainer table acc after continue $ case mode of
            WholePackument -> everyScalar TimeField table
            OneRelease version -> oneScalar (encodeUtf8 version) (TimeField version) table
        "dist-tags" -> container TagsContainer table acc after continue $ case mode of
            WholePackument -> everyScalar TagField table
            OneRelease _ -> oneScalar "latest" (TagField "latest") table
        _ -> withElement after $ \element afterKey -> skipFrom element afterKey (continue (Walked table acc))
    emit = Walk.emit step
    target = case mode of
        OneRelease version -> Just (encodeUtf8 version)
        WholePackument -> Nothing

    -- A container's first object claims it. Null claims it empty, and any other value marks it invalid.
    container slot table acc after continue inner = withElement after $ \element rest ->
        if depth - 1 <= 0
            then tooDeep element rest
            else case element of
                ObjectBegin -> emit acc (BeginContainer slot) $ \begun ->
                    inner begun rest $ \(Walked table' acc') afterObject ->
                        emit acc' IgnoredField (\ended -> continue (Walked table' ended) afterObject)
                JValue Null -> emit acc (BeginContainer slot) (\begun -> continue (Walked table begun) rest)
                _ -> skipFrom element rest (\afterValue -> emit acc (InvalidContainer slot) (\marked -> continue (Walked table marked) afterValue))

    releases table acc rest done = eachMember release done (Walked table acc) rest
    release (Walked table acc) key after continue = case target of
        Just wanted | nameBytes key /= wanted -> withElement after $ \element rest ->
            skipFrom element rest (\afterValue -> emit acc (VersionField "" Nothing) (\acc' -> continue (Walked table acc') afterValue))
        _
            | keeps acc text -> case internName key table of
                Interned entry keyed -> withElement after $ \element rest ->
                    readShape build releaseShape Share keyed element rest $ \release' table' afterValue ->
                        emit acc (VersionField (entryText entry) (Just release')) (\acc' -> continue (Walked table' acc') afterValue)
            | otherwise -> withElement after $ \element rest ->
                readShape build releaseShape Keep table element rest $ \release' _ afterValue ->
                    emit acc (VersionField text (Just release')) (\acc' -> continue (Walked table acc') afterValue)
      where
        text = nameText key

    everyScalar field table acc rest done = eachMember (timestamp field) done (Walked table acc) rest
    timestamp field (Walked table acc) key after continue = withElement after $ \element rest ->
        readShape Trees (Scalar (depth - 2)) Keep table element rest $ \scalar _ afterValue ->
            emit acc (field (nameText key) scalar) (\acc' -> continue (Walked table acc') afterValue)

    -- json-stream's objectWithKey: the first match yields, and the rest of the object is skipped unread.
    oneScalar wanted field table acc rest done = eachMember visit done (Walked table acc) rest
      where
        visit (Walked held current) key after continue
            | nameBytes key == wanted = withElement after $ \element afterKey ->
                readShape Trees (Scalar (depth - 2)) Keep held element afterKey $ \scalar _ afterValue ->
                    emit current (field scalar) (skipRest 1 afterValue . done . Walked held)
            | otherwise = withElement after $ \element afterKey -> skipFrom element afterKey (continue (Walked held current))

    releaseShape = Checked (depth - 2) (ObjectOr Null (releaseMembers table0 depth))

-- The release fields "Ecluse.Core.Registry.Npm.Streaming" retains for full and selected reads.
releaseMembers :: InternTable -> Int -> Members
releaseMembers table depth = named (shapedFields <> [(key, Generic (depth - 3)) | key <- versionFields])
  where
    named = prepareMembers table . namedMembers
    shapedFields =
        [ ("_npmUser", personValue ["name", "email", "url"] (depth - 3))
        , ("license", personValue ["type", "url"] (depth - 3))
        , ("dist", objectValue (depth - 3) distFields)
        , ("peerDependenciesMeta", dependencyMeta)
        , ("dependenciesMeta", dependencyMeta)
        , ("directories", fixed ["lib", "bin", "man", "doc", "example", "test"] (depth - 3))
        , ("devEngines", objectValue (depth - 3) (named [(key, devEngine) | key <- ["cpu", "os", "libc", "runtime", "packageManager"]]))
        , ("publishConfig", objectValue (depth - 3) publishFields)
        , ("workspaces", ArrayWith (depth - 3) (Scalar (depth - 4)) (objectValue (depth - 3) workspaceFields))
        ]
            <> [ (key, objectValue (depth - 3) (everyMember (Scalar (depth - 4))))
               | key <- ["dependencies", "acceptDependencies", "devDependencies", "optionalDependencies", "peerDependencies", "engines", "scripts", "bin", "browser"]
               ]
            <> [ (key, Scalar (depth - 3))
               | key <- ["name", "version", "_hasShrinkwrap", "hasInstallScript", "deprecated", "main", "module", "type", "types", "typings", "gypfile", "preferGlobal", "packageManager", "engineStrict"]
               ]
    personValue keys budget = StringOr budget (fixed keys budget)
    objectValue budget members = ObjectWith budget members (Scalar budget)
    arrayValue budget entry = ArrayWith budget entry (Scalar budget)
    fixed keys budget = objectValue budget (named [(key, Scalar (budget - 1)) | key <- keys])
    distFields =
        named
            ( [ ("signatures", arrayValue (depth - 4) (fixed ["keyid", "sig"] (depth - 5)))
              , ("attestations", objectValue (depth - 4) (named [("url", Scalar (depth - 5)), ("provenance", fixed ["predicateType"] (depth - 5))]))
              ]
                <> [(key, Scalar (depth - 4)) | key <- ["tarball", "shasum", "integrity", "unpackedSize", "fileCount"]]
            )
    dependencyMeta = objectValue (depth - 3) (everyMember (fixed ["optional"] (depth - 4)))
    devEngine = ArrayWith (depth - 4) (fixed ["name", "version", "onFail"] (depth - 5)) (fixed ["name", "version", "onFail"] (depth - 4))
    publishFields = named [(key, Generic (depth - 4)) | key <- ["registry", "tag", "access", "provenance", "ignore-scripts", "directory", "linkDirectory", "executableFiles", "main", "module", "types", "typings", "exports", "imports", "bin", "browser"]]
    workspaceFields = named [(key, arrayValue (depth - 4) (Scalar (depth - 5))) | key <- ["packages", "nohoist"]]
