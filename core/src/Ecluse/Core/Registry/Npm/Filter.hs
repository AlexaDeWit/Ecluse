-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Assemble admitted npm installation metadata from source snapshots and rebase artifact URLs.
module Ecluse.Core.Registry.Npm.Filter (
    -- * URL rewriting
    rewriteVersion,

    -- * Assembling the served document
    assembleMergedPackument,
    npmDocumentName,

    -- * The served-document boundary (npm's 'CachedDoc' capabilities)
    assembleMergedDocument,
    serialiseMergedDocument,
) where

import Data.Aeson (Value (Object, String))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Package.Merge (MergePlan (mpDistTags, mpTime), SourceId)
import Ecluse.Core.Registry.CachedDocument (CachedDoc, npmCached)
import Ecluse.Core.Registry.Npm.Project (projectName)
import Ecluse.Core.Registry.Npm.Route (tarballPath)
import Ecluse.Core.Registry.ServedDocument (
    adjustField,
    assembleAcross,
    documentObject,
    overlayObjectSurvivors,
    rebaseArtifactUrl,
    safeDocumentName,
    serialiseAcross,
    stringField,
 )
import Ecluse.Core.Snapshot (Snapshot)
import Ecluse.Core.Text (joinUrlPath, renderIso8601Utc)
import Ecluse.Core.Version (renderVersion)

-- | The packument's own @name@, safety-gated before it is interpolated into a rewritten path.
npmDocumentName :: KeyMap Value -> Maybe PackageName
npmDocumentName = safeDocumentName (rightToMaybe . projectName)

-- | Rebase @dist.tarball@ while preserving its filename. Gate the prefix through 'npmDocumentName'.
rewriteVersion :: (Text -> Maybe Text) -> Value -> Value
rewriteVersion servedUrl = \case
    Object vo -> Object (adjustField "dist" (rewriteDist servedUrl) vo)
    other -> other

-- A @dist@ with no readable file name is left unchanged.
rewriteDist :: (Text -> Maybe Text) -> Value -> Value
rewriteDist servedUrl = \case
    Object dist
        | Just url <- stringField "tarball" dist
        , Just rebased <- rebaseArtifactUrl servedUrl url ->
            Object (KeyMap.insert "tarball" (String rebased) dist)
    other -> other

-- | The plan supplies versions, tags and timestamps. Other top-level fields come from the base.
assembleMergedPackument :: Text -> Map SourceId (Snapshot Value) -> MergePlan -> Value -> Value
assembleMergedPackument mountBase bySource plan base =
    Object rebuilt
  where
    rebuilt :: KeyMap Value
    rebuilt =
        baseObject
            & KeyMap.insert "versions" (Object survivingVersions)
            & KeyMap.insert "dist-tags" (Object distTags)
            & KeyMap.insert "time" (Object reconciledTime)

    baseObject :: KeyMap Value
    baseObject = documentObject base

    -- An unusable document name must never enter a rewritten URL.
    rewriteSurvivor :: Value -> Value
    rewriteSurvivor = maybe id (rewriteVersion . servedTarballUrl mountBase) (npmDocumentName baseObject)

    -- A missing source object drops the survivor instead of inventing installation metadata.
    survivingVersions :: KeyMap Value
    survivingVersions =
        KeyMap.fromList
            [ (Key.fromText version, rewriteSurvivor object)
            | (version, object) <- overlayObjectSurvivors versionEntries bySource plan
            ]

    distTags :: KeyMap Value
    distTags =
        KeyMap.fromList
            [ (Key.fromText tag, String (renderVersion v))
            | (tag, v) <- Map.toList (mpDistTags plan)
            ]

    reconciledTime :: KeyMap Value
    reconciledTime =
        bookkeepingTime
            <> KeyMap.fromList
                [ (Key.fromText version, String (renderIso8601Utc t))
                | (version, t) <- Map.toList (mpTime plan)
                ]

    bookkeepingTime :: KeyMap Value
    bookkeepingTime = case KeyMap.lookup "time" baseObject of
        Just (Object timeObject) ->
            KeyMap.fromList
                [ (k, value)
                | name <- timeBookkeepingKeys
                , let k = Key.fromText name
                , Just value <- [KeyMap.lookup k timeObject]
                ]
        _ -> mempty

-- | npm's 'Ecluse.Core.Registry.Adapter.Capability.metadataAssemble', over npm's own boundary.
assembleMergedDocument :: Text -> Map SourceId (Snapshot CachedDoc) -> MergePlan -> Maybe CachedDoc -> CachedDoc
assembleMergedDocument = assembleAcross npmCached assembleMergedPackument

-- | npm's 'Ecluse.Core.Registry.Adapter.Capability.metadataSerialise'.
serialiseMergedDocument :: CachedDoc -> LByteString
serialiseMergedDocument = serialiseAcross (snd npmCached)

versionEntries :: Value -> KeyMap Value
versionEntries = \case
    Object o
        | Just (Object versions) <- KeyMap.lookup "versions" o ->
            versions
    _ -> mempty

timeBookkeepingKeys :: [Text]
timeBookkeepingKeys = ["created", "modified"]

servedTarballUrl :: Text -> PackageName -> Text -> Maybe Text
servedTarballUrl mountBase name file = joinUrlPath mountBase <$> tarballPath name file
