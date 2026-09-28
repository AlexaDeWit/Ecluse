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

import Data.Aeson (Value (Object, String), toEncoding)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Package.Merge (MergePlan (mpDistTags, mpTime), SourceId)
import Ecluse.Core.Registry.CachedDocument (CachedDoc, npmCached, npmPacked, npmRendered, rendered)
import Ecluse.Core.Registry.Json.Packed (Piece (..), Pieces (ObjectPieces), RenderPlan (..), holeText, renderPlan, replacement)
import Ecluse.Core.Registry.Npm.Document (PackedPackument (..))
import Ecluse.Core.Registry.Npm.Project (projectName)
import Ecluse.Core.Registry.Npm.Route (tarballPath)
import Ecluse.Core.Registry.ServedDocument (
    adjustField,
    assembleAcross,
    documentObject,
    objectField,
    overlayObjectSources,
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

{- | Rebase @dist.tarball@ through the given artifact URL renderer, keeping its filename.
Build the renderer only from a document name that 'npmDocumentName' admits.
-}
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
    Object (KeyMap.insert "versions" (Object survivingVersions) (servedMembers plan baseObject))
  where
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

-- The base's top-level members with the plan's tags and times, before the served versions go in.
servedMembers :: MergePlan -> KeyMap Value -> KeyMap Value
servedMembers plan baseObject =
    baseObject
        & KeyMap.insert "dist-tags" (Object distTags)
        & KeyMap.insert "time" (Object reconciledTime)
  where
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

{- | 'assembleMergedPackument' over packed full reads: the survivors render from their own sources'
tables, and each rebased tarball URL replaces its release's hole.
-}
assemblePackedPackument :: Text -> Map SourceId (Snapshot PackedPackument) -> MergePlan -> Maybe PackedPackument -> RenderPlan
assemblePackedPackument mountBase bySource plan base =
    RenderPlan (servedMembers plan baseObject) "versions" (ObjectPieces [(Key.toText version, served) | (version, served) <- KeyMap.toAscList survivors])
  where
    baseObject = maybe mempty packumentTop base
    servedUrl = servedTarballUrl mountBase <$> npmDocumentName baseObject
    survivors = KeyMap.fromList [(Key.fromText version, piece source packed) | (version, source, packed) <- overlayObjectSources packumentVersions bySource plan]
    piece source packed = Piece (packumentTable source) packed (replacement <$> (servedUrl >>= \render -> holeText (packumentTable source) packed >>= rebaseArtifactUrl render))

{- | npm's 'Ecluse.Core.Registry.Adapter.Capability.metadataAssemble', over npm's own boundary.
Packed sources and base render from their tables. Any other document goes through aeson's trees.
-}
assembleMergedDocument :: Text -> Map SourceId (Snapshot CachedDoc) -> MergePlan -> Maybe CachedDoc -> CachedDoc
assembleMergedDocument mountBase bySource plan base =
    case (traverse (traverse (snd npmPacked)) bySource, traverse (snd npmPacked) base) of
        (Just packed, Just packedBase) -> npmRendered (assemblePackedPackument mountBase packed plan packedBase)
        _ -> assembleAcross npmCached assembleMergedPackument mountBase bySource plan base

-- | npm's 'Ecluse.Core.Registry.Adapter.Capability.metadataSerialise'.
serialiseMergedDocument :: CachedDoc -> LByteString
serialiseMergedDocument doc = maybe (serialiseAcross (fmap toEncoding . snd npmCached) doc) (fromStrict . renderPlan) (rendered doc)

versionEntries :: Value -> KeyMap Value
versionEntries = fromMaybe mempty . objectField "versions" . documentObject

timeBookkeepingKeys :: [Text]
timeBookkeepingKeys = ["created", "modified"]

servedTarballUrl :: Text -> PackageName -> Text -> Maybe Text
servedTarballUrl mountBase name file = joinUrlPath mountBase <$> tarballPath name file
