-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The fields a Simple-index read passes to its projection, and the fold that selects one release's
files. Selected reads retain pending fields only until the first filename excludes the release.
-}
module Ecluse.Core.Registry.PyPI.Streaming (
    PyPIRead (..),
    PyPIField (..),
    fileScalars,
    hashNames,
    SelectedFileEvent (..),
    SelectedFile (..),
    collectSelected,
    finishSelected,
) where

import Data.Aeson (Value (Object, String))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Universe.Class qualified as Universe

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Package.Hash (HashAlg (SRI), renderHashAlg)
import Ecluse.Core.Registry.PyPI.Project (FilenameMemo, fcVersionKey, readLatestCoordinate)

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

-- | The scalar members a file retains.
fileScalars :: [Text]
fileScalars = ["filename", "url", "requires-python", "size", "upload-time", "yanked", "provenance"]

-- | The digest names a file's @hashes@ object can use. Any other name keeps its own key.
hashNames :: [Text]
hashNames = [renderHashAlg alg | alg <- Universe.universe, alg /= SRI]

-- | One member event of a file object that a selected read inspects.
data SelectedFileEvent
    = FileScalar Key.Key Value
    | HashesStart
    | HashesEnd
    | HashField Key.Key Value
    | HashesValue Value

-- | A file under selection: rejected by its name, or a candidate with its fields so far.
data SelectedFile
    = RejectedFile
    | CandidateFile Bool [(Key.Key, Value)] (Maybe Value) Bool

{- | Fold one member event into a file under selection, reading its name through the read's memo.
The first of each field wins. The memo holds the latest version text only, so the read holds
nothing for the files it rejects.
-}
collectSelected :: Text -> FilenameMemo -> SelectedFile -> SelectedFileEvent -> (SelectedFile, FilenameMemo)
collectSelected _ memo RejectedFile _ = (RejectedFile, memo)
collectSelected wanted memo current@(CandidateFile matched scalars hashes active) event = case event of
    FileScalar key value
        | any ((== key) . fst) scalars -> (current, memo)
        | key == "filename" -> case value of
            String filename -> case readLatestCoordinate memo filename of
                (coordinate, remembered)
                    | fmap fcVersionKey coordinate == Just wanted ->
                        (CandidateFile True ((key, value) : scalars) hashes active, remembered)
                    | otherwise -> (RejectedFile, remembered)
            _ -> (RejectedFile, memo)
        | otherwise -> (CandidateFile matched ((key, value) : scalars) hashes active, memo)
    HashesStart ->
        (CandidateFile matched scalars (hashes <|> Just (Object mempty)) (isNothing hashes), memo)
    HashesEnd -> (CandidateFile matched scalars hashes False, memo)
    HashesValue value -> (CandidateFile matched scalars (hashes <|> Just value) active, memo)
    HashField key value
        | active
        , Just (Object fields) <- hashes
        , not (KeyMap.member key fields) ->
            (CandidateFile matched scalars (Just (Object (KeyMap.insert key value fields))) active, memo)
        | otherwise -> (current, memo)

-- | The retained object of a file the requested release names.
finishSelected :: SelectedFile -> Maybe Value
finishSelected (CandidateFile True fields hashes _) =
    Just (Object (KeyMap.fromList (maybeToList ((,) "hashes" <$> hashes) <> fields)))
finishSelected _ = Nothing
