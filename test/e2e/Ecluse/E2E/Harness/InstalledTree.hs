-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | What a client's install left on disk, as comparable data. Two installs of one request must
leave the same entries whichever store supplied them, so a case snapshots both trees and reports
where they differ. The snapshot keeps what a client or a runtime can tell apart: which paths exist,
each file's bytes and whether it may be executed, and each link's target.
-}
module Ecluse.E2E.Harness.InstalledTree (
    InstalledTree,
    TreeEntry (..),
    snapshotTree,
    redactTree,
    treeDifferences,
) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import System.Directory (
    doesDirectoryExist,
    doesPathExist,
    executable,
    getPermissions,
    getSymbolicLinkTarget,
    listDirectory,
    pathIsSymbolicLink,
 )
import System.FilePath ((</>))

-- | Every entry of a tree, keyed by its path under the tree's base directory.
type InstalledTree = Map FilePath TreeEntry

-- | One entry of an installed tree.
data TreeEntry
    = TreeDirectory
    | -- | A symbolic link and the target it names, unresolved.
      TreeLink FilePath
    | -- | Whether the file's owner may execute it, and its bytes.
      TreeFile Bool ByteString
    deriving stock (Eq, Show)

-- | Snapshot the named roots of @base@ and everything under them. A root that is absent adds nothing.
snapshotTree :: FilePath -> [FilePath] -> IO InstalledTree
snapshotTree base roots = do
    present <- filterM (doesPathExist . (base </>)) roots
    Map.fromList . concat <$> traverse (entriesUnder base) present

-- A link is recorded and never followed, so a link that leaves the tree cannot widen the snapshot.
entriesUnder :: FilePath -> FilePath -> IO [(FilePath, TreeEntry)]
entriesUnder base path = do
    link <- pathIsSymbolicLink full
    directory <- doesDirectoryExist full
    case (link, directory) of
        (True, _) -> (\target -> [(path, TreeLink target)]) <$> getSymbolicLinkTarget full
        (False, True) -> do
            children <- listDirectory full
            ((path, TreeDirectory) :) . concat <$> traverse (entriesUnder base . (path </>)) children
        (False, False) -> do
            mayExecute <- executable <$> getPermissions full
            bytes <- readFileBS full
            pure [(path, TreeFile mayExecute bytes)]
  where
    full = base </> path

{- | Replace every occurrence of @needle@ in every file's bytes with @marker@, for a value that two
installs must write differently. An empty needle changes nothing.
-}
redactTree :: ByteString -> ByteString -> InstalledTree -> InstalledTree
redactTree needle marker
    | BS.null needle = id
    | otherwise = Map.map redact
  where
    redact = \case
        TreeFile mayExecute bytes -> TreeFile mayExecute (replaced bytes)
        TreeLink target -> TreeLink target
        TreeDirectory -> TreeDirectory
    replaced bytes = case BS.breakSubstring needle bytes of
        (before, found)
            | BS.null found -> before
            | otherwise -> before <> marker <> replaced (BS.drop (BS.length needle) found)

{- | Each path the two trees disagree on, with what each side holds there, under the caller's name
for each side. A file shows the lines the other side's file lacks. Empty when the trees are identical.
-}
treeDifferences :: (Text, InstalledTree) -> (Text, InstalledTree) -> [Text]
treeDifferences (leftName, left) (rightName, right) =
    mapMaybe difference (Map.keys (Map.union left right))
  where
    difference path = case (Map.lookup path left, Map.lookup path right) of
        (held, other) | held == other -> Nothing
        (held, other) -> Just (toText path <> shown leftName held other <> shown rightName other held)
    shown name held other = "\n  " <> name <> ": " <> maybe "nothing" (describeAgainst other) held

describeAgainst :: Maybe TreeEntry -> TreeEntry -> Text
describeAgainst other = \case
    TreeDirectory -> "a directory"
    TreeLink target -> "a link to " <> toText target
    TreeFile mayExecute bytes ->
        (if mayExecute then "an executable file" else "a file")
            <> foldMap ("\n    " <>) (filter (`notElem` otherLines) (lines (decodeUtf8 bytes)))
  where
    otherLines = case other of
        Just (TreeFile _ bytes) -> lines (decodeUtf8 bytes)
        _ -> []
