-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Installed paths, bytes, executable permissions, and unresolved link targets.
module Ecluse.Test.InstalledTree (
    InstalledTree,
    TreeEntry (..),
    snapshotTree,
    normaliseNpmSources,
    treeDifferences,
) where

import Data.Aeson (Value (String), decodeStrict)
import Data.ByteString.Char8 qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
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
import Text.ParserCombinators.ReadP qualified as P

import Ecluse.Test.Json (encodeStrict)

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

{- | Normalise only package source URLs in npm's main and hidden lockfiles.
All bytes outside those JSON string values remain unchanged, including whitespace.
-}
normaliseNpmSources :: Text -> Text -> InstalledTree -> InstalledTree
normaliseNpmSources proxy marker = Map.mapWithKey normalise
  where
    normalise path (TreeFile mayExecute bytes)
        | path `elem` ["package-lock.json", "node_modules/.package-lock.json"] =
            TreeFile mayExecute (normaliseLock proxy marker bytes)
    normalise _ entry = entry

normaliseLock :: Text -> Text -> ByteString -> ByteString
normaliseLock proxy marker bytes =
    case (decodeStrict bytes :: Maybe Value, P.readP_to_S (jsonBytes [] proxy marker <* P.eof) (BS.unpack bytes)) of
        (Just _, [(result, "")]) -> BS.pack result
        _ -> bytes

jsonBytes :: [Text] -> Text -> Text -> P.ReadP String
jsonBytes path proxy marker = do
    spaces <- P.munch (`elem` [' ', '\n', '\r', '\t'])
    value <- object P.<++ array P.<++ stringValue P.<++ P.munch1 (`notElem` [',', ']', '}', ' ', '\n', '\r', '\t'])
    trailing <- P.munch (`elem` [' ', '\n', '\r', '\t'])
    pure (spaces <> value <> trailing)
  where
    object = container '{' '}' member
    array = container '[' ']' (jsonBytes (path <> ["[]", "[]"]) proxy marker)
    member = do
        spaces <- P.munch (`elem` [' ', '\n', '\r', '\t'])
        key <- jsonString
        between <- P.munch (`elem` [' ', '\n', '\r', '\t'])
        colon <- P.char ':'
        name <- maybe P.pfail pure (decodeStrict (BS.pack key) :: Maybe Text)
        value <- jsonBytes (path <> [name]) proxy marker
        pure (spaces <> key <> between <> [colon] <> value)
    stringValue = do
        raw <- jsonString
        pure $ case (path, decodeStrict (BS.pack raw) :: Maybe Text) of
            (["packages", package, "resolved"], Just url)
                | "node_modules/" `T.isPrefixOf` package
                , not (T.null proxy)
                , Just suffix <- T.stripPrefix (proxy <> "/npm/") url ->
                    BS.unpack (encodeStrict (String (marker <> "/npm/" <> suffix)))
            _ -> raw

container :: Char -> Char -> P.ReadP String -> P.ReadP String
container open close item = do
    _ <- P.char open
    spaces <- P.munch (`elem` [' ', '\n', '\r', '\t'])
    contents <-
        (P.char close $> "") P.<++ do
            initial <- item
            rest <- P.many (P.char ',' *> item)
            _ <- P.char close
            pure (intercalate "," (initial : rest))
    pure ([open] <> spaces <> contents <> [close])

jsonString :: P.ReadP String
jsonString = do
    _ <- P.char '"'
    chunks <- P.many (((\c -> ['\\', c]) <$> (P.char '\\' *> P.get)) P.<++ ((: []) <$> P.satisfy (`notElem` ['"', '\\'])))
    _ <- P.char '"'
    pure ("\"" <> concat chunks <> "\"")

-- | Report each differing path, entry kind, permissions, and exact file bytes under each side's name.
treeDifferences :: (Text, InstalledTree) -> (Text, InstalledTree) -> [Text]
treeDifferences (leftName, left) (rightName, right) =
    mapMaybe difference (Map.keys (Map.union left right))
  where
    difference path = case (Map.lookup path left, Map.lookup path right) of
        (held, other) | held == other -> Nothing
        (held, other) -> Just (toText path <> shown leftName held <> shown rightName other)
    shown name held = "\n  " <> name <> ": " <> maybe "nothing" describeEntry held

describeEntry :: TreeEntry -> Text
describeEntry = \case
    TreeDirectory -> "a directory"
    TreeLink target -> "a link to " <> toText target
    TreeFile mayExecute bytes ->
        (if mayExecute then "an executable file" else "a file") <> " with bytes " <> show bytes
