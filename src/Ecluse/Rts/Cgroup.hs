-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Linux cgroup v2 limits and memory observations, independent of GHC runtime planning.
module Ecluse.Rts.Cgroup (
    CgroupLimits (..),
    readCgroupLimits,
    parseCpuMax,
    parseMemoryMax,
    parseInactiveFile,
    readIfExists,
    usePermille,
    cgroupMemoryUse,
) where

import Data.Text qualified as T
import System.IO.Error (isDoesNotExistError)
import UnliftIO (tryIO, tryJust)

-- | The cgroup v2 grant per axis: CPU quota in cores, @memory.max@ in bytes, 'Nothing' when absent or @max@.
data CgroupLimits = CgroupLimits
    { cgCpuCores :: Maybe Double
    , cgMemoryMaxBytes :: Maybe Int
    }
    deriving stock (Eq, Show)

-- | Parse a cgroup v2 @cpu.max@ body into granted cores. The @max@ sentinel or a malformed body yields 'Nothing'.
parseCpuMax :: Text -> Maybe Double
parseCpuMax body = case T.words (T.strip body) of
    [quota, period] -> do
        q <- readMaybe (toString quota) :: Maybe Double
        p <- readMaybe (toString period) :: Maybe Double
        guard (q > 0 && p > 0)
        pure (q / p)
    _ -> Nothing

-- | Parse a cgroup v2 @memory.max@ body into bytes. The @max@ sentinel or a malformed body yields 'Nothing'.
parseMemoryMax :: Text -> Maybe Int
parseMemoryMax body = do
    n <- readMaybe (toString (T.strip body)) :: Maybe Int
    guard (n > 0)
    pure n

-- | The @inactive_file@ bytes in a cgroup-v2 @memory.stat@ body: page cache the kernel reclaims first.
parseInactiveFile :: Text -> Maybe Int
parseInactiveFile body =
    listToMaybe
        [ n
        | line <- lines body
        , ["inactive_file", value] <- [T.words line]
        , Just n <- [readMaybe (toString value)]
        ]

-- | A reader for cgroup memory use less inactive file pages, in thousandths of the tightest @memory.max@, else 'Nothing'.
cgroupMemoryUse :: IO (IO (Maybe Int))
cgroupMemoryUse = do
    dirs <- cgroupDirectories
    limits <- traverse (\dir -> fmap (dir,) <$> limitAt parseMemoryMax "/memory.max" dir) dirs
    pure $ case sortOn snd (catMaybes limits) of
        [] -> pure Nothing
        (dir, limit) : _ -> readUse dir limit

readUse :: FilePath -> Int -> IO (Maybe Int)
readUse dir limit = do
    current <- fromRight Nothing <$> tryIO (limitAt parseMemoryMax "/memory.current" dir)
    inactive <- fromRight Nothing <$> tryIO ((>>= parseInactiveFile) <$> readIfExists (dir <> "/memory.stat"))
    pure (usePermille limit inactive <$> current)

-- | Memory in use less reclaimable page cache, in thousandths of the limit, from a cgroup's readings.
usePermille :: Int -> Maybe Int -> Int -> Int
usePermille limit inactive current = max 0 (current - fromMaybe 0 inactive) * 1000 `div` max 1 limit

-- | Read the tightest CPU and memory limits across the process cgroup and its ancestors.
readCgroupLimits :: IO CgroupLimits
readCgroupLimits = do
    dirs <- cgroupDirectories
    cpus <- traverse (limitAt parseCpuMax "/cpu.max") dirs
    memories <- traverse (limitAt parseMemoryMax "/memory.max") dirs
    pure
        CgroupLimits
            { cgCpuCores = tightest cpus
            , cgMemoryMaxBytes = tightest memories
            }

cgroupDirectories :: IO [FilePath]
cgroupDirectories = do
    selfCgroup <- readIfExists "/proc/self/cgroup"
    let relative = fromMaybe "/" (selfCgroup >>= parseCgroupSelfPath)
    pure [cgroupRoot <> toString suffix | suffix <- ancestorPaths relative]

cgroupRoot :: FilePath
cgroupRoot = "/sys/fs/cgroup"

limitAt :: (Text -> Maybe a) -> String -> FilePath -> IO (Maybe a)
limitAt parse file dir = (>>= parse) <$> readIfExists (dir <> file)

tightest :: (Ord a) => [Maybe a] -> Maybe a
tightest found = case catMaybes found of
    [] -> Nothing
    (x : xs) -> Just (foldl' min x xs)

-- | Read a file that may be absent, as off a cgroup-v2 host. Every other IO error propagates.
readIfExists :: FilePath -> IO (Maybe Text)
readIfExists path =
    rightToMaybe <$> tryJust (guard . isDoesNotExistError) (decodeUtf8 <$> readFileBS path)

-- The cgroup v2 path on the @0::@ line of @\/proc\/self\/cgroup@. 'Nothing' on a pure cgroup v1 host.
parseCgroupSelfPath :: Text -> Maybe Text
parseCgroupSelfPath body =
    listToMaybe (mapMaybe (T.stripPrefix "0::") (lines (T.strip body)))

-- A cgroup path and its ancestors, leaf first: @"\/a\/b"@ yields @["\/a\/b", "\/a", ""]@.
ancestorPaths :: Text -> [Text]
ancestorPaths path = case filter (not . T.null) (T.splitOn "/" (T.strip path)) of
    [] -> [""]
    segments ->
        [T.concat ["/" <> seg | seg <- take n segments] | n <- [length segments, length segments - 1 .. 1]] <> [""]
