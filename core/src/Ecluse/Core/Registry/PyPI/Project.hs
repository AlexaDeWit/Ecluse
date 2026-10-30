-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Project decoded PyPI files into releases keyed by canonical PEP 440 versions.
The same filename parser supplies coordinates for upstream projection and inbound routes. An upload's file name passes a stricter form of it.
-}
module Ecluse.Core.Registry.PyPI.Project (
    -- * Projection
    projectSimpleIndex,
    ProjectedFile,
    projectFile,
    projectedEntryKey,

    -- * File coordinates
    FileCoordinate (..),
    fcVersionKey,
    DistributionKind (..),
    fileCoordinate,
    builtFileCoordinate,
    FilenameMemo,
    filenameMemo,
    readCoordinate,
    readLatestCoordinate,

    -- * Name validation
    projectName,
    canonicalName,
    isCanonicalName,
    isNameSeparator,
    pypiNameLeadChars,
) where

import Data.Aeson (toJSON)
import Data.Char (isAlphaNum, isAscii, isDigit)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime)

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Package (
    Artifact (..),
    Availability (Available, Yanked),
    CodeExecSignal (NoCodeOnInstall, RunsCodeOnInstall),
    DependencySources (RegistryDependencies),
    Hash,
    InvalidEntry,
    InvalidEntryKind (InvalidIndexFile),
    PackageDetails (..),
    PackageInfo (..),
    PackageName,
    canonicalise,
    mkHash,
    mkInvalidEntry,
    mkPackageName,
    parseHashAlg,
    renderPackageName,
 )
import Ecluse.Core.Package.Entry (EntryKey)
import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.PyPI.Wire (
    IndexFile (..),
    YankState (FileWithdrawn),
 )
import Ecluse.Core.Registry.WireSupport (
    nameComponentWith,
    newestUpload,
    withinNameLimit,
 )
import Ecluse.Core.Strict (strictElements)
import Ecluse.Core.Version (Version, canonicalPep440, renderVersion, selectLatest)

-- | A filename's canonical release and distribution kind.
data FileCoordinate = FileCoordinate
    { fcVersion :: Version
    -- ^ The file's version under its canonical PEP 440 spelling.
    , fcKind :: DistributionKind
    }
    deriving stock (Eq, Show)

-- | The release key: the file's version in canonical PEP 440 form.
fcVersionKey :: FileCoordinate -> Text
fcVersionKey = renderVersion . fcVersion

-- | Whether a file is a source distribution, whose install runs its own build, or a wheel.
data DistributionKind = Sdist | Wheel
    deriving stock (Eq, Show)

-- | A file's domain facts or identity diagnostic, independent of its decoded wire record.
data ProjectedFile
    = ReleaseFile {-# UNPACK #-} !FileRelease
    | UncoordinatedFile EntryKey InvalidEntry

data FileRelease = FileRelease
    { frArtifact :: Artifact
    , frVersion :: Version
    , frKind :: DistributionKind
    , frPublishedAt :: Maybe UTCTime
    , frYanked :: YankState
    }

-- | Project before retaining a file, so distribution hash maps and wire-only fields can be released.
projectFile :: IndexFile -> Maybe FileCoordinate -> ProjectedFile
projectFile file = \case
    Just coordinate ->
        ReleaseFile
            FileRelease
                { frArtifact = projectArtifact file
                , frVersion = fcVersion coordinate
                , frKind = fcKind coordinate
                , frPublishedAt = ifUploadTime file
                , frYanked = ifYanked file
                }
    Nothing -> UncoordinatedFile (ifEntryKey file) (uncoordinatedDrop file)

-- | The original array coordinate remains available for replay, including rejected filenames.
projectedEntryKey :: ProjectedFile -> EntryKey
projectedEntryKey = \case
    ReleaseFile file -> artEntryKey (frArtifact file)
    UncoordinatedFile key _ -> key

-- | Group files supplied in reverse source order, keeping artifact and diagnostic order. Decode errors come first.
projectSimpleIndex :: PackageName -> [InvalidEntry] -> [ProjectedFile] -> PackageInfo
projectSimpleIndex name invalid files =
    PackageInfo
        { infoName = name
        , infoVersions = versions
        , infoDistTags = latestTag versions
        , infoInvalidEntries = strictElements (invalid <> fileDrops)
        }
  where
    (versions, fileDrops) = projectVersions name files

projectVersions :: PackageName -> [ProjectedFile] -> (Map Text PackageDetails, [InvalidEntry])
projectVersions name files = (closeRun run byVersion, drops)
  where
    VersionFold byVersion run drops = foldl' (placeFile name) (VersionFold Map.empty NoRun []) files

-- The open run collects adjacent files of one release, so the map takes one write per run, not per file.
data VersionFold = VersionFold (Map Text PackageDetails) OpenRun [InvalidEntry]

data OpenRun = NoRun | OpenRun Text PackageDetails

placeFile :: PackageName -> VersionFold -> ProjectedFile -> VersionFold
placeFile _ (VersionFold byVersion run drops) (UncoordinatedFile _ failure) = VersionFold byVersion run (failure : drops)
placeFile name (VersionFold byVersion run drops) (ReleaseFile file) = case run of
    OpenRun open held | open == key -> extendRun byVersion (Just held)
    _ -> extendRun settled (Map.lookup key settled)
  where
    key = renderVersion (frVersion file)
    settled = closeRun run byVersion
    extendRun into held = VersionFold into (OpenRun key (projectDetails name file held)) drops

closeRun :: OpenRun -> Map Text PackageDetails -> Map Text PackageDetails
closeRun = \case
    NoRun -> id
    OpenRun key details -> Map.insert key details

-- 'mkInvalidEntry' reduces the location to its authority before logging.
uncoordinatedDrop :: IndexFile -> InvalidEntry
uncoordinatedDrop file =
    mkInvalidEntry
        InvalidIndexFile
        (ifFilename file)
        (toJSON (ifUrl file))
        "file name names no PEP 440 release of this project"

latestTag :: Map Text PackageDetails -> Map Text Version
latestTag versions =
    maybe Map.empty (Map.singleton "latest") (selectLatest Nothing (map pkgVersion (Map.elems versions)))

-- A retained release must not keep its decoded files alive.
projectDetails :: PackageName -> FileRelease -> Maybe PackageDetails -> PackageDetails
projectDetails name file held =
    artifact `seq` case held of
        Nothing ->
            PackageDetails
                { pkgName = name
                , pkgVersion = frVersion file
                , pkgPublishedAt = frPublishedAt file
                , pkgInstallCode = releaseInstallCode (frKind file) NoCodeOnInstall
                , -- pypi.org refuses a direct-reference dependency at upload.
                  pkgDependencySources = RegistryDependencies
                , pkgAvailability = releaseAvailability (frYanked file) Yanked
                , pkgArtifacts = artifact :| []
                }
        Just details ->
            let artifacts = toList (pkgArtifacts details)
             in artifacts `seq`
                    details
                        { pkgVersion = frVersion file
                        , pkgPublishedAt = newestUpload (frPublishedAt file) (pkgPublishedAt details)
                        , pkgInstallCode = releaseInstallCode (frKind file) (pkgInstallCode details)
                        , pkgAvailability = releaseAvailability (frYanked file) (pkgAvailability details)
                        , pkgArtifacts = artifact :| artifacts
                        }
  where
    artifact = frArtifact file

releaseInstallCode :: DistributionKind -> CodeExecSignal -> CodeExecSignal
releaseInstallCode kind previous
    | kind == Sdist =
        RunsCodeOnInstall "offers a source distribution, which runs its own build"
    | otherwise = previous

-- A release is withdrawn only when PEP 592 withdraws every file of it.
releaseAvailability :: YankState -> Availability -> Availability
releaseAvailability yanked previous
    | yanked == FileWithdrawn = previous
    | otherwise = Available

-- The location stays verbatim. 'Ecluse.Core.Package.Filter' folds its scheme and authority
-- against the egress and host policies afterward.
projectArtifact :: IndexFile -> Artifact
projectArtifact file =
    Artifact
        { artEntryKey = ifEntryKey file
        , artFilename = ifFilename file
        , artUrl = ifUrl file
        , artHashes = strictElements (mapMaybe indexHash (Map.toAscList (ifHashes file)))
        , artCoreMetadata = strictElements (ifCoreMetadata file)
        , artSize = ifSize file
        }

indexHash :: (Text, Text) -> Maybe Hash
indexHash (algorithm, digest) = do
    algo <- rightToMaybe (parseHashAlg algorithm)
    rightToMaybe (mkHash algo digest)

-- | Read a filename's coordinate, rejecting another project, an unknown archive, or invalid PEP 440.
fileCoordinate :: PackageName -> Text -> Maybe FileCoordinate
fileCoordinate name = partsCoordinate <=< filenameParts (fileProject name)

-- | Read as 'fileCoordinate' does, refusing a name no current build tool writes: a non-PEP 625 sdist, or a version not led by a digit or holding @-@.
builtFileCoordinate :: PackageName -> Text -> Maybe FileCoordinate
builtFileCoordinate name file = do
    parts@(version, _) <- wheelParts project file <|> pep625SdistParts project file
    guard (maybe False (isDigit . fst) (T.uncons version) && not (T.any (== '-') version))
    partsCoordinate parts
  where
    project = fileProject name

partsCoordinate :: (Text, DistributionKind) -> Maybe FileCoordinate
partsCoordinate (version, kind) = (`FileCoordinate` kind) <$> canonicalPep440 version

-- A project's PEP 503 key, prepared once for reading many of its filenames.
data FileProject = FileProject
    { fpCanonical :: Text
    , fpChunks :: [Text]
    }

fileProject :: PackageName -> FileProject
fileProject name = FileProject canonical (T.splitOn "-" canonical)
  where
    canonical = canonicalName name

-- | One read's PEP 440 versions by version text, so the files of one release parse their version once.
data FilenameMemo = FilenameMemo
    { memoProject :: FileProject
    , memoVersions :: Map Text (Maybe Version)
    }

-- | Start a memo for one index read. It ends with the read, so no request inherits its versions.
filenameMemo :: PackageName -> FilenameMemo
filenameMemo name = FilenameMemo (fileProject name) Map.empty

-- | Read a coordinate as 'fileCoordinate' does, parsing each distinct version text once.
readCoordinate :: FilenameMemo -> Text -> (Maybe FileCoordinate, FilenameMemo)
readCoordinate = readRemembering Map.insert

-- | Read as 'readCoordinate' does, retaining only the latest version text for this read.
readLatestCoordinate :: FilenameMemo -> Text -> (Maybe FileCoordinate, FilenameMemo)
readLatestCoordinate = readRemembering (\version parsed _ -> Map.singleton version parsed)

readRemembering :: (Text -> Maybe Version -> Map Text (Maybe Version) -> Map Text (Maybe Version)) -> FilenameMemo -> Text -> (Maybe FileCoordinate, FilenameMemo)
readRemembering remember memo file = maybe (Nothing, memo) known (filenameParts (memoProject memo) file)
  where
    known (version, kind) = case Map.lookup version (memoVersions memo) of
        Just held -> (coordinate kind held, memo)
        Nothing ->
            let parsed = canonicalPep440 version
             in (coordinate kind parsed, memo{memoVersions = remember version parsed (memoVersions memo)})
    coordinate kind = fmap (`FileCoordinate` kind)

filenameParts :: FileProject -> Text -> Maybe (Text, DistributionKind)
filenameParts project file = wheelParts project file <|> sdistParts project file

-- Five or six components qualify, even when build or tag components are empty.
wheelParts :: FileProject -> Text -> Maybe (Text, DistributionKind)
wheelParts project file = do
    stem <- T.stripSuffix ".whl" file
    let (name, afterName) = T.break (== '-') stem
        (version, tags) = T.break (== '-') (T.drop 1 afterName)
    guard (wheelTailFits tags)
    guard (canonicalise PyPI name == fpCanonical project)
    pure (version, Wheel)

data WheelDelimiterCount
    = ZeroDelimiters
    | OneDelimiter
    | TwoDelimiters
    | ThreeDelimiters
    | FourDelimiters
    | ExcessDelimiters

wheelTailFits :: Text -> Bool
wheelTailFits tags = case T.foldl' advanceCount ZeroDelimiters tags of
    ThreeDelimiters -> True
    FourDelimiters -> True
    _ -> False
  where
    advanceCount delimiterCount character
        | character /= '-' = delimiterCount
        | otherwise = case delimiterCount of
            ZeroDelimiters -> OneDelimiter
            OneDelimiter -> TwoDelimiters
            TwoDelimiters -> ThreeDelimiters
            ThreeDelimiters -> FourDelimiters
            FourDelimiters -> ExcessDelimiters
            ExcessDelimiters -> ExcessDelimiters
{-# INLINE wheelTailFits #-}

-- Legacy project names and versions can share separators. Keep the longest matching project prefix.
sdistParts :: FileProject -> Text -> Maybe (Text, DistributionKind)
sdistParts project file = do
    stem <- asum (map (`T.stripSuffix` file) sdistSuffixes)
    version <- afterProjectName project stem
    pure (version, Sdist)

sdistSuffixes :: [Text]
sdistSuffixes = [".tar.gz", ".tgz", ".zip", ".tar.bz2", ".tar.xz"]

-- PEP 625 spells the project as its canonical name with `_` for `-`, so the project part holds no `-`.
pep625SdistParts :: FileProject -> Text -> Maybe (Text, DistributionKind)
pep625SdistParts project file = do
    stem <- T.stripSuffix ".tar.gz" file
    version <- T.stripPrefix (T.replace "-" "_" (fpCanonical project) <> "-") stem
    pure (version, Sdist)

-- Compare disjoint chunks so unauthenticated filenames cannot trigger repeated prefix work.
afterProjectName :: FileProject -> Text -> Maybe Text
afterProjectName project stem
    | T.null (fpCanonical project) = do
        (separator, _) <- T.uncons stem
        guard (isNameSeparator separator)
        pure (T.dropWhile isNameSeparator stem)
    | otherwise = matchProjectChunks (fpChunks project) (T.dropWhile isNameSeparator stem)

matchProjectChunks :: [Text] -> Text -> Maybe Text
matchProjectChunks [] rest = Just rest
matchProjectChunks (expected : remaining) rest = do
    let (chunk, separated) = T.break isNameSeparator rest
    guard (canonicalise PyPI chunk == expected)
    guard (not (T.null separated))
    matchProjectChunks remaining (T.dropWhile isNameSeparator separated)

-- | The characters PEP 503 treats as one separator when it normalises a name.
isNameSeparator :: Char -> Bool
isNameSeparator c = c == '-' || c == '_' || c == '.'

-- | The PEP 503 key used for filename comparison and upstream Simple-index URLs.
canonicalName :: PackageName -> Text
canonicalName = canonicalise PyPI . renderPackageName

-- | Parse one PyPI name component under the shared floor and PEP 508 grammar.
projectName :: Text -> Either ParseError PackageName
projectName raw = do
    withinNameLimit "PyPI project name" pypiNameLimit raw
    mkPackageName PyPI Nothing <$> nameComponent raw

-- | Whether the route can claim this name without a canonical-spelling redirect.
isCanonicalName :: Text -> Bool
isCanonicalName raw = canonicalise PyPI raw == raw

nameComponent :: Text -> Either ParseError Text
nameComponent = nameComponentWith "PyPI project name" usableComponent

-- | Initial characters for partitioning canonical PyPI names during a store walk.
pypiNameLeadChars :: [Char]
pypiNameLeadChars = ['a' .. 'z'] <> ['0' .. '9']

usableComponent :: Text -> Bool
usableComponent component =
    T.all nameChar component
        && maybe False (nameEdge . fst) (T.uncons component)
        && maybe False (nameEdge . snd) (T.unsnoc component)

nameChar :: Char -> Bool
nameChar ch = nameEdge ch || isNameSeparator ch

nameEdge :: Char -> Bool
nameEdge ch = isAscii ch && isAlphaNum ch

pypiNameLimit :: Int
pypiNameLimit = 100
