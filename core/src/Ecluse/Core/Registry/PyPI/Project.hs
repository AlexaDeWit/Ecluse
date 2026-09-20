-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Project decoded PyPI files into releases keyed by canonical PEP 440 versions.
The same filename parser supplies coordinates for upstream projection and inbound routes.
-}
module Ecluse.Core.Registry.PyPI.Project (
    -- * Projection
    projectSimpleIndex,

    -- * File coordinates
    FileCoordinate (..),
    fileCoordinate,
    fileVersionKey,

    -- * Name validation
    projectName,
    canonicalName,
    isCanonicalName,
    isNameSeparator,
    pypiNameLeadChars,
) where

import Data.Aeson (toJSON)
import Data.Char (isAlphaNum, isAscii)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime)

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Package (
    Artifact (..),
    ArtifactKind (Sdist, Wheel),
    Availability (Available, Yanked),
    CodeExecSignal (NoCodeOnInstall, RunsCodeOnInstall),
    Hash,
    InvalidEntry,
    InvalidEntryKind (InvalidIndexFile),
    PackageDetails (..),
    PackageInfo (..),
    PackageName,
    Trust (TrustUnknown),
    canonicalise,
    mkHash,
    mkInvalidEntry,
    mkPackageName,
    parseHashAlg,
    renderPackageName,
 )
import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.PyPI.Wire (
    IndexFile (..),
    SimpleIndex (..),
    YankState (FileOffered, FileWithdrawn),
 )
import Ecluse.Core.Registry.WireSupport (
    nameComponentWith,
    withinNameLimit,
 )
import Ecluse.Core.Version (Version, canonicalPep440, mkVersion, selectLatest)

-- | A filename's canonical release and artifact kind.
data FileCoordinate = FileCoordinate
    { fcVersionKey :: Text
    -- ^ The release key: the file's version in canonical PEP 440 form.
    , fcKind :: ArtifactKind
    -- ^ An 'Sdist', or a 'Wheel' carrying its compatibility tag (@py3-none-any@).
    }
    deriving stock (Eq, Show)

-- | Group decoded files without changing their source coordinates or release policy inputs.
projectSimpleIndex :: PackageName -> SimpleIndex -> PackageInfo
projectSimpleIndex name index =
    PackageInfo
        { infoName = name
        , infoVersions = versions
        , infoDistTags = latestTag versions
        , infoInvalidEntries = siInvalidEntries index <> fileDrops
        }
  where
    (versions, fileDrops) = projectVersions name (siFiles index)

projectVersions :: PackageName -> [IndexFile] -> (Map Text PackageDetails, [InvalidEntry])
projectVersions name files =
    (Map.map (projectDetails name) grouped, drops)
  where
    (grouped, drops) = foldr place (Map.empty, []) files

    place file (byVersion, dropAcc) = case fileCoordinate name (ifFilename file) of
        Just coordinate ->
            ( Map.insertWith (<>) (fcVersionKey coordinate) ((file, coordinate) :| []) byVersion
            , dropAcc
            )
        Nothing -> (byVersion, uncoordinatedDrop file : dropAcc)

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

projectDetails :: PackageName -> NonEmpty (IndexFile, FileCoordinate) -> PackageDetails
projectDetails name entries =
    PackageDetails
        { pkgName = name
        , pkgVersion = mkVersion PyPI (fcVersionKey (snd (NE.head entries)))
        , pkgPublishedAt = newestUpload files
        , pkgInstallCode = releaseInstallCode entries
        , pkgTrust = TrustUnknown
        , pkgAvailability = releaseAvailability files
        , pkgArtifacts = fmap (uncurry projectArtifact) entries
        , -- Licence and publisher live in distribution metadata, outside the Simple index.
          pkgLicenses = []
        , pkgPublisher = Nothing
        }
  where
    files = fmap fst entries

-- An unknown-age file cannot borrow a sibling's expired quarantine. A later wheel restarts
-- quarantine when every timestamp is known.
newestUpload :: NonEmpty IndexFile -> Maybe UTCTime
newestUpload files = (\(instant :| rest) -> foldl' max instant rest) <$> traverse ifUploadTime files

releaseInstallCode :: NonEmpty (IndexFile, FileCoordinate) -> CodeExecSignal
releaseInstallCode entries
    | any ((== Sdist) . fcKind . snd) entries =
        RunsCodeOnInstall "offers a source distribution, which runs its own build"
    | otherwise = NoCodeOnInstall

-- A release is withdrawn only when PEP 592 withdraws every file of it.
releaseAvailability :: NonEmpty IndexFile -> Availability
releaseAvailability files = case traverse withdrawnReason files of
    Just reasons -> Yanked (asum reasons)
    Nothing -> Available

withdrawnReason :: IndexFile -> Maybe (Maybe Text)
withdrawnReason file = case ifYanked file of
    FileWithdrawn reason -> Just reason
    FileOffered -> Nothing

-- The location stays verbatim. 'Ecluse.Core.Package.Filter' folds its scheme and authority
-- against the egress and host policies afterward.
projectArtifact :: IndexFile -> FileCoordinate -> Artifact
projectArtifact file coordinate =
    Artifact
        { artEntryKey = ifEntryKey file
        , artFilename = ifFilename file
        , artUrl = ifUrl file
        , artKind = fcKind coordinate
        , artHashes = mapMaybe indexHash (Map.toAscList (ifHashes file))
        , artSize = ifSize file
        , artInterpreter = ifRequiresPython file
        , artYanked = case ifYanked file of
            FileWithdrawn _ -> True
            FileOffered -> False
        , artProvenance = ifProvenance file
        }

indexHash :: (Text, Text) -> Maybe Hash
indexHash (algorithm, digest) = do
    algo <- rightToMaybe (parseHashAlg algorithm)
    rightToMaybe (mkHash algo digest)

-- | Read a filename's coordinate, rejecting another project, an unknown archive, or invalid PEP 440.
fileCoordinate :: PackageName -> Text -> Maybe FileCoordinate
fileCoordinate name file = wheelCoordinate name file <|> sdistCoordinate name file

-- | Read a filename's release key, with the same refusals as 'fileCoordinate'.
fileVersionKey :: PackageName -> Text -> Maybe Text
fileVersionKey name = fmap fcVersionKey . fileCoordinate name

-- @{project}-{version}(-{build})?-{python}-{abi}-{platform}.whl@. The project and version
-- parts escape @-@ as @_@, so the parts split exactly and the project part compares whole.
wheelCoordinate :: PackageName -> Text -> Maybe FileCoordinate
wheelCoordinate name file = do
    stem <- T.stripSuffix ".whl" file
    parts <- nonEmpty (T.splitOn "-" stem)
    guard (length parts == 5 || length parts == 6)
    guard (canonicalise PyPI (NE.head parts) == canonicalName name)
    version <- canonicalPep440 =<< (toList parts !!? 1)
    pure (FileCoordinate version (Wheel (T.intercalate "-" (lastThree parts))))
  where
    lastThree parts = drop (length parts - 3) (toList parts)

-- @{project}-{version}{archive suffix}@. A legacy project name can carry the separator a
-- version can, so the split takes the longest project part that canonicalises to this one.
sdistCoordinate :: PackageName -> Text -> Maybe FileCoordinate
sdistCoordinate name file = do
    stem <- asum (map (`T.stripSuffix` file) sdistSuffixes)
    version <- canonicalPep440 =<< afterProjectName name stem
    pure (FileCoordinate version Sdist)

sdistSuffixes :: [Text]
sdistSuffixes = [".tar.gz", ".tgz", ".zip", ".tar.bz2", ".tar.xz"]

-- Compare disjoint chunks so unauthenticated filenames cannot trigger repeated prefix work.
afterProjectName :: PackageName -> Text -> Maybe Text
afterProjectName name stem
    | T.null expected = do
        (separator, _) <- T.uncons stem
        guard (isNameSeparator separator)
        pure (T.dropWhile isNameSeparator stem)
    | otherwise = matchProjectChunks (T.splitOn "-" expected) (T.dropWhile isNameSeparator stem)
  where
    expected = canonicalName name

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

-- A separator is legal inside a name, never at either end.
nameChar :: Char -> Bool
nameChar ch = nameEdge ch || isNameSeparator ch

nameEdge :: Char -> Bool
nameEdge ch = isAscii ch && isAlphaNum ch

-- PyPI's own cap on a project name, the one its own validator applies.
pypiNameLimit :: Int
pypiNameLimit = 100
