-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native RubyGems identities preserve published versions and platform spellings.
module Ecluse.Core.Registry.RubyGems.Project (
    projectName,
    projectVersion,
    GemPlatform,
    mkGemPlatform,
    unGemPlatform,
    GemCoordinate,
    mkCoordinate,
    coordinateName,
    coordinateVersion,
    coordinatePlatform,
    coordinateFilename,
    coordinateStem,
    candidateCoordinates,
) where

import Data.ByteString qualified as BS
import Data.Char (isDigit)
import Data.Text qualified as T

import Ecluse.Core.Ecosystem (Ecosystem (RubyGems))
import Ecluse.Core.Package (PackageName, mkPackageName, pkgEcosystem, pkgNamespace, renderPackageName)
import Ecluse.Core.Registry (ParseError (ParseError))
import Ecluse.Core.Registry.WireSupport (nameComponentWith)
import Ecluse.Core.Server.Path (Filename, mkFilename, unFilename)
import Ecluse.Core.Text (isAsciiAlphaNum)
import Ecluse.Core.Version (Version, mkVersion, renderVersion)
import Ecluse.Core.Version.Gem (parseGem)

-- | Parse names without rewriting legacy registry punctuation or case.
projectName :: Text -> Either ParseError PackageName
projectName raw = mkPackageName RubyGems Nothing <$> nameComponentWith "RubyGems package name" (T.all nameChar) raw
  where
    nameChar c = isAsciiAlphaNum c || c `elem` ['.', '-', '_']

-- | Parse a digit-leading native version and retain its exact source spelling.
projectVersion :: Text -> Either ParseError Version
projectVersion raw = do
    unless (T.strip raw == raw && maybe False (isDigit . fst) (T.uncons raw) && isJust (parseGem raw)) (Left (ParseError "invalid RubyGems version"))
    pure (mkVersion RubyGems (T.copy raw))

-- | A native platform label, distinct from a version or a content address.
newtype GemPlatform = GemPlatform Text
    deriving stock (Eq, Ord, Show)

-- | Parse a platform label without normalising OS, architecture or ABI spelling.
mkGemPlatform :: Text -> Either ParseError GemPlatform
mkGemPlatform raw = do
    checked <- nameComponentWith "RubyGems platform" (T.all platformChar) raw
    let (_, separated) = T.breakOn "-" checked
        os = if T.null separated then checked else T.drop 1 separated
    unless (checked == "ruby" || knownOs checked || knownOs os) (Left (ParseError "unsupported RubyGems platform"))
    pure (GemPlatform (T.copy checked))
  where
    platformChar c = isAsciiAlphaNum c || c `elem` ['.', '-', '_']
    knownOs value = any (family value) ["aix", "cygwin", "darwin", "macruby", "freebsd", "java", "jruby", "dalvik", "dotnet", "linux", "mingw32", "mingw", "mswin32", "mswin64", "netbsdelf", "openbsd", "solaris", "wasi"]
    family value prefix = case T.stripPrefix prefix value of
        Just rest -> T.null rest || maybe False (\(c, _) -> c == '-' || c == '_' || isDigit c) (T.uncons rest)
        Nothing -> False

-- | Return the exact source platform, including @ruby@ for the generic artifact.
unGemPlatform :: GemPlatform -> Text
unGemPlatform (GemPlatform raw) = raw

-- | An artifact identity whose filename follows its checked native components.
data GemCoordinate = GemCoordinate
    { coordinateName :: PackageName
    , coordinateVersion :: Version
    , coordinatePlatform :: GemPlatform
    , coordinateFilename :: Filename
    }
    deriving stock (Eq, Show)

-- | Bind a raw version and platform to one unscoped RubyGems package.
mkCoordinate :: PackageName -> Version -> GemPlatform -> Either ParseError GemCoordinate
mkCoordinate name version platform = do
    unless (pkgEcosystem name == RubyGems && isNothing (pkgNamespace name)) (Left (ParseError "not an unscoped RubyGems package"))
    checkedName <- projectName (renderPackageName name)
    checkedVersion <- projectVersion (renderVersion version)
    let raw = renderVersion checkedVersion
    let suffix = if unGemPlatform platform == "ruby" then "" else "-" <> unGemPlatform platform
        filename = renderPackageName checkedName <> "-" <> raw <> suffix <> ".gem"
    file <- maybeToRight (ParseError "invalid RubyGems artifact filename") (mkFilename filename)
    pure (GemCoordinate checkedName checkedVersion platform file)

-- | Return the native filename without its @.gem@ extension for quick specifications.
coordinateStem :: GemCoordinate -> Text
coordinateStem = T.dropEnd 4 . unFilename . coordinateFilename

-- | Enumerate bounded native capture candidates before checking admitted artifact inventory.
candidateCoordinates :: Int -> Filename -> Either ParseError [GemCoordinate]
candidateCoordinates cap filename = do
    let raw = unFilename filename
    when (BS.length (encodeUtf8 raw) > cap) (Left (ParseError "RubyGems coordinate capture exceeds its bound"))
    stem <- maybeToRight (ParseError "RubyGems artifact capture has no .gem extension") (T.stripSuffix ".gem" raw)
    when (T.count "-" stem > 100) (Left (ParseError "RubyGems coordinate capture has too many separators"))
    when (toInteger (BS.length (encodeUtf8 raw)) * (1 + toInteger (T.count "-" stem)) > toInteger cap) (Left (ParseError "RubyGems coordinate work exceeds its bound"))
    let candidates = take 101 (concatMap splitVersion (splits stem))
    when (length candidates > 100) (Left (ParseError "RubyGems coordinate capture has too many candidates"))
    when (null candidates) (Left (ParseError "invalid RubyGems artifact coordinate"))
    pure candidates
  where
    splits text = [(T.take at text, T.drop (at + 1) text) | (at, char) <- zip [0 ..] (T.unpack text), char == '-']
    splitVersion (nameText, tailText) = case projectName nameText of
        Left _ -> []
        Right name -> mapMaybe (build name) ((tailText, "ruby") : splits tailText)
    build name (version, platform) = rightToMaybe $ do
        parsed <- mkGemPlatform platform
        mkCoordinate name (mkVersion RubyGems version) parsed
