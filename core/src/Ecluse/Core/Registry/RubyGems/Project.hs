-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native RubyGems identities preserve published versions and platform spellings.
module Ecluse.Core.Registry.RubyGems.Project (
    projectName,
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
    matchCoordinate,
) where

import Data.Text qualified as T

import Ecluse.Core.Ecosystem (Ecosystem (RubyGems))
import Ecluse.Core.Package (PackageName, mkPackageName, pkgEcosystem, pkgNamespace, renderPackageName)
import Ecluse.Core.Registry (ParseError (ParseError))
import Ecluse.Core.Registry.WireSupport (nameComponentWith)
import Ecluse.Core.Server.Path (Filename, mkFilename, unFilename)
import Ecluse.Core.Text (isAsciiAlphaNum)
import Ecluse.Core.Version (Version, renderVersion)
import Ecluse.Core.Version.Gem (parseGem)

-- | Parse names without rewriting legacy registry punctuation or case.
projectName :: Text -> Either ParseError PackageName
projectName raw = mkPackageName RubyGems Nothing <$> nameComponentWith "RubyGems package name" (T.all nameChar) raw
  where
    nameChar c = isAsciiAlphaNum c || c `elem` ['.', '-', '_']

-- | A native platform label, distinct from a version or a content address.
newtype GemPlatform = GemPlatform Text
    deriving stock (Eq, Ord, Show)

-- | Parse a platform label without normalising OS, architecture or ABI spelling.
mkGemPlatform :: Text -> Either ParseError GemPlatform
mkGemPlatform raw = GemPlatform <$> nameComponentWith "RubyGems platform" (T.all platformChar) raw
  where
    platformChar c = isAsciiAlphaNum c || c `elem` ['.', '-', '_']

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
    let raw = renderVersion version
    unless (T.strip raw == raw && isJust (parseGem raw)) (Left (ParseError "invalid RubyGems version"))
    let suffix = if unGemPlatform platform == "ruby" then "" else "-" <> unGemPlatform platform
        filename = renderPackageName checkedName <> "-" <> raw <> suffix <> ".gem"
    file <- maybeToRight (ParseError "invalid RubyGems artifact filename") (mkFilename filename)
    pure (GemCoordinate checkedName version platform file)

-- | Return the native filename without its @.gem@ extension for quick specifications.
coordinateStem :: GemCoordinate -> Text
coordinateStem = T.dropEnd 4 . unFilename . coordinateFilename

-- | Match complete checked identities, refusing missing or ambiguous captures.
matchCoordinate :: PackageName -> [GemCoordinate] -> Filename -> Either ParseError GemCoordinate
matchCoordinate name coordinates filename = case filter matches coordinates of
    [coordinate] -> Right coordinate
    _ -> Left (ParseError "unknown or ambiguous RubyGems artifact coordinate")
  where
    matches coordinate = coordinateName coordinate == name && coordinateFilename coordinate == filename
