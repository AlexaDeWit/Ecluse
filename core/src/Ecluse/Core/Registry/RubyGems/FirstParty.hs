-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | RubyGems ownership declarations use exact names and separator-delimited prefixes.
module Ecluse.Core.Registry.RubyGems.FirstParty (
    RubyGemsPrefix,
    RubyGemsFirstParty (..),
    projectFirstPartyEntry,
    rubyGemsFirstPartyName,
) where

import Data.Text qualified as T

import Ecluse.Core.Ecosystem (Ecosystem (RubyGems))
import Ecluse.Core.Package (PackageName, pkgEcosystem, pkgNamespace, renderPackageName)
import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.RubyGems.Project (projectName)

-- | A native prefix with a validated terminal name separator.
newtype RubyGemsPrefix = RubyGemsPrefix Text
    deriving stock (Eq, Show)

-- | An exact name or a prefix declared by the registry operator.
data RubyGemsFirstParty
    = -- | Claim this exact native name.
      RubyGemsOwnedName PackageName
    | -- | Claim names beneath this native prefix, including its terminal separator.
      RubyGemsOwnedPrefix RubyGemsPrefix
    deriving stock (Eq, Show)

-- | Parse an exact gem name or a prefix with a terminal separator followed by @*@.
projectFirstPartyEntry :: Text -> Either ParseError RubyGemsFirstParty
projectFirstPartyEntry entry = case T.stripSuffix "*" entry of
    Just prefix | endsAtSeparator prefix -> do
        name <- projectName prefix
        pure (RubyGemsOwnedPrefix (RubyGemsPrefix (renderPackageName name)))
    Just _ -> invalid
    Nothing -> RubyGemsOwnedName <$> projectName entry
  where
    invalid :: Either ParseError a
    invalid = Left (ParseError ("invalid RubyGems first-party entry: " <> show entry))
    endsAtSeparator = maybe False (\(_, c) -> c `elem` ['-', '_', '.']) . T.unsnoc

-- | Match exact native identities, excluding foreign ecosystems and npm scopes.
rubyGemsFirstPartyName :: NonEmpty RubyGemsFirstParty -> PackageName -> Bool
rubyGemsFirstPartyName entries name =
    pkgEcosystem name == RubyGems && isNothing (pkgNamespace name) && any (`owns` renderPackageName name) entries

owns :: RubyGemsFirstParty -> Text -> Bool
owns entry key = case entry of
    RubyGemsOwnedName name -> renderPackageName name == key
    RubyGemsOwnedPrefix (RubyGemsPrefix prefix) -> prefix `T.isPrefixOf` key
