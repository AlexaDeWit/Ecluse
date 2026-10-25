-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Cargo ownership declarations use native collision keys and separator-delimited prefixes.
module Ecluse.Core.Registry.Cargo.FirstParty (
    CargoPrefix,
    CargoFirstParty (..),
    projectFirstPartyEntry,
    cargoFirstPartyName,
) where

import Data.Text qualified as T
import Data.Text.Short (ShortText)
import Data.Text.Short qualified as TS

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.Cargo.Project (cargoCollisionKey, projectName)

-- | A native collision prefix with a validated terminal hyphen or underscore.
newtype CargoPrefix = CargoPrefix ShortText
    deriving stock (Eq, Show)

-- | An exact name or a prefix declared by the registry operator.
data CargoFirstParty
    = -- | Claim the native case and hyphen/underscore collision aliases of this name.
      CargoOwnedName PackageName
    | -- | Claim names beneath this native prefix, including its terminal separator.
      CargoOwnedPrefix CargoPrefix
    deriving stock (Eq, Show)

-- | Parse an exact crate name or a prefix with a terminal separator followed by @*@.
projectFirstPartyEntry :: Text -> Either ParseError CargoFirstParty
projectFirstPartyEntry entry = case T.stripSuffix "*" entry of
    Just prefix | endsAtSeparator prefix -> do
        name <- projectName prefix
        key <- maybe invalid Right (cargoCollisionKey name)
        pure (CargoOwnedPrefix (CargoPrefix (TS.fromText key)))
    Just _ -> invalid
    Nothing -> CargoOwnedName <$> projectName entry
  where
    invalid :: Either ParseError a
    invalid = Left (ParseError ("invalid Cargo first-party entry: " <> show entry))
    endsAtSeparator = maybe False (\(_, c) -> c == '-' || c == '_') . T.unsnoc

-- | Match native ownership aliases, excluding foreign ecosystems and npm scopes.
cargoFirstPartyName :: NonEmpty CargoFirstParty -> PackageName -> Bool
cargoFirstPartyName entries name = maybe False (\key -> any (`owns` key) entries) (cargoCollisionKey name)

owns :: CargoFirstParty -> Text -> Bool
owns entry key = case entry of
    CargoOwnedName name -> cargoCollisionKey name == Just key
    CargoOwnedPrefix (CargoPrefix prefix) -> TS.toText prefix `T.isPrefixOf` key
