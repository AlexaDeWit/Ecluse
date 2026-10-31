-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT
{-# LANGUAGE RoleAnnotations #-}

{- | First-party declarations by exact name or separator-delimited prefix, compared through one
ownership key. Each adapter declares how it reads and matches its entries, so a mount's
'FirstParty' names no ecosystem. Prefix grammars share 'FirstPartyGrammar', and its tag keeps one
ecosystem's entries apart from another's.
-}
module Ecluse.Core.Registry.FirstParty (
    -- * Adapter declarations
    FirstPartyNames (..),
    FirstPartyDeclaration (..),
    FirstParty (..),
    firstPartyName,

    -- * Shared name and prefix grammar
    FirstPartyGrammar (..),
    FirstPartyEntry (..),
    FirstPartyPrefix,
    parseFirstPartyEntry,
    ownsFirstPartyName,
) where

import Data.Text qualified as T
import Data.Text.Short (ShortText)
import Data.Text.Short qualified as TS
import Data.Typeable (cast)
import Text.Show (showParen, showString, showsPrec)

import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry (ParseError, parseRefusal)

-- | How an adapter reads its configured @firstParty@ entries and matches a name against them.
data FirstPartyNames entry = FirstPartyNames
    { namesEntry :: Text -> Either ParseError entry
    -- ^ Read one configured entry.
    , namesOwn :: NonEmpty entry -> PackageName -> Bool
    -- ^ Whether the declared entries own a name. Deny by default.
    , namesEntryNoun :: Text
    -- ^ What a refused entry is called, as in @invalid scope in firstParty@.
    , namesListNoun :: Text
    -- ^ What a declaration must name at least one of.
    }

-- | An adapter's first-party grammar, whatever its entry type.
data FirstPartyDeclaration = forall entry. (Eq entry, Show entry, Typeable entry) => FirstPartyDeclaration (FirstPartyNames entry)

-- | A mount's declared entries, under the grammar of the adapter that read them.
data FirstParty = forall entry. (Eq entry, Show entry, Typeable entry) => FirstParty (FirstPartyNames entry) (NonEmpty entry)

-- | Declarations compare by their entries. Entries of another type, such as another adapter's, never compare equal.
instance Eq FirstParty where
    FirstParty _ left == FirstParty _ right = cast right == Just left

instance Show FirstParty where
    showsPrec precedence (FirstParty _ entries) = showParen (precedence > 10) (showString "FirstParty " . showsPrec 11 entries)

-- | Whether the mount's declaration owns a name: the one predicate every consumer of the privilege reads.
firstPartyName :: FirstParty -> PackageName -> Bool
firstPartyName (FirstParty names entries) = namesOwn names entries

-- | How one ecosystem spells and compares first-party names.
data FirstPartyGrammar ecosystem = FirstPartyGrammar
    { fpLabel :: Text
    -- ^ The ecosystem's name in a refusal.
    , fpSeparator :: Char -> Bool
    -- ^ The characters a prefix must end with before its @*@.
    , fpProjectName :: Text -> Either ParseError PackageName
    -- ^ The native name parser, applied to exact names and to prefixes with their separator.
    , fpOwnershipKey :: PackageName -> Maybe Text
    -- ^ The text ownership compares. 'Nothing' for a name the ecosystem never owns.
    }

-- RoleAnnotations is not in GHC2021. Without these lines the tag takes GHC's phantom role, and
-- coerce would move one ecosystem's entries under another's grammar.
type role FirstPartyGrammar nominal

-- | An ownership-key prefix with a validated terminal separator.
newtype FirstPartyPrefix = FirstPartyPrefix ShortText
    deriving stock (Eq, Show)

-- | An exact name or a prefix declared by the registry operator.
data FirstPartyEntry ecosystem
    = -- | Claim the names whose ownership key equals this name's.
      OwnedName PackageName
    | -- | Claim the names whose ownership key begins with this prefix, separator included.
      OwnedPrefix FirstPartyPrefix
    deriving stock (Eq, Show)

type role FirstPartyEntry nominal

-- | Parse an exact name or a prefix ending in a separator then @*@. Refuse @acme*@, which would claim @acmeco@.
parseFirstPartyEntry :: FirstPartyGrammar ecosystem -> Text -> Either ParseError (FirstPartyEntry ecosystem)
parseFirstPartyEntry grammar entry = case T.stripSuffix "*" entry of
    Just prefix | endsAtSeparator prefix -> do
        name <- fpProjectName grammar prefix
        key <- maybe invalid Right (fpOwnershipKey grammar name)
        pure (OwnedPrefix (FirstPartyPrefix (TS.fromText key)))
    Just _ -> invalid
    Nothing -> OwnedName <$> fpProjectName grammar entry
  where
    invalid :: Either ParseError a
    invalid = parseRefusal ("invalid " <> fpLabel grammar <> " first-party entry: " <> show entry)
    endsAtSeparator = maybe False (fpSeparator grammar . snd) . T.unsnoc

-- | Whether any entry owns the name. A name without an ownership key is never owned.
ownsFirstPartyName :: FirstPartyGrammar ecosystem -> NonEmpty (FirstPartyEntry ecosystem) -> PackageName -> Bool
ownsFirstPartyName grammar entries name = maybe False (\key -> any (owns key) entries) (fpOwnershipKey grammar name)
  where
    owns key = \case
        OwnedName owned -> fpOwnershipKey grammar owned == Just key
        OwnedPrefix (FirstPartyPrefix prefix) -> TS.toText prefix `T.isPrefixOf` key
