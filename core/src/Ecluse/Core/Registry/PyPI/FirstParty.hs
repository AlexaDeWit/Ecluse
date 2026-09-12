-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | PyPI first-party ownership by exact project name or separator-delimited prefix.
Exact declarations share the routed grammar in "Ecluse.Core.Registry.PyPI.Project".
-}
module Ecluse.Core.Registry.PyPI.FirstParty (
    -- * Name prefixes
    PyPIPrefix,
    mkPyPIPrefix,
    underPyPIPrefix,

    -- * First-party declarations
    PyPIFirstParty (..),
    projectFirstPartyEntry,
    pypiFirstPartyName,
) where

import Data.Char (isAlphaNum, isAscii)
import Data.Text qualified as T
import Data.Text.Short (ShortText)
import Data.Text.Short qualified as TS

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Package (PackageName, canonicalise, pkgCanonical, pkgEcosystem)
import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.PyPI.Project (projectName)
import Ecluse.Core.Registry.WireSupport (parseNameComponent)

-- | A distribution-name prefix in PEP 503 canonical form.
newtype PyPIPrefix = PyPIPrefix ShortText
    deriving stock (Eq, Show)

{- | Build a canonical prefix, accepting terminal separators.
Empty, separator-only, or non-name text yields 'Nothing'.
-}
mkPyPIPrefix :: Text -> Maybe PyPIPrefix
mkPyPIPrefix raw = do
    canonical <- rightToMaybe (parseNameComponent (canonicalise PyPI raw))
    guard (T.all canonicalPyPIChar canonical)
    pure (PyPIPrefix (TS.fromText canonical))

-- PEP 503's canonical alphabet, the form the PyPI canonicaliser leaves a legal name in.
canonicalPyPIChar :: Char -> Bool
canonicalPyPIChar c = c == '-' || (isAscii c && isAlphaNum c)

-- | Match at a separator: @acme@ covers @acme-tools@, excluding @acmeco@ and bare @acme@.
underPyPIPrefix :: PyPIPrefix -> PackageName -> Bool
underPyPIPrefix (PyPIPrefix prefix) name =
    pkgEcosystem name == PyPI && TS.isPrefixOf (prefix <> "-") (pkgCanonical name)

-- | A distribution or a prefix owned by the deployment.
data PyPIFirstParty
    = -- | A distribution the deployment owns, matched on its PEP 503 canonical name.
      PyPIOwnedName PackageName
    | -- | A prefix the deployment owns, matched at PEP 503's separator boundary.
      PyPIOwnedPrefix PyPIPrefix
    deriving stock (Eq, Show)

{- | Parse an exact 'projectName' or a prefix ending in a separator then @*@.
Refuse @acme*@, which would otherwise claim names such as @acmeco@.
-}
projectFirstPartyEntry :: Text -> Either ParseError PyPIFirstParty
projectFirstPartyEntry entry = case T.stripSuffix "*" entry of
    Just prefix
        | endsAtSeparator prefix -> maybe invalid (Right . PyPIOwnedPrefix) (mkPyPIPrefix prefix)
        | otherwise -> invalid
    Nothing -> either (const invalid) (Right . PyPIOwnedName) (projectName entry)
  where
    endsAtSeparator :: Text -> Bool
    endsAtSeparator prefix = maybe False ((`elem` ("-_." :: String)) . snd) (T.unsnoc prefix)

    invalid :: Either ParseError a
    invalid = Left (ParseError ("invalid PyPI first-party entry: " <> show entry))

-- | Match an exact canonical name or a declared prefix. Deny by default.
pypiFirstPartyName :: NonEmpty PyPIFirstParty -> PackageName -> Bool
pypiFirstPartyName entries name = any (`owns` name) entries
  where
    owns :: PyPIFirstParty -> PackageName -> Bool
    owns = \case
        PyPIOwnedName owned -> (== owned)
        PyPIOwnedPrefix prefix -> underPyPIPrefix prefix
