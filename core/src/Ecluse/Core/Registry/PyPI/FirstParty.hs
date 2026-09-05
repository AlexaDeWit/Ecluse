-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | PyPI's name projection for the first-party privilege: the prefix a deployment owns, the
grammar a configured entry parses under, and the predicate the composition root dispatches to. It
is the PyPI counterpart of 'Ecluse.Core.Registry.Npm.Project.projectScope' and
'Ecluse.Core.Registry.Npm.Publish.npmPublishAllowed'.

PyPI carries no structural namespace, so a deployment either names a distribution or owns a prefix
of its distributions' names. Every entry and every candidate reads through the package module's
canonicaliser 'Ecluse.Core.Package.canonicalise', so one spelling has one verdict.
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
import Ecluse.Core.Package (PackageName, canonicalise, mkPackageName, pkgCanonical, pkgEcosystem)
import Ecluse.Core.Registry (ParseError (..))
import Ecluse.Core.Registry.WireSupport (parseNameComponent)

{- | A PyPI name prefix in PEP 503 canonical form. PyPI has no structural namespace like npm's
'Ecluse.Core.Package.Scope', so a deployment owns a prefix of its distributions' names.
-}
newtype PyPIPrefix = PyPIPrefix ShortText
    deriving stock (Eq, Show)

{- | Build a prefix through the canonicaliser 'mkPackageName' uses, over the shared name floor.
'Nothing' for text no PyPI name can start with, or that canonicalises to nothing and would cover
every name.
-}
mkPyPIPrefix :: Text -> Maybe PyPIPrefix
mkPyPIPrefix raw = do
    canonical <- rightToMaybe (parseNameComponent (canonicalise PyPI raw))
    guard (T.all canonicalPyPIChar canonical)
    pure (PyPIPrefix (TS.fromText canonical))

-- PEP 503's canonical alphabet, the form the PyPI canonicaliser leaves a legal name in.
canonicalPyPIChar :: Char -> Bool
canonicalPyPIChar c = c == '-' || (isAscii c && isAlphaNum c)

{- | Whether a name sits under a prefix, ending at PEP 503's separator: @acme@ covers @acme-tools@,
not @acmeco@, and not the bare @acme@, which is declared as a name.
-}
underPyPIPrefix :: PyPIPrefix -> PackageName -> Bool
underPyPIPrefix (PyPIPrefix prefix) name =
    pkgEcosystem name == PyPI && TS.isPrefixOf (prefix <> "-") (pkgCanonical name)

{- | One PyPI first-party declaration. PyPI carries no structural namespace, so a deployment either
names a distribution it owns or the prefix its distributions share.
-}
data PyPIFirstParty
    = -- | A distribution the deployment owns, matched on its PEP 503 canonical name.
      PyPIOwnedName PackageName
    | -- | A prefix the deployment owns, matched at PEP 503's separator boundary.
      PyPIOwnedPrefix PyPIPrefix
    deriving stock (Eq, Show)

{- | Parse one configured entry: a trailing @*@ after a separator marks a prefix (@acme-*@), anything
else a distribution (@acme@). @acme*@ is refused as a typo, not a claim on @acmeco@.
-}
projectFirstPartyEntry :: Text -> Either ParseError PyPIFirstParty
projectFirstPartyEntry entry = case T.stripSuffix "*" entry of
    Just prefix
        | endsAtSeparator prefix -> maybe invalid (Right . PyPIOwnedPrefix) (mkPyPIPrefix prefix)
        | otherwise -> invalid
    Nothing
        | isJust (mkPyPIPrefix entry) -> Right (PyPIOwnedName (mkPackageName PyPI Nothing entry))
        | otherwise -> invalid
  where
    endsAtSeparator :: Text -> Bool
    endsAtSeparator prefix = maybe False ((`elem` ("-_." :: String)) . snd) (T.unsnoc prefix)

    invalid :: Either ParseError a
    invalid = Left (ParseError ("invalid PyPI first-party entry: " <> show entry))

{- | Whether PyPI's first-party declarations cover a name: it equals a declared distribution's
canonical name, or sits under a declared prefix. Deny by default.
-}
pypiFirstPartyName :: NonEmpty PyPIFirstParty -> PackageName -> Bool
pypiFirstPartyName entries name = any (`owns` name) entries
  where
    owns :: PyPIFirstParty -> PackageName -> Bool
    owns = \case
        PyPIOwnedName owned -> (== owned)
        PyPIOwnedPrefix prefix -> underPyPIPrefix prefix
