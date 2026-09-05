-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | How one ecosystem is spelled through an OSV compile pass.

osv.dev and Écluse do not always agree on the spelling, so a pass that carried one name would
either fetch a directory that does not exist or write an artifact the proxy's sync refuses.
This module holds the pair, and "Ecluse.Core.Osv.Compile" takes it rather than a bare name.
-}
module Ecluse.Core.Osv.Ecosystem (
    OsvEcosystem (..),
    osvEcosystemFor,
    osvEcosystemNamed,
) where

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems), ecosystemName, parseEcosystem)

-- | The two spellings one compile pass needs.
data OsvEcosystem = OsvEcosystem
    { osvExportDirectory :: Text
    {- ^ osv.dev's own spelling: the directory its export archive sits under, and the value an
    advisory's affected package carries, which is what the row filter matches.
    -}
    , osvWireName :: Text
    {- ^ Écluse's spelling ('ecosystemName'): it names the published artifact and stamps the
    @meta@ row the proxy's sync checks.
    -}
    }
    deriving stock (Eq, Show)

{- | An ecosystem's pair of spellings. npm agrees with osv.dev, PyPI and RubyGems do not.

>>> osvEcosystemFor PyPI
OsvEcosystem {osvExportDirectory = "PyPI", osvWireName = "pypi"}
-}
osvEcosystemFor :: Ecosystem -> OsvEcosystem
osvEcosystemFor eco = OsvEcosystem{osvExportDirectory = exportDirectory, osvWireName = ecosystemName eco}
  where
    exportDirectory = case eco of
        Npm -> "npm"
        PyPI -> "PyPI"
        RubyGems -> "RubyGems"

{- | The pair for a name a one-shot compile was given. A name this build serves resolves to
'osvEcosystemFor', and any other spells itself on both halves, so an export beyond the mounted
set still compiles.

>>> osvEcosystemNamed "pypi"
OsvEcosystem {osvExportDirectory = "PyPI", osvWireName = "pypi"}
-}
osvEcosystemNamed :: Text -> OsvEcosystem
osvEcosystemNamed name =
    maybe (OsvEcosystem{osvExportDirectory = name, osvWireName = name}) osvEcosystemFor (parseEcosystem name)
