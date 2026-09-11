-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | What one OSV compile pass needs to know about the ecosystem it compiles.

osv.dev and Écluse do not always agree on the spelling, so a pass that carried one name would
either fetch a directory that does not exist or write an artifact the proxy's sync refuses. The
pass also needs the version grammar that orders the advisory bounds it ingests, and the fan-out
an ordinary advisory of the feed stays under. This module holds all four, and
"Ecluse.Core.Osv.Compile" takes it rather than a bare name.
-}
module Ecluse.Core.Osv.Ecosystem (
    OsvEcosystem (..),
    osvEcosystemFor,
    osvEcosystemNamed,
) where

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI, RubyGems), ecosystemName, parseEcosystem)

-- | The two spellings, the version grammar, and the fan-out bound one compile pass needs.
data OsvEcosystem = OsvEcosystem
    { osvExportDirectory :: Text
    {- ^ osv.dev's own spelling: the directory its export archive sits under, and the value an
    advisory's affected package carries, which is what the row filter matches.
    -}
    , osvWireName :: Text
    {- ^ Écluse's spelling ('ecosystemName'): it names the published artifact and stamps the
    @meta@ row the proxy's sync checks.
    -}
    , osvEcosystemTag :: Maybe Ecosystem
    {- ^ The ecosystem whose version grammar orders this pass's advisory bounds. 'Nothing' for a
    name this build does not serve, and then the pass tallies nothing.
    -}
    , osvMaxAdvisoryFanOut :: Int
    {- ^ Ranges one advisory of this feed may expand into before the ingest flags it as
    anomalous. The ingest keeps the advisory either way, so the number only sizes the alarm.
    -}
    }
    deriving stock (Eq, Show)

-- One advisory of the npm export names a few hundred ranges, and a feed no one has measured
-- borrows this bound.
npmAdvisoryFanOut :: Int
npmAdvisoryFanOut = 256

-- The largest advisory of today's PyPI export names 2459 ranges, so no ordinary one trips this.
pypiAdvisoryFanOut :: Int
pypiAdvisoryFanOut = 4096

{- | An ecosystem's pair of spellings. npm agrees with osv.dev, PyPI and RubyGems do not.

>>> osvEcosystemFor PyPI
OsvEcosystem {osvExportDirectory = "PyPI", osvWireName = "pypi", osvEcosystemTag = Just PyPI, osvMaxAdvisoryFanOut = 4096}
-}
osvEcosystemFor :: Ecosystem -> OsvEcosystem
osvEcosystemFor eco =
    OsvEcosystem
        { osvExportDirectory = exportDirectory
        , osvWireName = ecosystemName eco
        , osvEcosystemTag = Just eco
        , osvMaxAdvisoryFanOut = fanOut
        }
  where
    exportDirectory = case eco of
        Npm -> "npm"
        PyPI -> "PyPI"
        RubyGems -> "RubyGems"

    fanOut = case eco of
        Npm -> npmAdvisoryFanOut
        PyPI -> pypiAdvisoryFanOut
        RubyGems -> npmAdvisoryFanOut

{- | The pair for a name a one-shot compile was given: a name this build serves resolves through
'osvEcosystemFor', and any other spells itself on both halves.

>>> osvEcosystemNamed "pypi"
OsvEcosystem {osvExportDirectory = "PyPI", osvWireName = "pypi", osvEcosystemTag = Just PyPI, osvMaxAdvisoryFanOut = 4096}
-}
osvEcosystemNamed :: Text -> OsvEcosystem
osvEcosystemNamed name = maybe unserved osvEcosystemFor (parseEcosystem name)
  where
    unserved =
        OsvEcosystem
            { osvExportDirectory = name
            , osvWireName = name
            , osvEcosystemTag = Nothing
            , osvMaxAdvisoryFanOut = npmAdvisoryFanOut
            }
