-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | OSV export names and native version grammars for each advisory compile pass.
module Ecluse.Core.Osv.Ecosystem (
    OsvEcosystem (..),
    osvEcosystemFor,
    osvEcosystemNamed,
) where

import Ecluse.Core.Ecosystem (Ecosystem (Cargo, Npm, PyPI, RubyGems), ecosystemName, parseEcosystem)

-- | The two spellings, the version grammar, and the fan-out bound one compile pass needs.
data OsvEcosystem = OsvEcosystem
    { osvExportDirectory :: Text
    -- ^ The export directory and affected-package ecosystem spelling at osv.dev.
    , osvWireName :: Text
    -- ^ The internal name on the compiled artifact and its checked metadata row.
    , osvEcosystemTag :: Maybe Ecosystem
    -- ^ The native version grammar, absent for an unknown export.
    , osvMaxAdvisoryFanOut :: Int
    -- ^ The per-advisory fan-out alarm threshold. It never discards evidence.
    }
    deriving stock (Eq, Show)

-- One advisory of the npm export names a few hundred ranges, and a feed no one has measured
-- borrows this bound.
npmAdvisoryFanOut :: Int
npmAdvisoryFanOut = 256

-- The largest advisory of today's PyPI export names 2459 ranges, so no ordinary one trips this.
pypiAdvisoryFanOut :: Int
pypiAdvisoryFanOut = 4096

-- | Pair the OSV export directory with the internal ecosystem name and version grammar.
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
        Cargo -> "crates.io"

    fanOut = case eco of
        Npm -> npmAdvisoryFanOut
        PyPI -> pypiAdvisoryFanOut
        RubyGems -> npmAdvisoryFanOut
        Cargo -> npmAdvisoryFanOut

-- | Resolve an internal ecosystem name, retaining unknown export names without a version grammar.
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
