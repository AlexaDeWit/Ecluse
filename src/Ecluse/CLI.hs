-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.CLI (
    AppCommand (..),
    commandParser,
    execCLI,
) where

import Data.Version (showVersion)
import Options.Applicative
import Paths_ecluse (version)

import Ecluse.Pilot (PilotCompileOptions (..))

data AppCommand
    = RunProxy
    | RunPilot
    | RunPilotCompile PilotCompileOptions
    | RunDredger
    deriving stock (Eq, Show)

commandParser :: Parser AppCommand
commandParser =
    hsubparser
        ( command "proxy" (info (pure RunProxy) (progDesc "Run the Écluse proxy server"))
            <> command "pilot" (info pilotCommandParser (progDesc "Run the Écluse Pilot (OSV ingestion pipeline)"))
            <> command "dredger" (info (pure RunDredger) (progDesc "Run the Écluse Dredger (mirror pruning worker)"))
        )
        <|> pure RunProxy

-- A bare @pilot@ keeps its serve-and-export meaning; @pilot compile@ selects
-- the one-shot mode.
pilotCommandParser :: Parser AppCommand
pilotCommandParser =
    hsubparser
        ( command
            "compile"
            (info (RunPilotCompile <$> pilotCompileOptionsParser) (progDesc "Compile one ecosystem's OSV export into a local osv.db artifact, then exit"))
        )
        <|> pure RunPilot

pilotCompileOptionsParser :: Parser PilotCompileOptions
pilotCompileOptionsParser =
    PilotCompileOptions
        <$> strOption (long "ecosystem" <> metavar "ECOSYSTEM" <> value "npm" <> showDefault <> help "Ecosystem whose OSV export to compile")
        <*> optional (strOption (long "source" <> metavar "URL" <> help "OSV export URL (defaults to the configured osvExportBaseUrl for ECOSYSTEM)"))
        <*> strOption (long "out" <> metavar "DIR" <> help "Directory the artifact is written into")
        <*> switch (long "upload" <> help "After compiling, upload the artifact to the configured vulnerability-database bucket (one full sync cycle)")

execCLI :: IO AppCommand
execCLI =
    execParser $
        info
            (commandParser <**> helper <**> versionOption)
            ( fullDesc
                <> progDesc "Écluse - supply-chain resilience proxy"
                <> header "ecluse - a configurable policy gate for package registries"
            )
  where
    versionOption = infoOption (showVersion version) (long "version" <> help "Show version")
