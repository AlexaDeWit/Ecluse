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

import Ecluse.Composition.MirrorRole (MirrorRole (MirrorOnly, ServeAndMirror, ServeOnly))
import Ecluse.Pilot (PilotCompileOptions (..))

data AppCommand
    = -- | The mirror pipeline in the selected role: @proxy@, @proxy --no-worker@, or @mirror@.
      RunService MirrorRole
    | RunPilot
    | RunPilotCompile PilotCompileOptions
    | RunDredger
    | RunCheckConfig
    deriving stock (Eq, Show)

commandParser :: Parser AppCommand
commandParser =
    hsubparser
        ( command "proxy" (info (RunService <$> proxyRoleParser) (progDesc "Run the Écluse proxy server"))
            <> command "mirror" (info (pure (RunService MirrorOnly)) (progDesc "Run the Écluse mirror worker alone, for a worker fleet scaled on queue depth"))
            <> command "pilot" (info pilotCommandParser (progDesc "Run the Écluse Pilot (OSV ingestion pipeline)"))
            <> command "dredger" (info (pure RunDredger) (progDesc "Run the Écluse Dredger (mirror pruning worker)"))
            <> command "check-config" (info (pure RunCheckConfig) (progDesc "Validate the configuration and print the resolved posture, then exit (0 valid, 2 refused)"))
        )
        <|> pure (RunService ServeAndMirror)

-- Absent, the proxy embeds the worker, which is what the in-memory queue requires.
proxyRoleParser :: Parser MirrorRole
proxyRoleParser =
    flag
        ServeAndMirror
        ServeOnly
        (long "no-worker" <> help "Serve without the embedded mirror worker; needs a durable ECLUSE_QUEUE__URL and an 'ecluse mirror' fleet to drain it")

-- A bare @pilot@ keeps its serve-and-export meaning. @pilot compile@ selects the
-- one-shot mode.
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
        <*> optional (strOption (long "epss-source" <> metavar "URL" <> help "EPSS feed URL (defaults to the configured epssFeedUrl)"))
        <*> strOption (long "out" <> metavar "DIR" <> help "Directory the artifact is written into")
        <*> switch (long "upload" <> help "After compiling, upload the artifact to the configured advisory store (one full sync cycle)")

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
