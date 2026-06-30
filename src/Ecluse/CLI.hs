module Ecluse.CLI (
    AppCommand (..),
    commandParser,
    execCLI,
) where

import Options.Applicative

data AppCommand
    = RunProxy
    | RunPilot
    | RunDredger
    deriving stock (Eq, Show)

commandParser :: Parser AppCommand
commandParser =
    subparser
        ( command "proxy" (info (pure RunProxy) (progDesc "Run the Écluse proxy server"))
            <> command "pilot" (info (pure RunPilot) (progDesc "Run the Écluse Pilot (OSV ingestion pipeline)"))
            <> command "dredger" (info (pure RunDredger) (progDesc "Run the Écluse Dredger (mirror pruning worker)"))
        )

execCLI :: IO AppCommand
execCLI =
    execParser $
        info
            (commandParser <**> helper)
            ( fullDesc
                <> progDesc "Écluse - supply-chain resilience proxy"
                <> header "ecluse - a configurable policy gate for package registries"
            )
