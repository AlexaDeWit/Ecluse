-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The @ecluse@ command line: the subcommand grammar and the flags each one settles. A bare
invocation is @proxy@, so an operator who names no role gets the single-process pipeline.
'Ecluse.Startup.runWith' dispatches the 'AppCommand' this yields.
-}
module Ecluse.CLI (
    AppCommand (..),
    commandParser,
    execCLI,
) where

import Data.Scientific (Scientific, base10Exponent)
import Options.Applicative

import Ecluse.Composition.Types (MirrorRole (MirrorOnly, ServeAndMirror, ServeOnly))
import Ecluse.Core.BuildIdentity (productVersion)
import Ecluse.Core.Osv.Publication (ObjectRevision (ObjectRevision), Percent, Rebaseline (Rebaseline), mkPercent)
import Ecluse.Dredger.Plan (
    DredgerOptions (DredgerOptions),
    SweepMode (SweepDeletes, SweepPreviews),
    SweepRepetition (SweepContinuously, SweepOnce),
 )
import Ecluse.Pilot (PilotCompileOptions (..), UploadRequest (..))

data AppCommand
    = -- | The mirror pipeline in the selected role: @proxy@, @proxy --no-worker@, or @mirror@.
      RunService MirrorRole
    | RunPilot
    | RunPilotCompile PilotCompileOptions
    | -- | @ecluse dredger@, with the repetition and the mode its flags settled.
      RunDredger DredgerOptions
    | RunCheckConfig
    deriving stock (Eq, Show)

commandParser :: Parser AppCommand
commandParser =
    hsubparser
        ( command "proxy" (info (RunService <$> proxyRoleParser) (progDesc "Run the Écluse proxy server"))
            <> command "mirror" (info (pure (RunService MirrorOnly)) (progDesc "Run the Écluse mirror worker alone, for a worker fleet scaled on queue depth"))
            <> command "pilot" (info pilotCommandParser (progDesc "Run the Écluse Pilot (OSV ingestion pipeline)"))
            <> command "dredger" (info (RunDredger <$> dredgerOptionsParser) (progDesc "Run the Écluse Dredger (mirror pruning worker)"))
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

{- Both flags narrow what one invocation does, so absent they give the shipped behaviour: cycle
for the life of the process, and delete what a named decisive deny condemns. -}
dredgerOptionsParser :: Parser DredgerOptions
dredgerOptionsParser =
    DredgerOptions
        <$> flag
            SweepDeletes
            SweepPreviews
            (long "dry-run" <> help "Report what a cycle would delete and delete nothing; it holds no delete, writes no walk marker, and needs no deletion consent")
        <*> flag
            SweepContinuously
            SweepOnce
            (long "once" <> help "Run one cycle, then exit: 0 when it completed, 1 when it halted")

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
        <*> strOption (long "out" <> metavar "DIR" <> help "Directory the artifact is written into")
        <*> uploadRequestParser

-- A rebaseline names the revision it replaces, so it parses only beside --upload.
uploadRequestParser :: Parser UploadRequest
uploadRequestParser =
    flag' () (long "upload" <> help "After compiling, publish the artifact to the configured advisory store under the publication guard")
        *> (maybe Upload UploadRebaselined <$> optional rebaselineParser)
            <|> pure NoUpload

rebaselineParser :: Parser Rebaseline
rebaselineParser =
    (Rebaseline . ObjectRevision <$> strOption (long "rebaseline-etag" <> metavar "ETAG" <> help "Replace only the stored object with this ETag, then restart the size baseline at the new artifact"))
        <*> option percentReader (long "rebaseline-max-shrink-percent" <> metavar "PERCENT" <> help "The shrink this one rebaselining upload may take, from 0 to 100")

-- The exponent is bounded before the rational is realised, as the configuration's own percentages are.
percentReader :: ReadM Percent
percentReader = eitherReader $ \raw -> case readMaybe raw :: Maybe Scientific of
    Just n | abs (base10Exponent n) <= 9, Just percent <- mkPercent (toRational n) -> Right percent
    _ -> Left "expected a percentage from 0 to 100"

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
    versionOption = infoOption (toString productVersion) (long "version" <> help "Show version")
