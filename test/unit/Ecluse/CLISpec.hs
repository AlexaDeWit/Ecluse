-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.CLISpec (spec) where

import Options.Applicative (ParserResult (..), defaultPrefs, execParserPure, helper, idm, info)
import Test.Hspec

import Ecluse.CLI (AppCommand (..), commandParser)
import Ecluse.Composition.Types (MirrorRole (MirrorOnly, ServeAndMirror, ServeOnly))
import Ecluse.Dredger.Plan (
    DredgerOptions (DredgerOptions, doMode, doRepetition),
    SweepMode (SweepDeletes, SweepPreviews),
    SweepRepetition (SweepContinuously, SweepOnce),
 )
import Ecluse.Pilot (PilotCompileOptions (..))

parseCLI :: [String] -> ParserResult AppCommand
parseCLI = execParserPure defaultPrefs (info (commandParser <**> helper) idm)

spec :: Spec
spec = describe "CLI commandParser" $ do
    for_ acceptedInvocations $ \(name, args, expected) ->
        it name $ case parseCLI args of
            Success cmd -> cmd `shouldBe` expected
            _ -> expectationFailure ("expected Success " <> show expected)

    for_ refusedInvocations $ \(name, args) ->
        it name $ case parseCLI args of
            Success cmd -> expectationFailure ("expected a parse failure, got " <> show cmd)
            _ -> pass

{- | Every invocation the parser accepts, and the command it settles on. The name carries what
the invocation means to an operator, which the argument list alone does not say.
-}
acceptedInvocations :: [(String, [String], AppCommand)]
acceptedInvocations =
    [ ("defaults to the serve-and-mirror role when no arguments are provided", [], RunService ServeAndMirror)
    , ("parses 'proxy' as the serve-and-mirror role (the worker stays embedded)", ["proxy"], RunService ServeAndMirror)
    , ("parses 'proxy --no-worker' as the serve-only role", ["proxy", "--no-worker"], RunService ServeOnly)
    , ("parses 'mirror' as the worker-only role", ["mirror"], RunService MirrorOnly)
    , ("parses 'pilot' as RunPilot", ["pilot"], RunPilot)
    ,
        ( "parses 'dredger' as the shipped invocation: cycling, and deleting"
        , ["dredger"]
        , RunDredger DredgerOptions{doMode = SweepDeletes, doRepetition = SweepContinuously}
        )
    , -- Both flags only narrow what one invocation does, so they compose.

        ( "parses 'dredger --once --dry-run' as one preview cycle"
        , ["dredger", "--once", "--dry-run"]
        , RunDredger DredgerOptions{doMode = SweepPreviews, doRepetition = SweepOnce}
        )
    ,
        ( "parses 'pilot compile' with the default ecosystem and canonical source"
        , ["pilot", "compile", "--out", "/tmp/osv"]
        , RunPilotCompile
            PilotCompileOptions
                { pcoEcosystem = "npm"
                , pcoSource = Nothing
                , pcoEpssSource = Nothing
                , pcoOutDir = "/tmp/osv"
                , pcoUpload = False
                }
        )
    ,
        ( "parses 'pilot compile' with ecosystem, both source overrides, and upload"
        , ["pilot", "compile", "--ecosystem", "npm", "--source", "http://127.0.0.1:9/all.zip", "--epss-source", "http://127.0.0.1:9/epss.csv.gz", "--out", "out", "--upload"]
        , RunPilotCompile
            PilotCompileOptions
                { pcoEcosystem = "npm"
                , pcoSource = Just "http://127.0.0.1:9/all.zip"
                , pcoEpssSource = Just "http://127.0.0.1:9/epss.csv.gz"
                , pcoOutDir = "out"
                , pcoUpload = True
                }
        )
    ]

-- | Every invocation the parser must refuse rather than settle on a nearby command.
refusedInvocations :: [(String, [String])]
refusedInvocations =
    [ ("rejects --no-worker on the dedicated worker, which has no worker to drop", ["mirror", "--no-worker"])
    , ("rejects 'pilot compile' without --out", ["pilot", "compile"])
    ]
