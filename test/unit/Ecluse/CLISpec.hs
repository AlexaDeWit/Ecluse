module Ecluse.CLISpec (spec) where

import Options.Applicative (ParserResult (..), defaultPrefs, execParserPure, helper, idm, info)
import Test.Hspec

import Ecluse.CLI (AppCommand (..), commandParser)
import Ecluse.Pilot (PilotCompileOptions (..))

parseCLI :: [String] -> ParserResult AppCommand
parseCLI = execParserPure defaultPrefs (info (commandParser <**> helper) idm)

spec :: Spec
spec = do
    describe "CLI commandParser" $ do
        it "defaults to RunProxy when no arguments are provided" $ do
            case parseCLI [] of
                Success cmd -> cmd `shouldBe` RunProxy
                _ -> expectationFailure "expected Success RunProxy"

        it "parses 'proxy' as RunProxy" $ do
            case parseCLI ["proxy"] of
                Success cmd -> cmd `shouldBe` RunProxy
                _ -> expectationFailure "expected Success RunProxy"

        it "parses 'pilot' as RunPilot" $ do
            case parseCLI ["pilot"] of
                Success cmd -> cmd `shouldBe` RunPilot
                _ -> expectationFailure "expected Success RunPilot"

        it "parses 'dredger' as RunDredger" $ do
            case parseCLI ["dredger"] of
                Success cmd -> cmd `shouldBe` RunDredger
                _ -> expectationFailure "expected Success RunDredger"

        it "parses 'pilot compile' with the default ecosystem and canonical source" $ do
            case parseCLI ["pilot", "compile", "--out", "/tmp/osv"] of
                Success cmd ->
                    cmd
                        `shouldBe` RunPilotCompile
                            PilotCompileOptions
                                { pcoEcosystem = "npm"
                                , pcoSource = Nothing
                                , pcoOutDir = "/tmp/osv"
                                , pcoUpload = False
                                }
                _ -> expectationFailure "expected Success RunPilotCompile"

        it "parses 'pilot compile' with ecosystem, source, and upload overrides" $ do
            case parseCLI ["pilot", "compile", "--ecosystem", "npm", "--source", "http://127.0.0.1:9/all.zip", "--out", "out", "--upload"] of
                Success cmd ->
                    cmd
                        `shouldBe` RunPilotCompile
                            PilotCompileOptions
                                { pcoEcosystem = "npm"
                                , pcoSource = Just "http://127.0.0.1:9/all.zip"
                                , pcoOutDir = "out"
                                , pcoUpload = True
                                }
                _ -> expectationFailure "expected Success RunPilotCompile"

        it "rejects 'pilot compile' without --out" $ do
            case parseCLI ["pilot", "compile"] of
                Success cmd -> expectationFailure ("expected a parse failure, got " <> show cmd)
                _ -> pure ()
