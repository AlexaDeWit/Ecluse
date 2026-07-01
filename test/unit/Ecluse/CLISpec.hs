module Ecluse.CLISpec (spec) where

import Options.Applicative (ParserResult (..), defaultPrefs, execParserPure, helper, idm, info)
import Test.Hspec

import Ecluse.CLI (AppCommand (..), commandParser)

parseCLI :: [String] -> ParserResult AppCommand
parseCLI args = execParserPure defaultPrefs (info (commandParser <**> helper) idm) args

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
