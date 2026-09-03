-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Composition.TypesSpec (spec) where

import Data.Text qualified as T
import Test.Hspec

import Ecluse.Composition.BootError (BootError (SplitRoleNeedsDurableQueue), renderBootError)
import Ecluse.Composition.Types (
    BootRole (BootMirrorPipeline, BootStorePruner, BootWithoutPipeline),
    MirrorRole (MirrorOnly, ServeAndMirror, ServeOnly),
    bootInvocation,
    everyBootRole,
    roleInvocation,
 )

spec :: Spec
spec = do
    describe "roleInvocation -- the refusal quotes what the operator typed" $
        it "spells each role as its command line, so the message names a runnable fix" $ do
            roleInvocation ServeAndMirror `shouldBe` "ecluse proxy"
            roleInvocation ServeOnly `shouldBe` "ecluse proxy --no-worker"
            roleInvocation MirrorOnly `shouldBe` "ecluse mirror"
            renderBootError (SplitRoleNeedsDurableQueue (roleInvocation MirrorOnly))
                `shouldSatisfy` T.isInfixOf "ecluse mirror"

    describe "everyBootRole -- what a checker with no subcommand has to cover" $ do
        it "holds every role a boot runs under, so no role's rules go unrun" $
            everyBootRole
                `shouldBe` [ BootMirrorPipeline ServeAndMirror
                           , BootMirrorPipeline ServeOnly
                           , BootMirrorPipeline MirrorOnly
                           , BootStorePruner
                           , BootWithoutPipeline
                           ]

        it "names each entry as the command an operator would run" $
            map bootInvocation everyBootRole
                `shouldBe` ["ecluse proxy", "ecluse proxy --no-worker", "ecluse mirror", "ecluse dredger", "ecluse pilot"]
