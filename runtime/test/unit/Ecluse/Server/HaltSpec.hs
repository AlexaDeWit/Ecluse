-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

module Ecluse.Server.HaltSpec (spec) where

import Test.Hspec
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Timeout (timeout)

import Ecluse.Runtime.Server.Halt (InteractiveHalt (..), withInteractiveHalt)

spec :: Spec
spec =
    describe "withInteractiveHalt (local-dev quit key)" $ do
        -- The real wiring (TTY guard, stdin EOF, _exit) is process-global and not drivable
        -- in-process, so these cases wire the three injection points and test the combinator.

        it "runs the action and never halts when NOT interactive (the production guard)" $ do
            halted <- newIORef False
            let ih =
                    InteractiveHalt
                        { haltOnInteractive = pure False
                        , awaitHaltSignal = pass -- would fire immediately, but must be ignored
                        , halt = writeIORef halted True
                        }
            result <- withInteractiveHalt ih (pure "served")
            result `shouldBe` ("served" :: Text)
            -- No watcher was installed, so the halt path was never reached.
            readIORef halted `shouldReturn` False

        it "halts when interactive and the halt signal fires (Ctrl-D)" $ do
            halted <- newEmptyMVar
            let ih =
                    InteractiveHalt
                        { haltOnInteractive = pure True
                        , awaitHaltSignal = pass -- stands in for an immediate stdin EOF
                        , halt = putMVar halted ()
                        }
            -- The action blocks. The watcher's signal fires at once and runs halt.
            -- In production halt is _exit. Here it records, so the test observes it.
            outcome <- timeout 1_000_000 (withInteractiveHalt ih (void (threadDelay 5_000_000)))
            -- The action did not complete on its own, since it was meant to be cut
            -- short. What matters is that halt ran.
            outcome `shouldBe` Nothing
            fired <- timeout 1_000_000 (takeMVar halted)
            fired `shouldBe` Just ()

        it "tears the watcher down with the action -- halt never fires once the action returns" $ do
            halted <- newIORef False
            let ih =
                    InteractiveHalt
                        { haltOnInteractive = pure True
                        , awaitHaltSignal = threadDelay 5_000_000 -- never fires within the test
                        , halt = writeIORef halted True
                        }
            -- The action completes promptly. 'withAsync' cancels the still-blocked
            -- watcher on the way out, so halt is never reached.
            result <- withInteractiveHalt ih (pure "done")
            result `shouldBe` ("done" :: Text)
            -- Give a cancelled watcher every chance to (wrongly) fire.
            threadDelay 50_000
            readIORef halted `shouldReturn` False
