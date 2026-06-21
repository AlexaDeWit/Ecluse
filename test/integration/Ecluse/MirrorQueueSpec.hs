module Ecluse.MirrorQueueSpec (spec) where

import Test.Hspec

{- | Integration tests exercise AWS-backed code against a real endpoint provided
by a @ministack@ container (launched via @testcontainers@). @amazonka@ is pointed
at the container with throwaway credentials, so they are hermetic and __gating__
— but they require a running Docker daemon and no real AWS.

Real cases are added alongside the mirror-queue implementation; this placeholder
keeps the suite wired up.
-}
spec :: Spec
spec =
    describe "mirror queue (ministack)" $
        it "enqueues and consumes an SQS mirror job" pending
