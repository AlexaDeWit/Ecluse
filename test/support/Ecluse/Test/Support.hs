-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | Cross-cutting test helpers no single subsystem owns.

A helper that belongs to one subsystem lives in that subsystem's @Ecluse.Test.*@
module instead.
-}
module Ecluse.Test.Support (
    supportLinkageSpec,
    testServeAdmission,
    newTestClock,
    expectRight,
    decodeJsonOrFail,
    parseRequestOrFail,
    TestContractEscape (..),
) where

import Data.Aeson (FromJSON, eitherDecodeStrict)
import Data.Time (UTCTime)
import Network.HTTP.Client qualified as Client

import Ecluse.Core.Package (HashAlg (SHA256), renderHashAlg)
import Ecluse.Core.Server.Admission (ServeAdmission, newServeAdmission)
import Test.Hspec (Spec, describe, it, shouldBe)

{- | A trivial spec that touches a stable export of the library under test. A suite
that runs this spec compiled and linked against both this support library and
@ecluse@.
-}
supportLinkageSpec :: Spec
supportLinkageSpec =
    describe "ecluse-test-support" $
        it "is linked into the suite and can see the library under test" $
            renderHashAlg SHA256 `shouldBe` "sha256"

{- | A serve admission for suites that do not test overload. Its capacity sits far above any
test's in-flight load, so it never sheds.
-}
testServeAdmission :: IO ServeAdmission
testServeAdmission = newServeAdmission 1_000_000

{- | An IORef-backed clock a test advances by hand, so a case can elapse wall-clock time
without sleeping. The pair is the read action and the setter.
-}
newTestClock :: UTCTime -> IO (IO UTCTime, UTCTime -> IO ())
newTestClock start = do
    ref <- newIORef start
    pure (readIORef ref, writeIORef ref)

-- | Assert a 'Right' and return its value, failing the running example otherwise.
expectRight :: (Show e) => Either e a -> IO a
expectRight = either (\e -> fail ("expected Right, got Left " <> show e)) pure

-- | Decode JSON, failing the running example with the aeson error rather than crashing.
decodeJsonOrFail :: (FromJSON a) => ByteString -> IO a
decodeJsonOrFail bs = either (\e -> fail ("decode failure: " <> e)) pure (eitherDecodeStrict bs)

{- | Build an HTTP request from a URL, failing the running example on an unparseable one.
'Client.parseRequest' reports the failure through 'MonadThrow', which is 'IO' here.
-}
parseRequestOrFail :: Text -> IO Client.Request
parseRequestOrFail = Client.parseRequest . toString

{- | A typed stand-in for an exception thrown past a handle's typed contract. A test double
that must never be called throws this instead of a stringly exception.
-}
newtype TestContractEscape = TestContractEscape Text
    deriving stock (Eq, Show)

instance Exception TestContractEscape
