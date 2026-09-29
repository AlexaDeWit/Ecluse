-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The Simple-index walk against the json-stream field parser it replaces, on generated bodies.
module Ecluse.Core.Registry.PyPI.ReaderSpec (spec) where

import Data.ByteString qualified as BS
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Registry.Json.Shape (Trees (..))
import Ecluse.Core.Registry.Json.Walk (Walked (..), pureStep)
import Ecluse.Core.Registry.JsonStream (StreamResult (..))
import Ecluse.Core.Registry.PyPI.Reader (fileUniqueFields, pypiWalk)
import Ecluse.Core.Registry.PyPI.Streaming (PyPIField, PyPIRead (..))
import Ecluse.Core.Registry.PyPI.StreamingProjection (TreeRead, emptyTreeRead, keepsTreeFile, treeStep)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (TooManyVersions), defaultLimits)
import Ecluse.Test.Package (unscopedPyPI)
import Ecluse.Test.Registry.JsonBytes (damaged, genChunks, genSimpleIndexBytes, releaseKeys)
import Ecluse.Test.Registry.JsonStream (parseJsonChunks, readOutcome, testTable, walkJsonChunks)
import Ecluse.Test.Registry.PyPI.Streaming (pypiFields)

{- | Every generated body reads to the same fields, refusal or failure class as json-stream's reader,
through the production projection with its keep predicate or with every file kept.
-}
spec :: Spec
spec = describe "pypiWalk" $
    modifyMaxSuccess (const 2000) $
        it "emits json-stream's fields and outcome for generated Simple indexes" $
            hedgehog $ do
                body <- forAll (genSimpleIndexBytes >>= damaged)
                chunks <- forAll (genChunks body)
                depth <- forAll (Gen.frequency [(3, pure 64), (2, Gen.int (Range.linear 0 6))])
                selected <- forAll (Gen.maybe (Gen.element ("9.9" : map decodeUtf8 releaseKeys)))
                cap <- forAll (Gen.maybe (Gen.int (Range.linear 0 20)))
                production <- forAll Gen.bool
                let mode = maybe FullRead (SelectedRead (unscopedPyPI "thing")) selected
                    bound = MetadataBodyLimit (BS.length body)
                    start = (emptyTreeRead (unscopedPyPI "thing"), [])
                    keeps (projection, _) = not production || keepsTreeFile projection
                    step (projection, events) field = do
                        projected <- treeStep defaultLimits mode projection field
                        case cap of
                            Just most | length events >= most -> Left (TooManyVersions (length events) most)
                            _ -> Right (projected, field : events)
                emitted (parseJsonChunks bound (pypiFields depth mode) step start chunks)
                    === emitted (unwalked <$> walkJsonChunks bound (pypiWalk Trees depth mode (pureStep step) keeps (testTable fileUniqueFields) start) chunks)

emitted :: Either LimitError (StreamResult (TreeRead, [PyPIField])) -> Either LimitError (Int, Either Bool [PyPIField])
emitted = fmap (second (fmap snd)) . readOutcome

-- The consumer's state a walk finished with, without the read's table.
unwalked :: StreamResult (Walked s) -> StreamResult s
unwalked result = result{streamValue = (\(Walked _ held) -> held) <$> streamValue result}
