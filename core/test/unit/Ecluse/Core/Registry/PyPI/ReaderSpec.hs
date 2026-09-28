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

import Ecluse.Core.Registry.JsonStream (StreamResult)
import Ecluse.Core.Registry.PyPI.Reader (fileUniqueFields, pypiWalk)
import Ecluse.Core.Registry.PyPI.Streaming (PyPIField, PyPIRead (..))
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (TooManyVersions))
import Ecluse.Test.Package (unscopedPyPI)
import Ecluse.Test.Registry.JsonBytes (damaged, genChunks, genSimpleIndexBytes, releaseKeys)
import Ecluse.Test.Registry.JsonStream (parseJsonChunks, readOutcome, testTable, walkJsonChunks)
import Ecluse.Test.Registry.PyPI.Streaming (pypiFields)

-- | Every generated body reads to the same fields, refusal or failure class as json-stream's reader.
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
                let mode = maybe FullRead (SelectedRead (unscopedPyPI "thing")) selected
                    bound = MetadataBodyLimit (BS.length body)
                    step events field = case cap of
                        Just most | length events >= most -> Left (TooManyVersions (length events) most)
                        _ -> Right (field : events)
                readOutcome (parseJsonChunks bound (pypiFields depth mode) step [] chunks)
                    === readOutcome (walkJsonChunks bound (pypiWalk depth mode step (const True) (testTable fileUniqueFields) []) chunks :: Either LimitError (StreamResult [PyPIField]))
