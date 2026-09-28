-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The packument walk against the json-stream field parser it replaces, on generated and edge-case bodies.
module Ecluse.Core.Registry.Npm.ReaderSpec (spec) where

import Data.ByteString qualified as BS
import Hedgehog (forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Registry.JsonStream (StreamResult)
import Ecluse.Core.Registry.Npm.Reader (PackumentRead (..), npmWalk, releaseUniqueFields)
import Ecluse.Core.Registry.Npm.Streaming (NpmField, NpmRead (..), npmFields)
import Ecluse.Core.Registry.Npm.StreamingProjection (NpmProjection, collectField, emptyProjection, keepsRelease)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (TooManyVersions), defaultLimits)
import Ecluse.Test.Package (unscopedNpm)
import Ecluse.Test.Registry.JsonBytes (damaged, genChunks, genPackumentBytes, releaseKeys)
import Ecluse.Test.Registry.JsonStream (parseJsonChunks, readOutcome, testTable, walkJsonChunks)

-- | Every generated body reads to the same fields, refusal or failure class as json-stream's reader.
spec :: Spec
spec = describe "npmWalk" $ do
    modifyMaxSuccess (const 2000) $
        it "emits json-stream's fields and outcome for generated packuments" $
            hedgehog $ do
                body <- forAll (genPackumentBytes >>= damaged)
                chunks <- forAll (genChunks body)
                depth <- forAll (Gen.frequency [(3, pure 64), (2, Gen.int (Range.linear 0 7))])
                selected <- forAll (Gen.maybe (Gen.element ("absent" : map decodeUtf8 releaseKeys)))
                cap <- forAll (Gen.maybe (Gen.int (Range.linear 0 20)))
                production <- forAll Gen.bool
                let (reference, walked) = bothReads production depth selected cap chunks
                reference === walked

    it "drops a member whose key runs past 64 KiB across three pieces, as json-stream does" $ do
        let key = BS.replicate 70000 0x6b
            body = "{\"name\":\"thing\",\"" <> key <> "\":{\"a\":1},\"versions\":{}}"
            chunks = [BS.take 20 body, BS.take 32768 (BS.drop 20 body), BS.drop 32788 body]
            (reference, walked) = bothReads True 64 Nothing Nothing chunks
        walked `shouldBe` reference
        walked `shouldSatisfy` either (const False) (isRight . snd)

    it "fails a number past json-stream's digit limit across pieces, as json-stream does" $ do
        let body = "{\"name\":" <> BS.replicate 300000 0x31 <> "}"
            chunks = [BS.take 32768 (BS.drop offset body) | offset <- [0, 32768 .. BS.length body]]
            (reference, walked) = bothReads True 64 Nothing Nothing chunks
        walked `shouldBe` reference
        walked `shouldSatisfy` either (const False) (isLeft . snd)

{- | Both reads through the production projection, with its keep predicate or with every release
kept, compared by the fields each emits.
-}
bothReads :: Bool -> Int -> Maybe Text -> Maybe Int -> [ByteString] -> (Either LimitError (Int, Either Bool [NpmField]), Either LimitError (Int, Either Bool [NpmField]))
bothReads production depth selected cap chunks =
    ( emitted (parseJsonChunks bound (npmFields depth (maybe FullRead SelectedRead selected)) step start chunks)
    , emitted (walkJsonChunks bound (npmWalk depth (maybe WholePackument OneRelease selected) step keeps (testTable releaseUniqueFields) start) chunks)
    )
  where
    bound = MetadataBodyLimit (sum (map BS.length chunks))
    start = (emptyProjection, [])
    keeps (projection, _) key = not production || keepsRelease projection key
    emitted :: Either LimitError (StreamResult (NpmProjection, [NpmField])) -> Either LimitError (Int, Either Bool [NpmField])
    emitted = fmap (second (fmap snd)) . readOutcome
    step (projection, events) field = do
        projected <- collectField defaultLimits (unscopedNpm "thing") projection field
        case cap of
            Just most | length events >= most -> Left (TooManyVersions (length events) most)
            _ -> Right (projected, field : events)
