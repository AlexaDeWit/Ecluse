-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The walk's driver and token primitives against the json-stream parsers they stand in for.
module Ecluse.Core.Registry.Json.WalkSpec (spec) where

import Data.ByteString qualified as BS
import Data.JsonStream.Parser qualified as J
import Data.JsonStream.TokenParser (Element (ArrayBegin, ObjectBegin), TokenResult)
import Hedgehog (forAll, (===))
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Ecluse.Core.Registry (ParseError (ParseError))
import Ecluse.Core.Registry.Json.Intern (nameText)
import Ecluse.Core.Registry.Json.Walk
import Ecluse.Core.Registry.JsonStream (StreamResult (..))
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (BodyTooLarge, TooManyVersions))
import Ecluse.Test.Registry.JsonBytes (damaged, genChunks, genJsonBytes)
import Ecluse.Test.Registry.JsonStream (parseJsonChunks, readOutcome, walkJsonChunks)

-- | Each primitive matches its json-stream parser on generated bodies, and the driver keeps its contract.
spec :: Spec
spec = do
    describe "readJsonWalk" $ do
        it "drains trailing chunks after the walk finishes" $ do
            let chunks = ["\"thing\"", " trailing", " bytes"]
            walkJsonChunks bound (\tokens -> withElement tokens (\element rest -> readString element rest (\name _ -> Finished (nameText name)))) chunks
                `shouldBe` Right (StreamResult (Right "thing") (BS.length (BS.concat chunks)))

        it "refuses a body past its ceiling, including ignored trailing bytes" $
            walkJsonChunks (MetadataBodyLimit 2) skipOne ["{}", "x"] `shouldBe` Left (BodyTooLarge (MetadataBodyLimit 2))

        it "reports a body that ends inside a value as incomplete" $
            walkJsonChunks bound skipOne ["{\"keep\":"] `shouldBe` Right (StreamResult (Left (ParseError "incomplete registry JSON")) 8)

        it "passes a refused field through as the read's refusal" $ do
            let refused = emit (pureStep (\() () -> Left (TooManyVersions 2 1))) () () Finished
            walkJsonChunks bound (\tokens -> withElement tokens (\element rest -> skipFrom element rest (const refused))) ["{}"]
                `shouldBe` (Left (TooManyVersions 2 1) :: Either LimitError (StreamResult ()))

        it "fails an exhausted budget with the nesting limit after skipping the value" $
            readOutcome (walkJsonChunks bound (`withElement` tooDeep) ["[[1]]"] :: Either LimitError (StreamResult ()))
                `shouldBe` Right (5, Left True)

    describe "properties" $
        modifyMaxSuccess (const 1000) $ do
            it "skips a value where json-stream's ignoreVal does" $
                hedgehog $ do
                    (body, chunks) <- forAll generated
                    readOutcome (parseJsonChunks (limitOf body) (mempty :: J.Parser ()) (\s _ -> Right s) () chunks)
                        === readOutcome (walkJsonChunks (limitOf body) skipOne chunks)

            it "decodes a string where json-stream's string does, and skips anything else" $
                hedgehog $ do
                    (body, chunks) <- forAll generated
                    readOutcome (parseJsonChunks (limitOf body) J.string (\_ text -> Right (Just text)) Nothing chunks)
                        === readOutcome (walkJsonChunks (limitOf body) readOne chunks)

            it "visits the members json-stream's objectItems visits" $
                hedgehog $ do
                    (body, chunks) <- forAll generated
                    readOutcome (parseJsonChunks (limitOf body) (fst <$> J.objectItems (pure ())) (\keys key -> Right (key : keys)) [] chunks)
                        === readOutcome (walkJsonChunks (limitOf body) members chunks)

            it "visits the items json-stream's indexedArrayOf visits" $
                hedgehog $ do
                    (body, chunks) <- forAll generated
                    readOutcome (parseJsonChunks (limitOf body) (fst <$> J.indexedArrayOf (pure ())) (\positions position -> Right (position : positions)) [] chunks)
                        === readOutcome (walkJsonChunks (limitOf body) items chunks)
  where
    bound = MetadataBodyLimit (1024 * 1024)
    limitOf = MetadataBodyLimit . BS.length
    generated = do
        body <- genJsonBytes ["a", "b"] >>= damaged
        (body,) <$> genChunks body

skipOne :: TokenResult -> Step ()
skipOne tokens = withElement tokens $ \element rest -> skipFrom element rest (const (Finished ()))

readOne :: TokenResult -> Step (Maybe Text)
readOne tokens = withElement tokens $ \element rest ->
    if isString element
        then readString element rest (\name _ -> Finished (Just (nameText name)))
        else skipFrom element rest (const (Finished Nothing))

-- The keys of a top-level object, most recent first. Any other value is skipped.
members :: TokenResult -> Step [Text]
members tokens = withElement tokens $ \element rest -> case element of
    ObjectBegin -> eachMember visit (\keys _ -> Finished keys) [] rest
    _ -> skipFrom element rest (const (Finished []))
  where
    visit keys name rest continue = withElement rest $ \value afterKey -> skipFrom value afterKey (continue (nameText name : keys))

-- The positions of a top-level array, most recent first. Any other value is skipped.
items :: TokenResult -> Step [Int]
items tokens = withElement tokens $ \element rest -> case element of
    ArrayBegin -> eachItem visit (\positions _ -> Finished positions) [] rest
    _ -> skipFrom element rest (const (Finished []))
  where
    visit positions position value rest continue = skipFrom value rest (continue (position : positions))
