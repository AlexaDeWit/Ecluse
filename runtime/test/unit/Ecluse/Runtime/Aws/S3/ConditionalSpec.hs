-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The conditional write signs its precondition, and the fallback listing proves absence only when complete and empty.
module Ecluse.Runtime.Aws.S3.ConditionalSpec (spec) where

import Amazonka qualified as AWS
import Amazonka.S3 qualified as S3
import Conduit (runResourceT)
import Data.ByteString qualified as BS
import Data.List (lookup)
import Network.HTTP.Types
import Network.HTTP.Types.Header (hETag, hIfMatch, hIfNoneMatch)
import Network.Wai (queryString, requestHeaders, responseLBS, strictRequestBody)
import Test.Hspec

import Ecluse.Runtime.Aws.S3.Conditional
import Ecluse.Runtime.Test.S3 (emptyListing, populatedListing, reply, withS3, xmlError)

spec :: Spec
spec = do
    describe "ConditionalPut" $ do
        for_ [(IfAbsent, hIfNoneMatch, "*", hIfMatch, "if-none-match"), (IfRevision "\"old\"", hIfMatch, "\"old\"", hIfNoneMatch, "if-match")] $ \(precondition, signed, value, absent, signedName) ->
            it ("signs " <> show signed <> " for " <> show precondition <> ", and only that precondition") $ do
                seen <- newIORef Nothing
                withS3
                    ( \request respond -> do
                        body <- strictRequestBody request
                        writeIORef seen (Just (requestHeaders request, body))
                        respond (responseLBS status200 [(hETag, "\"new\"")] "")
                    )
                    $ \env -> void (putArtifact env precondition >>= either (fail . show) pure)
                readIORef seen >>= \case
                    Nothing -> expectationFailure "no request reached the endpoint"
                    Just (headers, body) -> do
                        lookup signed headers `shouldBe` Just value
                        lookup absent headers `shouldBe` Nothing
                        lookup hAuthorization headers `shouldSatisfy` maybe False (BS.isInfixOf signedName)
                        body `shouldBe` "artifact"

        for_ [(status409, True), (status412, True), (status403, False), (status500, False)] $ \(status, conflict) ->
            it ("reads " <> show (statusCode status) <> " as a lost race: " <> show conflict) $
                withS3 (reply status [] (xmlError "Refused")) $ \env ->
                    putArtifact env IfAbsent >>= \case
                        Left err -> isWriteConflict err `shouldBe` conflict
                        Right _ -> expectationFailure "the refused write succeeded"

    describe "absenceListing" $ do
        it "lists one key under the exact prefix"
            $ withS3
                ( \request respond -> do
                    lookup "prefix" (queryString request) `shouldBe` Just (Just "records/key")
                    lookup "max-keys" (queryString request) `shouldBe` Just (Just "1")
                    respond (responseLBS status200 [] emptyListing)
                )
            $ \env -> list env `shouldReturn` True

        for_
            [ ("a complete, empty listing", emptyListing, True)
            , ("a truncated listing", "<ListBucketResult><IsTruncated>true</IsTruncated></ListBucketResult>", False)
            , ("a listing without truncation evidence", "<ListBucketResult><KeyCount>0</KeyCount></ListBucketResult>", False)
            , ("a listing holding the key", populatedListing, False)
            ]
            $ \(label, listing, absent) ->
                it ("proves absence from " <> label <> ": " <> show absent) $
                    withS3 (reply status200 [] listing) $ \env ->
                        list env `shouldReturn` absent
  where
    putArtifact env precondition =
        runResourceT (AWS.sendEither env (ConditionalPut precondition (S3.newPutObject (S3.BucketName "records") (S3.ObjectKey "key") (AWS.toBody ("artifact" :: ByteString)))))
    list env = runResourceT (AWS.sendEither env (absenceListing "records" "records/key")) >>= either (fail . show) (pure . listingProvesAbsence)
