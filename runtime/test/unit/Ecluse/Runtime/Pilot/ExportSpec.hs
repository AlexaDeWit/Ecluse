-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Guarded publication over a conditional S3 endpoint: what it reads, what it writes, and how it settles a race or a lost response.
module Ecluse.Runtime.Pilot.ExportSpec (spec) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.List (lookup)
import Data.Text qualified as T
import Network.HTTP.Types
import Network.HTTP.Types.Header (hETag, hIfMatch, hIfNoneMatch)
import Network.Wai (Application, Request, queryString, requestHeaders, requestMethod, responseLBS, responseRaw, strictRequestBody)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Ecluse.Core.Ecosystem (Ecosystem (Npm, PyPI))
import Ecluse.Core.Fault (TransportFault (tfDetail))
import Ecluse.Core.Osv.Compile (OsvCandidate, candidatePath)
import Ecluse.Core.Osv.Publication
import Ecluse.Runtime.Aws.S3.Conditional (maxCappedResponseBytes)
import Ecluse.Runtime.Pilot.Export
import Ecluse.Runtime.Test.S3 (emptyListing, listingHolding, reply, withS3, xmlError)
import Ecluse.Test.Log (runQuietKatip)
import Ecluse.Test.Osv (CorpusVersion (CorpusV1), osvCorpusZip)
import Ecluse.Test.OsvDb (withCompiledCandidate)

spec :: Spec
spec = describe "guarded advisory publication over S3" $
    for_ [Npm, PyPI] $ \eco -> describe (show eco) $ do
        it "reads only the destination's metadata, then creates the object only if it is still absent" $
            withPublication eco noHooks $ \fake candidate publish -> do
                fmap (plPrecondition . pubPlan) <$> publish `shouldReturn` Right CreateOnly
                requests <- readIORef (fakeLog fake)
                map (\seen -> (seenMethod seen, seenQuery seen)) requests `shouldBe` [(methodHead, []), (methodPut, [])]
                (lookup hIfNoneMatch . seenHeaders =<< latest requests) `shouldBe` Just "*"
                stored <- expectStored fake
                readFileLBS (candidatePath candidate) `shouldReturn` storedBody stored
                lookup "x-amz-meta-baseline-adjustment" (storedMeta stored) `shouldBe` Just "initial"

        it "replaces only the revision it read, carrying the new metadata in the same write" $
            withPublication eco noHooks $ \fake _ publish -> do
                void publish
                first' <- expectStored fake
                fmap (plPrecondition . pubPlan) <$> publish `shouldReturn` Right (ReplaceRevision (ObjectRevision (decodeUtf8 (storedEtag first'))))
                requests <- readIORef (fakeLog fake)
                (lookup hIfMatch . seenHeaders =<< latest requests) `shouldBe` Just (storedEtag first')
                replaced <- expectStored fake
                storedEtag replaced `shouldNotBe` storedEtag first'
                lookup nonceHeader (storedMeta replaced) `shouldNotBe` lookup nonceHeader (storedMeta first')

        it "rereads and decides again after losing a race, then publishes" $
            withPublication eco (onPut 1 (Respond status412)) $ \fake _ publish -> do
                publish >>= (`shouldSatisfy` isRight)
                map seenMethod <$> readIORef (fakeLog fake) `shouldReturn` [methodHead, methodPut, methodHead, methodPut]

        it "gives up once every attempt loses a race" $
            withPublication eco (\_ seen -> Respond status412 <$ guard (seenMethod seen == methodPut)) $ \fake _ publish -> do
                publish `shouldReturn` Left (PublicationFaulted (StoreConflictsExhausted maxPublicationAttempts))
                length . filter ((== methodPut) . seenMethod) <$> readIORef (fakeLog fake) `shouldReturn` maxPublicationAttempts

        it "recognises its own write after the acknowledgement is lost" $
            withPublication eco (onPut 1 StoreThenBreak) $ \fake _ publish -> do
                published <- publish >>= either (fail . show) pure
                stored <- expectStored fake
                lookup nonceHeader (storedMeta stored) `shouldBe` Just (encodeUtf8 (renderPublicationNonce (psNonce (plState (pubPlan published)))))
                fmap seenMethod . latest <$> readIORef (fakeLog fake) `shouldReturn` Just methodHead

        it "reports an unknown outcome, and that the reread found no object, when no write landed" $
            withPublication eco (\_ seen -> Break <$ guard (seenMethod seen == methodPut)) $ \fake _ publish -> do
                publish >>= (`shouldSatisfy` uncertainWith RereadNoObject)
                isNothing <$> readIORef (fakeObject fake) `shouldReturn` True

        it "names the other revision a reread finds after an unanswered write" $
            withPublication eco (\_ seen -> StoreOtherThenBreak <$ guard (seenMethod seen == methodPut)) $ \_ _ publish ->
                publish >>= (`shouldSatisfy` uncertainWith (RereadOtherRevision (ObjectRevision "\"someone-else\"")))

        it "bounds an error body before the decoder reads it, then lets the reread decide" $
            withPublication eco (\_ seen -> Serve (reply status500 [] (LBS.replicate 70000 0x78)) <$ guard (seenMethod seen == methodPut)) $ \_ _ publish ->
                publish >>= \case
                    Left (PublicationFaulted (StoreWriteUncertain fault RereadNoObject)) -> tfDetail fault `shouldSatisfy` T.isInfixOf (show maxCappedResponseBytes <> "-byte bound")
                    other -> expectationFailure ("expected a bounded, unproven write, got " <> show other)

        it "refuses to replace an object without publication metadata, and writes nothing" $
            withPublication eco noHooks $ \fake _ publish -> do
                writeIORef (fakeObject fake) (Just (Stored "\"legacy\"" "legacy bytes" []))
                publish >>= \case
                    Left (PublicationRefused (PredecessorUnreadable (EnvelopeMissing keys))) -> toList keys `shouldSatisfy` (\missing -> all (`elem` missing) ["publication-version", "baseline-bytes"])
                    other -> expectationFailure ("expected a missing-metadata refusal, got " <> show other)
                map seenMethod <$> readIORef (fakeLog fake) `shouldReturn` [methodHead]

        it "refuses stored metadata past the store's limit" $
            withPublication eco noHooks $ \fake _ publish -> do
                writeIORef (fakeObject fake) (Just (Stored "\"padded\"" "bytes" [("x-amz-meta-padding", BS.replicate 2100 0x78)]))
                publish >>= (`shouldSatisfy` tooLarge)

        it "refuses a store that does not implement conditional writes" $
            withPublication eco (onPut 1 (Respond status501)) $ \_ _ publish ->
                publish `shouldReturn` Left (PublicationFaulted StoreConditionalWriteUnsupported)

        for_
            [ ("an empty listing proves the key absent", reply status200 [] emptyListing, Right CreateOnly)
            , ("a listing holding the key is a denied read", reply status200 [] (listingHolding "advisories/artifact.db"), Left (PublicationFaulted StoreReadDenied))
            , ("a truncated listing proves nothing", reply status200 [] "<ListBucketResult><IsTruncated>true</IsTruncated></ListBucketResult>", Left (PublicationFaulted StoreAbsenceUnknown))
            , ("a refused listing names the permissions", reply status403 [] (xmlError "AccessDenied"), Left (PublicationFaulted StoreListDenied))
            , ("an oversized listing is refused unread", reply status200 [] (LBS.replicate 70000 0x20), Left (PublicationFaulted (StoreResponseTooLarge maxCappedResponseBytes)))
            ]
            $ \(label, listing, expected) ->
                it ("after a refused HEAD, " <> label) $
                    withPublication eco (deniedHead listing) $ \_ _ publish ->
                        fmap (plPrecondition . pubPlan) <$> publish `shouldReturn` expected
  where
    uncertainWith finding = \case
        Left (PublicationFaulted (StoreWriteUncertain _ found)) -> found == finding
        _ -> False
    tooLarge = \case
        Left (PublicationRefused (PredecessorUnreadable (EnvelopeTooLarge _))) -> True
        _ -> False

nonceHeader :: HeaderName
nonceHeader = "x-amz-meta-publication-nonce"

-- The one object the fake holds.
data Stored = Stored
    { storedEtag :: ByteString
    , storedBody :: LByteString
    , storedMeta :: [(HeaderName, ByteString)]
    }

-- One request as the fake saw it.
data Seen = Seen
    { seenMethod :: Method
    , seenQuery :: Query
    , seenHeaders :: RequestHeaders
    , seenBody :: LByteString
    }

data Fake = Fake
    { fakeObject :: IORef (Maybe Stored)
    , fakeLog :: IORef [Seen]
    , fakePuts :: IORef Int
    }

-- What a hooked request answers in place of the fake's conditional store.
data Override = Respond Status | StoreThenBreak | StoreOtherThenBreak | Break | Serve Application

-- A hook sees each request with the count of PUTs so far, this one included.
type Hooks = Int -> Seen -> Maybe Override

noHooks :: Hooks
noHooks _ _ = Nothing

onPut :: Int -> Override -> Hooks
onPut nth override puts seen = override <$ guard (seenMethod seen == methodPut && puts == nth)

deniedHead :: Application -> Hooks
deniedHead listing _ seen
    | seenMethod seen == methodHead = Just (Respond status403)
    | not (null (seenQuery seen)) = Just (Serve listing)
    | otherwise = Nothing

withPublication :: Ecosystem -> Hooks -> (Fake -> OsvCandidate -> IO (Either PublicationStop Published) -> IO ()) -> IO ()
withPublication eco hooks body = withSystemTempDirectory "ecluse-export-spec" $ \dir -> do
    archive <- osvCorpusZip CorpusV1
    fake <- Fake <$> newIORef Nothing <*> newIORef [] <*> newIORef 0
    policy <- maybe (fail "fixture policy") pure (ShrinkPolicy <$> mkPercent 2 <*> mkPercent 5)
    withCompiledCandidate eco archive dir $ \candidate ->
        withS3 (fakeS3 fake hooks) $ \env ->
            body fake candidate (runQuietKatip (publishCandidate Nothing env policy DurableBaseline Nothing "records" "advisories/artifact.db" candidate))

-- An S3 that keeps one object, enforces both preconditions, and returns the metadata it stored.
fakeS3 :: Fake -> Hooks -> Application
fakeS3 fake hooks request respond = do
    seen <- observe request
    puts <- if seenMethod seen == methodPut then atomicModifyIORef' (fakePuts fake) (\n -> (n + 1, n + 1)) else readIORef (fakePuts fake)
    modifyIORef' (fakeLog fake) (<> [seen])
    case hooks puts seen of
        Just (Respond status) -> respond (responseLBS status [] (if seenMethod seen == methodHead then "" else xmlError "Refused"))
        Just (Serve application) -> application request respond
        Just StoreThenBreak -> store puts seen >> lostResponse request respond
        Just StoreOtherThenBreak -> writeIORef (fakeObject fake) (Just (Stored "\"someone-else\"" "other bytes" [])) >> lostResponse request respond
        Just Break -> lostResponse request respond
        Nothing
            | seenMethod seen == methodHead ->
                readIORef (fakeObject fake) >>= \case
                    Nothing -> respond (responseLBS status404 [] "")
                    Just stored -> respond (responseLBS status200 ([(hETag, storedEtag stored), (hContentLength, show (LBS.length (storedBody stored)))] <> storedMeta stored) "")
            | seenMethod seen == methodPut -> conditionalPut puts seen >>= respond
            | otherwise -> respond (responseLBS status400 [] (xmlError "Unexpected"))
  where
    conditionalPut puts seen = do
        current <- readIORef (fakeObject fake)
        let allowed = case (lookup hIfNoneMatch (seenHeaders seen), lookup hIfMatch (seenHeaders seen)) of
                (Just "*", _) -> isNothing current
                (_, Just expected) -> (storedEtag <$> current) == Just expected
                _ -> True
        if allowed
            then (\stored -> responseLBS status200 [(hETag, storedEtag stored)] "") <$> store puts seen
            else pure (responseLBS status412 [] (xmlError "PreconditionFailed"))
    store puts seen = do
        let stored = Stored ("\"revision-" <> show puts <> "\"") (seenBody seen) [(name, value) | (name, value) <- seenHeaders seen, "x-amz-meta-" `BS.isPrefixOf` CI.foldedCase name]
        stored <$ writeIORef (fakeObject fake) (Just stored)

-- Close the connection without answering, as a lost acknowledgement does.
lostResponse :: Application
lostResponse _ respond = respond (responseRaw (\_ _ -> pass) (responseLBS status500 [] "raw response required"))

observe :: Request -> IO Seen
observe request = Seen (requestMethod request) (queryString request) (requestHeaders request) <$> strictRequestBody request

expectStored :: Fake -> IO Stored
expectStored fake = readIORef (fakeObject fake) >>= maybe (fail "nothing was stored") pure

latest :: [a] -> Maybe a
latest = listToMaybe . reverse
