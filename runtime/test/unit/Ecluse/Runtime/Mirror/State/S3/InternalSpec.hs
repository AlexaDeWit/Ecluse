-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Wire tests use static credentials and an in-process S3 endpoint.
module Ecluse.Runtime.Mirror.State.S3.InternalSpec (spec) where

import Data.ByteString qualified as BS
import Data.List (lookup)
import Network.HTTP.Types
import Network.HTTP.Types.Header (hETag, hIfMatch, hIfNoneMatch)
import Network.Wai (Application, queryString, requestHeaders, requestMethod, responseLBS, responseStream, strictRequestBody)
import Test.Hspec
import UnliftIO.Async (AsyncCancelled (AsyncCancelled), cancel, waitCatch, withAsync)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (finally, throwIO)
import UnliftIO.Timeout (timeout)

import Ecluse.Core.Ecosystem (Ecosystem (PyPI))
import Ecluse.Core.Mirror.State (MirrorState (..), MirrorStateFault (..), extendProjectRecord, stateTarget, verifyCompanionBytes)
import Ecluse.Core.Mirror.State.Codec (stateExchangeCharge)
import Ecluse.Core.Mirror.State.Internal (CompanionRef (..), StateRevision (..))
import Ecluse.Core.Package (HashAlg (SHA256), mkHash, mkPackageName)
import Ecluse.Core.Registry.MirrorUpload (inventoryFiles)
import Ecluse.Core.Security (Limits (..), defaultLimits, mkProgressFloor)
import Ecluse.Core.Server.Admission.Meter (newChargeRegion)
import Ecluse.Core.Server.MemoryModel (bufferedChunkCharge)
import Ecluse.Runtime.Mirror.State.S3 (mirrorStateForEnv)
import Ecluse.Runtime.Mirror.State.S3.Internal
import Ecluse.Runtime.Test.S3 (brokenResponse, emptyListing, listingReply, populatedListing, reply, withS3, xmlError)
import Ecluse.Test.Mirror.State (mirrorPackage, mirrorSample)
import Ecluse.Test.Package (hexSha256Of)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape), expectRight)

spec :: Spec
spec = describe "conditional mirror metadata storage" $ do
    it "pays each streamed chunk before the server releases the next chunk" $ do
        release <- newEmptyMVar
        payments <- newIORef ([] :: [Int])
        charge <- newChargeRegion $ \cost -> do
            paid <- atomicModifyIORef' payments (\previous -> let updated = cost : previous in (updated, sum updated))
            when (paid == stateExchangeCharge + bufferedChunkCharge 1) (putMVar release True)
        withS3 (gatedChunks release) $ \env ->
            ( timeout paymentWaitMicros (readObject charge env limits{maxMetadataBytes = 2} "records" "key" Nothing)
                `shouldReturn` Just (Right (ObjectLoaded "\"one\"" "ab"))
            )
                `finally` void (tryPutMVar release False)
        reverse <$> readIORef payments `shouldReturn` [stateExchangeCharge, bufferedChunkCharge 1, bufferedChunkCharge 1]

    it "propagates refused chunk payment without returning a decoded project" $ do
        release <- newEmptyMVar
        paid <- newIORef (0 :: Int)
        let refused = TestContractEscape "state chunk payment refused"
        charge <- newChargeRegion $ \cost -> do
            previous <- readIORef paid
            if previous == 0 then writeIORef paid cost else throwIO refused
        withS3 (gatedChunks release) $ \env -> do
            let store = mirrorStateForEnv limits "records" "state" (stateTarget "https://private.test") env
            ( timeout paymentWaitMicros (msReadProject store charge (mkPackageName PyPI Nothing "demo") Nothing)
                    `shouldThrow` (== refused)
                )
                `finally` void (tryPutMVar release False)
        readIORef paid `shouldReturn` stateExchangeCharge

    it "propagates cancellation during a streamed chunk payment" $ do
        release <- newEmptyMVar
        entered <- newEmptyMVar
        blocked <- newEmptyMVar
        paid <- newIORef (0 :: Int)
        charge <- newChargeRegion $ \cost -> do
            previous <- readIORef paid
            if previous == 0 then writeIORef paid cost else putMVar entered () >> takeMVar blocked
        withS3 (gatedChunks release) $ \env ->
            withAsync
                (readObject charge env limits "records" "key" Nothing)
                ( \worker -> do
                    timeout paymentWaitMicros (takeMVar entered) `shouldReturn` Just ()
                    cancel worker
                    either fromException (const Nothing) <$> waitCatch worker `shouldReturn` Just AsyncCancelled
                )
                `finally` void (tryPutMVar release False)
        readIORef paid `shouldReturn` stateExchangeCharge

    it "keeps admission pauses outside the upstream progress window" $ do
        floorSpec <- expectRight (mkProgressFloor 1 0.05 1)
        payments <- newIORef ([] :: [Int])
        charge <- newChargeRegion $ \cost -> do
            modifyIORef' payments (cost :)
            threadDelay 100_000
        withS3 (reply status200 [(hETag, "\"one\"")] "record") $ \env ->
            readObject charge env limits{progressFloor = floorSpec} "records" "key" Nothing `shouldReturn` Right (ObjectLoaded "\"one\"" "record")
        readIORef payments >>= (`shouldSatisfy` (not . null))

    it "charges repeated conditional exchanges by high-water size rather than transfer count" $ do
        requests <- newIORef (0 :: Int)
        paid <- newIORef (0 :: Int)
        charge <- newChargeRegion (\cost -> modifyIORef' paid (+ cost))
        withS3
            ( \request respond -> do
                modifyIORef' requests (+ 1)
                reply status200 [(hETag, "\"one\"")] "" request respond
            )
            $ \env -> do
                writeObject charge env limits "records" "key" Nothing "record" `shouldReturn` Right (ObjectStored "\"one\"")
                firstPaid <- readIORef paid
                replicateM_ 4 (writeObject charge env limits "records" "key" (Just "\"one\"") "record" >>= (\result -> result `shouldBe` Right (ObjectStored "\"one\"")))
                readIORef paid `shouldReturn` firstPaid
        readIORef requests `shouldReturn` 5

    it "charges error and fallback-list bodies without reserving their maximum size" $ do
        paid <- newIORef (0 :: Int)
        charge <- newChargeRegion (\cost -> modifyIORef' paid (+ cost))
        withS3 (listingReply emptyListing) $ \env ->
            readObject charge env defaultLimits "records" "key" Nothing `shouldReturn` Right ObjectAbsent
        total <- readIORef paid
        total `shouldSatisfy` (> 4096)
        total `shouldSatisfy` (< maxMetadataBytes defaultLimits)

    it "keeps a conditional 304 charge independent of the metadata limit" $ do
        paid <- newIORef (0 :: Int)
        charge <- newChargeRegion (\cost -> modifyIORef' paid (+ cost))
        withS3 (reply status304 [] "") $ \env ->
            readObject charge env defaultLimits "records" "key" (Just "\"one\"") `shouldReturn` Right (ObjectUnchanged "\"one\"")
        readIORef paid `shouldReturn` stateExchangeCharge

    it "charges retained companion bytes proportionally before returning them" $ do
        digest <- expectRight (mkHash SHA256 (hexSha256Of "metadata"))
        paid <- newIORef (0 :: Int)
        charge <- newChargeRegion (\cost -> modifyIORef' paid (+ cost))
        withS3 (reply status200 [(hETag, "\"one\"")] "metadata") $ \env -> do
            let store = mirrorStateForEnv defaultLimits "records" "state" (stateTarget "https://private.test") env
            msReadCompanion store charge (CompanionRef digest 8) `shouldReturn` Right "metadata"
        total <- readIORef paid
        total `shouldSatisfy` (> 8)
        total `shouldSatisfy` (< maxMetadataBytes defaultLimits)

    it "signs create-if-absent and preserves the exact body" $ do
        observed <- newIORef Nothing
        withS3
            ( \request respond -> do
                bytes <- strictRequestBody request
                writeIORef observed (Just (requestHeaders request, bytes))
                respond (responseLBS status200 [(hETag, "\"one\"")] "")
            )
            $ \env -> do
                writeObject (const pass) env limits "records" "prefix/project" Nothing "record" `shouldReturn` Right (ObjectStored "\"one\"")
        readIORef observed >>= \case
            Nothing -> expectationFailure "no request reached the endpoint"
            Just (headers, bytes) -> do
                lookup hIfNoneMatch headers `shouldBe` Just "*"
                lookup hIfMatch headers `shouldBe` Nothing
                lookup hAuthorization headers `shouldSatisfy` maybe False (BS.isInfixOf "if-none-match")
                bytes `shouldBe` "record"

    it "signs revision-matched replacement"
        $ withS3
            ( \request respond -> do
                lookup hIfMatch (requestHeaders request) `shouldBe` Just "\"old\""
                lookup hIfNoneMatch (requestHeaders request) `shouldBe` Nothing
                lookup hAuthorization (requestHeaders request) `shouldSatisfy` maybe False (BS.isInfixOf "if-match")
                respond (responseLBS status200 [(hETag, "\"new\"")] "")
            )
        $ \env ->
            writeObject (const pass) env limits "records" "key" (Just "\"old\"") "new" `shouldReturn` Right (ObjectStored "\"new\"")

    for_ [status409, status412] $ \status ->
        it ("returns conditional collision " <> show (statusCode status)) $
            withS3 (reply status [] (xmlError "PreconditionFailed")) $ \env ->
                writeObject (const pass) env limits "records" "key" Nothing "record" `shouldReturn` Right ObjectConflict

    it "bounds XML error bodies before the SDK decoder sees them" $
        withS3 (reply status403 [] (xmlError "AccessDenied")) $ \env ->
            readObject (const pass) env limits{maxMetadataBytes = 8} "records" "key" Nothing `shouldReturn` Left (ObjectTooLarge 8)

    it "refuses a response body stalled below the configured progress floor" $ do
        floorSpec <- expectRight (mkProgressFloor 1 0.1 1)
        withS3
            ( \_ respond ->
                respond
                    ( responseStream
                        status200
                        [(hETag, "\"one\"")]
                        ( \write flush -> do
                            write "x"
                            flush
                            threadDelay 500_000
                            write "y"
                        )
                    )
            )
            $ \env ->
                readObject (const pass) env limits{progressFloor = floorSpec} "records" "key" Nothing >>= (`shouldSatisfy` isLeft)

    it "refuses slow body drips rather than treating each chunk as enough progress" $ do
        floorSpec <- expectRight (mkProgressFloor 1 0.1 8)
        withS3
            ( \_ respond ->
                respond
                    ( responseStream
                        status200
                        [(hETag, "\"one\"")]
                        ( \write flush ->
                            replicateM_ 12 (write "x" >> flush >> threadDelay 50_000)
                        )
                    )
            )
            $ \env ->
                readObject (const pass) env limits{progressFloor = floorSpec} "records" "key" Nothing >>= (`shouldSatisfy` isLeft)

    it "allows a progressing background transfer past the foreground serve cap" $ do
        floorSpec <- expectRight (mkProgressFloor 0.3 0.1 1)
        withS3
            ( \_ respond ->
                respond
                    ( responseStream
                        status200
                        [(hETag, "\"one\"")]
                        ( \write flush ->
                            replicateM_ 12 (write "x" >> flush >> threadDelay 50_000)
                        )
                    )
            )
            $ \env ->
                readObject (const pass) env limits{progressFloor = floorSpec} "records" "key" Nothing `shouldReturn` Right (ObjectLoaded "\"one\"" "xxxxxxxxxxxx")

    it "propagates cancellation while a response body is blocked" $ do
        started <- newEmptyMVar
        release <- newEmptyMVar
        withS3
            ( \_ respond ->
                respond
                    ( responseStream
                        status200
                        [(hETag, "\"one\"")]
                        ( \write flush -> do
                            write "x"
                            flush
                            putMVar started ()
                            takeMVar release
                        )
                    )
            )
            $ \env ->
                withAsync (readObject (const pass) env limits "records" "key" Nothing) $ \worker -> do
                    takeMVar started
                    cancel worker
                    putMVar release ()
                    either fromException (const Nothing) <$> waitCatch worker `shouldReturn` Just AsyncCancelled

    for_ [shortLengthBody, interruptedChunkBody] $ \body -> do
        it "returns a typed transport fault when a project GET body closes early" $
            withS3 (brokenResponse "200 OK" body) $ \env -> do
                let store = mirrorStateForEnv limits "records" "state" (stateTarget "https://private.test") env
                msReadProject store (const pass) (mkPackageName PyPI Nothing "demo") Nothing >>= (`shouldSatisfy` stateTransportFailure)

        it "returns a typed transport fault when a retained companion body closes early" $ do
            digest <- expectRight (mkHash SHA256 (hexSha256Of "metadata"))
            withS3 (brokenResponse "200 OK" body) $ \env -> do
                let store = mirrorStateForEnv limits "records" "state" (stateTarget "https://private.test") env
                msReadCompanion store (const pass) (CompanionRef digest 8) >>= (`shouldSatisfy` stateTransportFailure)

        it "returns a typed transport fault when an error body closes early" $
            withS3 (brokenResponse "403 Forbidden" body) $ \env ->
                readObject (const pass) env limits "records" "key" Nothing >>= (`shouldSatisfy` objectTransportFailure)

        it "does not infer absence when the fallback list body closes early"
            $ withS3
                ( \request respond ->
                    if null (queryString request)
                        then reply status403 [] (xmlError "AccessDenied") request respond
                        else brokenResponse "200 OK" body request respond
                )
            $ \env ->
                readObject (const pass) env limits "records" "key" Nothing >>= (`shouldSatisfy` objectTransportFailure)

        it "returns a typed transport fault when a conditional PUT error body closes early" $
            withS3 (brokenResponse "412 Precondition Failed" body) $ \env ->
                writeObject (const pass) env limits "records" "key" Nothing "record" >>= (`shouldSatisfy` objectTransportFailure)

    it "requires an ETag after a successful write" $
        withS3 (reply status200 [] "") $ \env ->
            writeObject (const pass) env limits "records" "key" Nothing "record" `shouldReturn` Left ObjectNoRevision

    it "returns a loaded object's own revision" $
        withS3 (reply status200 [(hETag, "\"one\"")] "record") $ \env ->
            readObject (const pass) env limits "records" "key" Nothing `shouldReturn` Right (ObjectLoaded "\"one\"" "record")

    it "reuses only the revision sent with a conditional GET"
        $ withS3
            ( \request respond -> do
                lookup hIfNoneMatch (requestHeaders request) `shouldBe` Just "\"one\""
                respond (responseLBS status304 [] "")
            )
        $ \env ->
            readObject (const pass) env limits "records" "key" (Just "\"one\"") `shouldReturn` Right (ObjectUnchanged "\"one\"")

    it "refuses an unsolicited 304" $
        withS3 (reply status304 [] "") $ \env ->
            readObject (const pass) env limits "records" "key" Nothing `shouldReturn` Left ObjectNoRevision

    it "requires a loaded object's revision" $
        withS3 (reply status200 [] "record") $ \env ->
            readObject (const pass) env limits "records" "key" Nothing `shouldReturn` Left ObjectNoRevision

    it "enforces the configured body cap" $
        withS3 (reply status200 [(hETag, "\"one\"")] "123456789") $ \env ->
            readObject (const pass) env limits{maxMetadataBytes = 8} "records" "key" Nothing `shouldReturn` Left (ObjectTooLarge 8)

    it "accepts exactly the configured body cap" $
        withS3 (reply status200 [(hETag, "\"one\"")] "12345678") $ \env ->
            readObject (const pass) env limits{maxMetadataBytes = 8} "records" "key" Nothing `shouldReturn` Right (ObjectLoaded "\"one\"" "12345678")

    it "recognises NoSuchKey as conclusive absence" $
        withS3 (reply status404 [] (xmlError "NoSuchKey")) $ \env ->
            readObject (const pass) env limits "records" "key" Nothing `shouldReturn` Right ObjectAbsent

    it "does not treat a missing bucket as an absent record" $
        withS3 (reply status404 [] (xmlError "NoSuchBucket")) $ \env ->
            readObject (const pass) env limits "records" "key" Nothing >>= (`shouldSatisfy` isLeft)

    it "uses one exact-prefix listing to disambiguate a refused GET"
        $ withS3
            ( \request respond ->
                if null (queryString request)
                    then respond (responseLBS status403 [] (xmlError "AccessDenied"))
                    else do
                        lookup "prefix" (queryString request) `shouldBe` Just (Just "key")
                        lookup "max-keys" (queryString request) `shouldBe` Just (Just "1")
                        respond (responseLBS status200 [] emptyListing)
            )
        $ \env ->
            readObject (const pass) env limits "records" "key" Nothing `shouldReturn` Right ObjectAbsent

    it "does not infer absence when listing is refused" $
        withS3 (reply status403 [] (xmlError "AccessDenied")) $ \env ->
            readObject (const pass) env limits "records" "key" Nothing >>= (`shouldSatisfy` isLeft)

    it "does not infer absence from a truncated listing" $
        withS3 (listingReply "<ListBucketResult><IsTruncated>true</IsTruncated></ListBucketResult>") $ \env ->
            readObject (const pass) env limits "records" "key" Nothing `shouldReturn` Left ObjectUnknownAbsence

    it "does not infer absence from a malformed listing" $
        withS3 (listingReply "broken") $ \env ->
            readObject (const pass) env limits "records" "key" Nothing >>= (`shouldSatisfy` isLeft)

    it "rejects an oversized write before opening the endpoint" $
        withS3 (\_ _ -> expectationFailure "unexpected write" >> fail "unexpected write") $ \env ->
            writeObject (const pass) env limits{maxMetadataBytes = 8} "records" "key" Nothing "123456789" `shouldReturn` Left (ObjectTooLarge 8)

    for_ [("without truncation evidence", "<ListBucketResult><KeyCount>0</KeyCount></ListBucketResult>"), ("with a matching object", populatedListing)] $ \(label, listing) ->
        it ("refuses absence from a listing " <> label) $
            withS3 (listingReply listing) $ \env ->
                readObject (const pass) env limits "records" "key" Nothing `shouldReturn` Left ObjectUnknownAbsence

    it "refuses an empty revision on a loaded object" $
        withS3 (reply status200 [(hETag, "")] "record") $ \env ->
            readObject (const pass) env limits "records" "key" Nothing `shouldReturn` Left ObjectNoRevision

    it "reports a nonconditional write refusal as a transport fault" $
        withS3 (reply status403 [] (xmlError "AccessDenied")) $ \env ->
            writeObject (const pass) env limits "records" "key" Nothing "record" >>= (`shouldSatisfy` objectTransportFailure)

    it "distinguishes missing retained companions from corrupt retained bytes" $ do
        digest <- expectRight (mkHash SHA256 (hexSha256Of "metadata"))
        let reference = CompanionRef digest 8
        for_ [(reply status404 [] (xmlError "NoSuchKey"), Left StateMissingCompanion), (reply status200 [(hETag, "one")] "Metadata", Left (StateInvalid "retained companion identity mismatch")), (reply status200 [(hETag, "one")] "metadata", Right "metadata"), (reply status200 [] "metadata", Left (StateInvalid "state response omitted its revision"))] $ \(application, expected) ->
            withS3 application $ \env -> do
                let store = mirrorStateForEnv limits "records" "state" (stateTarget "https://private.test") env
                msReadCompanion store (const pass) reference `shouldReturn` expected

    it "maps uncertain object absence to unavailable companion state" $ do
        digest <- expectRight (mkHash SHA256 (hexSha256Of "metadata"))
        withS3 (listingReply "<ListBucketResult><IsTruncated>true</IsTruncated></ListBucketResult>") $ \env -> do
            let store = mirrorStateForEnv limits "records" "state" (stateTarget "https://private.test") env
            msReadCompanion store (const pass) (CompanionRef digest 8)
                `shouldReturn` Left (StateInvalid "state absence could not be established")

    it "rejects revisions from another target or project before an exchange" $ do
        inventory <- mirrorSample [1] False "https://first.test/" "original"
        let target = stateTarget "https://private.test"
            otherTarget = stateTarget "https://other.test"
            otherPackage = mkPackageName PyPI Nothing "other"
        record <- expectRight (extendProjectRecord defaultLimits target inventory Nothing)
        withS3 (\_ _ -> fail "revision mismatch must not reach storage") $ \env -> do
            let store = mirrorStateForEnv limits "records" "state" target env
            for_ [StateRevision otherTarget mirrorPackage "one", StateRevision target otherPackage "one"] $ \revision -> do
                msReadProject store (const pass) mirrorPackage (Just revision) `shouldReturn` Left StateRevisionMismatch
                msWriteProject store (const pass) mirrorPackage (Just revision) record `shouldReturn` Left StateRevisionMismatch
            msWriteProject store (const pass) otherPackage Nothing record `shouldReturn` Left StateIdentityMismatch

    for_ [(status412, "metadata", Nothing), (status200, "metadata", Nothing), (status412, "Metadata", Just (StateInvalid "retained companion identity mismatch"))] $ \(putStatus, retainedBody, fault) ->
        it ("reconciles uncertain companion storage through verified bytes: " <> show (statusCode putStatus, fault)) $ do
            inventory <- mirrorSample [1] True "https://first.test/" "original"
            let file :| _ = inventoryFiles inventory
            verified <- expectRight (verifyCompanionBytes limits file "metadata")
            requests <- newIORef ([] :: [Method])
            withS3
                ( \request respond -> do
                    modifyIORef' requests (requestMethod request :)
                    if requestMethod request == methodPut
                        then do
                            lookup hIfNoneMatch (requestHeaders request) `shouldBe` Just "*"
                            strictRequestBody request `shouldReturn` "metadata"
                            reply putStatus [] (if putStatus == status412 then xmlError "PreconditionFailed" else "") request respond
                        else reply status200 [(hETag, "one")] retainedBody request respond
                )
                $ \env -> do
                    let store = mirrorStateForEnv limits "records" "state" (stateTarget "https://private.test") env
                    digest <- expectRight (mkHash SHA256 (hexSha256Of "metadata"))
                    msRetainCompanion store (const pass) verified
                        `shouldReturn` maybe (Right (CompanionRef digest 8)) Left fault
            reverse <$> readIORef requests `shouldReturn` [methodPut, methodGet]

gatedChunks :: MVar Bool -> Application
gatedChunks release _ respond =
    respond
        ( responseStream status200 [(hETag, "\"one\"")] $ \write flush -> do
            write "a"
            flush
            sendNext <- takeMVar release
            when sendNext (write "b" >> flush)
        )

paymentWaitMicros :: Int
paymentWaitMicros = 5_000_000

limits :: Limits
limits = defaultLimits{maxMetadataBytes = 1024}

shortLengthBody :: ByteString
shortLengthBody = "Content-Length: 20\r\n\r\nshort"

interruptedChunkBody :: ByteString
interruptedChunkBody = "Transfer-Encoding: chunked\r\n\r\n8\r\nshort"

objectTransportFailure :: Either ObjectFault a -> Bool
objectTransportFailure (Left (ObjectTransport _)) = True
objectTransportFailure _ = False

stateTransportFailure :: Either MirrorStateFault a -> Bool
stateTransportFailure (Left (StateTransport _)) = True
stateTransportFailure _ = False
