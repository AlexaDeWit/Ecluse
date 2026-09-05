-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The protocol leaf driven against an in-process upstream that records what Écluse sent.
The upstream implements none of a store's behaviour: it answers by path and method, so the
leaf's sequencing, fault mapping, and verdicts are what the assertions read.
-}
module Ecluse.Registry.Maintenance.ProtocolSpec (spec) where

import Data.Aeson (Object, Value (Object), decodeStrict, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types.Status (Status, status200, status201, status404, status500, status503)
import Test.Hspec

import Ecluse.Core.Credential (mkSecret)
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.Adapter.Capability (
    AdapterMaintenance (maintenanceListing, maintenanceVersionDelete),
 )
import Ecluse.Core.Registry.Maintenance (
    CompletionNotion (CompletesOnCall),
    ConsentVerdict (ConsentGranted, ConsentWithheld),
    DeleteCeiling (AtMost),
    RefillPosture (RefillPermitted),
    RetryAdvice (RetryFutile, RetryWorthwhile),
    StoreClass (StoreDestroyable),
    StoreFacts (..),
    StoreFault (faultRetry),
    StoreMaintenance (..),
    StoredVersion (storedPresence, storedVersion),
    VersionOutcome (VersionRefused, VersionRemoved),
    VersionPresence (VersionServed),
    refusalCode,
 )
import Ecluse.Core.Registry.Maintenance.Protocol (ProtocolStore (..), newProtocolMaintenance)
import Ecluse.Core.Registry.Npm.Maintenance (npmMaintenance)
import Ecluse.Core.Registry.Npm.Publish (npmPublishCodec)
import Ecluse.Core.Registry.Origin (OriginClient (OriginClient, ocBaseUrl, ocLimits, ocManager, ocToken))
import Ecluse.Core.Security (defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Test.Package (unscopedNpm)
import Ecluse.Test.Stub (
    Captured (capBody, capMethod, capPath),
    Stub,
    allCaptured,
    headerValue,
    stubLocalhostUrl,
    withRoutedStub,
 )

spec :: Spec
spec = do
    factsSpec
    enumerationSpec
    deletionSpec

factsSpec :: Spec
factsSpec = describe "what the backend supplies without a call" $ do
    it "reports the backend name it was built under, and the one-version delete ceiling" $
        withStore True answerNothing $ \handle _ -> do
            let facts = storeFacts handle
            factBackend facts `shouldBe` "verdaccio"
            factDeleteCeiling facts `shouldBe` AtMost 1
            factRefill facts `shouldBe` RefillPermitted
            factCompletion facts `shouldBe` CompletesOnCall

    it "offers no rehearsal, because the protocol spells no dry-run request" $
        withStore True answerNothing $ \handle _ ->
            isNothing (rehearseDelete handle) `shouldBe` True

    it "reads consent and classification off the operator's key, with no call" $
        withStore True answerNothing $ \handle _ -> do
            verifyConsent handle `shouldReturn` Right ConsentGranted
            classifyStore handle `shouldReturn` Right StoreDestroyable

    it "withholds consent, naming the key, when the store carries none" $
        withStore False answerNothing $ \handle _ ->
            verifyConsent handle `shouldReturn` Right (ConsentWithheld consentKey)

enumerationSpec :: Spec
enumerationSpec = describe "enumeration over the protocol's own reads" $ do
    it "reads the store's packages from the listing it answers 200 to" $
        withStore True answerStore $ \handle stub -> do
            enumeratePackages handle `shouldReturn` Right [unscopedNpm "leftpad", unscopedNpm "rightpad"]
            calls stub `shouldReturn` [("GET", "/-/all")]

    it "faults with RetryFutile on a store that does not answer the listing" $
        withStore True answerNothing $ \handle _ ->
            (fmap faultRetry . leftToMaybe <$> enumeratePackages handle)
                `shouldReturn` Just RetryFutile

    it "reads a package's versions through the presence probe, all served" $
        withStore True answerStore $ \handle _ -> do
            stored <- enumerateVersions handle leftpad
            fmap (map storedVersion) stored `shouldBe` Right [version "1.0.0", version "2.0.0"]
            fmap (map storedPresence) stored `shouldBe` Right [VersionServed, VersionServed]

    it "reads a package the store no longer holds as holding no versions" $
        withStore True answerNothing $ \handle _ ->
            enumerateVersions handle leftpad `shouldReturn` Right []

    it "advises another attempt when a read fails server-side, unlike an absent listing" $
        withStore True (answerAll status503 "{}") $ \handle _ ->
            (fmap faultRetry . leftToMaybe <$> enumerateVersions handle leftpad)
                `shouldReturn` Just RetryWorthwhile

deletionSpec :: Spec
deletionSpec = describe "deletion over the protocol's own request sequence" $ do
    it "reads the document, edits it, then deletes the tarball, in that order" $
        withStore True answerStore $ \handle stub -> do
            deleteVersions handle leftpad [version "1.0.0"]
                `shouldReturn` [(version "1.0.0", VersionRemoved)]
            calls stub
                `shouldReturn` [ ("GET", "/leftpad")
                               , ("PUT", "/leftpad/-rev/3-abc")
                               , ("DELETE", "/leftpad/-/leftpad-1.0.0.tgz/-rev/3-abc")
                               ]

    it "carries the store's write credential on every call of the sequence" $
        withStore True answerStore $ \handle stub -> do
            _ <- deleteVersions handle leftpad [version "1.0.0"]
            sent <- allCaptured stub
            map (headerValue "Authorization") sent `shouldBe` replicate 3 (Just "Bearer write-token")

    it "sends a packument edit with the deleted version gone and the rest intact" $
        withStore True answerStore $ \handle stub -> do
            _ <- deleteVersions handle leftpad [version "1.0.0"]
            edited <- editedPackument stub
            keysUnder "versions" edited `shouldBe` ["2.0.0"]
            keysUnder "time" edited `shouldBe` ["2.0.0"]

    it "re-reads the document for every version, because the edit addresses its revision" $
        withStore True answerStore $ \handle stub -> do
            _ <- deleteVersions handle leftpad [version "1.0.0", version "2.0.0"]
            documentReads <- filter ((== "GET") . capMethod) <$> allCaptured stub
            length documentReads `shouldBe` 2

    it "refuses the version, and sends no tarball delete, when the store refuses the edit" $
        withStore True answerRefusingEdit $ \handle stub -> do
            outcomes <- deleteVersions handle leftpad [version "1.0.0"]
            map (refusedAs . snd) outcomes `shouldBe` [Just "HTTP 500"]
            calls stub `shouldReturn` [("GET", "/leftpad"), ("PUT", "/leftpad/-rev/3-abc")]

    it "refuses the version when the store holds no document for the package" $
        withStore True answerNothing $ \handle _ -> do
            outcomes <- deleteVersions handle leftpad [version "1.0.0"]
            map (refusedAs . snd) outcomes `shouldBe` [Just "NOT_FOUND"]

{- Build the handle over a stub and run the assertion against both. npm fills the maintenance
slice, so an empty verb here is a wiring fault the case reports rather than works around. -}
withStore ::
    Bool ->
    (Captured -> (Status, LBS.ByteString)) ->
    (StoreMaintenance -> Stub -> IO a) ->
    IO a
withStore permitted answer action =
    withRoutedStub reply $ \stub -> do
        manager <- newManager defaultManagerSettings
        listing <- required "listing" (maintenanceListing npmMaintenance)
        delete <- required "version delete" (maintenanceVersionDelete npmMaintenance)
        let origin =
                OriginClient
                    { ocBaseUrl = loopbackRegistryUrl (stubLocalhostUrl stub)
                    , ocManager = manager
                    , ocToken = Just (mkSecret "write-token")
                    , ocLimits = defaultLimits
                    }
            store =
                ProtocolStore
                    { psOrigin = origin
                    , psListing = listing
                    , psDelete = delete
                    , psCodec = npmPublishCodec
                    , psBackendName = "verdaccio"
                    , psPermitDeletion = permitted
                    , psConsentDescriptor = consentKey
                    }
        action (newProtocolMaintenance store) stub
  where
    reply captured = let (status, body) = answer captured in (status, [], body)
    required verb = maybe (fail ("npm fills no " <> verb <> " verb")) pure

consentKey :: Text
consentKey = "set mounts.npm.mirrorTarget.verdaccio.permitDeletion to true"

leftpad :: PackageName
leftpad = unscopedNpm "leftpad"

version :: Text -> Version
version = mkVersion Npm

-- A store answering the listing, the packument, and both writes of the delete sequence.
answerStore :: Captured -> (Status, LBS.ByteString)
answerStore captured = case (capMethod captured, capPath captured) of
    ("GET", "/-/all") -> (status200, encode listingDocument)
    ("GET", _) -> (status200, encode packumentDocument)
    _ -> (status201, "{\"ok\":true}")

-- The store refuses the packument edit, so the tarball delete must never be sent.
answerRefusingEdit :: Captured -> (Status, LBS.ByteString)
answerRefusingEdit captured = case capMethod captured of
    "GET" -> (status200, encode packumentDocument)
    _ -> (status500, "{\"error\":\"refused\"}")

-- A store holding nothing: the listing, the packument, and every read answer 404.
answerNothing :: Captured -> (Status, LBS.ByteString)
answerNothing = answerAll status404 "{}"

answerAll :: Status -> LBS.ByteString -> Captured -> (Status, LBS.ByteString)
answerAll status body = const (status, body)

listingDocument :: Value
listingDocument =
    object ["_updated" .= (1 :: Int), "leftpad" .= object [], "rightpad" .= object []]

packumentDocument :: Value
packumentDocument =
    object
        [ "_id" .= ("leftpad" :: Text)
        , "_rev" .= ("3-abc" :: Text)
        , "name" .= ("leftpad" :: Text)
        , "dist-tags" .= object ["latest" .= ("2.0.0" :: Text)]
        , "versions" .= object [Key.fromText raw .= manifest raw | raw <- held]
        , "time" .= object [Key.fromText raw .= ("2020-01-01T00:00:00.000Z" :: Text) | raw <- held]
        ]
  where
    held = ["1.0.0", "2.0.0"] :: [Text]
    manifest raw =
        object
            [ "name" .= ("leftpad" :: Text)
            , "version" .= raw
            , "dist" .= object ["tarball" .= ("http://store.test/leftpad/-/leftpad-" <> raw <> ".tgz")]
            ]

-- The document the store's first packument edit carried.
editedPackument :: Stub -> IO Object
editedPackument stub = do
    sent <- filter ((== "PUT") . capMethod) <$> allCaptured stub
    case sent of
        edit : _ -> maybe (fail "the packument edit did not decode") pure (decodeStrict (capBody edit))
        [] -> fail "the store was sent no packument edit"

calls :: Stub -> IO [(ByteString, ByteString)]
calls stub = map (\captured -> (capMethod captured, capPath captured)) <$> allCaptured stub

keysUnder :: Key.Key -> Object -> [Text]
keysUnder key document = case KeyMap.lookup key document of
    Just value -> maybe [] (map Key.toText . KeyMap.keys) (asObject value)
    Nothing -> []
  where
    asObject = \case
        Object inner -> Just inner
        _ -> Nothing

refusedAs :: VersionOutcome -> Maybe Text
refusedAs = \case
    VersionRefused refusal -> Just (refusalCode refusal)
    _ -> Nothing
