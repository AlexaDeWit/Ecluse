-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Native RubyGems persistence over production data-plane handles and an independent wire boundary.
module Ecluse.Test.Contract.PackageStore.RubyGems (rubyGemsStoreDriver, readRubyGemsStoreManifestOver) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Types (Header, Status, status200, status400, status401, status404, status409, status503)

import Ecluse.Core.Credential (ClientCredential (..), mkSecret)
import Ecluse.Core.Ecosystem (Ecosystem (RubyGems))
import Ecluse.Core.Registry.CachedDocument (CachedDoc, rubyGemsCached)
import Ecluse.Core.Registry.Maintenance (StoreMaintenance, StoreManifestRead)
import Ecluse.Core.Registry.Origin (OriginClient (..), ocBaseUrl, originClient)
import Ecluse.Core.Registry.Publish (MirrorTransport (..), newMirrorPublish)
import Ecluse.Core.Registry.Request (ServicePresentation, artifactRequestByUrl)
import Ecluse.Core.Registry.RubyGems.Credential (rubyReadCredential)
import Ecluse.Core.Registry.RubyGems.Document (GemDocument (..), GemRow (grSource))
import Ecluse.Core.Registry.RubyGems.Metadata (prepareGemRead, rubyGemsChargeFactors)
import Ecluse.Core.Registry.RubyGems.Publish (rubyPublishCodec)
import Ecluse.Core.Registry.RubyGems.Request (gemPath)
import Ecluse.Core.Security (BodyLimit (MirrorArtifactBodyLimit), defaultLimits)
import Ecluse.Core.Security.Egress.DevHttp (loopbackRegistryUrl)
import Ecluse.Core.Version (mkVersion)
import Ecluse.Test.Contract.PackageStore
import Ecluse.Test.Contract.PackageStore.Provider
import Ecluse.Test.Contract.PackageStore.RubyGems.Fixtures
import Ecluse.Test.Port (passthroughTracingPort)
import Ecluse.Test.Registry.RubyGems.Reference (RubyInfoRow (..), referenceGemIdentity, referenceInfo)
import Ecluse.Test.Stub (Captured (..), headerValue)

-- | Every law uses Compact Index reads, raw archive uploads and archive downloads under the store's presentation.
rubyGemsStoreDriver :: Text -> ServicePresentation -> (OriginClient -> StoreBoundary -> IO StoreMaintenance) -> FullPackageStoreDriver
rubyGemsStoreDriver subject presentation maintenance =
    FullPackageStoreDriver
        { psdSubject = subject
        , psdPackage = packageText
        , psdPrimary = storeRelease primaryNative
        , psdSibling = storeRelease siblingNative
        , psdWithSession = withRubyGemsSession presentation maintenance
        }

withRubyGemsSession :: ServicePresentation -> (OriginClient -> StoreBoundary -> IO StoreMaintenance) -> (PackageStoreSession -> IO ()) -> IO ()
withRubyGemsSession presentation maintenance use = do
    held <- newIORef Map.empty
    writes <- newIORef []
    failWrite <- newIORef False
    failDelete <- newIORef False
    let boundary = heldBoundary storeRelease held failDelete
    withStoreSession ((== "/registry" <> infoPath) . capPath) (reply held writes failWrite) (build boundary) (reverse <$> readIORef writes) failWrite failDelete use
  where
    build boundary base exchanges = do
        let origin = originClient defaultLimits exchanges (loopbackRegistryUrl (base <> "/registry")) (Just credential)
            source = originClient defaultLimits exchanges (loopbackRegistryUrl (base <> "/source")) Nothing
            publisher = newMirrorPublish (MirrorTransport exchanges (pure (Just credential)) defaultLimits passthroughTracingPort) (ocBaseUrl origin) (ocBaseUrl origin) (rubyPublishCodec rubyGemsChargeFactors presentation)
            readArchive _ filename = fetchStoredArtifact (MirrorArtifactBodyLimit 32768) origin (artifactRequestByUrl rubyReadCredential (ocToken origin) (base <> "/registry/" <> gemPath filename))
        store <- maintenance origin boundary
        pure (storeOperations packageName (mkVersion RubyGems) (readNativeRelease rubyGemsRead origin) readArchive (publishNativeRelease rubyGemsRead source publisher) store)

-- | Prepare discovery under the authenticated store origin supplied by backend composition.
readRubyGemsStoreManifestOver :: OriginClient -> StoreManifestRead
readRubyGemsStoreManifestOver = prepareStoreManifestOver prepareGemRead

rubyGemsRead :: NativeStoreRead
rubyGemsRead = NativeStoreRead prepareGemRead (artifactRequestByUrl rubyReadCredential) packageName (mkVersion RubyGems) selectedFields

-- The independent reader decodes each row the production read retained.
selectedFields :: CachedDoc -> Either Text (Map Text Text)
selectedFields cached = do
    document <- maybeToRight "production read lost RubyGems source rows" (snd rubyGemsCached cached)
    rows <- referenceInfo (gemHeader document <> BS.intercalate "\n" (map grSource (gemRows document)))
    Map.fromList . concat <$> traverse fieldsOf rows
  where
    fieldsOf row = do
        unless (rowPlatform row == "ruby") (Left "production read retained a platform the store never held")
        ruby <- field "ruby" row
        rubygems <- field "rubygems" row
        let filename = gemFilename (rowVersion row)
            dependencies = T.intercalate "," [name <> ":" <> T.intercalate "&" clauses | (name, clauses) <- rowDependencies row]
        pure [(filename <> " dependencies", dependencies), (filename <> " ruby", ruby), (filename <> " rubygems", rubygems)]
    field key row = maybeToRight ("native row lacks " <> key) (lookup key (rowFields row))

-- Reads present CodeArtifact's Basic @aws@ pair, and publication its Bearer token.
reply :: IORef (Map Text NativeGem) -> IORef [PackageStoreRelease] -> IORef Bool -> Captured -> IO (Status, [Header], LByteString)
reply held writes failWrite captured
    | "/source/" `BS.isPrefixOf` capPath captured =
        pure $
            if isNothing (headerValue "Authorization" captured)
                then readReply captured "/source" [primaryNative, siblingNative]
                else (status401, [], "public source requires anonymous reads")
    | (capMethod captured, capPath captured) == ("POST", "/registry/api/v1/gems") =
        if headerValue "Authorization" captured /= Just "Bearer fixture-token"
            then pure (status401, [], "publication token required")
            else do
                blocked <- takeStoreFailure failWrite
                if blocked then pure (status503, [], "injected write failure") else acceptUpload held writes (capBody captured)
    | headerValue "Authorization" captured /= Just "Basic YXdzOmZpeHR1cmUtdG9rZW4=" = pure (status401, [], "credential required")
    | otherwise = readReply captured "/registry" . Map.elems <$> readIORef held

readReply :: Captured -> ByteString -> [NativeGem] -> (Status, [Header], LByteString)
readReply captured root gems
    | capMethod captured /= "GET" = (status404, [], "missing")
    | capPath captured == root <> infoPath =
        if null gems then (status404, [], "missing") else (status200, [("Content-Type", "text/plain; charset=utf-8")], encodeUtf8 ("---\n" <> T.unlines (map nativeRow gems)))
    | otherwise = case find (\gem -> capPath captured == root <> "/gems/" <> encodeUtf8 (gemFilename (ngVersion gem))) gems of
        Nothing -> (status404, [], "missing")
        Just gem -> (status200, [("Content-Type", "application/octet-stream")], LBS.fromStrict (ngArchive gem))

-- The archive's own metadata names the version. The store keeps the bytes as sent.
acceptUpload :: IORef (Map Text NativeGem) -> IORef [PackageStoreRelease] -> ByteString -> IO (Status, [Header], LByteString)
acceptUpload held writes bytes = case decodeNativeUpload bytes of
    Left reason -> pure (status400, [], encodeUtf8 reason)
    Right native -> do
        duplicate <- Map.member (ngVersion native) <$> readIORef held
        if duplicate
            then pure (status409, [], "Repushing of gem versions is not allowed.")
            else do
                modifyIORef' held (Map.insert (ngVersion native) native)
                modifyIORef' writes (storeRelease native :)
                pure (status200, [("Content-Type", "text/plain")], encodeUtf8 ("Successfully registered gem: " <> gemFilename (ngVersion native)))

decodeNativeUpload :: ByteString -> Either Text NativeGem
decodeNativeUpload bytes = do
    (name, version, platform) <- referenceGemIdentity bytes
    unless (name == packageText && platform == "ruby") (Left "native upload names another gem or platform")
    declared <- maybeToRight "native upload names an undeclared version" (find ((== version) . ngVersion) [primaryNative, siblingNative])
    pure declared{ngArchive = bytes}

infoPath :: ByteString
infoPath = "/info/" <> encodeUtf8 packageText

credential :: ClientCredential
credential = ClientCredential (Just "aws") (mkSecret "fixture-token")
