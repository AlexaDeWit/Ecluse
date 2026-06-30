module Ecluse.Registry.Npm.PublishSpec (spec) where

import Data.Text qualified as T
import Network.HTTP.Types.Status (status200, status404, status409, status500)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldSatisfy)

import Data.List.NonEmpty qualified as NE
import Ecluse.Core.Ecosystem (Ecosystem (Npm))
import Ecluse.Core.Package (HashAlg (..), PackageName, mkPackageName)
import Ecluse.Core.Queue (MirrorArtifact (..))
import Ecluse.Core.Registry (
    PublishError (publishErrorMessage),
    PublishFault (PublishRejected, PublishUrlUnformable),
    RegistryClient (publishArtifact),
 )
import Ecluse.Core.Registry.Npm (newNpmClient)
import Ecluse.Core.Registry.Npm.Publish (npmPublishDocument)
import Ecluse.Core.Version (Version, mkVersion)
import Ecluse.Test.Package (unsafeHash, validSha1)

import Ecluse.Test.Stub (
    capBody,
    capMethod,
    capPath,
    headerValue,
    lastCaptured,
    stubConfig,
    withStub,
 )

spec :: Spec
spec = publishSpec

publishSpec :: Spec
publishSpec = describe "publishArtifact idempotency" $ do
    it "PUTs the publish document to the package path" $
        withStub status200 "{}" $ \stub -> do
            config <- stubConfig stub
            client <- newNpmClient config
            _ <- publishArtifact client isOdd v1 dummyArtifact dummyTarballBytes
            cap <- lastCaptured stub
            capMethod cap `shouldBe` "PUT"
            capPath cap `shouldBe` "/is-odd"
            capBody cap `shouldBe` publishDoc
            -- The publish body must be declared application/json: a spec-compliant
            -- registry (e.g. Verdaccio) 415s a publish that omits it.
            headerValue "content-type" cap `shouldBe` Just "application/json"

    it "treats a 2xx as success" $
        withStub status200 "{}" $ \stub -> do
            config <- stubConfig stub
            client <- newNpmClient config
            publishArtifact client isOdd v1 dummyArtifact dummyTarballBytes `shouldReturn` Right ()

    it "treats a 409 Conflict as idempotent success (the immutable version is already present)" $
        withStub status409 "{\"error\":\"version already exists\"}" $ \stub -> do
            config <- stubConfig stub
            client <- newNpmClient config
            publishArtifact client isOdd v1 dummyArtifact dummyTarballBytes `shouldReturn` Right ()

    it "reports a 404 as a publish error naming the status (so the mirror job is retried)" $
        withStub status404 "{\"error\":\"Not found\"}" $ \stub -> do
            config <- stubConfig stub
            client <- newNpmClient config
            outcome <- publishArtifact client isOdd v1 dummyArtifact dummyTarballBytes
            -- Force the error message so the failure carries the status it saw.
            leftMessage outcome `shouldSatisfy` maybe False (T.isInfixOf "404")

    it "reports a 500 as a publish error" $
        withStub status500 "boom" $ \stub -> do
            config <- stubConfig stub
            client <- newNpmClient config
            outcome <- publishArtifact client isOdd v1 dummyArtifact dummyTarballBytes
            outcome `shouldSatisfy` isLeft

isOdd :: PackageName
isOdd = mkPackageName Npm Nothing "is-odd"

v1 :: Version
v1 = mkVersion Npm "1.0.0"

dummyArtifact :: MirrorArtifact
dummyArtifact =
    MirrorArtifact
        { maFilename = "is-odd-1.0.0.tgz"
        , maHashes = NE.singleton (unsafeHash SHA1 validSha1)
        , maSize = Just 1234
        }

dummyTarballBytes :: ByteString
dummyTarballBytes = "tarball-bytes"

-- | The expected publish document assembled by the adapter.
publishDoc :: ByteString
publishDoc = npmPublishDocument isOdd v1 "is-odd-1.0.0.tgz" Nothing (Just validSha1) dummyTarballBytes

{- | The (forced) error message of a publish 'Left', or 'Nothing' on a 'Right'.
Forcing the message exercises the error-construction path.
-}
leftMessage :: Either PublishFault a -> Maybe Text
leftMessage outcome = case outcome of
    Left (PublishRejected err) -> Just (publishErrorMessage err)
    Left (PublishUrlUnformable _) -> Nothing
    Right _ -> Nothing
