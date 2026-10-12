-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | S3-compatible state keeps distributions in the registry and only declared companions in metadata storage.
module Ecluse.Runtime.Mirror.State.S3 (
    newS3MirrorState,
    mirrorStateForEnv,
) where

import Amazonka qualified as AWS
import Data.Text qualified as T
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)

import Ecluse.Core.Mirror.State
import Ecluse.Core.Mirror.State.Codec (chargedDecodeProjectRecord, chargedEncodeProjectRecord)
import Ecluse.Core.Mirror.State.Internal (StateRevision (..), VerifiedCompanionBytes (..), projectKey)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.Exchange (singleAttemptSettings)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (BodyTooLarge), Limits (..))
import Ecluse.Runtime.Aws.Env (AwsEndpoint)
import Ecluse.Runtime.Aws.S3 (buildS3Env)
import Ecluse.Runtime.Mirror.State.S3.Internal

-- | The composition root supplies a validated explicit bucket and prefix, independent of advisory storage.
newS3MirrorState :: Maybe AwsEndpoint -> Limits -> Text -> Text -> StateTarget -> IO MirrorState
newS3MirrorState endpoint limits bucket prefix target = do
    env <- buildS3Env endpoint
    manager <- newTlsManagerWith (singleAttemptSettings tlsManagerSettings)
    pure (mirrorStateForEnv limits bucket prefix target env{AWS.manager = manager})

-- | Bind one configured namespace to a single-attempt environment, including deterministic signed-wire tests.
mirrorStateForEnv :: Limits -> Text -> Text -> StateTarget -> AWS.Env -> MirrorState
mirrorStateForEnv limits bucket prefix target env =
    MirrorState
        { msReadProject = readProject
        , msWriteProject = writeProject
        , msRetainCompanion = retain
        , msReadCompanion = readCompanion
        }
  where
    key name = underPrefix (projectKey target name <> "/project.json")
    underPrefix suffix = if T.null prefix then suffix else T.dropWhileEnd (== '/') prefix <> "/" <> suffix
    referenceKey reference = underPrefix ("companions/sha256/" <> companionDigest reference)
    readProject charge name revision = case revisionBytes target name revision of
        Left fault -> pure (Left fault)
        Right expected ->
            readObject charge env limits bucket (key name) expected >>= \case
                Left fault -> pure (Left (objectFault fault))
                Right ObjectAbsent -> pure (Right StateAbsent)
                Right (ObjectUnchanged etag) -> pure (Right (StateUnchanged (StateRevision target name etag)))
                Right (ObjectLoaded etag bytes) -> fmap (StateLoaded (StateRevision target name etag)) <$> chargedDecodeProjectRecord charge limits target name bytes
    writeProject charge name revision record
        | recordTarget record /= target || recordPackage record /= name = pure (Left StateIdentityMismatch)
        | otherwise = case revisionBytes target name revision of
            Left fault -> pure (Left fault)
            Right expected ->
                chargedEncodeProjectRecord charge limits record >>= \case
                    Left fault -> pure (Left fault)
                    Right bytes ->
                        fmap (first objectFault) (writeObject charge env limits bucket (key name) expected bytes)
                            <&> fmap
                                ( \case
                                    ObjectStored etag -> StateStored (StateRevision target name etag)
                                    ObjectConflict -> StateConflict
                                )
    retain charge verified@(VerifiedCompanionBytes _ reference _) = do
        result <- writeObject charge env limits bucket (referenceKey reference) Nothing (companionBody verified)
        case result of
            Right (ObjectStored _) -> pure (Right reference)
            _ -> readCompanion charge reference <&> fmap (const reference)
    readCompanion charge reference =
        fmap (first objectFault) (readObject charge env limits bucket (referenceKey reference) Nothing)
            <&> ( >>=
                    \case
                        ObjectLoaded _ bytes -> verifyRetainedCompanion reference bytes
                        ObjectAbsent -> Left StateMissingCompanion
                        ObjectUnchanged _ -> Left StateRevisionMismatch
                )

revisionBytes :: StateTarget -> PackageName -> Maybe StateRevision -> Either MirrorStateFault (Maybe ByteString)
revisionBytes target name = traverse $ \(StateRevision recordedTarget recordedName bytes) ->
    if recordedTarget == target && recordedName == name then Right bytes else Left StateRevisionMismatch

objectFault :: ObjectFault -> MirrorStateFault
objectFault = \case
    ObjectTransport fault -> StateTransport fault
    ObjectTooLarge cap -> StateBound (BodyTooLarge (MetadataBodyLimit cap))
    ObjectNoRevision -> StateInvalid "state response omitted its revision"
    ObjectUnknownAbsence -> StateInvalid "state absence could not be established"
