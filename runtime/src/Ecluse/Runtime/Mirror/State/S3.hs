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

import Ecluse.Core.Deadline (Deadlines)
import Ecluse.Core.Mirror.State
import Ecluse.Core.Mirror.State.Codec (chargedDecodeProjectRecord, chargedEncodeProjectRecord)
import Ecluse.Core.Mirror.State.Format (MirrorStateFormat)
import Ecluse.Core.Mirror.State.Internal (StateRevision (..), VerifiedCompanionBytes (..), projectKey)
import Ecluse.Core.Package (PackageName)
import Ecluse.Core.Registry.Exchange (newSingleAttemptManager)
import Ecluse.Core.Security (BodyLimit (MetadataBodyLimit), LimitError (BodyTooLarge), Limits (..))
import Ecluse.Runtime.Aws.Env (AwsEndpoint)
import Ecluse.Runtime.Aws.S3 (buildS3Env)
import Ecluse.Runtime.Mirror.State.S3.Internal

-- | The composition root supplies a validated explicit bucket and prefix, independent of advisory storage, and the mount's state format.
newS3MirrorState :: Maybe AwsEndpoint -> Deadlines -> Limits -> MirrorStateFormat -> Text -> Text -> StateTarget -> IO MirrorState
newS3MirrorState endpoint deadlines limits format bucket prefix target = buildS3Env endpoint >>= mirrorStateForEnv deadlines limits format bucket prefix target

-- | Bind one configured namespace to this env. Reads keep its manager's resend on a stale pooled connection, and writes go once.
mirrorStateForEnv :: Deadlines -> Limits -> MirrorStateFormat -> Text -> Text -> StateTarget -> AWS.Env -> IO MirrorState
mirrorStateForEnv deadlines limits format bucket prefix target env =
    newSingleAttemptManager <&> \writes -> stateOver deadlines limits format bucket prefix target env env{AWS.manager = writes}

-- Reads and writes share the endpoint and credentials, and differ only in their manager.
stateOver :: Deadlines -> Limits -> MirrorStateFormat -> Text -> Text -> StateTarget -> AWS.Env -> AWS.Env -> MirrorState
stateOver deadlines limits format bucket prefix target readEnv writeEnv =
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
            readObject charge deadlines readEnv limits bucket (key name) expected >>= \case
                Left fault -> pure (Left (objectFault fault))
                Right ObjectAbsent -> pure (Right StateAbsent)
                Right (ObjectUnchanged etag) -> pure (Right (StateUnchanged (StateRevision target name etag)))
                Right (ObjectLoaded etag bytes) -> fmap (StateLoaded (StateRevision target name etag)) <$> chargedDecodeProjectRecord format charge limits target name bytes
    writeProject charge name revision record
        | recordTarget record /= target || recordPackage record /= name = pure (Left StateIdentityMismatch)
        | otherwise = case revisionBytes target name revision of
            Left fault -> pure (Left fault)
            Right expected ->
                chargedEncodeProjectRecord format charge limits record >>= \case
                    Left fault -> pure (Left fault)
                    Right bytes ->
                        fmap (first objectFault) (writeObject charge deadlines writeEnv limits bucket (key name) expected bytes)
                            <&> fmap
                                ( \case
                                    ObjectStored etag -> StateStored (StateRevision target name etag)
                                    ObjectConflict -> StateConflict
                                )
    -- A PUT that died unsent leaves the object absent. The key is content-addressed and the PUT conditional, so one more cannot overwrite other bytes.
    retain charge verified =
        storeOrReread charge verified >>= \case
            Left StateMissingCompanion -> storeOrReread charge verified
            settled -> pure settled
    storeOrReread charge verified@(VerifiedCompanionBytes _ reference _) = do
        result <- writeObject charge deadlines writeEnv limits bucket (referenceKey reference) Nothing (companionBody verified)
        case result of
            Right (ObjectStored _) -> pure (Right reference)
            _ -> readCompanion charge reference <&> fmap (const reference)
    readCompanion charge reference =
        fmap (first objectFault) (readObject charge deadlines readEnv limits bucket (referenceKey reference) Nothing)
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
