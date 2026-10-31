-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | Preparation binds the plan and image archive before Task starts a subject.
module Ecluse.Test.Contract.Artifact.Identity (
    PreparedIdentity (..),
    beginPreparation,
    finishPreparation,
    verifyPreparation,
    verifyPlan,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.Directory (createDirectory, createDirectoryIfMissing, doesPathExist)
import System.FilePath ((</>))

import Ecluse.Test.Contract.Manifest
import Ecluse.Test.Contract.Result (ExecutionIdentity (..))

-- | The archive digest supplements the execution identity used by native reports.
data PreparedIdentity = PreparedIdentity
    { preparedSchemaVersion :: Int
    , preparedExecution :: ExecutionIdentity
    , preparedArchiveDigest :: Text
    }
    deriving stock (Eq, Show)

instance ToJSON PreparedIdentity where
    toJSON prepared =
        object
            [ "schemaVersion" .= preparedSchemaVersion prepared
            , "source" .= identitySource execution
            , "image" .= identityImage execution
            , "architecture" .= identityArchitecture execution
            , "planDigest" .= identityPlanDigest execution
            , "runId" .= identityRunId execution
            , "archiveDigest" .= preparedArchiveDigest prepared
            ]
      where
        execution = preparedExecution prepared

instance FromJSON PreparedIdentity where
    parseJSON = withObject "prepared identity" $ \o ->
        PreparedIdentity
            <$> o .: "schemaVersion"
            <*> (ExecutionIdentity <$> o .: "source" <*> o .: "image" <*> o .: "architecture" <*> o .: "planDigest" <*> o .: "runId")
            <*> o .: "archiveDigest"

-- | The caller holds the artifact lock until preparation finishes or fails.
beginPreparation :: FilePath -> IO ()
beginPreparation root = do
    exists <- traverse (doesPathExist . (root </>)) ["plan.json", "identity.json", "docker-image.tar", ".preparation"]
    when (or exists) $ failRunner "prepared inputs already exist, choose a new ECLTEST_ARTIFACT_DIR"
    createDirectoryIfMissing True root
    createDirectory (root </> ".preparation")

-- | A failed preparation cannot retry in the same reserved directory.
finishPreparation :: FilePath -> Text -> IO ()
finishPreparation root image = do
    reserved <- doesPathExist (root </> ".preparation")
    exists <- doesPathExist (root </> "identity.json")
    unless (reserved && not exists) $ failRunner "preparation is absent or already complete"
    source <- requiredField "ECLTEST_SOURCE"
    architecture <- normaliseArchitecture <$> requiredField "ECLTEST_ARCHITECTURE"
    runId <- requiredField "ECLTEST_RUN_ID"
    planDigest <- digestFile (root </> "plan.json")
    archiveDigest <- digestFile (root </> "docker-image.tar")
    let prepared = PreparedIdentity 1 (ExecutionIdentity source image architecture planDigest runId) archiveDigest
    checkProblems (preparationProblems source architecture (Just runId) planDigest archiveDigest Nothing prepared)
    writeJsonFile (root </> "identity.json") prepared

-- | Recheck distributed bytes and optional loaded image configuration before exporting identity.
verifyPreparation :: FilePath -> Maybe Text -> IO ExecutionIdentity
verifyPreparation = verifyInputs True

-- | Final report audits need the distributed plan but do not download the image archive.
verifyPlan :: FilePath -> IO ExecutionIdentity
verifyPlan root = verifyInputs False root Nothing

verifyInputs :: Bool -> FilePath -> Maybe Text -> IO ExecutionIdentity
verifyInputs includeArchive root loadedImage = do
    prepared <- readJsonFile (root </> "identity.json")
    source <- requiredField "ECLTEST_SOURCE"
    architecture <- normaliseArchitecture <$> requiredField "ECLTEST_ARCHITECTURE"
    runId <- fmap toText <$> lookupEnv "ECLTEST_RUN_ID"
    planDigest <- digestFile (root </> "plan.json")
    archiveDigest <- if includeArchive then digestFile (root </> "docker-image.tar") else pure (preparedArchiveDigest prepared)
    checkProblems (preparationProblems source architecture (runId >>= nonempty) planDigest archiveDigest loadedImage prepared)
    let execution = preparedExecution prepared
    writeArtifactFile (root </> "identity.env") (TE.encodeUtf8 (identityEnvironment execution))
    pure execution
  where
    nonempty value = if T.null value then Nothing else Just value

-- Every declared identity field and distributed digest contributes to acceptance.
preparationProblems :: Text -> Text -> Maybe Text -> Text -> Text -> Maybe Text -> PreparedIdentity -> [Text]
preparationProblems source architecture runId planDigest archiveDigest loadedImage prepared =
    ["unsupported preparation schema" | preparedSchemaVersion prepared /= 1]
        <> ["source identity mismatch" | identitySource execution /= source || T.null source]
        <> ["architecture identity mismatch" | identityArchitecture execution /= architecture || architecture `notElem` ["amd64", "arm64"]]
        <> ["run identity mismatch" | T.null (identityRunId execution) || maybe False (/= identityRunId execution) runId]
        <> ["invalid image configuration identity" | not (maybe False validDigest (T.stripPrefix "sha256:" (identityImage execution)))]
        <> ["plan digest mismatch" | not (validDigest (identityPlanDigest execution)) || identityPlanDigest execution /= planDigest]
        <> ["archive digest mismatch" | not (validDigest (preparedArchiveDigest prepared)) || preparedArchiveDigest prepared /= archiveDigest]
        <> ["loaded image configuration mismatch" | maybe False (/= identityImage execution) loadedImage]
  where
    execution = preparedExecution prepared
    validDigest value = T.length value == 64 && T.all (`elem` ("0123456789abcdef" :: String)) value

-- Quote values as shell data, including apostrophes and command substitutions.
identityEnvironment :: ExecutionIdentity -> Text
identityEnvironment execution =
    T.unlines
        [ "export " <> name <> "='" <> T.replace "'" "'\"'\"'" value <> "'"
        | (name, value) <-
            [ ("ECLTEST_SOURCE", identitySource execution)
            , ("ECLTEST_IMAGE_ID", identityImage execution)
            , ("ECLTEST_ARCHITECTURE", identityArchitecture execution)
            , ("ECLTEST_PLAN_DIGEST", identityPlanDigest execution)
            , ("ECLTEST_RUN_ID", identityRunId execution)
            ]
        ]

requiredField :: String -> IO Text
requiredField name =
    lookupEnv name >>= \case
        Just value | not (null value) -> pure (toText value)
        _ -> failRunner ("missing required execution identity: " <> toText name)

checkProblems :: [Text] -> IO ()
checkProblems problems = unless (null problems) (failRunner (T.unlines problems))
