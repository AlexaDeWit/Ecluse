-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The ambient cloud-SDK environment: the handful of @AWS_*@ variables Écluse
itself consults, read straight from the process environment at boot and carried
beside the parsed configuration, never through the config document or its
environment overlay.

Keeping them out of the config AST makes "secrets never live in the structured
config" structural: a document key like @awsSecretAccessKey@ is an unknown key
and a loud parse failure, not a silently ignored ghost. The AWS SDK's own
credential discovery (@AWS_ACCESS_KEY_ID@, @AWS_SECRET_ACCESS_KEY@, the instance
role) is untouched; this record carries only the values Écluse reads explicitly.
-}
module Ecluse.Config.Ambient (
    AmbientAws (..),
    ambientAwsFromEnv,
) where

import Data.List (lookup)
import Data.Text qualified as T

{- | The @AWS_*@ values Écluse consults directly (region scoping and endpoint
overrides); each is 'Nothing' when the variable is unset. Blank-value handling
stays with each consumer, so sourcing these ambiently changes no behaviour.
-}
data AmbientAws = AmbientAws
    { ambientAwsRegion :: Maybe Text
    {- ^ @AWS_REGION@: scopes the SQS mirror queue. (CodeArtifact's mint region
    comes from the mirror-target host, not from here.)
    -}
    , ambientAwsEndpointUrlSqs :: Maybe Text
    {- ^ @AWS_ENDPOINT_URL_SQS@: the SQS endpoint override (a local emulator or a
    VPC endpoint).
    -}
    , ambientAwsEndpointUrl :: Maybe Text
    {- ^ @AWS_ENDPOINT_URL@: the generic endpoint override, consulted by the S3
    advisory-database client (the proxy's sync and Pilot's export).
    -}
    }
    deriving stock (Eq, Show)

{- | Read the ambient AWS values from the process environment (as
'System.Environment.getEnvironment' returns it).
-}
ambientAwsFromEnv :: [(String, String)] -> AmbientAws
ambientAwsFromEnv env =
    AmbientAws
        { ambientAwsRegion = look "AWS_REGION"
        , ambientAwsEndpointUrlSqs = look "AWS_ENDPOINT_URL_SQS"
        , ambientAwsEndpointUrl = look "AWS_ENDPOINT_URL"
        }
  where
    look name = T.pack <$> lookup name env
