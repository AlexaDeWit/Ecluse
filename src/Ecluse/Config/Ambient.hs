-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The ambient cloud-SDK environment: the handful of @AWS_*@ variables Écluse
itself consults. It reads them straight from the process environment at boot, and
carries them beside the parsed configuration. They never pass through the config
document or its environment overlay.

Keeping them out of the config AST makes "secrets never live in the structured
config" structural. A document key like @awsSecretAccessKey@ is an unknown key and a
loud parse failure, not a silently ignored ghost. Nothing here touches the AWS SDK's
own credential discovery (@AWS_ACCESS_KEY_ID@, @AWS_SECRET_ACCESS_KEY@, the instance
role). This record carries only the values Écluse reads explicitly.
-}
module Ecluse.Config.Ambient (
    AmbientAws (..),
    ambientAwsFromEnv,
    parseEndpointUrl,
) where

import Data.List (lookup)
import Data.Text qualified as T

import Ecluse.Config.Parser (HttpScheme (..), splitHttpScheme)
import Ecluse.Core.Security (HostPort (..), carriesUserinfo, hostPortAddressWithDefault)

{- | The @AWS_*@ values Écluse consults directly: region scoping and endpoint overrides. A
field is 'Nothing' when its variable is unset, and each consumer handles a blank value itself.
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

{- | Parse an endpoint override into its (TLS flag, host, port), reading the authority the way
the egress gate reads one. Userinfo, or a port outside the gate's grammar, yields 'Nothing'.
-}
parseEndpointUrl :: Text -> Maybe (Bool, Text, Int)
parseEndpointUrl raw = do
    (scheme, _) <- splitHttpScheme raw
    guard (not (carriesUserinfo raw))
    let (secure, portless) = schemeDial scheme
    HostPort host port <- hostPortAddressWithDefault portless raw
    pure (secure, host, fromIntegral port)

-- The TLS flag and the port a scheme dials when the URL writes no port.
schemeDial :: HttpScheme -> (Bool, Word16)
schemeDial = \case
    Https -> (True, 443)
    Http -> (False, 80)
