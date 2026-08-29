-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The ambient cloud-SDK environment: the handful of @AWS_*@ variables Écluse itself consults,
read straight from the process environment at boot rather than from the config document.

Keeping them out of the document makes "secrets never live in the structured config" structural,
because a key like @awsSecretAccessKey@ is then an unknown key and a loud parse failure. Nothing
here touches the AWS SDK's own credential discovery.
-}
module Ecluse.Config.Ambient (
    AmbientAws (..),
    ambientAwsFromEnv,
    parseEndpointUrl,
    ambientS3Endpoint,
) where

import Data.List (lookup)
import Data.Text qualified as T

import Ecluse.Config.Types (HttpScheme (..), splitHttpScheme)
import Ecluse.Core.Credential (Secret, mkSecret)
import Ecluse.Core.Security (HostPort (..), hostPortAddressWithDefault, refuseCredentialMaterial)
import Ecluse.Core.Text (nonBlank)
import Ecluse.Runtime.Aws.Env (AwsEndpoint (..))

{- | The @AWS_*@ values Écluse consults directly: region scoping and endpoint overrides. A
field is 'Nothing' when its variable is unset, and each consumer handles a blank value itself.
-}
data AmbientAws = AmbientAws
    { ambientAwsRegion :: Maybe Text
    {- ^ @AWS_REGION@: read here only to scope SQS under an @AWS_ENDPOINT_URL_SQS@ override,
    because a real SQS URL carries its own. The SDK reads it itself to region every other client.
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

{- | Parse an endpoint override, reading the authority the way the egress gate reads one. Userinfo,
a query, a fragment, or a port outside its grammar refuses.
-}
parseEndpointUrl :: Text -> Either Secret AwsEndpoint
parseEndpointUrl raw = maybeToRight (mkSecret raw) $ do
    (scheme, _) <- splitHttpScheme raw
    guard (isRight (refuseCredentialMaterial "endpoint override" raw))
    let (secure, portless) = schemeDial scheme
    HostPort host port <- hostPortAddressWithDefault portless raw
    pure AwsEndpoint{endpointSecure = secure, endpointHost = host, endpointPort = fromIntegral port}

{- | The S3 advisory client's endpoint override, parsed once at boot. 'Nothing' is an unset or
blank @AWS_ENDPOINT_URL@, and a set-but-refused value is the 'Left', never a silent 'Nothing'.
-}
ambientS3Endpoint :: AmbientAws -> Either Secret (Maybe AwsEndpoint)
ambientS3Endpoint = traverse parseEndpointUrl . (nonBlank <=< ambientAwsEndpointUrl)

-- The TLS flag and the port a scheme dials when the URL writes no port.
schemeDial :: HttpScheme -> (Bool, Word16)
schemeDial = \case
    Https -> (True, 443)
    Http -> (False, 80)
