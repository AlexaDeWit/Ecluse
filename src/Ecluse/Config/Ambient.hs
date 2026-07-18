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
    parseEndpointUrl,
) where

import Data.List (lookup)
import Data.Text qualified as T

import Ecluse.Core.Security (splitHostPort)
import Ecluse.Core.Text (nonBlank)

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

{- | Parse an endpoint override URL (an 'ambientAwsEndpointUrl' or
'ambientAwsEndpointUrlSqs' value) into its (TLS flag, host, port). The scheme picks
the TLS flag and the default port (443\/80) when none is given; an absent scheme or a
non-numeric port yields 'Nothing'. The @host[:port]@ authority is split by the shared
bracket-aware 'Ecluse.Core.Security.splitHostPort', so a bracketed IPv6 literal
(@[::1]:4566@) is split on its closing bracket, not on an inner colon, and the host is
returned without brackets -- the same primitive the data-plane host extractor uses, so
the two cannot drift on an authority edge case.
-}
parseEndpointUrl :: Text -> Maybe (Bool, Text, Int)
parseEndpointUrl raw = do
    (secure, afterScheme) <-
        ((True,) <$> T.stripPrefix "https://" raw) <|> ((False,) <$> T.stripPrefix "http://" raw)
    let authority = T.takeWhile (`notElem` ['/', '?', '#']) afterScheme
    (hostText, portText) <- splitHostPort authority
    host <- nonBlank hostText
    port <- case T.stripPrefix ":" portText of
        Nothing -> Just (if secure then 443 else 80)
        Just digits -> readMaybe (toString digits)
    pure (secure, host, port)
