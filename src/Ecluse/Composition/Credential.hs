{- | The composition root's credential resolution: the process-global credential
providers and the mirror-target provider selection.

== Global providers, per-mount reference

A 'Ecluse.Core.Credential.CredentialProvider' is the service's own cloud identity,
built __once__ from the environment layer ('initCredentialProviders') and held
process-global; a mount does not carry a provider, it only __names__ which backend
it draws on (its @mtCredential@). The boot-time check is the resolution of that
reference: every distinct credential backend named across all mounts must resolve
to an __initialised__ provider, or the app halts at boot (see
@docs\/architecture\/cloud-backends.md@ → "Credential Provider"). Only the
@static@ backend has a leaf (from @ECLUSE_MIRROR_TARGET_TOKEN@); a mount naming
@codeartifact@ or @adc@ resolves to no provider and is an honest boot failure.

The pure half of the selection ('planMirrorCredential',
'resolveCodeArtifactConfig') is separated from the effectful build
('initCredentialProviders' minting an eager CodeArtifact token) so the resolution
rules are unit-tested without touching AWS. Failures aggregate as
'Ecluse.Composition.BootError.BootError's, so one run reports every unresolved
input.
-}
module Ecluse.Composition.Credential (
    -- * Global credential providers
    CredentialProviders,
    initCredentialProviders,
    initializedEcosystems,
    lookupProvider,

    -- * Mirror-target credential provider selection
    planMirrorCredential,
    resolveCodeArtifactConfig,

    -- * Internals exported for testing
    parseCodeArtifactHost,
) where

import Data.Char (isDigit)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import UnliftIO (tryAny)

import Ecluse.Composition.BootError (BootError (..), mountEnvKey)
import Ecluse.Config (
    AppConfig (..),
    CredentialBackend (..),
    MountConfig (..),
 )
import Ecluse.Core.Credential (AuthToken (..), CredentialProvider, staticProvider)
import Ecluse.Core.Credential.Refresh (CredentialReporters)
import Ecluse.Core.Ecosystem (Ecosystem)
import Ecluse.Core.Security (hostAddress)
import Ecluse.Core.Security.Egress (registryUrlText)
import Ecluse.Core.Text (displayExceptionT, nonBlank)
import Ecluse.Runtime.Credential.CodeArtifact (CodeArtifactConfig (..), newCodeArtifactProvider)

{- | The process-global credential providers, keyed by the ecosystem they
serve. Built __once__ at the composition root from the environment layer; a
mount references one by name and never holds its own.

The keyset (see 'initializedEcosystems') is the boot-check's pure surface -- a mount
that names an ecosystem absent from it has an unresolved credential reference.
-}
newtype CredentialProviders = CredentialProviders (Map Ecosystem CredentialProvider)

{- | Build the global credential providers from the environment layer, or the
aggregated boot errors that block them. The mirror-target write provider is
selected by 'cfgCredentialProvider' (see 'planMirrorCredential'):

* @static@ -- built from @ECLUSE_MIRROR_TARGET_TOKEN@ ('cfgMirrorTargetToken') when set;
  absent, no static provider is initialised, so a mount naming @static@ fails the
  boot-time credential-reference check.
* @codeartifact@ -- the CodeArtifact inputs are resolved
  ('resolveCodeArtifactConfig'); a required input that resolves by neither an
  explicit key nor the mirror-target host is a fail-loud boot error. On success the
  generic refresh\/cache wrapper around the CodeArtifact mint leaf
  ('newCodeArtifactProvider') is built, which mints once eagerly -- so a misconfigured
  identity fails here at boot. AWS credentials are the ambient container\/task role
  (the standard chain), never an Écluse key. A mint that throws at boot (a transient
  AWS error, or a permanent one like a bad domain\/region or missing permission) is
  caught and rendered as a 'CodeArtifactMintFailed' boot error rather than escaping as
  a raw exception, so it joins the aggregated failure block.
* @gcp-artifact-registry@ -- recognised but not built in this binary, so selecting
  it is a fail-loud boot error rather than a silent fall-through.

The @static@ provider is also built whenever @ECLUSE_MIRROR_TARGET_TOKEN@ is present,
independent of the selector, so a static token never goes unused.

The 'CredentialReporters' are handed to the refreshing CodeArtifact provider so its
mint breaker and refresh outcomes record to telemetry; the static provider never
refreshes, so they do not concern it. The composition root supplies the deferred
reporters that go live once the telemetry substrate exists.
-}
initCredentialProviders :: CredentialReporters -> AppConfig -> IO (Either [BootError] CredentialProviders)
initCredentialProviders reporters app = do
    let plans = map (\(eco, mcfg) -> (eco, mcfg, planMirrorCredential eco app mcfg)) (Map.toList (cfgMounts app))
    let errs = concat [e | (_, _, Left e) <- plans]
    if not (null errs)
        then pure (Left errs)
        else do
            let validPlans = [(eco, mcfg, mca) | (eco, mcfg, Right mca) <- plans]
            results <- traverse (\(eco, mcfg, mca) -> initProviderFor reporters eco mcfg mca) validPlans
            let (initErrs, valid) = partitionEithers results
            if not (null initErrs)
                then pure (Left (concat initErrs))
                else pure (Right (CredentialProviders (Map.fromList [(eco, p) | (eco, Just p) <- valid])))

-- One mount plan's provider build, the effectful half of 'initCredentialProviders':
-- no CodeArtifact selection means the static token provider when its token is set
-- (else no provider, the unresolved-reference case the boot check rejects); a
-- CodeArtifact selection mints once eagerly, a throw rendered as a
-- 'CodeArtifactMintFailed' boot error so it joins the aggregated failure block.
initProviderFor :: CredentialReporters -> Ecosystem -> MountConfig -> Maybe CodeArtifactConfig -> IO (Either [BootError] (Ecosystem, Maybe CredentialProvider))
initProviderFor reporters eco mcfg = \case
    Nothing -> pure (Right (eco, staticTokenProvider mcfg))
    Just caConfig ->
        tryAny (newCodeArtifactProvider reporters caConfig) <&> \case
            Left err -> Left [CodeArtifactMintFailed (displayExceptionT err)]
            Right provider -> Right (eco, Just provider)

-- The static mirror-target write provider, when its token
-- (ECLUSE_MIRROR_TARGET_TOKEN) is configured.
staticTokenProvider :: MountConfig -> Maybe CredentialProvider
staticTokenProvider mcfg =
    mntMirrorTargetToken mcfg <&> \token ->
        staticProvider AuthToken{authSecret = token, authExpiresAt = Nothing}

{- | The set of ecosystems that resolved to an initialised provider -- the
pure surface the boot-time credential-reference check reasons over.
-}
initializedEcosystems :: CredentialProviders -> Set Ecosystem
initializedEcosystems (CredentialProviders ps) = Map.keysSet ps

{- | Look up the initialised provider for an ecosystem, 'Nothing' when none is
initialised (the unresolved-reference case the boot check rejects).
-}
lookupProvider :: Ecosystem -> CredentialProviders -> Maybe CredentialProvider
lookupProvider eco (CredentialProviders ps) = Map.lookup eco ps

{- | Decide what mirror-target write provider the environment layer selects, as the
pure half of 'initCredentialProviders': 'Nothing' when the @static@ provider is
selected (its leaf is the @ECLUSE_MIRROR_TARGET_TOKEN@ already handled there), 'Just' a
resolved 'CodeArtifactConfig' when @codeartifact@ is selected, or the aggregated
boot errors that block the selection.

The @gcp-artifact-registry@ arm is recognised but not built in this binary, so it is
a fail-loud 'MirrorCredentialProviderUnavailable' boot error -- never a silent
fall-through to a different provider, mirroring how
'Ecluse.Composition.MirrorQueue.planMirrorQueue' treats the GCP queue arm.
-}
planMirrorCredential :: Ecosystem -> AppConfig -> MountConfig -> Either [BootError] (Maybe CodeArtifactConfig)
planMirrorCredential eco app mcfg = case mntCredentialProvider mcfg of
    StaticCredential -> Right Nothing
    CodeArtifactCredential -> Just <$> resolveCodeArtifactConfig eco app mcfg
    AdcCredential -> Left [MirrorCredentialProviderUnavailable AdcCredential]

{- | Resolve the CodeArtifact inputs for the mirror-target token, or the aggregated
boot errors naming each input that could not be resolved.

Each required input is resolved __(a) from its explicit @MIRROR_TARGET_CODEARTIFACT_*@
key, else (b) by parsing the mirror-target URL host__ of the form
@{domain}-{owner}.d.codeartifact.{region}.amazonaws.com@ (the documented host
fallback). The region resolves explicit key → host → @AWS_REGION@: the endpoint host
encodes the domain's authoritative region, so it outranks the process-wide
@AWS_REGION@ (a cross-region deploy mints against the domain's region, not the
caller's). The mirror-target URL is the resolved one -- an unset @ECLUSE_MIRROR_TARGET@
has already folded onto the private upstream -- so a private-upstream CodeArtifact
endpoint is parsed too. The optional token-duration carries through
('cfgMirrorCodeArtifactTokenDuration').

The @{owner}@ is a 12-digit AWS account id: a resolved owner (from either source)
that is not 12 digits is a fail-loud 'CodeArtifactConfigInvalid' error, and a host
whose tail after the last hyphen is not an account id is not a CodeArtifact endpoint
at all (so it falls through to the named-key check). If a required input resolves by
__neither__ source, that is a fail-loud 'CodeArtifactConfigMissing' boot error naming
the exact key the operator must set, aggregated so one run reports every problem.
-}
resolveCodeArtifactConfig :: Ecosystem -> AppConfig -> MountConfig -> Either [BootError] CodeArtifactConfig
resolveCodeArtifactConfig eco app mcfg =
    case partitionEithers [domainE, ownerE, regionE] of
        ([], [domain, owner, region]) ->
            Right
                CodeArtifactConfig
                    { caRegion = region
                    , caDomain = domain
                    , caDomainOwner = Just owner
                    , caDurationSeconds = mntMirrorCodeArtifactTokenDuration mcfg
                    }
        (errs, _) -> Left errs
  where
    -- An unset mirror target falls back to the private upstream (the fold the
    -- Haddock above describes); with neither set the parse yields 'Nothing', so
    -- a still-unresolved input is reported as its missing explicit key.
    parsed :: Maybe (Text, Text, Text)
    parsed =
        parseCodeArtifactHost . hostAddress . registryUrlText
            =<< (mntMirrorTarget mcfg <|> mntPrivateUpstream mcfg)

    resolve :: Text -> [Maybe Text] -> Either BootError Text
    resolve key candidates =
        maybe (Left (CodeArtifactConfigMissing (mountEnvKey eco key))) Right (asum (map (>>= nonBlank) candidates))

    domainE = resolve "MIRROR_CODE_ARTIFACT_DOMAIN" [mntMirrorCodeArtifactDomain mcfg, fst3 <$> parsed]
    ownerE =
        resolve "MIRROR_CODE_ARTIFACT_DOMAIN_OWNER" [mntMirrorCodeArtifactDomainOwner mcfg, snd3 <$> parsed]
            >>= validateAccountId (mountEnvKey eco "MIRROR_CODE_ARTIFACT_DOMAIN_OWNER")
    regionE = resolve "MIRROR_CODE_ARTIFACT_REGION" [mntMirrorCodeArtifactRegion mcfg, thd3 <$> parsed, cfgAwsRegion app]

    fst3 (a, _, _) = a
    snd3 (_, b, _) = b
    thd3 (_, _, c) = c

-- A resolved owner must be a 12-digit AWS account id (an explicit key can supply a
-- malformed one; a host owner is already validated by 'parseCodeArtifactHost').
validateAccountId :: Text -> Text -> Either BootError Text
validateAccountId key owner
    | isAccountId owner = Right owner
    | otherwise = Left (CodeArtifactConfigInvalid key "expected a 12-digit AWS account id")

-- Whether a value is a 12-digit AWS account id.
isAccountId :: Text -> Bool
isAccountId t = T.compareLength t 12 == EQ && T.all isDigit t

{- | Parse a CodeArtifact npm endpoint host into its (domain, owner, region). The host
shape is @{domain}-{owner}.d.codeartifact.{region}.amazonaws.com@; the @{owner}@ is the
12-digit account id after the __last__ hyphen of the first label, so a domain may
itself contain hyphens. 'Nothing' for any host that is not this shape -- including one
whose tail after the last hyphen is not an account id, so a hyphen-bearing
non-CodeArtifact host never mis-parses into a bogus owner.
-}
parseCodeArtifactHost :: Text -> Maybe (Text, Text, Text)
parseCodeArtifactHost host =
    -- The accepted shape is exactly one @.d.codeartifact.@ marker, splitting the host
    -- into its @{domain}-{owner}@ label and its @{region}.amazonaws.com@ tail; any
    -- other number of parts (none, or a host carrying the marker twice) is not a
    -- CodeArtifact endpoint and is rejected here rather than via an implicit
    -- pattern-match failure in the 'Maybe' monad.
    case T.splitOn ".d.codeartifact." host of
        [domainOwner, regionTail] -> do
            region <- nonBlank =<< T.stripSuffix ".amazonaws.com" regionTail
            let (domainDash, owner) = T.breakOnEnd "-" domainOwner
            domain <- nonBlank (T.dropEnd 1 domainDash)
            guard (isAccountId owner)
            pure (domain, owner, region)
        _ -> Nothing
