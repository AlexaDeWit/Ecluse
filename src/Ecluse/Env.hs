{- | The composition root: the single record from which every effectful
component is reached.

'Env' is the one place backend choice is resolved. It holds the proxy's __seams__
— the registry-protocol client, the mirror queue, and the outbound-credential
provider — each an opaque record of functions (the Handle pattern) whose closures
already capture their backend's private state. Nothing downstream inspects which
backend a seam is; it only applies the field. Alongside the seams it carries the
shared @http-client@ 'Manager' that the data plane (metadata fetch, artifact
streaming) reuses across every request, so connection pooling and TLS setup are
established once.

Two invariants make this hold together:

* __No backend SDK appears here.__ 'Env' imports only the seam /records/, never a
  cloud SDK (no @amazonka@, no GCP client). Each seam's effectful fields return
  'IO' (not 'Ecluse.App.App'), so an adapter never imports back into this module —
  there is no import cycle and no recursive @Env@-holds-a-seam-whose-methods-need-@Env@
  knot (see @docs\/architecture\/technology-stack.md@ → "Key Decisions").

* __It is the sole composition root.__ The server and worker are each a
  self-contained entry function over this shared record
  (@runServer :: Env -> IO ()@, @runWorker :: Env -> IO ()@ in "Ecluse"), so the
  single-process program and any future split into separate binaries both wire up
  through here and nowhere else (see
  @docs\/architecture\/cloud-backends.md@ → "Process model").

Request handlers read an 'Env' in plain 'IO'; the worker\/service layer reads it
through "Ecluse.App"'s @App@ monad.
-}
module Ecluse.Env (
    -- * Composition root
    Env (..),
    newEnv,
    withEnv,
) where

import Network.HTTP.Client (Manager)
import UnliftIO (MonadUnliftIO, bracket)

import Ecluse.Credential (CredentialProvider)
import Ecluse.Queue (MirrorQueue)
import Ecluse.Registry (RegistryClient)

{- | The composition-root record: the seams plus the shared HTTP manager, from
which the whole effectful shell is reached. See the module header for the
no-SDK and sole-composition-root invariants it upholds.
-}
data Env = Env
    { envRegistry :: RegistryClient
    -- ^ The registry-protocol seam (fetch\/publish\/parse). One npm client is
    -- reused across every cloud, since protocol and auth are orthogonal axes.
    , envQueue :: MirrorQueue
    -- ^ The mirror-queue seam: the durable hand-off from the request path to the
    -- mirror worker.
    , envCredentials :: CredentialProvider
    -- ^ The outbound-credential seam: mints the bearer token used to write
    -- approved packages to the mirror target.
    , envManager :: Manager
    -- ^ The shared @http-client@ 'Manager' for the data plane (metadata fetch and
    -- artifact streaming), so connection pooling and TLS are established once and
    -- reused across requests.
    }

{- | Assemble an 'Env' from its constructed seams and a shared HTTP 'Manager'.

The 'Manager' is taken as an argument rather than built here: a 'Manager' owns a
connection pool whose lifetime should be bracketed by the caller that also owns
teardown (see 'withEnv'), and injecting it keeps 'Env' assembly pure of network
setup — so it can be exercised in tests against in-memory seam doubles with no
sockets opened. Backend selection happens in the seam smart constructors that
produce the arguments; this only gathers them.
-}
newEnv :: RegistryClient -> MirrorQueue -> CredentialProvider -> Manager -> IO Env
newEnv registry queue credentials manager =
    pure
        Env
            { envRegistry = registry
            , envQueue = queue
            , envCredentials = credentials
            , envManager = manager
            }

{- | Build an 'Env', run an action against it, and tear it down — even on
exception or asynchronous cancellation. The teardown is bracketed via @unliftio@,
so the composition root's resources are released along every exit path; this is
the scope within which the server and worker run.
-}
withEnv ::
    (MonadUnliftIO m) =>
    RegistryClient ->
    MirrorQueue ->
    CredentialProvider ->
    Manager ->
    (Env -> m a) ->
    m a
withEnv registry queue credentials manager =
    bracket
        (liftIO (newEnv registry queue credentials manager))
        teardown
  where
    -- The connection pool behind the 'Manager' is owned and released by whoever
    -- provided it, and the seams hold no resource this root acquired, so the
    -- composition root has nothing of its own to release.
    teardown :: (MonadUnliftIO m) => Env -> m ()
    teardown _ = pure ()
