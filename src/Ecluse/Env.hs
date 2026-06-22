{- | The composition root: the single record from which every effectful
component is reached.

'Env' is the one place backend choice is resolved. It holds the proxy's __handles__
— the registry-protocol client, the mirror queue, and the outbound-credential
provider — each an opaque record of functions (the Handle pattern) whose closures
already capture their backend's private state. Nothing downstream inspects which
backend a handle is; it only applies the field. Alongside the handles it carries the
shared @http-client@ 'Manager' that the data plane (metadata fetch, artifact
streaming) reuses across every request, so connection pooling and TLS setup are
established once.

Two invariants make this hold together:

* __No backend SDK appears here.__ 'Env' imports only the handle /records/, never a
  cloud SDK (no @amazonka@, no GCP client). Each handle's effectful fields return
  'IO' (not 'Ecluse.App.App'), so an adapter never imports back into this module —
  there is no import cycle and no recursive @Env@-holds-a-handle-whose-methods-need-@Env@
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

import Katip (LogEnv)
import Network.HTTP.Client (Manager)
import UnliftIO (MonadUnliftIO, bracket)

import Ecluse.Credential (CredentialProvider)
import Ecluse.Queue (MirrorQueue)
import Ecluse.Registry (RegistryClient)

{- | The composition-root record: the handles plus the shared HTTP manager, from
which the whole effectful shell is reached. See the module header for the
no-SDK and sole-composition-root invariants it upholds.
-}
data Env = Env
    { envRegistry :: RegistryClient
    {- ^ The registry-protocol handle (fetch\/publish\/parse). One npm client is
    reused across every cloud, since protocol and auth are orthogonal axes.
    -}
    , envQueue :: MirrorQueue
    {- ^ The mirror-queue handle: the durable hand-off from the request path to the
    mirror worker.
    -}
    , envCredentials :: CredentialProvider
    {- ^ The outbound-credential handle: mints the bearer token used to write
    approved packages to the mirror target.
    -}
    , envManager :: Manager
    {- ^ The shared @http-client@ 'Manager' for the data plane (metadata fetch and
    artifact streaming), so connection pooling and TLS are established once and
    reused across requests.
    -}
    , envLogEnv :: LogEnv
    {- ^ The @katip@ logging environment (see "Ecluse.Log"): the structured-log
    stream every layer attaches context to, with its stdout scribe and format
    chosen at startup.
    -}
    }

{- | Assemble an 'Env' from its constructed handles and a shared HTTP 'Manager'.

The 'Manager' and 'LogEnv' are taken as arguments rather than built here: a
'Manager' owns a connection pool whose lifetime should be bracketed by the caller
that also owns teardown (see 'withEnv'), and injecting both keeps 'Env' assembly
pure of network and logging setup — so it can be exercised in tests against
in-memory handle doubles with no sockets opened and no scribe attached to stdout.
Backend selection happens in the handle smart constructors that produce the
arguments; this only gathers them.
-}
newEnv :: RegistryClient -> MirrorQueue -> CredentialProvider -> Manager -> LogEnv -> IO Env
newEnv registry queue credentials manager logEnv =
    pure
        Env
            { envRegistry = registry
            , envQueue = queue
            , envCredentials = credentials
            , envManager = manager
            , envLogEnv = logEnv
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
    LogEnv ->
    (Env -> m a) ->
    m a
withEnv registry queue credentials manager logEnv =
    bracket
        (liftIO (newEnv registry queue credentials manager logEnv))
        teardown
  where
    -- The connection pool behind the 'Manager' is owned and released by whoever
    -- provided it, and the handles hold no resource this root acquired, so the
    -- composition root has nothing of its own to release.
    teardown :: (MonadUnliftIO m) => Env -> m ()
    teardown _ = pure ()
