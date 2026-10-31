-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

-- | The production listener on an ephemeral loopback port, and the signal its shutdown handler catches.
module Ecluse.Runtime.Test.Server (withProductionListener, raiseShutdownSignal) where

import Data.Text qualified as T
import Katip (closeScribes)
import Network.Wai (Application)
import System.Posix.Signals qualified as Posix
import UnliftIO.Async (Async, link, withAsync)
import UnliftIO.Exception (bracket, throwIO)

import Ecluse.Runtime.Server.Internal (ServerConfig (scPort), listeningPrefix, runWarp)
import Ecluse.Test.Log (memoryLogEnv)
import Ecluse.Test.Poll (pollUntil)
import Ecluse.Test.Support (TestContractEscape (TestContractEscape))

{- | Run 'runWarp' on an ephemeral port and pass the port it logged and its thread. The thread is
linked, and the suite's @SIGTERM@ and @SIGINT@ handlers are restored on every exit.
-}
withProductionListener :: Text -> ServerConfig -> (ServerConfig -> IO Application) -> (Int -> Async () -> IO a) -> IO a
withProductionListener listener cfg getApp action = withSavedSignalHandlers $
    bracket memoryLogEnv (void . closeScribes . fst) $ \(logEnv, messages) ->
        withAsync (runWarp logEnv listener cfg{scPort = 0} getApp) $ \server -> do
            link server
            port <- pollUntil 200 5_000 isJust (loggedPort <$> messages)
            maybe (throwIO (TestContractEscape "listener did not report its port")) (`action` server) port
  where
    loggedPort = listToMaybe . mapMaybe (readMaybe . toString <=< T.stripPrefix (listeningPrefix listener))

{- | Deliver @SIGTERM@ to this process, as an orchestrator stopping the pod does. Outside a
'withProductionListener' the saved disposition ignores it.
-}
raiseShutdownSignal :: IO ()
raiseShutdownSignal = Posix.raiseSignal Posix.sigTERM

-- runWarp installs process-wide handlers. Ignore both signals until it does, and restore the
-- suite's handlers on every exit.
withSavedSignalHandlers :: IO a -> IO a
withSavedSignalHandlers action = bracket save restore (const action)
  where
    save = forM [Posix.sigTERM, Posix.sigINT] $ \signal ->
        (signal,) <$> Posix.installHandler signal Posix.Ignore Nothing
    restore = traverse_ (\(signal, handler) -> void (Posix.installHandler signal handler Nothing))
