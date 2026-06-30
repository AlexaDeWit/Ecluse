{- | Non-queuing admission control for metadata-bearing serve work.

The handle caps concurrent operations without retaining waiters: an operation either
acquires a slot immediately or is refused. This bounds aggregate metadata residency
and overload latency by construction. Acquired slots are released across normal
completion, failure, and asynchronous cancellation.
-}
module Ecluse.Core.Server.Admission (
    ServeAdmission,
    newServeAdmission,
    unlimitedServeAdmission,
    withServeAdmission,
) where

import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception qualified as UE

import Ecluse.Core.Telemetry.Record (MetricsPort (..))

{- | A process-wide serve admission handle. The constructor is hidden so only the
checked acquire/release operation can mutate its remaining capacity.
-}
data ServeAdmission
    = UnlimitedServeAdmission
    | BoundedServeAdmission (TVar Int)

{- | Allocate a bounded handle with the given positive capacity.

Configuration parsing enforces the precondition. The unchecked integer stays at this
internal composition boundary so every request pays only an STM decrement, not another
validation step.
-}
-- The configuration parser guarantees capacity > 0; this is a defense-in-depth bounds check.
{- HLINT ignore newServeAdmission "Avoid restricted function" -}
newServeAdmission :: Int -> IO ServeAdmission
newServeAdmission capacity
    | capacity <= 0 = error "ServeAdmission capacity must be positive"
    | otherwise = BoundedServeAdmission <$> newTVarIO capacity

{- | An admission handle that never refuses, for embedded applications and tests
whose subject is unrelated to overload.
-}
unlimitedServeAdmission :: ServeAdmission
unlimitedServeAdmission = UnlimitedServeAdmission

{- | Run an action only when a slot is immediately available. 'Nothing' means the
bound was already full; the caller should shed the request rather than queue it.
-}
withServeAdmission :: (MonadUnliftIO m) => MetricsPort -> ServeAdmission -> m a -> m (Maybe a)
withServeAdmission _ UnlimitedServeAdmission action = Just <$> action
withServeAdmission metrics (BoundedServeAdmission slots) action =
    UE.mask $ \restore -> do
        acquired <- atomically $ do
            available <- readTVar slots
            if available <= 0
                then pure False
                else writeTVar slots (available - 1) >> pure True
        if acquired
            then Just <$> (restore (liftIO (mpServeAdmissionInFlight metrics 1) >> action) `UE.finally` release)
            else pure Nothing
  where
    release = atomically (modifyTVar' slots (+ 1)) >> liftIO (mpServeAdmissionInFlight metrics (-1))
