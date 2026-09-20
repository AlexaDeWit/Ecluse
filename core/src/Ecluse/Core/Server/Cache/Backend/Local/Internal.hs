-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
-- SPDX-License-Identifier: MIT

-- | Atomic publication of local access stamps after concurrent clock allocation.
module Ecluse.Core.Server.Cache.Backend.Local.Internal (publishAccessStamp) where

-- | A delayed access must not replace a more recent stamp already published by another caller.
publishAccessStamp :: IORef Word64 -> Word64 -> IO ()
publishAccessStamp stamp incoming = atomicModifyIORef' stamp (\held -> (max held incoming, ()))
