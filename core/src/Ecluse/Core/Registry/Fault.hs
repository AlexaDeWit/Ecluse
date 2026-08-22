-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The registry layer's ecosystem-agnostic fault vocabulary: the exception form
of a response-bound breach.

Every registry data plane reads response bodies through
'Ecluse.Core.Security.boundedRead' against a 'Ecluse.Core.Security.Limits' budget. The
bounded read reports a breach as a __value__ (a 'Ecluse.Core.Security.LimitError'). Some
consumers sit behind an exception-shaped boundary instead, and this module owns the
typed form they carry there. The breach concerns the budget, never one ecosystem's wire
format, so the vocabulary lives beside the agnostic registry contract rather than in a
protocol module.
-}
module Ecluse.Core.Registry.Fault (
    ResponseBoundExceeded (..),
) where

import Ecluse.Core.Security (LimitError)

{- | The typed exception form of a response-bound breach: a body that crossed the
'Ecluse.Core.Security.maxBodyBytes' ceiling, carried as its 'Ecluse.Core.Security.LimitError'.
A breach at an exception-shaped boundary stays a classified refusal, never a truncated body.
-}
newtype ResponseBoundExceeded = ResponseBoundExceeded LimitError
    deriving stock (Eq, Show)

instance Exception ResponseBoundExceeded
