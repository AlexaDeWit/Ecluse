-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | The structured-logging pipeline: the @katip@ 'LogEnv' every layer attaches context to, in
the format and at the severity floor configuration chose. 'JsonLog' writes one compact JSON
object per line to stdout, the shape a log collector's stdout autodiscovery consumes directly,
and 'ConsoleLog' the human-readable bracketed form for local development. Colour is forced off
either way, so a captured JSON line stays valid JSON. A bearer token reaches no field here: it
is the redacted @Secret@ of "Ecluse.Core.Credential", and a URL is reduced to its authority
before it names anything in a line.
-}
module Ecluse.Runtime.Log (
    -- * Log format
    LogFormat (..),
    parseLogFormat,

    -- * Log level
    LogLevel (..),
    parseLogLevel,

    -- * Pipeline construction
    newLogEnv,

    -- * Structured context
    moduleField,

    -- * Log lines outside a handler
    moduleContext,
    logLine,
    moduleLog,

    -- * Datadog trace correlation
    DdContext (..),
    DdSpan (..),
    ddField,
) where

import Ecluse.Runtime.Log.Internal (
    DdContext (..),
    DdSpan (..),
    LogFormat (..),
    LogLevel (..),
    ddField,
    logLine,
    moduleContext,
    moduleField,
    moduleLog,
    newLogEnv,
    parseLogFormat,
    parseLogLevel,
 )
