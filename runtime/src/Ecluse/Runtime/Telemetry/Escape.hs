-- SPDX-FileCopyrightText: 2026 Alexandra de Wit
--
-- SPDX-License-Identifier: MIT

{- | What a span records when an exception escapes it. @hs-opentelemetry@ records an escaping
exception's 'displayException'. For an HTTP client or @amazonka@ exception, that text holds the
request, header and query included, or a proxy variable's value. 'withEscapeHandler' tells the
library to ignore those, and 'recordEscape' records a typed cause and the error status in their place.
-}
module Ecluse.Runtime.Telemetry.Escape (
    escapedRequestCause,
    withEscapeHandler,
    recordEscape,
    escapeRecordingMiddleware,
) where

import Amazonka qualified as AWS
import Data.HashMap.Strict qualified as HashMap
import Data.Typeable (typeOf)
import Network.HTTP.Client (HttpException)
import Network.HTTP.Client.Internal (unHttpExceptionContentWrapper)
import Network.Wai (Middleware)
import OpenTelemetry.Attributes (toAttribute)
import OpenTelemetry.Trace.Core (
    ExceptionClassification (IgnoredException),
    ExceptionHandler,
    ExceptionResponse (ExceptionResponse),
    NewEvent (NewEvent, newEventAttributes, newEventName, newEventTimestamp),
    Span,
    SpanStatus (Error),
    TracerProviderOptions (tracerProviderOptionsExceptionHandlers),
    addEvent,
    getActiveSpan,
    setStatus,
 )
import UnliftIO.Exception (withException)

import Ecluse.Core.Fault (renderTransportFault)
import Ecluse.Core.Fault.Http (displayFaultT)
import Ecluse.Runtime.Aws.Fault (classifyAwsTransport)

-- | The typed cause of an exception whose own rendering can print a request or a proxy credential, or 'Nothing' for any other.
escapedRequestCause :: SomeException -> Maybe Text
escapedRequestCause err
    | Just (_ :: HttpException) <- fromException err = Just (displayFaultT err)
    -- A fault http-client raises outside a request, such as a malformed proxy variable, carries no HttpException.
    | Just _ <- unHttpExceptionContentWrapper <$> fromException err = Just (displayFaultT err)
    | Just (aws :: AWS.Error) <- fromException err = Just (renderTransportFault (classifyAwsTransport aws))
    | otherwise = Nothing

-- | Make the library ignore an exception 'escapedRequestCause' knows, so only 'recordEscape' records it.
withEscapeHandler :: TracerProviderOptions -> TracerProviderOptions
withEscapeHandler options =
    options{tracerProviderOptionsExceptionHandlers = ignoreRequestFaults : tracerProviderOptionsExceptionHandlers options}

ignoreRequestFaults :: ExceptionHandler
ignoreRequestFaults err = ExceptionResponse IgnoredException mempty <$ escapedRequestCause err

-- | Record an escaping exception the library ignores: its typed cause as the error status, and as an @exception@ event.
recordEscape :: (MonadIO m) => Span -> SomeException -> m ()
recordEscape theSpan err@(SomeException inner) = whenJust (escapedRequestCause err) $ \cause -> do
    setStatus theSpan (Error cause)
    addEvent
        theSpan
        NewEvent
            { newEventName = "exception"
            , newEventAttributes =
                HashMap.fromList
                    [ ("exception.type", toAttribute (show (typeOf inner) :: Text))
                    , ("exception.message", toAttribute cause)
                    , ("exception.escaped", toAttribute True)
                    ]
            , newEventTimestamp = Nothing
            }

-- | Record an exception escaping the application on the active span, the WAI server span when this runs inside its middleware.
escapeRecordingMiddleware :: Middleware
escapeRecordingMiddleware app request respond =
    app request respond `withException` \err -> getActiveSpan >>= traverse_ (`recordEscape` err)
