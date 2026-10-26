/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.plugin.ai.proxy.enhanced.service;

import org.apache.shenyu.plugin.ai.proxy.enhanced.transport.AiTransportException;

/**
 * Decides whether an AI upstream failure should be retried, switched to fallback,
 * forwarded to the client, or propagated.
 */
final class AiProxyRetryPolicy {

    Decision decideStatus(
            final int status,
            final int retriesRemaining,
            final boolean fallbackAvailable) {
        if (!retryableStatus(status)) {
            return Decision.FORWARD_RESPONSE;
        }
        if (retriesRemaining > 0) {
            return Decision.RETRY;
        }
        if (fallbackAvailable) {
            return Decision.FALLBACK;
        }
        return Decision.FORWARD_RESPONSE;
    }

    Decision decideTransport(
            final AiTransportException.Kind kind,
            final int retriesRemaining,
            final boolean fallbackAvailable) {
        if (retriesRemaining > 0 && retryableTransportFailure(kind)) {
            return Decision.RETRY;
        }
        if (fallbackAvailable && fallbackEligibleTransportFailure(kind)) {
            return Decision.FALLBACK;
        }
        return Decision.PROPAGATE_ERROR;
    }

    private boolean retryableStatus(final int status) {
        return status == 429 || status >= 500 && status < 600;
    }

    private boolean retryableTransportFailure(final AiTransportException.Kind kind) {
        return switch (kind) {
            case CONNECT_TIMEOUT,
                 RESPONSE_HEADER_TIMEOUT,
                 STREAM_IDLE_TIMEOUT,
                 CONNECTION_RESET,
                 IO_FAILURE -> true;
            case STREAM_DURATION_TIMEOUT,
                 RESPONSE_TOO_LARGE,
                 DNS_FAILURE,
                 TLS_FAILURE,
                 UNKNOWN -> false;
        };
    }

    private boolean fallbackEligibleTransportFailure(final AiTransportException.Kind kind) {
        return switch (kind) {
            case CONNECT_TIMEOUT,
                 RESPONSE_HEADER_TIMEOUT,
                 RESPONSE_TOO_LARGE,
                 CONNECTION_RESET,
                 IO_FAILURE -> true;
            case STREAM_IDLE_TIMEOUT,
                 STREAM_DURATION_TIMEOUT,
                 DNS_FAILURE,
                 TLS_FAILURE,
                 UNKNOWN -> false;
        };
    }

    enum Decision {
        RETRY,
        FALLBACK,
        FORWARD_RESPONSE,
        PROPAGATE_ERROR
    }
}
