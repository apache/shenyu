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

package org.apache.shenyu.plugin.ai.proxy.enhanced.transport;

/**
 * Classified failure raised by an AI transport.
 */
public final class AiTransportException extends RuntimeException {

    private static final long serialVersionUID = 1L;

    private final Kind kind;

    /**
     * Create an AI transport exception.
     *
     * @param kind transport failure kind
     * @param message failure message
     * @param cause original failure
     */
    public AiTransportException(
            final Kind kind,
            final String message,
            final Throwable cause) {
        super(message, cause);
        this.kind = kind;
    }

    /**
     * Get transport failure kind.
     *
     * @return transport failure kind
     */
    public Kind getKind() {
        return kind;
    }

    /**
     * Transport failure classification.
     */
    public enum Kind {
        CONNECT_TIMEOUT,
        RESPONSE_HEADER_TIMEOUT,
        STREAM_IDLE_TIMEOUT,
        STREAM_DURATION_TIMEOUT,
        RESPONSE_TOO_LARGE,
        DNS_FAILURE,
        TLS_FAILURE,
        CONNECTION_RESET,
        IO_FAILURE,
        UNKNOWN
    }
}
