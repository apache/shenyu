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

package org.apache.shenyu.plugin.mcp.server.transport;

/**
 * Raised when a JSON-RPC message cannot be admitted to its SSE session because the
 * bounded pending budget refuses it. The message content and session identity are
 * deliberately kept out of the message to keep log output low-cardinality.
 */
final class SseSessionRejectException extends RuntimeException {

    private static final long serialVersionUID = 1L;

    private final SseSessionState.Reservation reservation;

    private final int byteLength;

    SseSessionRejectException(final SseSessionState.Reservation reservation, final int byteLength) {
        super("SSE session rejected the message (reservation=" + reservation
                + ", byteLength=" + byteLength + ")");
        this.reservation = reservation;
        this.byteLength = byteLength;
    }

    /**
     * The admission outcome that caused the rejection.
     *
     * @return the reservation outcome
     */
    SseSessionState.Reservation getReservation() {
        return reservation;
    }

    /**
     * The serialized UTF-8 length of the rejected payload.
     *
     * @return the byte length
     */
    int getByteLength() {
        return byteLength;
    }
}
