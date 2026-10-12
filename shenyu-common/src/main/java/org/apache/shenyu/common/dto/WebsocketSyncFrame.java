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

package org.apache.shenyu.common.dto;

/**
 * Opt-in initial synchronization frame. A null payload marks the end of an attempt.
 */
public final class WebsocketSyncFrame {

    public static final String REQUEST_PREFIX = "MYSELF_V1:";

    public static final String EVENT_TYPE = "INITIAL_SYNC_V1";

    private final String eventType = EVENT_TYPE;

    private final String requestId;

    private final int sequence;

    private final String payload;

    /**
     * Create a frame.
     * @param requestId connection-scoped request identifier
     * @param sequence number of preceding configuration frames
     * @param payload original configuration message, or null for completion
     */
    public WebsocketSyncFrame(final String requestId, final int sequence, final String payload) {
        this.requestId = requestId;
        this.sequence = sequence;
        this.payload = payload;
    }

    /**
     * Get the request identifier.
     * @return request identifier
     */
    public String getRequestId() {
        return requestId;
    }

    /**
     * Get the sequence.
     * @return sequence
     */
    public int getSequence() {
        return sequence;
    }

    /**
     * Get the payload.
     * @return payload
     */
    public String getPayload() {
        return payload;
    }
}
