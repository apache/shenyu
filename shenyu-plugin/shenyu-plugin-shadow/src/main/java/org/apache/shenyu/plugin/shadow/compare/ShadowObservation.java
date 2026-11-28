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

package org.apache.shenyu.plugin.shadow.compare;

import org.springframework.http.HttpHeaders;

/**
 * One bounded response observation, for the primary or the shadow call.
 *
 * <p>Headers are copied and the body is already an immutable copy, so the
 * observation is independent of the exchange or client response lifetime.</p>
 */
public final class ShadowObservation {

    private final int status;

    private final HttpHeaders headers;

    private final byte[] body;

    private final boolean truncated;

    private final long durationNanos;

    public ShadowObservation(final int status, final HttpHeaders headers, final byte[] body,
                             final boolean truncated, final long durationNanos) {
        this.status = status;
        this.headers = new HttpHeaders();
        this.headers.putAll(headers);
        this.body = body;
        this.truncated = truncated;
        this.durationNanos = durationNanos;
    }

    /**
     * Gets the response status code, or -1 when the exchange never set one.
     *
     * @return the response status code
     */
    public int getStatus() {
        return status;
    }

    /**
     * Gets the response headers.
     *
     * @return the response headers
     */
    public HttpHeaders getHeaders() {
        return headers;
    }

    /**
     * Gets the bounded response body copy.
     *
     * @return the bounded response body copy
     */
    public byte[] getBody() {
        return body;
    }

    /**
     * Whether the body exceeded the configured bound.
     *
     * @return true when the copy stopped before the body ended
     */
    public boolean isTruncated() {
        return truncated;
    }

    /**
     * Gets the observed duration.
     *
     * @return the observed duration in nanoseconds
     */
    public long getDurationNanos() {
        return durationNanos;
    }
}
