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

import io.modelcontextprotocol.util.Assert;

import java.time.Duration;
import java.util.Objects;

/**
 * Admission policy for one server-produced SSE session of the MCP server transport.
 *
 * <p>The transport serializes each JSON-RPC message, reserves budget for its UTF-8 bytes
 * and one event slot, and only then hands the event to the reactive stream. When the
 * client stops reading, the reserved budget fills up and further sends are rejected
 * instead of growing an unbounded buffer; a session whose pending output makes no
 * progress for {@code stallTimeout} is terminated.</p>
 *
 * <p>Defaults are deliberately conservative so that ordinary MCP traffic (whose responses
 * are small JSON documents) never hits them: at most {@value #DEFAULT_MAX_PENDING_EVENTS}
 * pending events and {@value #DEFAULT_MAX_PENDING_BYTES} pending serialized bytes per
 * session, with a {@value #DEFAULT_STALL_TIMEOUT_SECONDS}s output-stall bound. The policy
 * is installed per provider through the builder and may be tuned per deployment; the
 * byte budget covers the serialized JSON-RPC payload only, not the fixed SSE framing
 * overhead, which stays proportional to the (also bounded) event count.</p>
 */
public final class SseSessionPolicy {

    /**
     * Default upper bound of events accepted but not yet consumed by the SSE stream.
     */
    public static final int DEFAULT_MAX_PENDING_EVENTS = 256;

    /**
     * Default upper bound of serialized UTF-8 bytes accepted but not yet consumed by the
     * SSE stream.
     */
    public static final int DEFAULT_MAX_PENDING_BYTES = 1024 * 1024;

    /**
     * Default maximum age of pending output without any consumption progress.
     */
    public static final Duration DEFAULT_STALL_TIMEOUT = Duration.ofSeconds(30);

    private final int maxPendingEvents;

    private final int maxPendingBytes;

    private final Duration stallTimeout;

    private SseSessionPolicy(final Builder builder) {
        this.maxPendingEvents = builder.maxPendingEvents;
        this.maxPendingBytes = builder.maxPendingBytes;
        this.stallTimeout = builder.stallTimeout;
    }

    /**
     * Create a policy with the documented defaults.
     *
     * @return the default policy
     */
    public static SseSessionPolicy defaults() {
        return builder().build();
    }

    /**
     * Create a builder initialized with the documented defaults.
     *
     * @return a fresh builder
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Maximum number of events accepted but not yet consumed by the SSE stream.
     *
     * @return the pending-event limit
     */
    public int getMaxPendingEvents() {
        return maxPendingEvents;
    }

    /**
     * Maximum number of serialized UTF-8 bytes accepted but not yet consumed by the SSE
     * stream.
     *
     * @return the pending-byte limit
     */
    public int getMaxPendingBytes() {
        return maxPendingBytes;
    }

    /**
     * Maximum age of pending output without any consumption progress. A session without
     * pending output is never considered stalled.
     *
     * @return the stall timeout
     */
    public Duration getStallTimeout() {
        return stallTimeout;
    }

    @Override
    public String toString() {
        return "SseSessionPolicy{maxPendingEvents=" + maxPendingEvents
                + ", maxPendingBytes=" + maxPendingBytes
                + ", stallTimeout=" + stallTimeout + '}';
    }

    /**
     * Builder for {@link SseSessionPolicy}.
     */
    public static final class Builder {

        private int maxPendingEvents = DEFAULT_MAX_PENDING_EVENTS;

        private int maxPendingBytes = DEFAULT_MAX_PENDING_BYTES;

        private Duration stallTimeout = DEFAULT_STALL_TIMEOUT;

        private Builder() {
        }

        /**
         * Set the pending-event limit.
         *
         * @param maxPendingEvents must be positive
         * @return this builder
         */
        public Builder maxPendingEvents(final int maxPendingEvents) {
            this.maxPendingEvents = maxPendingEvents;
            return this;
        }

        /**
         * Set the pending-byte limit.
         *
         * @param maxPendingBytes must be positive
         * @return this builder
         */
        public Builder maxPendingBytes(final int maxPendingBytes) {
            this.maxPendingBytes = maxPendingBytes;
            return this;
        }

        /**
         * Set the output-stall timeout.
         *
         * @param stallTimeout must be positive
         * @return this builder
         */
        public Builder stallTimeout(final Duration stallTimeout) {
            this.stallTimeout = stallTimeout;
            return this;
        }

        /**
         * Build the policy.
         *
         * @return the immutable policy
         * @throws IllegalArgumentException when a limit is not positive or the timeout is
         *                                  not positive
         */
        public SseSessionPolicy build() {
            Assert.isTrue(maxPendingEvents > 0, "maxPendingEvents must be positive");
            Assert.isTrue(maxPendingBytes > 0, "maxPendingBytes must be positive");
            Assert.isTrue(Objects.nonNull(stallTimeout) && !stallTimeout.isNegative() && !stallTimeout.isZero(),
                    "stallTimeout must be positive");
            return new SseSessionPolicy(this);
        }
    }
}
