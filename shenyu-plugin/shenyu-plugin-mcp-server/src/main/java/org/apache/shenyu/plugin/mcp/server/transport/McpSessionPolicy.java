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

import java.time.Duration;
import java.util.Objects;

/**
 * Startup-owned per-transport session limits. Zero preserves legacy behavior.
 *
 * @param maxSessions maximum live sessions, or zero for unlimited
 * @param idleTimeout idle age, or zero to disable expiry
 * @param sweepInterval positive expiry scan interval
 */
public record McpSessionPolicy(int maxSessions, Duration idleTimeout, Duration sweepInterval) {

    /**
     * Validate limits and reject durations outside monotonic clock arithmetic.
     */
    public McpSessionPolicy {
        Objects.requireNonNull(idleTimeout, "idleTimeout");
        Objects.requireNonNull(sweepInterval, "sweepInterval");
        if (maxSessions < 0 || idleTimeout.isNegative() || sweepInterval.isNegative() || sweepInterval.isZero()) {
            throw new IllegalArgumentException("Invalid MCP session limits");
        }
        if (!idleTimeout.isZero() && sweepInterval.compareTo(idleTimeout) > 0) {
            throw new IllegalArgumentException("Session sweep interval exceeds idle timeout");
        }
        idleTimeout.toNanos();
        sweepInterval.toNanos();
    }

    /**
     * Preserve existing constructors and deployments until limits are configured.
     *
     * @return disabled capacity and idle limits
     */
    public static McpSessionPolicy unlimited() {
        return new McpSessionPolicy(0, Duration.ZERO, Duration.ofSeconds(1));
    }
}
