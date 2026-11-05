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

package org.apache.shenyu.plugin.agent.gateway.governance;

import java.time.Duration;
import java.util.Objects;
import java.util.UUID;

/** Immutable deployment-owned MCP quota policy. */
public final class AgentMcpQuotaConfig {

    private final Mode mode;

    private final long invocationLimit;

    private final Duration invocationWindow;

    private final int concurrencyLimit;

    private final Duration leaseTtl;

    private final boolean failOpen;

    private AgentMcpQuotaConfig(final Mode mode, final long invocationLimit, final Duration invocationWindow,
                                final int concurrencyLimit, final Duration leaseTtl, final boolean failOpen) {
        this.mode = Objects.requireNonNull(mode, "mode");
        if (invocationLimit < 1 || invocationLimit > 1_000_000_000L) {
            throw new IllegalArgumentException("invocationLimit must be between 1 and 1000000000");
        }
        this.invocationLimit = invocationLimit;
        this.invocationWindow = requireBounded(invocationWindow, Duration.ofSeconds(1), Duration.ofDays(1), "invocationWindow");
        if (concurrencyLimit < 1 || concurrencyLimit > 100_000) {
            throw new IllegalArgumentException("concurrencyLimit must be between 1 and 100000");
        }
        this.concurrencyLimit = concurrencyLimit;
        this.leaseTtl = requireBounded(leaseTtl, Duration.ofSeconds(1), Duration.ofMinutes(10), "leaseTtl");
        this.failOpen = failOpen;
    }

    public static AgentMcpQuotaConfig of(final Mode mode, final long invocationLimit, final Duration invocationWindow,
                                         final int concurrencyLimit, final Duration leaseTtl, final boolean failOpen) {
        return new AgentMcpQuotaConfig(mode, invocationLimit, invocationWindow, concurrencyLimit, leaseTtl, failOpen);
    }

    public Mode getMode() {
        return mode;
    }

    public long getInvocationLimit() {
        return invocationLimit;
    }

    public Duration getInvocationWindow() {
        return invocationWindow;
    }

    public int getConcurrencyLimit() {
        return concurrencyLimit;
    }

    public Duration getLeaseTtl() {
        return leaseTtl;
    }

    public boolean isFailOpen() {
        return failOpen;
    }

    public String newGeneration() {
        return UUID.randomUUID().toString();
    }

    private static Duration requireBounded(final Duration value, final Duration minimum, final Duration maximum, final String name) {
        if (Objects.isNull(value) || value.compareTo(minimum) < 0 || value.compareTo(maximum) > 0) {
            throw new IllegalArgumentException(name + " is outside its allowed range");
        }
        return value;
    }

    public enum Mode {
        SHADOW,
        ENFORCE
    }
}
