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

package org.apache.shenyu.plugin.agent.gateway.handle;

import org.apache.shenyu.common.dto.AgentGatewayMcpConfig;

import java.util.concurrent.atomic.AtomicLong;

/**
 * Immutable, validated rule handle for the agent gateway plugin.
 */
public final class AgentGatewayRuleHandle {

    private static final AtomicLong GENERATIONS = new AtomicLong();

    private final long generation;

    private final AgentGatewayMcpConfig mcp;

    private final String rawHandle;

    private final String trafficType;

    private final boolean responseRequestId;

    private final boolean valid;

    private final String errorMessage;

    private AgentGatewayRuleHandle(final String rawHandle, final String trafficType,
                                   final boolean responseRequestId, final boolean valid,
                                   final String errorMessage, final AgentGatewayMcpConfig mcp) {
        this.generation = GENERATIONS.incrementAndGet();
        this.mcp = mcp;
        this.rawHandle = rawHandle;
        this.trafficType = trafficType;
        this.responseRequestId = responseRequestId;
        this.valid = valid;
        this.errorMessage = errorMessage;
    }

    public static AgentGatewayRuleHandle valid(final String rawHandle, final String trafficType,
                                               final boolean responseRequestId) {
        return new AgentGatewayRuleHandle(rawHandle, trafficType, responseRequestId, true, null, null);
    }

    public static AgentGatewayRuleHandle mcp(final String rawHandle, final boolean responseRequestId, final AgentGatewayMcpConfig config) {
        return new AgentGatewayRuleHandle(rawHandle, "mcp", responseRequestId, true, null, config);
    }

    public long getGeneration() {
        return generation;
    }

    public AgentGatewayMcpConfig getMcp() {
        return mcp;
    }

    public static AgentGatewayRuleHandle invalid(final String rawHandle, final String errorMessage) {
        return new AgentGatewayRuleHandle(rawHandle, null, false, false, errorMessage, null);
    }

    public String getRawHandle() {
        return rawHandle;
    }

    public String getTrafficType() {
        return trafficType;
    }

    public boolean isResponseRequestId() {
        return responseRequestId;
    }

    public boolean isValid() {
        return valid;
    }

    public String getErrorMessage() {
        return errorMessage;
    }
}
