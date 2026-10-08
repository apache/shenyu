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

package org.apache.shenyu.plugin.agent.gateway;

/**
 * Constants used by the agent gateway plugin.
 */
public final class AgentGatewayConstants {

    /**
     * Exchange attribute containing the current request context.
     */
    public static final String REQUEST_CONTEXT_ATTRIBUTE = AgentGatewayConstants.class.getName() + ".REQUEST_CONTEXT";

    /**
     * Reactor context key containing the current request context.
     */
    public static final String REACTOR_CONTEXT_KEY = AgentGatewayConstants.class.getName() + ".REACTOR_CONTEXT";

    /**
     * Response header carrying the generated request id.
     */
    public static final String REQUEST_ID_HEADER = "X-Shenyu-Agent-Request-Id";

    /**
     * Stable error code used for invalid plugin configuration.
     */
    public static final String CONFIG_INVALID_CODE = "AGENT_GATEWAY_CONFIG_INVALID";

    private AgentGatewayConstants() {
    }
}
