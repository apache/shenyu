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

package org.apache.shenyu.springboot.starter.plugin.mcp.server;

import org.apache.shenyu.plugin.mcp.server.transport.McpSessionPolicy;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.bind.DefaultValue;

import java.time.Duration;

/**
 * Startup-owned limits for each legacy MCP transport instance.
 *
 * @param maxSessions maximum sessions, zero disables the limit
 * @param idleTimeout idle expiry, zero disables it
 * @param sweepInterval expiry scan interval
 */
@ConfigurationProperties(prefix = "shenyu.plugins.mcp.server.session", ignoreUnknownFields = false)
public record McpSessionProperties(@DefaultValue("0") int maxSessions,
                                   @DefaultValue("0s") Duration idleTimeout,
                                   @DefaultValue("1s") Duration sweepInterval) {

    /**
     * Validate even if no transport has yet been created.
     */
    public McpSessionProperties {
        new McpSessionPolicy(maxSessions, idleTimeout, sweepInterval);
    }

    /**
     * Freeze the validated startup policy for transport creation.
     *
     * @return immutable policy
     */
    public McpSessionPolicy policy() {
        return new McpSessionPolicy(maxSessions, idleTimeout, sweepInterval);
    }
}
