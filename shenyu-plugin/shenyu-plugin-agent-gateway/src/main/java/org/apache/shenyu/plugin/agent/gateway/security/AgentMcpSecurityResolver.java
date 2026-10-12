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

package org.apache.shenyu.plugin.agent.gateway.security;

import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

/**
 * Deployment adapter for an existing, verified authentication and authorization source.
 * Implementations must not trust client headers, clientInfo or session identifiers as identity.
 */
@FunctionalInterface
public interface AgentMcpSecurityResolver {

    /**
     * Resolve trusted subject and grants before the MCP body is consumed.
     *
     * @param exchange current exchange
     * @return one immutable identity, or empty when not authenticated
     */
    Mono<AgentMcpIdentity> resolve(ServerWebExchange exchange);
}
