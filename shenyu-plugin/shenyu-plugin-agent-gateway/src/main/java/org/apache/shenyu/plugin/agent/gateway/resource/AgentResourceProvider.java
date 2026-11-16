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

package org.apache.shenyu.plugin.agent.gateway.resource;

import reactor.core.publisher.Mono;

import java.util.List;

/**
 * Explicit local resource provider. The gateway never dereferences resource URIs.
 */
public interface AgentResourceProvider {

    /**
     * Get immutable metadata.
     * @return registration definition
     */
    AgentResourceDefinition getDefinition();

    /**
     * Read lazily with trusted request-local identity and cancellation.
     * Implementations must enforce record-level authorization.
     * @param invocation trusted read input
     * @return one nonempty list of embedded resource content
     */
    Mono<List<AgentResourceContent>> read(AgentResourceInvocation invocation);
}
