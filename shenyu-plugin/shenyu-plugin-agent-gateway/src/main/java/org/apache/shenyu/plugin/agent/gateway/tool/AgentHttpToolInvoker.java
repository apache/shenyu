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

package org.apache.shenyu.plugin.agent.gateway.tool;

import com.google.gson.JsonObject;
import org.apache.shenyu.common.dto.AgentGatewayHttpToolConfig;
import reactor.core.publisher.Mono;

/**
 * Request-owned dispatch into configured gateway HTTP routes.
 */
@FunctionalInterface
public interface AgentHttpToolInvoker {

    /**
     * Execute a captured plan once, inheriting the subscriber's context and cancellation.
     * @param tool immutable configured target
     * @param invocation trusted arguments and deadline
     * @return private business JSON object
     */
    Mono<JsonObject> invoke(AgentGatewayHttpToolConfig.Tool tool, AgentToolInvocation invocation);
}
