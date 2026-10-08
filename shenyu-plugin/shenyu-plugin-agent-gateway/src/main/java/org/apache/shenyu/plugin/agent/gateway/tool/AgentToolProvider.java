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
import reactor.core.publisher.Mono;

/**
 * Internal contract for explicitly registered, single-result reactive tools.
 * Implementations must not retain request state or detach subscriptions.
 */
public interface AgentToolProvider {

    /**
     * Get the unique tool name.
     * @return tool name
     */
    String getName();

    /**
     * Get the human-readable local tool description.
     * @return description
     */
    String getDescription();

    /**
     * Get the local input schema, without remote references.
     * @return input schema
     */
    JsonObject getInputSchema();

    /**
     * Get required client capability objects and nested true feature markers.
     * Registration freezes this declaration; it is not an authorization grant.
     * Empty objects require presence, and no capabilities are required by default.
     * @return capability requirement tree, without request-local state
     */
    default JsonObject getRequiredClientCapabilities() {
        return new JsonObject();
    }

    /**
     * Validate arguments without executing business work.
     * @param arguments request-local arguments
     */
    void validate(JsonObject arguments);

    /**
     * Execute lazily, propagate cancellation, and produce exactly one result.
     * @param invocation trusted request-local input
     * @return single business result, without transport framing
     */
    Mono<JsonObject> invoke(AgentToolInvocation invocation);
}
