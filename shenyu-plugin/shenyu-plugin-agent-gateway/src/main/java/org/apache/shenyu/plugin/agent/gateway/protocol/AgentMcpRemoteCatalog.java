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

package org.apache.shenyu.plugin.agent.gateway.protocol;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.google.gson.JsonObject;
import reactor.core.publisher.Mono;

import java.util.Map;
import java.util.function.Function;

/**
 * Candidate-only remote catalog port. The implementation owns bounded discovery and generation leases.
 */
public interface AgentMcpRemoteCatalog {

    /**
     * Execute once with a complete immutable catalog generation, releasing on every terminal signal.
     * @param operation request operation, without internal subscriptions or retries
     * @return the single native protocol result
     */
    Mono<ObjectNode> withSnapshot(Function<Snapshot, Mono<ObjectNode>> operation);

    /**
     * Request-owned view of one pinned generation.
     */
    interface Snapshot {

        /**
         * Return independent remote tool definitions using the exposed names.
         * @return metadata copies; no credentials
         */
        Map<String, ObjectNode> definitions();

        /**
         * Invoke a permitted exposed name and preserve its native remote result.
         * @param name exposed tool name
         * @param arguments request-private arguments
         * @param context trusted context, not inbound metadata
         * @return native result, preserving business errors
         */
        Mono<ObjectNode> invoke(String name, JsonObject arguments, AgentMcpExecutionContext context);
    }
}
