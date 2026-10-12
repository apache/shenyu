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

package org.apache.shenyu.plugin.agent.gateway.remote;

import com.fasterxml.jackson.databind.node.ObjectNode;
import io.modelcontextprotocol.spec.McpSchema;
import reactor.core.publisher.Mono;

/** Exclusively owned remote session with request-scoped cancellation. */
public interface RemoteMcpEndpoint {
    /**
     * Negotiate the fixed outbound protocol.
     * @return initialization result
     */
    Mono<McpSchema.InitializeResult> initialize();

    /**
     * Fetch exactly one page without automatic pagination.
     * @param cursor opaque cursor or null
     * @return one page
     */
    Mono<McpSchema.ListToolsResult> listTools(String cursor);

    /**
     * Invoke and decode the SDK business result model.
     * @param request trusted invocation
     * @return typed result
     */
    Mono<McpSchema.CallToolResult> callTool(McpSchema.CallToolRequest request);

    /**
     * Invoke without dropping unknown result fields.
     * @param request trusted invocation
     * @return complete native result
     */
    Mono<ObjectNode> callRaw(McpSchema.CallToolRequest request);

    /**
     * Close the owned session, retaining cleanup failures.
     * @return cleanup completion
     */
    Mono<Void> closeGracefully();

    /**
     * Count current owned HTTP requests.
     * @return in-flight count
     */
    int pending();
}
