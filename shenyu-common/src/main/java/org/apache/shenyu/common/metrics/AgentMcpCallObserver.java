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

package org.apache.shenyu.common.metrics;

/**
 * Server-side terminal observation for a parsed, authenticated MCP tools/call.
 * Implementations must be nonblocking and must not retain request data.
 */
@FunctionalInterface
public interface AgentMcpCallObserver {

    /**
     * Record one logical call after execution and response writing terminate.
     *
     * @param outcome bounded terminal classification
     * @param elapsedMillis monotonic elapsed time since the validated call was parsed
     */
    void record(Outcome outcome, long elapsedMillis);

    /**
     * Bounded labels; no tool names, principals or client-controlled identifiers.
     */
    enum Outcome {
        /** Successful tool result and completed response write. */
        SUCCESS,
        /** Completed MCP result with isError=true. */
        TOOL_ERROR,
        /** Protocol or authorization rejection after a valid call was parsed. */
        REJECTED,
        /** Internal, encoding or response-writing failure. */
        SERVER_ERROR,
        /** The gateway request deadline expired. */
        TIMEOUT,
        /** The downstream subscription was cancelled. */
        CANCELLED
    }
}
