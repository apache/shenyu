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
 * Optional application-owned sink for matched MCP request terminal events.
 * Implementations must be nonblocking and bound any queues and retention.
 * This covers authentication and parsing failures as well as validated calls;
 * do not count these events into the parsed tools/call metrics counter.
 */
@FunctionalInterface
public interface AgentMcpTerminalObserver {

    /**
     * Record once after execution, recovery and response writing terminate.
     * @param event immutable, body-free per-subscription snapshot
     */
    void record(AgentMcpTerminalEvent event);

    /**
     * Record an actual tool execution separately from the logical completion.
     * @param event trusted terminal snapshot
     */
    default void recordAttempt(final AgentMcpTerminalEvent event) {
    }
}
