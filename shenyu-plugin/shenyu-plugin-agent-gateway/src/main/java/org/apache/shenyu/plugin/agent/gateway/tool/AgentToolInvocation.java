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

import java.util.Objects;
import java.time.Instant;

/**
 * Request-local input for a trusted, explicitly registered tool.
 */
public final class AgentToolInvocation {

    private final String requestId;

    private final String subject;

    private final JsonObject arguments;

    private final JsonObject clientCapabilities;

    private final String ruleId;

    private final long configurationVersion;

    private final Instant deadline;

    public AgentToolInvocation(final String requestId, final String subject, final JsonObject arguments) {
        this(requestId, subject, arguments, "internal", 0, Instant.MAX);
    }

    public AgentToolInvocation(final String requestId, final String subject, final JsonObject arguments,
                               final String ruleId, final long configurationVersion, final Instant deadline) {
        this(requestId, subject, arguments, ruleId, configurationVersion, deadline, new JsonObject());
    }

    public AgentToolInvocation(final String requestId, final String subject, final JsonObject arguments,
                               final String ruleId, final long configurationVersion, final Instant deadline, final JsonObject clientCapabilities) {
        this.ruleId = Objects.requireNonNull(ruleId, "ruleId");
        if (ruleId.isBlank() || configurationVersion < 0) {
            throw new IllegalArgumentException("Invalid configuration snapshot");
        }
        this.configurationVersion = configurationVersion;
        this.deadline = Objects.requireNonNull(deadline, "deadline");
        this.requestId = Objects.requireNonNull(requestId, "requestId");
        this.subject = Objects.requireNonNull(subject, "subject");
        if (requestId.isBlank() || subject.isBlank()) {
            throw new IllegalArgumentException("requestId and subject must not be blank");
        }
        this.arguments = Objects.requireNonNull(arguments, "arguments").deepCopy();
        this.clientCapabilities = Objects.requireNonNull(clientCapabilities, "clientCapabilities").deepCopy();
    }

    public String getRequestId() {
        return requestId;
    }

    public String getSubject() {
        return subject;
    }

    public String getRuleId() {
        return ruleId;
    }

    public long getConfigurationVersion() {
        return configurationVersion;
    }

    public Instant getDeadline() {
        return deadline;
    }

    public JsonObject getArguments() {
        return arguments.deepCopy();
    }

    /**
     * Get this request's declared protocol capabilities, not trusted identity or grants.
     * @return an independent copy, never inherited from another request or session
     */
    public JsonObject getClientCapabilities() {
        return clientCapabilities.deepCopy();
    }
}
