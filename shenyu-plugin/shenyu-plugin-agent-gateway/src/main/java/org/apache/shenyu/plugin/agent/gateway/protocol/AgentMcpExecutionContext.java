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

import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
import java.time.Instant;

/**
 * Immutable authorization input supplied exclusively by a trusted server-side adapter.
 * Creating this object does not authenticate its caller.
 */
public final class AgentMcpExecutionContext {

    private final String requestId;

    private final String subject;

    private final String ruleId;

    private final long configurationVersion;

    private final Set<String> ruleTools;

    private final Set<String> allowedTools;

    private final Instant deadline;

    public AgentMcpExecutionContext(final String requestId, final String subject, final String ruleId, final long configurationVersion,
                                    final Set<String> ruleTools, final Set<String> securityGrants) {
        this(requestId, subject, ruleId, configurationVersion, ruleTools, securityGrants, Instant.MAX);
    }

    public AgentMcpExecutionContext(final String requestId, final String subject, final String ruleId, final long configurationVersion,
                                    final Set<String> ruleTools, final Set<String> securityGrants, final Instant deadline) {
        this.deadline = Objects.requireNonNull(deadline, "deadline");
        this.requestId = requireText(requestId, "requestId");
        this.subject = requireText(subject, "subject");
        this.ruleId = requireText(ruleId, "ruleId");
        if (configurationVersion < 0) {
            throw new IllegalArgumentException("configurationVersion must not be negative");
        }
        this.configurationVersion = configurationVersion;
        this.ruleTools = Set.copyOf(ruleTools);
        Set<String> intersection = new HashSet<>(this.ruleTools);
        intersection.retainAll(Set.copyOf(securityGrants));
        allowedTools = Set.copyOf(intersection);
    }

    public String getRequestId() {
        return requestId;
    }

    public Instant getDeadline() {
        return deadline;
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

    public Set<String> getRuleTools() {
        return ruleTools;
    }

    public Set<String> getAllowedTools() {
        return allowedTools;
    }

    private String requireText(final String value, final String name) {
        if (Objects.requireNonNull(value, name).isBlank()) {
            throw new IllegalArgumentException(name + " must not be blank");
        }
        return value;
    }
}
