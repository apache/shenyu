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

import java.util.Objects;
import java.util.Set;

/**
 * Verified identity and tool grants from a server-side security adapter, never from wire metadata.
 */
public final class AgentMcpIdentity {

    private final String subject;

    private final Set<String> toolGrants;

    public AgentMcpIdentity(final String subject, final Set<String> toolGrants) {
        if (Objects.requireNonNull(subject, "subject").isBlank()) {
            throw new IllegalArgumentException("subject must not be blank");
        }
        this.subject = subject;
        this.toolGrants = Set.copyOf(toolGrants);
    }

    public String getSubject() {
        return subject;
    }

    public Set<String> getToolGrants() {
        return toolGrants;
    }
}
