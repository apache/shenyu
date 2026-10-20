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
import java.util.HashSet;
import java.util.Optional;

/**
 * Verified identity and tool grants from a server-side security adapter, never from wire metadata.
 */
public final class AgentMcpIdentity {

    private final String subject;

    private final Set<String> toolGrants;

    private final Set<String> resourceGrants;

    private final Set<String> toolDenials;

    private final String tenant;

    public AgentMcpIdentity(final String subject, final Set<String> toolGrants) {
        this(subject, toolGrants, Set.of(), Set.of(), null);
    }

    public AgentMcpIdentity(final String subject, final Set<String> toolGrants, final Set<String> resourceGrants) {
        this(subject, toolGrants, Set.of(), resourceGrants, null);
    }

    /**
     * Snapshot a tenant-bound identity supplied by a verified server adapter.
     * @param tenant explicitly resolved tenant, never an anonymous default
     * @param subject verified subject within that tenant
     * @param toolGrants exact tool names granted by the authority
     * @param toolDenials exact names denied, taking precedence over grants
     */
    public AgentMcpIdentity(final String tenant, final String subject, final Set<String> toolGrants, final Set<String> toolDenials) {
        this(subject, toolGrants, toolDenials, Set.of(), Objects.requireNonNull(tenant, "tenant"));
    }

    public AgentMcpIdentity(final String tenant, final String subject, final Set<String> toolGrants, final Set<String> toolDenials, final Set<String> resourceGrants) {
        this(subject, toolGrants, toolDenials, resourceGrants, Objects.requireNonNull(tenant, "tenant"));
    }

    private AgentMcpIdentity(final String subject, final Set<String> toolGrants, final Set<String> toolDenials, final Set<String> resourceGrants, final String tenant) {
        if (Objects.requireNonNull(subject, "subject").isBlank()) {
            throw new IllegalArgumentException("subject must not be blank");
        }
        this.subject = subject;
        this.resourceGrants = Set.copyOf(resourceGrants);
        if (Objects.nonNull(tenant) && tenant.isBlank()) {
            throw new IllegalArgumentException("tenant must not be blank");
        }
        this.tenant = tenant;
        this.toolDenials = Set.copyOf(toolDenials);
        Set<String> effective = new HashSet<>(Set.copyOf(toolGrants));
        effective.removeAll(this.toolDenials);
        this.toolGrants = Set.copyOf(effective);
    }

    public String getSubject() {
        return subject;
    }

    public Set<String> getToolGrants() {
        return toolGrants;
    }

    public Set<String> getResourceGrants() {
        return resourceGrants;
    }

    public Set<String> getToolDenials() {
        return toolDenials;
    }

    /**
     * Return an explicitly assigned tenant; legacy adapters remain unassigned.
     * @return tenant, never a fabricated default
     */
    public Optional<String> getTenant() {
        return Optional.ofNullable(tenant);
    }
}
