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

package org.apache.shenyu.springboot.starter.plugin.agent.gateway;

import org.springframework.boot.context.properties.ConfigurationProperties;

import java.util.List;
import java.util.Set;

/**
 * Deployment-owned authorization bindings, containing rule references but never API keys.
 * Bindings are snapshotted at startup; editing this bean cannot change in-flight authorization.
 */
@ConfigurationProperties(prefix = "shenyu.plugins.agent.gateway.security.key-auth", ignoreUnknownFields = false)
public class KeyAuthMcpSecurityProperties {

    private boolean enabled;

    private List<Binding> bindings = List.of();

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(final boolean enabled) {
        this.enabled = enabled;
    }

    public List<Binding> getBindings() {
        return bindings;
    }

    public void setBindings(final List<Binding> bindings) {
        this.bindings = bindings;
    }

    /**
     * One exact authenticated rule mapped to one service identity and exact tool names.
     * Configuration namespace is part of the reference, never an implicit tenant.
     */
    public static class Binding {

        private String namespaceId;

        private String selectorId;

        private String ruleId;

        private String tenant;

        private String subject;

        private Set<String> toolGrants = Set.of();

        private Set<String> toolDenials = Set.of();

        public String getNamespaceId() {
            return namespaceId;
        }

        public void setNamespaceId(final String namespaceId) {
            this.namespaceId = namespaceId;
        }

        public String getSelectorId() {
            return selectorId;
        }

        public void setSelectorId(final String selectorId) {
            this.selectorId = selectorId;
        }

        public String getRuleId() {
            return ruleId;
        }

        public void setRuleId(final String ruleId) {
            this.ruleId = ruleId;
        }

        public String getTenant() {
            return tenant;
        }

        public void setTenant(final String tenant) {
            this.tenant = tenant;
        }

        public String getSubject() {
            return subject;
        }

        public void setSubject(final String subject) {
            this.subject = subject;
        }

        public Set<String> getToolGrants() {
            return toolGrants;
        }

        public void setToolGrants(final Set<String> toolGrants) {
            this.toolGrants = toolGrants;
        }

        public Set<String> getToolDenials() {
            return toolDenials;
        }

        public void setToolDenials(final Set<String> toolDenials) {
            this.toolDenials = toolDenials;
        }
    }
}
