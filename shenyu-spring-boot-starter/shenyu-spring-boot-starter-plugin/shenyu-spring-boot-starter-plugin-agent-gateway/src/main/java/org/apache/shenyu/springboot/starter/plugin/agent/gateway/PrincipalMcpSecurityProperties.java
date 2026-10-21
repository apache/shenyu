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
 * Explicit application-authenticated principal policy; never a credential source.
 */
@ConfigurationProperties(prefix = "shenyu.plugins.agent.gateway.security.principal", ignoreUnknownFields = false)
public class PrincipalMcpSecurityProperties {

    private boolean enabled;

    private String source;

    private String issuer;

    private String audience;

    private String registrationId;

    private String defaultTenant;

    private List<ScopeMapping> scopeMappings = List.of();

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(final boolean enabled) {
        this.enabled = enabled;
    }

    public String getSource() {
        return source;
    }

    public void setSource(final String source) {
        this.source = source;
    }

    public String getIssuer() {
        return issuer;
    }

    public void setIssuer(final String issuer) {
        this.issuer = issuer;
    }

    public String getAudience() {
        return audience;
    }

    public void setAudience(final String audience) {
        this.audience = audience;
    }

    public String getRegistrationId() {
        return registrationId;
    }

    public void setRegistrationId(final String registrationId) {
        this.registrationId = registrationId;
    }

    public String getDefaultTenant() {
        return defaultTenant;
    }

    public void setDefaultTenant(final String defaultTenant) {
        this.defaultTenant = defaultTenant;
    }

    public List<ScopeMapping> getScopeMappings() {
        return scopeMappings;
    }

    public void setScopeMappings(final List<ScopeMapping> scopeMappings) {
        this.scopeMappings = scopeMappings;
    }

    /**
     * One exact scope maps to exact tools; names are never implicitly grants.
     */
    public static class ScopeMapping {

        private String scope;

        private Set<String> toolGrants = Set.of();

        private Set<String> toolDenials = Set.of();

        public String getScope() {
            return scope;
        }

        public void setScope(final String scope) {
            this.scope = scope;
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
