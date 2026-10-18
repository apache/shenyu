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

import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpIdentity;
import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpSecurityResolver;
import org.springframework.security.oauth2.server.resource.authentication.JwtAuthenticationToken;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * Maps only an application-authenticated JWT principal, never a raw token or header.
 * The application's Resource Server owns signature, algorithm and temporal validation.
 */
public final class JwtAgentMcpSecurityResolver implements AgentMcpSecurityResolver {

    private final String issuer;

    private final String audience;

    private final String defaultTenant;

    public JwtAgentMcpSecurityResolver(final String issuer, final String audience, final String defaultTenant) {
        this.issuer = text(issuer);
        this.audience = text(audience);
        this.defaultTenant = Objects.isNull(defaultTenant) ? null : text(defaultTenant);
    }

    @Override
    public Mono<AgentMcpIdentity> resolve(final ServerWebExchange exchange) {
        return exchange.getPrincipal().ofType(JwtAuthenticationToken.class)
                .filter(JwtAuthenticationToken::isAuthenticated)
                .flatMap(authentication -> {
                    try {
                        Map<String, Object> claims = authentication.getTokenAttributes();
                        if (!issuer.equals(claims.get("iss")) || !names(claims.get("aud"), false).contains(audience)) {
                            return Mono.empty();
                        }
                        String subject = text(claims.get("sub"));
                        if (!subject.equals(authentication.getName())) {
                            return Mono.empty();
                        }
                        String tenant = claims.containsKey("tenant_id") ? text(claims.get("tenant_id")) : text(defaultTenant);
                        return Mono.just(new AgentMcpIdentity(tenant, subject,
                                names(claims.getOrDefault("tool_grants", Set.of()), true), names(claims.getOrDefault("tool_denies", Set.of()), true)));
                    } catch (IllegalArgumentException error) {
                        // Reject malformed trusted claims without leaking the JWT or claim values.
                        return Mono.empty();
                    }
                });
    }

    private static String text(final Object value) {
        if (!(value instanceof String string) || string.isBlank() || string.length() > 256
                || !string.equals(string.trim()) || string.chars().anyMatch(Character::isISOControl)) {
            throw new IllegalArgumentException("Invalid identity claim or policy");
        }
        return string;
    }

    private static Set<String> names(final Object value, final boolean tools) {
        if (!(value instanceof Collection<?> values) || values.size() > 1024 || !tools && values.isEmpty()) {
            throw new IllegalArgumentException("Invalid permission or audience claim");
        }
        Set<String> result = new HashSet<>();
        for (Object item : values) {
            String name = text(item);
            if (tools && (name.length() > 128 || !name.matches("[a-zA-Z0-9_.-]+"))) {
                throw new IllegalArgumentException("Invalid tool permission");
            }
            result.add(name);
        }
        return Set.copyOf(result);
    }
}
