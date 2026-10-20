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
import org.springframework.security.core.Authentication;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;

/**
 * Maps one configured, application-authenticated principal type into immutable tool permissions.
 * Authentication, token validation and the filter chain remain application-owned.
 * @param <T> the sole accepted authentication type
 */
public final class PrincipalMcpSecurityResolver<T extends Authentication> implements AgentMcpSecurityResolver {

    private final Class<T> type;

    private final Function<T, Map<String, Object>> attributes;

    private final Predicate<T> authority;

    private final Function<ServerWebExchange, Mono<Boolean>> conflict;

    private final String defaultTenant;

    private final Map<String, Set<String>> grants;

    private final Map<String, Set<String>> denials;

    public PrincipalMcpSecurityResolver(final PrincipalMcpSecurityProperties properties, final Class<T> type,
                                        final Function<T, Map<String, Object>> attributes, final Predicate<T> authority,
                                        final Function<ServerWebExchange, Mono<Boolean>> conflict) {
        this.type = Objects.requireNonNull(type);
        this.attributes = Objects.requireNonNull(attributes);
        this.authority = Objects.requireNonNull(authority);
        this.conflict = Objects.requireNonNull(conflict);
        defaultTenant = Objects.isNull(properties.getDefaultTenant()) ? null : text(properties.getDefaultTenant());
        Map<String, Set<String>> allowed = new HashMap<>();
        Map<String, Set<String>> denied = new HashMap<>();
        if (properties.getScopeMappings().size() > 1024) {
            throw new IllegalArgumentException("Too many scope mappings");
        }
        for (PrincipalMcpSecurityProperties.ScopeMapping mapping : properties.getScopeMappings()) {
            String scope = scope(mapping.getScope());
            if (Objects.nonNull(allowed.putIfAbsent(scope, names(mapping.getToolGrants(), true)))) {
                throw new IllegalArgumentException("Duplicate scope mapping");
            }
            denied.put(scope, names(mapping.getToolDenials(), true));
        }
        grants = Map.copyOf(allowed);
        denials = Map.copyOf(denied);
    }

    @Override
    public Mono<AgentMcpIdentity> resolve(final ServerWebExchange exchange) {
        return Mono.defer(() -> conflict.apply(exchange).flatMap(ambiguous -> ambiguous ? Mono.empty()
                : exchange.getPrincipal().ofType(type).filter(Authentication::isAuthenticated).flatMap(authentication -> {
                    try {
                        if (!authority.test(authentication)) {
                            return Mono.empty();
                        }
                        Map<String, Object> claims = attributes.apply(authentication);
                        String subject = text(claims.get("sub"));
                        if (!subject.equals(authentication.getName())) {
                            return Mono.empty();
                        }
                        String tenant = text(claims.containsKey("tenant_id") ? claims.get("tenant_id") : defaultTenant);
                        Set<String> allowed = new HashSet<>(names(claims.getOrDefault("tool_grants", Set.of()), true));
                        Set<String> denied = new HashSet<>(names(claims.getOrDefault("tool_denies", Set.of()), true));
                        for (String scope : scopes(claims)) {
                            allowed.addAll(grants.getOrDefault(scope, Set.of()));
                            denied.addAll(denials.getOrDefault(scope, Set.of()));
                        }
                        return Mono.just(new AgentMcpIdentity(tenant, subject, allowed, denied));
                    } catch (IllegalArgumentException error) {
                        // Invalid trusted attributes fail closed; do not log claims or credentials.
                        return Mono.empty();
                    }
                })));
    }

    static String text(final Object value) {
        if (!(value instanceof String string) || string.isBlank() || string.length() > 256
                || !string.equals(string.strip()) || string.chars().anyMatch(Character::isISOControl)) {
            throw new IllegalArgumentException("Invalid identity attribute or policy");
        }
        return string;
    }

    static Set<String> names(final Object value, final boolean tools) {
        if (!(value instanceof Collection<?> values) || values.size() > 1024) {
            throw new IllegalArgumentException("Invalid permission or audience attributes");
        }
        Set<String> result = new HashSet<>();
        for (Object item : values) {
            String name = text(item);
            if (tools && !name.matches("[A-Za-z0-9_.-]{1,128}")) {
                throw new IllegalArgumentException("Tool permissions must be exact names");
            }
            result.add(name);
        }
        return Set.copyOf(result);
    }

    private static Set<String> scopes(final Map<String, Object> claims) {
        Set<String> scope = claims.containsKey("scope") ? scopeNames(claims.get("scope")) : Set.of();
        Set<String> scp = claims.containsKey("scp") ? scopeNames(claims.get("scp")) : Set.of();
        if (claims.containsKey("scope") && claims.containsKey("scp") && !scope.equals(scp)) {
            throw new IllegalArgumentException("Conflicting scope attributes");
        }
        return claims.containsKey("scope") ? scope : scp;
    }

    private static Set<String> scopeNames(final Object value) {
        Object values = value;
        if (value instanceof String string) {
            if (string.length() > 8192 || !string.equals(string.strip())) {
                throw new IllegalArgumentException("Invalid scope string");
            }
            values = string.isEmpty() ? Set.of() : Arrays.asList(string.split(" ", -1));
        }
        Set<String> result = names(values, false);
        result.forEach(PrincipalMcpSecurityResolver::scope);
        return result;
    }

    private static String scope(final String value) {
        String result = text(value);
        if (result.chars().anyMatch(character -> character < 33 || character > 126 || character == 34 || character == 92)) {
            throw new IllegalArgumentException("Invalid scope name");
        }
        return result;
    }
}
