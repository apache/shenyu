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
import org.apache.shenyu.plugin.key.auth.KeyAuthAuthentication;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * Reuses key-auth's verified result, never reinterpreting wire credentials as identity.
 * A key-auth rule denotes a service identity; it does not identify individual users sharing a key.
 */
public final class KeyAuthMcpSecurityResolver implements AgentMcpSecurityResolver {

    private final Map<List<String>, AgentMcpIdentity> identities;

    public KeyAuthMcpSecurityResolver(final KeyAuthMcpSecurityProperties properties) {
        if (Objects.requireNonNull(properties.getBindings(), "bindings").size() > 1024) {
            throw new IllegalArgumentException("Too many key-auth authorization bindings");
        }
        Map<List<String>, AgentMcpIdentity> snapshot = new HashMap<>();
        for (KeyAuthMcpSecurityProperties.Binding binding : Objects.requireNonNull(properties.getBindings(), "bindings")) {
            List<String> reference = List.of(text(binding.getNamespaceId(), "namespaceId"), text(binding.getSelectorId(), "selectorId"), text(binding.getRuleId(), "ruleId"));
            AgentMcpIdentity identity = new AgentMcpIdentity(text(binding.getTenant(), "tenant"), text(binding.getSubject(), "subject"),
                    tools(binding.getToolGrants()), tools(binding.getToolDenials()));
            if (Objects.nonNull(snapshot.putIfAbsent(reference, identity))) {
                throw new IllegalArgumentException("Duplicate key-auth authorization binding");
            }
        }
        if (snapshot.isEmpty()) {
            throw new IllegalArgumentException("Key-auth authorization requires explicit bindings");
        }
        identities = Map.copyOf(snapshot);
    }

    @Override
    public Mono<AgentMcpIdentity> resolve(final ServerWebExchange exchange) {
        return Mono.defer(() -> exchange.getPrincipal().hasElement().flatMap(hasPrincipal -> {
            if (hasPrincipal) {
                return Mono.empty();
            }
            return KeyAuthAuthentication.from(exchange).flatMap(authentication -> {
                if (Objects.isNull(authentication.getNamespaceId()) || Objects.isNull(authentication.getSelectorId()) || Objects.isNull(authentication.getRuleId())) {
                    return Mono.empty();
                }
                AgentMcpIdentity identity = identities.get(List.of(authentication.getNamespaceId(), authentication.getSelectorId(), authentication.getRuleId()));
                return Mono.justOrEmpty(identity);
            });
        }));
    }

    private static String text(final String value, final String name) {
        if (Objects.isNull(value) || value.isBlank() || value.length() > 256 || !value.equals(value.strip()) || value.chars().anyMatch(Character::isISOControl)) {
            throw new IllegalArgumentException(name + " must be bounded nonblank text without surrounding whitespace or controls");
        }
        return value;
    }

    private static Set<String> tools(final Set<String> names) {
        if (Objects.requireNonNull(names, "tool names").size() > 1024) {
            throw new IllegalArgumentException("Too many tool permissions");
        }
        for (String name : Objects.requireNonNull(names, "tool names")) {
            if (!text(name, "tool name").matches("[A-Za-z0-9_.-]{1,128}")) {
                throw new IllegalArgumentException("Tool grants and denials require exact tool names");
            }
        }
        return Set.copyOf(names);
    }
}
