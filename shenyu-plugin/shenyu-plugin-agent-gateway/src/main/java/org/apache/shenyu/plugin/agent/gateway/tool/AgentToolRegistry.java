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
import reactor.core.publisher.Mono;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Supplier;

/**
 * Immutable tool registration snapshot. Authorization is supplied by the trusted caller.
 * This component neither authenticates HTTP requests nor exposes an endpoint.
 */
public final class AgentToolRegistry {

    private final Map<String, AgentToolProvider> providers;

    private final Map<String, AgentToolDefinition> definitions;

    public AgentToolRegistry(final Collection<AgentToolProvider> tools) {
        Map<String, AgentToolProvider> registered = new LinkedHashMap<>();
        Map<String, AgentToolDefinition> metadata = new LinkedHashMap<>();
        for (AgentToolProvider provider : Objects.requireNonNull(tools, "tools")) {
            Objects.requireNonNull(provider, "provider");
            String name = Objects.requireNonNull(provider.getName(), "tool name");
            if (name.isBlank() || registered.containsKey(name)) {
                throw new IllegalArgumentException("Tool names must be nonblank and unique");
            }
            registered.put(name, provider);
            metadata.put(name, new AgentToolDefinition(name, provider.getDescription(), provider.getInputSchema(), provider.getRequiredClientCapabilities()));
        }
        providers = Collections.unmodifiableMap(registered);
        definitions = Collections.unmodifiableMap(metadata);
    }

    /**
     * List only registered tools allowed by the trusted authorization snapshot.
     * @param allowedTools effective permission set, not client-supplied metadata
     * @return independent schema copies in registration order
     */
    public Map<String, JsonObject> list(final Set<String> allowedTools) {
        Set<String> permissions = Set.copyOf(allowedTools);
        Map<String, JsonObject> visible = new LinkedHashMap<>();
        definitions.forEach((name, definition) -> {
            if (permissions.contains(name)) {
                visible.put(name, definition.getInputSchema());
            }
        });
        return Collections.unmodifiableMap(visible);
    }

    /**
     * List frozen definitions with the same permission predicate used by invoke.
     * @param allowedTools trusted permission snapshot
     * @return immutable visible definitions in registration order
     */
    public Map<String, AgentToolDefinition> listDefinitions(final Set<String> allowedTools) {
        Set<String> permissions = Set.copyOf(allowedTools);
        Map<String, AgentToolDefinition> visible = new LinkedHashMap<>();
        definitions.forEach((name, definition) -> {
            if (permissions.contains(name)) {
                visible.put(name, definition);
            }
        });
        return Collections.unmodifiableMap(visible);
    }

    /**
     * Check startup-local namespace ownership without invoking a provider.
     * @param name candidate exposed name
     * @return whether a local provider owns this name
     */
    public boolean isRegistered(final String name) {
        return providers.containsKey(name);
    }

    /**
     * Validate remote metadata with the same bounded local-schema contract.
     * @param name exposed tool name
     * @param description human-readable description
     * @param schema input schema, not executed here
     */
    public static void validateDefinition(final String name, final String description, final JsonObject schema) {
        new AgentToolDefinition(name, description, schema);
    }

    /**
     * Invoke an authorized tool once per subscription without retries or shared results.
     * The input factory must obtain identity from a trusted source and create a fresh request id.
     * @param name tool name
     * @param allowedTools effective permission snapshot
     * @param input request-local input factory
     * @return independent result with cancellation linked to the provider
     */
    public Mono<JsonObject> invoke(final String name, final Set<String> allowedTools,
                                  final Supplier<AgentToolInvocation> input) {
        Set<String> permissions = Set.copyOf(allowedTools);
        Objects.requireNonNull(input, "input");
        return Mono.defer(() -> {
            AgentToolProvider provider = providers.get(name);
            if (!permissions.contains(name) || Objects.isNull(provider)) {
                return Mono.error(new SecurityException("Tool is not available"));
            }
            AgentToolInvocation invocation = Objects.requireNonNull(input.get(), "invocation");
            JsonObject missing = definitions.get(name).missingCapabilities(invocation.getClientCapabilities());
            if (missing.size() > 0) {
                return Mono.error(new AgentToolCapabilityException(missing));
            }
            try {
                provider.validate(invocation.getArguments());
            } catch (IllegalArgumentException error) {
                throw new AgentToolArgumentException(error);
            }
            return provider.invoke(invocation)
                    .switchIfEmpty(Mono.error(new IllegalStateException("Tool completed without a result")))
                    .map(JsonObject::deepCopy);
        });
    }
}
