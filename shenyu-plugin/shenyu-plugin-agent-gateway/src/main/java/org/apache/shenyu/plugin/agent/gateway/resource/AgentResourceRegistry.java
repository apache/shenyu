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

package org.apache.shenyu.plugin.agent.gateway.resource;

import com.google.gson.JsonArray;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolInvocation;
import reactor.core.publisher.Mono;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Supplier;
import java.util.stream.Collectors;

/**
 * Local resource registration and authorization. No generic URI fetching.
 */
public final class AgentResourceRegistry {

    private final Map<String, AgentResourceProvider> providers;

    private final Map<String, AgentResourceDefinition> definitions;

    public AgentResourceRegistry(final Collection<AgentResourceProvider> resources) {
        Map<String, AgentResourceProvider> registered = new LinkedHashMap<>();
        Map<String, AgentResourceDefinition> metadata = new LinkedHashMap<>();
        for (AgentResourceProvider provider : List.copyOf(resources)) {
            if (registered.size() >= 16) {
                throw new IllegalArgumentException("Local resource limit exceeded");
            }
            AgentResourceDefinition definition = Objects.requireNonNull(provider.getDefinition(), "definition");
            if (definition.getUri().startsWith("mcp-resource:")) {
                throw new IllegalArgumentException("Remote resource namespace is reserved");
            }
            if (Objects.nonNull(registered.putIfAbsent(definition.getName(), provider))) {
                throw new IllegalArgumentException("Resource names must be unique");
            }
            if (metadata.values().stream().anyMatch(existing -> existing.getUri().equals(definition.getUri()) && existing.isTemplate() == definition.isTemplate())) {
                throw new IllegalArgumentException("Duplicate resource definition");
            }
            metadata.put(definition.getName(), definition);
        }
        providers = Collections.unmodifiableMap(registered);
        definitions = Collections.unmodifiableMap(metadata);
    }

    public boolean isEmpty() {
        return definitions.isEmpty();
    }

    public Set<String> getNames() {
        return definitions.keySet();
    }

    public Map<String, AgentResourceDefinition> getDefinitions() {
        return definitions;
    }

    /**
     * List metadata in deterministic order using only trusted grants.
     * @param allowed effective resource grants
     * @param templates whether to list templates instead of direct resources
     * @return independent wire array
     */
    public JsonArray list(final Set<String> allowed, final boolean templates) {
        JsonArray result = new JsonArray();
        definitions.forEach((name, definition) -> {
            if (allowed.contains(name) && definition.isTemplate() == templates) {
                result.add(definition.toJson());
            }
        });
        return result;
    }

    /**
     * Read one authorized provider with no retries or shared results.
     * @param uri requested identifier
     * @param allowed effective resource grants
     * @param context trusted input factory
     * @return nonempty independent resource contents
     */
    public Mono<JsonArray> read(final String uri, final Set<String> allowed, final Supplier<AgentToolInvocation> context) {
        Set<String> permissions = Set.copyOf(allowed);
        return Mono.defer(() -> {
            AgentResourceContent.text(uri, null, "");
            List<AgentResourceDefinition> matches = definitions.values().stream()
                    .filter(definition -> permissions.contains(definition.getName()) && definition.matches(uri)).collect(Collectors.toList());
            if (matches.isEmpty()) {
                return Mono.error(new SecurityException("Resource is not available"));
            }
            if (matches.size() != 1) {
                return Mono.error(new IllegalStateException("Ambiguous resource registration"));
            }
            AgentResourceDefinition definition = matches.get(0);
            AgentResourceInvocation input = new AgentResourceInvocation(context.get(), uri, definition.variables(uri));
            return providers.get(definition.getName()).read(input)
                    .switchIfEmpty(Mono.error(new IllegalStateException("Resource completed without content")))
                    .map(content -> {
                        JsonArray result = new JsonArray();
                        for (AgentResourceContent item : List.copyOf(content)) {
                            if (result.size() >= 16 || !uri.equals(item.toJson().get("uri").getAsString())) {
                                throw new IllegalStateException("Resource content outside requested identity or count limit");
                            }
                            result.add(item.toJson());
                        }
                        if (result.size() == 0) {
                            throw new IllegalStateException("Resource provider returned empty content");
                        }
                        return result;
                    });
        });
    }
}
