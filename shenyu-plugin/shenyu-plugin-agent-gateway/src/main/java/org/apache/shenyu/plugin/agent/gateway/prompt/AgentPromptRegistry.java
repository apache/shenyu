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

package org.apache.shenyu.plugin.agent.gateway.prompt;

import com.fasterxml.jackson.databind.node.ObjectNode;
import reactor.core.publisher.Mono;

import org.apache.shenyu.common.utils.AgentPromptNames;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/** Immutable explicitly registered local prompt snapshot. */
public final class AgentPromptRegistry {

    private final Map<String, AgentPromptProvider> providers;

    private final Map<String, AgentPromptDefinition> definitions;

    public AgentPromptRegistry(final Collection<AgentPromptProvider> source) {
        Map<String, AgentPromptProvider> registered = new LinkedHashMap<>();
        Map<String, AgentPromptDefinition> metadata = new LinkedHashMap<>();
        if (source.size() > 16) {
            throw new IllegalArgumentException("Local prompt limit");
        }
        for (AgentPromptProvider provider : source) {
            AgentPromptDefinition definition = new AgentPromptDefinition(provider.definition().metadata());
            if (!AgentPromptNames.original(definition.name())) {
                throw new IllegalArgumentException("Local prompt name exceeds its original-name limit");
            }
            if (Objects.nonNull(registered.putIfAbsent(definition.name(), provider))) {
                throw new IllegalArgumentException("Duplicate local prompt");
            }
            metadata.put(definition.name(), definition);
        }
        providers = Map.copyOf(registered);
        definitions = Map.copyOf(metadata);
    }

    public Map<String, AgentPromptDefinition> definitions() {
        return definitions;
    }

    public Mono<ObjectNode> get(final String name, final Set<String> allowed, final AgentPromptInvocation invocation) {
        Set<String> grants = Set.copyOf(allowed);
        return Mono.defer(() -> {
            if (!grants.contains(name) || !providers.containsKey(name)) {
                return Mono.error(new SecurityException("Prompt is not available"));
            }
            definitions.get(name).validate(invocation.arguments());
            return Mono.defer(() -> providers.get(name).get(invocation))
                    .switchIfEmpty(Mono.error(new IllegalStateException("Empty prompt result")))
                    .map(AgentPromptDefinition::validateResult);
        });
    }
}
