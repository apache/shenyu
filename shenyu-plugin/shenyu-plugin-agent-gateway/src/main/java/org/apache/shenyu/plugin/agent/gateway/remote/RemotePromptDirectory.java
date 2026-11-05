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

package org.apache.shenyu.plugin.agent.gateway.remote;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.shenyu.plugin.agent.gateway.prompt.AgentPromptDefinition;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Function;

/** Complete prompt discovery owned by the existing leased catalog generation. */
public final class RemotePromptDirectory {

    private final Map<String, Entry> entries;

    private RemotePromptDirectory(final Map<String, Entry> entries) {
        this.entries = Map.copyOf(entries);
    }

    public static RemotePromptDirectory empty() {
        return new RemotePromptDirectory(Map.of());
    }

    public static Mono<RemotePromptDirectory> discover(final List<Target> targets) {
        List<Target> captured = List.copyOf(targets);
        if (captured.size() > 8 || captured.stream().map(Target::name).distinct().count() != captured.size()) {
            return Mono.error(new IllegalArgumentException("Invalid prompt target count"));
        }
        return Mono.defer(() -> Flux.fromIterable(captured)
                .concatMap(target -> pages(target, null, new HashSet<>(), new ArrayList<>()))
                .collectList().map(pages -> {
                    Map<String, Entry> result = new LinkedHashMap<>();
                    for (List<Entry> page : pages) {
                        for (Entry entry : page) {
                            if (Objects.nonNull(result.putIfAbsent(entry.definition().name(), entry)) || result.size() > 16) {
                                throw new IllegalArgumentException("Prompt namespace collision or limit");
                            }
                        }
                    }
                    return new RemotePromptDirectory(result);
                }));
    }

    private static Mono<List<Entry>> pages(final Target target, final String cursor, final Set<String> seen, final List<Entry> entries) {
        if (seen.size() >= 4 || !seen.add(Objects.isNull(cursor) ? "" : cursor)) {
            return Mono.error(new IllegalArgumentException("Prompt pagination limit or repeated cursor"));
        }
        return Mono.defer(() -> target.pages().apply(cursor))
                .switchIfEmpty(Mono.error(new IllegalStateException("Empty prompt page")))
                .flatMap(page -> {
                    if (!page.path("prompts").isArray()) {
                        return Mono.error(new IllegalArgumentException("Invalid prompt page"));
                    }
                    for (JsonNode node : page.get("prompts")) {
                        if (!node.isObject()) {
                            throw new IllegalArgumentException("Invalid prompt metadata");
                        }
                        AgentPromptDefinition original = new AgentPromptDefinition((ObjectNode) node);
                        ObjectNode metadata = original.metadata().put("name", target.name() + "." + original.name());
                        entries.add(new Entry(target, original.name(), new AgentPromptDefinition(metadata)));
                        if (entries.size() > 16) {
                            throw new IllegalArgumentException("Prompt discovery limit");
                        }
                    }
                    JsonNode next = page.get("nextCursor");
                    if (Objects.isNull(next)) {
                        return Mono.just(List.copyOf(entries));
                    }
                    if (!next.isTextual() || next.textValue().isEmpty() || next.textValue().length() > 4096) {
                        throw new IllegalArgumentException("Invalid prompt cursor");
                    }
                    return pages(target, next.textValue(), seen, entries);
                });
    }

    public Map<String, ObjectNode> definitions() {
        Map<String, ObjectNode> result = new LinkedHashMap<>();
        entries.forEach((name, entry) -> result.put(name, entry.definition().metadata()));
        return Map.copyOf(result);
    }

    public Mono<ObjectNode> get(final String name, final Set<String> grants, final Map<String, String> arguments) {
        Set<String> allowed = Set.copyOf(grants);
        Map<String, String> captured = Map.copyOf(arguments);
        return Mono.defer(() -> {
            Entry entry = entries.get(name);
            if (!allowed.contains(name) || Objects.isNull(entry)) {
                return Mono.error(new SecurityException("Prompt is not available"));
            }
            entry.definition().validate(captured);
            return Mono.defer(() -> entry.target().gets().apply(entry.originalName(), captured))
                    .switchIfEmpty(Mono.error(new IllegalStateException("Empty prompt result")))
                    .map(AgentPromptDefinition::validateResult);
        });
    }

    private record Entry(Target target, String originalName, AgentPromptDefinition definition) {
    }

    /** Fixed server-bound callbacks; prompt names never select an endpoint. */
    public record Target(String name, Function<String, Mono<ObjectNode>> pages, BiFunction<String, Map<String, String>, Mono<ObjectNode>> gets) {
        public Target {
            if (Objects.isNull(name) || !name.matches("[A-Za-z0-9_-]{1,48}")) {
                throw new IllegalArgumentException("Invalid prompt server");
            }
            Objects.requireNonNull(pages, "pages");
            Objects.requireNonNull(gets, "gets");
        }
    }
}
