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
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.shenyu.plugin.agent.gateway.resource.AgentResourceContent;
import org.apache.shenyu.plugin.agent.gateway.resource.AgentResourceDefinition;
import org.apache.shenyu.plugin.agent.gateway.resource.AgentResourceNamespace;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;

/** Immutable, bounded resource directory owned by the same generation as tools. */
public final class RemoteResourceDirectory {

    private static final ObjectMapper JSON = new ObjectMapper();

    private final Map<String, Entry> entries;

    private RemoteResourceDirectory(final Map<String, Entry> entries) {
        this.entries = Map.copyOf(entries);
    }

    /**
     * Create an explicitly empty resource snapshot.
     * @return empty directory
     */
    public static RemoteResourceDirectory empty() {
        return new RemoteResourceDirectory(Map.of());
    }

    /**
     * Discover all pages before publishing any resource routes.
     * @param targets only servers advertising resources
     * @return complete immutable directory
     */
    public static Mono<RemoteResourceDirectory> discover(final List<Target> targets) {
        List<Target> snapshot = List.copyOf(targets);
        return Mono.defer(() -> Flux.fromIterable(snapshot)
                .concatMap(target -> pages(target, false, null, 0, new HashSet<>(), new ArrayList<>())
                        .flatMapMany(Flux::fromIterable)
                        .concatWith(pages(target, true, null, 0, new HashSet<>(), new ArrayList<>()).flatMapMany(Flux::fromIterable)))
                .collectList().map(RemoteResourceDirectory::build));
    }

    private static Mono<List<Entry>> pages(final Target target, final boolean templates, final String cursor, final int page,
                                          final Set<String> seen, final List<Entry> values) {
        if (page >= 4 || Objects.nonNull(cursor) && !seen.add(cursor)) {
            return Mono.error(new IllegalArgumentException("Resource pagination limit or repeated cursor"));
        }
        return Mono.defer(() -> (templates ? target.templates() : target.resources()).apply(cursor))
                .switchIfEmpty(Mono.error(new IllegalStateException("Empty resource page")))
                .flatMap(reply -> {
                    String field = templates ? "resourceTemplates" : "resources";
                    if (!reply.path(field).isArray()) {
                        return Mono.error(new IllegalArgumentException("Invalid resource page"));
                    }
                    for (JsonNode node : reply.get(field)) {
                        values.add(entry(target, node, templates));
                        if (values.size() > 16) {
                            return Mono.error(new IllegalArgumentException("Resource directory limit"));
                        }
                    }
                    JsonNode next = reply.get("nextCursor");
                    if (Objects.isNull(next)) {
                        return Mono.just(List.copyOf(values));
                    }
                    if (!next.isTextual() || next.textValue().isEmpty() || next.textValue().length() > 4096) {
                        return Mono.error(new IllegalArgumentException("Invalid resource cursor"));
                    }
                    return pages(target, templates, next.textValue(), page + 1, seen, values);
                });
    }

    private static Entry entry(final Target target, final JsonNode node, final boolean template) {
        String field = template ? "uriTemplate" : "uri";
        if (!node.isObject() || !node.path("name").isTextual() || node.path("name").asText().isBlank() || !node.path(field).isTextual()
                || node.has(template ? "uri" : "uriTemplate")
                || node.has("description") && !node.get("description").isTextual() || node.has("mimeType") && !node.get("mimeType").isTextual()) {
            throw new IllegalArgumentException("Invalid resource metadata");
        }
        String name = target.name() + "." + node.get("name").textValue();
        String original = node.get(field).textValue();
        String description = node.path("description").asText("");
        String mimeType = node.has("mimeType") ? node.get("mimeType").textValue() : null;
        new AgentResourceDefinition(name, original, description, mimeType, template);
        String exposed = AgentResourceNamespace.expose(target.name(), original, template);
        AgentResourceDefinition definition = new AgentResourceDefinition(name, exposed, description, mimeType, template);
        ObjectNode metadata = ((ObjectNode) node).deepCopy().put("name", name).put(field, exposed);
        return new Entry(target, definition, metadata);
    }

    private static RemoteResourceDirectory build(final List<Entry> source) {
        if (source.size() > 16) {
            throw new IllegalArgumentException("Combined remote resource limit");
        }
        Map<String, Entry> entries = new LinkedHashMap<>();
        Set<String> identifiers = new HashSet<>();
        for (Entry entry : source) {
            if (Objects.nonNull(entries.putIfAbsent(entry.definition().getName(), entry))
                    || !identifiers.add(entry.definition().getUri())) {
                throw new IllegalArgumentException("Resource name or URI collision");
            }
        }
        return new RemoteResourceDirectory(entries);
    }

    /**
     * Return request-owned metadata, never callbacks or credentials.
     * @return independent metadata objects keyed by exposed permission name
     */
    public Map<String, ObjectNode> definitions() {
        Map<String, ObjectNode> result = new LinkedHashMap<>();
        entries.forEach((name, entry) -> result.put(name, entry.metadata().deepCopy()));
        return Map.copyOf(result);
    }

    /**
     * Read exactly one authorized registration, without generic URI fetching.
     * @param uri canonical exposed identifier
     * @param allowed effective resource grants
     * @return validated contents with exposed identifiers
     */
    public Mono<ObjectNode> read(final String uri, final Set<String> allowed) {
        Set<String> grants = Set.copyOf(allowed);
        return Mono.defer(() -> {
            List<Entry> matches = entries.values().stream()
                    .filter(entry -> grants.contains(entry.definition().getName()) && entry.definition().matches(uri)).toList();
            if (matches.isEmpty()) {
                return Mono.error(new SecurityException("Resource is not available"));
            }
            if (matches.size() != 1) {
                return Mono.error(new IllegalStateException("Ambiguous resource route"));
            }
            Entry entry = matches.get(0);
            String original = AgentResourceNamespace.original(entry.target().name(), uri);
            return Mono.defer(() -> entry.target().reads().apply(original))
                    .switchIfEmpty(Mono.error(new IllegalStateException("Missing resource result")))
                    .map(reply -> rewrite(reply, original, uri));
        });
    }

    private static ObjectNode rewrite(final ObjectNode source, final String original, final String exposed) {
        if (!source.path("contents").isArray() || source.get("contents").isEmpty() || source.get("contents").size() > 16) {
            throw new IllegalArgumentException("Invalid resource contents");
        }
        ObjectNode result = source.deepCopy();
        ArrayNode contents = result.withArray("contents");
        for (JsonNode item : contents) {
            if (!item.isObject() || !original.equals(item.path("uri").asText()) || item.has("text") == item.has("blob")
                    || item.has("mimeType") && !item.get("mimeType").isTextual()) {
                throw new IllegalArgumentException("Resource content identity or shape mismatch");
            }
            String field = item.has("text") ? "text" : "blob";
            if (!item.get(field).isTextual()) {
                throw new IllegalArgumentException("Invalid resource body");
            }
            String mime = item.has("mimeType") ? item.get("mimeType").textValue() : null;
            if ("text".equals(field)) {
                AgentResourceContent.text(original, mime, item.get(field).textValue());
            } else {
                AgentResourceContent.blob(original, mime, item.get(field).textValue());
            }
            ((ObjectNode) item).put("uri", exposed);
        }
        return result;
    }

    private record Entry(Target target, AgentResourceDefinition definition, ObjectNode metadata) {
    }

    /** Fixed server callbacks; reads do not accept caller-selected endpoints. */
    public record Target(String name, Function<String, Mono<ObjectNode>> resources, Function<String, Mono<ObjectNode>> templates,
                         Function<String, Mono<ObjectNode>> reads) {
        public Target {
            if (Objects.isNull(name) || !name.matches("[A-Za-z0-9_-]{1,48}")) {
                throw new IllegalArgumentException("Invalid resource server");
            }
            Objects.requireNonNull(resources, "resources");
            Objects.requireNonNull(templates, "templates");
            Objects.requireNonNull(reads, "reads");
        }
    }
}
