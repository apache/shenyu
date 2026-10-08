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

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.modelcontextprotocol.spec.McpSchema;
import java.time.Duration;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import reactor.core.publisher.Mono;

/** Complete bounded remote directory with stable exposed-name lookup and native result routing. */
public final class RemoteToolDirectory {

    private static final ObjectMapper JSON = new ObjectMapper();



    private final Map<String, Entry> entries;

    private RemoteToolDirectory(final Map<String, Entry> values) {
        entries = Map.copyOf(values);
    }

    /**
     * An explicitly configured local-only directory.
     * @return empty remote snapshot
     */
    public static RemoteToolDirectory empty() {
        return new RemoteToolDirectory(Map.of());
    }

    /**
     * discover.
     * @param source trusted source value
     * @param maxPages trusted maxPages value
     * @param maxTools trusted maxTools value
     * @return operation result
     */
    public static Mono<RemoteToolDirectory> discover(final List<Target> source, final int maxPages, final int maxTools) {
        List<Target> targets = List.copyOf(source);
        if (targets.isEmpty() || targets.size() > 8 || maxPages < 1 || maxTools < 1) {
            return Mono.error(new IllegalArgumentException("Invalid directory limits"));
        }
        if (targets.stream().map(Target::name).distinct().count() != targets.size()) {
            return Mono.error(new IllegalArgumentException("Duplicate server name"));
        }
        return Mono.defer(() ->
            reactor.core.publisher.Flux.fromIterable(targets)
                .concatMap(target -> pages(target, null, 0, maxPages, maxTools, new HashSet<>(), new ArrayList<>()))
                .collectList()
                .map(pages -> build(pages, maxTools))
                .timeout(Duration.ofSeconds(5))
        );
    }

    private static Mono<List<Entry>> pages(
        final Target target,
        final String cursor,
        final int page,
        final int maxPages,
        final int maxTools,
        final Set<String> seen,
        final List<Entry> result
    ) {
        if (page >= maxPages || (java.util.Objects.nonNull(cursor) && !seen.add(cursor))) {
            return Mono.error(new IllegalArgumentException("Pagination limit or repeated cursor"));
        }
        return Mono.defer(() -> target.pages().apply(cursor)).flatMap(reply -> {
            for (McpSchema.Tool tool : reply.tools()) {
                if (java.util.Objects.isNull(tool.name()) || tool.name().isBlank() || java.util.Objects.isNull(tool.description()) || tool.description().isBlank()) {
                    return Mono.error(new IllegalArgumentException("Invalid tool definition"));
                }
                org.apache.shenyu.plugin.agent.gateway.tool.AgentToolRegistry.validateDefinition(
                    target.name() + "." + tool.name(),
                    tool.description(),
                    com.google.gson.JsonParser.parseString(JSON.valueToTree(tool.inputSchema()).toString()).getAsJsonObject()
                );
                // SDK records contain nested mutable maps; the directory owns an independent copy.
                result.add(new Entry(target, tool.name(), JSON.convertValue(JSON.valueToTree(tool), McpSchema.Tool.class)));
                if (result.size() > maxTools) {
                    return Mono.error(new IllegalArgumentException("Tool limit"));
                }
            }
            if (java.util.Objects.isNull(reply.nextCursor()) || reply.nextCursor().isEmpty()) {
                return Mono.just(List.copyOf(result));
            }
            return pages(target, reply.nextCursor(), page + 1, maxPages, maxTools, seen, result);
        });
    }

    private static RemoteToolDirectory build(final List<List<Entry>> sources, final int maxTools) {
        List<Entry> entries = sources.stream().flatMap(List::stream).toList();
        if (entries.size() > maxTools) {
            throw new IllegalArgumentException("Total tool limit");
        }
        Set<String> originalPairs = new HashSet<>();
        for (Entry entry : entries) {
            if (!originalPairs.add(entry.target().name() + String.valueOf((char) 0) + entry.originalName())) {
                throw new IllegalArgumentException("Duplicate original tool in one server");
            }
        }
        Map<String, Entry> mapped = new LinkedHashMap<>();
        for (Entry entry : entries) {
            // User confirmed a stable namespace for every remote tool on 2026-10-05.
            String exposed = entry.target().name() + "." + entry.originalName();
            if (java.util.Objects.nonNull(mapped.putIfAbsent(exposed, entry))) {
                throw new IllegalArgumentException("Ambiguous exposed name");
            }
        }
        return new RemoteToolDirectory(mapped);
    }

    /**
     * visible.
     * @param ruleTools trusted ruleTools value
     * @param grants trusted grants value
     * @return operation result
     */
    public Set<String> visible(final Set<String> ruleTools, final Set<String> grants) {
        Set<String> result = new HashSet<>(entries.keySet());
        result.retainAll(Set.copyOf(ruleTools));
        result.retainAll(Set.copyOf(grants));
        return Set.copyOf(result);
    }

    /**
     * call.
     * @param name trusted name value
     * @param ruleTools trusted ruleTools value
     * @param grants trusted grants value
     * @param arguments trusted arguments value
     * @return operation result
     */
    public Mono<McpSchema.CallToolResult> call(final String name, final Set<String> ruleTools, final Set<String> grants, final Map<String, Object> arguments) {
        Set<String> permitted = visible(ruleTools, grants);
        JsonNode input = JSON.valueToTree(arguments).deepCopy();
        return Mono.defer(() -> {
            Entry entry = entries.get(name);
            if (java.util.Objects.isNull(entry) || !permitted.contains(name)) {
                return Mono.error(new SecurityException("Tool unavailable"));
            }
            Map<String, Object> privateArguments = JSON.convertValue(input.deepCopy(), new TypeReference<Map<String, Object>>() { });
            return entry.target().calls().apply(new McpSchema.CallToolRequest(entry.originalName(), privateArguments));
        });
    }

    /**
     * names.
     * @return operation result
     */
    public Set<String> names() {
        return entries.keySet();
    }

    /**
     * definitions.
     * @param rule trusted rule value
     * @param grants trusted grants value
     * @return operation result
     */
    public Map<String, McpSchema.Tool> definitions(final Set<String> rule, final Set<String> grants) {
        Set<String> permitted = visible(rule, grants);
        Map<String, McpSchema.Tool> result = new LinkedHashMap<>();
        for (String name : permitted) {
            var value = JSON.valueToTree(entries.get(name).metadata());
            ((com.fasterxml.jackson.databind.node.ObjectNode) value).put("name", name);
            result.put(name, JSON.convertValue(value, McpSchema.Tool.class));
        }
        return Map.copyOf(result);
    }

    /**
     * with Local.
     * @param source trusted source value
     * @param maxTools trusted maxTools value
     * @return operation result
     */
    public RemoteToolDirectory withLocal(final List<LocalTool> source, final int maxTools) {
        Map<String, Entry> result = new LinkedHashMap<>(entries);
        for (LocalTool tool : List.copyOf(source)) {
            var target = new Target(
                "internal",
                ignored -> Mono.error(new IllegalStateException("Local tool has no remote discovery")),
                request -> tool.rawCall.apply(request).map(value -> JSON.convertValue(value, McpSchema.CallToolResult.class)),
                tool.rawCall
            );
            String name = tool.definition.name();
            if (java.util.Objects.nonNull(result.putIfAbsent(name, new Entry(target, name, tool.definition)))) {
                throw new IllegalArgumentException("Local and remote tool namespace collision");
            }
            if (result.size() > maxTools) {
                throw new IllegalArgumentException("Combined local/remote tool budget exceeded");
            }
        }
        return new RemoteToolDirectory(result);
    }

    /**
     * call Raw.
     * @param name trusted name value
     * @param ruleTools trusted ruleTools value
     * @param grants trusted grants value
     * @param arguments trusted arguments value
     * @return operation result
     */
    public Mono<com.fasterxml.jackson.databind.node.ObjectNode> callRaw(
        final String name,
        final Set<String> ruleTools,
        final Set<String> grants,
        final Map<String, Object> arguments
    ) {
        Set<String> permitted = visible(ruleTools, grants);
        JsonNode input = JSON.valueToTree(arguments).deepCopy();
        return Mono.defer(() -> {
            Entry entry = entries.get(name);
            if (java.util.Objects.isNull(entry) || !permitted.contains(name)) {
                return Mono.error(new SecurityException("Tool unavailable"));
            }
            if (java.util.Objects.isNull(entry.target().rawCalls())) {
                return Mono.error(new IllegalStateException("Raw MCP result callback required"));
            }
            Map<String, Object> privateArguments = JSON.convertValue(input.deepCopy(), new TypeReference<Map<String, Object>>() { });
            return entry
                .target()
                .rawCalls()
                .apply(new McpSchema.CallToolRequest(entry.originalName(), privateArguments))
                .switchIfEmpty(Mono.error(new IllegalStateException("Missing raw MCP result")))
                .map(com.fasterxml.jackson.databind.node.ObjectNode::deepCopy);
        });
    }

    public record Entry(Target target, String originalName, McpSchema.Tool metadata) { }

    /** Local targets stay unprefixed; raw callbacks must return native protocol results, not business JSON. */
    public static final class LocalTool {

        private final McpSchema.Tool definition;

        private final Function<McpSchema.CallToolRequest, Mono<com.fasterxml.jackson.databind.node.ObjectNode>> rawCall;

        public LocalTool(final McpSchema.Tool definition, final Function<McpSchema.CallToolRequest, Mono<com.fasterxml.jackson.databind.node.ObjectNode>> rawCall) {
            if (
                java.util.Objects.isNull(definition)
                || java.util.Objects.isNull(definition.name())
                    || definition.name().isBlank()
                    || java.util.Objects.isNull(definition.description())
                    || definition.description().isBlank()
                    || java.util.Objects.isNull(rawCall)
            ) {
                throw new IllegalArgumentException("Invalid local tool");
            }
            this.definition = JSON.convertValue(JSON.valueToTree(definition), McpSchema.Tool.class);
            this.rawCall = rawCall;
        }
    }


    public record Target(
        String name,
        Function<String, Mono<McpSchema.ListToolsResult>> pages,
        Function<McpSchema.CallToolRequest, Mono<McpSchema.CallToolResult>> calls,
        Function<McpSchema.CallToolRequest, Mono<com.fasterxml.jackson.databind.node.ObjectNode>> rawCalls
    ) {
        public Target(
            final String name,
            final Function<String, Mono<McpSchema.ListToolsResult>> pages,
            final Function<McpSchema.CallToolRequest, Mono<McpSchema.CallToolResult>> calls
        ) {
            this(name, pages, calls, null);
        }

        public Target {
            if (java.util.Objects.isNull(name) || !name.matches("[A-Za-z0-9_-]{1,48}")) {
                throw new IllegalArgumentException("Invalid stable server name");
            }
        }
    }
}
