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

package org.apache.shenyu.common.dto;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

/** Strict, secret-free configuration shared by Admin and gateway. */
public record AgentGatewayAggregationConfig(String revision,
                                            List<Server> servers,
                                            int refreshConcurrency,
                                            AgentGatewayRefreshPolicy refreshPolicy) {

    /** Default to serial remote catalog discovery. */
    public static final int DEFAULT_REFRESH_CONCURRENCY = 1;

    /** Minimum supported number of concurrent refresh targets. */
    public static final int MIN_REFRESH_CONCURRENCY = 1;

    /** Hard upper bound on concurrent refresh targets. */
    public static final int MAX_REFRESH_CONCURRENCY = 8;

    private static final ObjectMapper JSON = new ObjectMapper().enable(JsonParser.Feature.STRICT_DUPLICATE_DETECTION)
            .enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS);

    /**
     * Retain serial refresh for existing callers.
     * @param revision trusted configuration revision
     * @param servers remote server references
     */
    public AgentGatewayAggregationConfig(final String revision,
                                         final List<Server> servers) {
        this(revision, servers, DEFAULT_REFRESH_CONCURRENCY);
    }

    /**
     * Retain default timing for callers configuring only concurrency.
     * @param revision trusted configuration revision
     * @param servers remote server references
     * @param refreshConcurrency bounded concurrent discovery targets
     */
    public AgentGatewayAggregationConfig(final String revision, final List<Server> servers, final int refreshConcurrency) {
        this(revision, servers, refreshConcurrency, AgentGatewayRefreshPolicy.defaults());
    }

    public AgentGatewayAggregationConfig {
        if (Objects.isNull(revision) || !revision.matches("[A-Za-z0-9_-]{1,48}")) {
            throw new IllegalArgumentException("Invalid aggregation revision");
        }
        servers = List.copyOf(servers);
        if (servers.size() > 8 || servers.stream().map(Server::name).distinct().count() != servers.size()) {
            throw new IllegalArgumentException("Too many or duplicate MCP Servers");
        }
        if (refreshConcurrency < MIN_REFRESH_CONCURRENCY || refreshConcurrency > MAX_REFRESH_CONCURRENCY) {
            throw new IllegalArgumentException("Invalid refresh concurrency");
        }
        if (Objects.isNull(refreshPolicy)) {
            throw new IllegalArgumentException("Missing refresh policy");
        }
    }

    /**
     * Validate the plugin config before persistence or gateway use.
     * @param source entire plugin configuration, never a rule or Agent request
     * @return immutable aggregation configuration
     */
    public static AgentGatewayAggregationConfig parsePluginConfig(final String source) {
        if (Objects.isNull(source) || source.isBlank()) {
            return new AgentGatewayAggregationConfig("empty", List.of(), DEFAULT_REFRESH_CONCURRENCY);
        }
        if (source.getBytes(StandardCharsets.UTF_8).length > 16384) {
            throw new IllegalArgumentException("Aggregation configuration byte limit");
        }
        try {
            JsonNode root = JSON.readTree(source);
            if (!root.isObject()) {
                throw new IllegalArgumentException("Expected plugin configuration object");
            }
            if (root.isEmpty()) {
                return new AgentGatewayAggregationConfig("empty", List.of(), DEFAULT_REFRESH_CONCURRENCY);
            }
            fields(root, Set.of("aggregation"));
            JsonNode aggregation = root.get("aggregation");
            fields(aggregation, Set.of("revision", "servers"), Set.of("revision", "servers", "refreshConcurrency", "refreshPolicy"));
            if (!aggregation.path("servers").isArray()) {
                throw new IllegalArgumentException("Expected MCP Server array");
            }
            List<Server> servers = new ArrayList<>();
            for (JsonNode server : aggregation.get("servers")) {
                fields(server, Set.of("name", "endpoint", "credentialRef", "credentialVersion"));
                servers.add(new Server(text(server, "name"), URI.create(text(server, "endpoint")),
                        text(server, "credentialRef"), text(server, "credentialVersion")));
            }
            int concurrency = DEFAULT_REFRESH_CONCURRENCY;
            if (aggregation.has("refreshConcurrency")) {
                concurrency = integer(aggregation, "refreshConcurrency");
            }
            return new AgentGatewayAggregationConfig(text(aggregation, "revision"), servers, concurrency, parseRefreshPolicy(aggregation));
        } catch (Exception error) {
            // Do not include raw config or a parse exception which could contain a supplied secret.
            throw new IllegalArgumentException("Invalid Agent Gateway aggregation configuration");
        }
    }

    private static AgentGatewayRefreshPolicy parseRefreshPolicy(final JsonNode aggregation) {
        if (!aggregation.has("refreshPolicy")) {
            return AgentGatewayRefreshPolicy.defaults();
        }
        JsonNode policy = aggregation.get("refreshPolicy");
        fields(policy, Set.of("refreshIntervalMs", "initialBackoffMs", "maxBackoffMs", "maxRetries"));
        return new AgentGatewayRefreshPolicy(
                Duration.ofMillis(integer(policy, "refreshIntervalMs")),
                Duration.ofMillis(integer(policy, "initialBackoffMs")),
                Duration.ofMillis(integer(policy, "maxBackoffMs")),
                integer(policy, "maxRetries"));
    }

    private static int integer(final JsonNode node, final String field) {
        JsonNode value = node.get(field);
        if (Objects.isNull(value) || !value.isIntegralNumber() || !value.canConvertToInt()) {
            throw new IllegalArgumentException("Expected configuration integer");
        }
        return value.intValue();
    }

    private static void fields(final JsonNode node, final Set<String> expected) {
        fields(node, expected, expected);
    }

    private static void fields(final JsonNode node, final Set<String> required, final Set<String> allowed) {
        if (Objects.isNull(node) || !node.isObject()) {
            throw new IllegalArgumentException("Expected configuration object");
        }
        Set<String> fields = new HashSet<>();
        node.fieldNames().forEachRemaining(fields::add);
        if (!fields.containsAll(required) || !allowed.containsAll(fields)) {
            throw new IllegalArgumentException("Unknown or missing configuration field");
        }
    }

    /**
     * Validate the agreed fixed-IP transport boundary at both Admin and gateway.
     * @param endpoint exact configured endpoint, never an Agent-supplied URL
     */
    public static void validateEndpoint(final URI endpoint) {
        if (Objects.isNull(endpoint) || Objects.isNull(endpoint.getHost()) || !literalIpv4(endpoint.getHost())
                || !("https".equals(endpoint.getScheme()) || "http".equals(endpoint.getScheme()) && "127.0.0.1".equals(endpoint.getHost()))
                || endpoint.getPort() < 1 || endpoint.getPort() > 65535 || Objects.nonNull(endpoint.getRawUserInfo())
                || Objects.nonNull(endpoint.getRawQuery()) || Objects.nonNull(endpoint.getRawFragment())
                || !endpoint.normalize().equals(endpoint) || !endpoint.getRawPath().matches("/(?:[A-Za-z0-9._~-]+/)*[A-Za-z0-9._~-]+")) {
            throw new IllegalArgumentException("Invalid fixed-IP MCP Server endpoint");
        }
        for (String part : endpoint.getRawPath().split("/")) {
            if (Set.of(".", "..").contains(part)) {
                throw new IllegalArgumentException("Ambiguous MCP endpoint");
            }
        }
    }

    private static boolean literalIpv4(final String host) {
        if (!host.matches("(?:0|[1-9][0-9]{0,2})(?:\\.(?:0|[1-9][0-9]{0,2})){3}")) {
            return false;
        }
        String[] parts = host.split("\\.");
        for (String part : parts) {
            if (Integer.parseInt(part) > 255) {
                return false;
            }
        }
        int first = Integer.parseInt(parts[0]);
        return first > 0 && first < 224 && !(first == 169 && "254".equals(parts[1]));
    }

    private static String text(final JsonNode node, final String field) {
        if (!node.path(field).isTextual()) {
            throw new IllegalArgumentException("Expected configuration string");
        }
        return node.get(field).textValue();
    }

    /** A remote target reference, containing no bearer token. */
    public record Server(String name, URI endpoint, String credentialRef, String credentialVersion) {
        public Server {
            if (Objects.isNull(name) || !name.matches("[A-Za-z0-9_-]{1,48}")) {
                throw new IllegalArgumentException("Invalid MCP Server target");
            }
            validateEndpoint(endpoint);
            if (Objects.isNull(credentialRef) || !credentialRef.matches("[A-Za-z0-9_./-]{1,96}") || credentialRef.contains("..")
                    || Objects.isNull(credentialVersion) || !credentialVersion.matches("[A-Za-z0-9_-]{1,48}")) {
                throw new IllegalArgumentException("Invalid service credential reference");
            }
        }
    }
}
