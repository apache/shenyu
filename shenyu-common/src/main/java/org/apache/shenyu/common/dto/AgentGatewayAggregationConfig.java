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
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

/** Strict, secret-free configuration shared by Admin and gateway. */
public record AgentGatewayAggregationConfig(String revision, List<Server> servers) {

    /** Legacy remote request budget, kept as the default for every existing configuration. */
    public static final int DEFAULT_REMOTE_REQUEST_BYTES = 8192;

    /** Legacy remote response budget, kept as the default for every existing configuration. */
    public static final int DEFAULT_REMOTE_RESPONSE_BYTES = 8192;

    /** Smallest configurable remote budget; below this the JSON-RPC envelope itself cannot fit. */
    public static final int MIN_REMOTE_BYTES = 1024;

    /** Hard maximum for one remote request body, regardless of configuration. */
    public static final int MAX_REMOTE_REQUEST_BYTES = 65536;

    /** Hard maximum for one remote response body, regardless of configuration. */
    public static final int MAX_REMOTE_RESPONSE_BYTES = 1048576;

    private static final ObjectMapper JSON = new ObjectMapper().enable(JsonParser.Feature.STRICT_DUPLICATE_DETECTION)
            .enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS);

    public AgentGatewayAggregationConfig {
        if (Objects.isNull(revision) || !revision.matches("[A-Za-z0-9_-]{1,48}")) {
            throw new IllegalArgumentException("Invalid aggregation revision");
        }
        servers = List.copyOf(servers);
        if (servers.size() > 8 || servers.stream().map(Server::name).distinct().count() != servers.size()) {
            throw new IllegalArgumentException("Too many or duplicate MCP Servers");
        }
    }

    /**
     * Validate the plugin config before persistence or gateway use.
     * @param source entire plugin configuration, never a rule or Agent request
     * @return immutable aggregation configuration
     */
    public static AgentGatewayAggregationConfig parsePluginConfig(final String source) {
        if (Objects.isNull(source) || source.isBlank()) {
            return new AgentGatewayAggregationConfig("empty", List.of());
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
                return new AgentGatewayAggregationConfig("empty", List.of());
            }
            fields(root, Set.of("aggregation"));
            JsonNode aggregation = root.get("aggregation");
            fields(aggregation, Set.of("revision", "servers"));
            if (!aggregation.path("servers").isArray()) {
                throw new IllegalArgumentException("Expected MCP Server array");
            }
            List<Server> servers = new ArrayList<>();
            for (JsonNode server : aggregation.get("servers")) {
                fields(server, Set.of("name", "endpoint", "credentialRef", "credentialVersion"),
                        Set.of("remoteRequestBytes", "remoteResponseBytes"));
                servers.add(new Server(text(server, "name"), URI.create(text(server, "endpoint")),
                        text(server, "credentialRef"), text(server, "credentialVersion"),
                        budget(server, "remoteRequestBytes", DEFAULT_REMOTE_REQUEST_BYTES),
                        budget(server, "remoteResponseBytes", DEFAULT_REMOTE_RESPONSE_BYTES)));
            }
            return new AgentGatewayAggregationConfig(text(aggregation, "revision"), servers);
        } catch (Exception error) {
            // Do not include raw config or a parse exception which could contain a supplied secret.
            throw new IllegalArgumentException("Invalid Agent Gateway aggregation configuration");
        }
    }

    private static void fields(final JsonNode node, final Set<String> expected) {
        fields(node, expected, Set.of());
    }

    private static void fields(final JsonNode node, final Set<String> required, final Set<String> optional) {
        if (Objects.isNull(node) || !node.isObject()) {
            throw new IllegalArgumentException("Expected configuration object");
        }
        Set<String> fields = new HashSet<>();
        node.fieldNames().forEachRemaining(fields::add);
        Set<String> allowed = new HashSet<>(required);
        allowed.addAll(optional);
        if (!allowed.containsAll(fields) || !fields.containsAll(required)) {
            throw new IllegalArgumentException("Unknown or missing configuration field");
        }
    }

    private static int budget(final JsonNode server, final String field, final int fallback) {
        JsonNode value = server.get(field);
        if (Objects.isNull(value)) {
            // Absent optional budget keeps the documented legacy default, never a shared maximum.
            return fallback;
        }
        if (!value.isIntegralNumber() || !value.canConvertToInt()) {
            throw new IllegalArgumentException("Invalid remote payload budget");
        }
        return value.intValue();
    }

    /**
     * Validate one remote payload budget pair, shared by Admin persistence and the gateway.
     * @param requestBytes configured remote request body budget
     * @param responseBytes configured remote response body budget
     */
    public static void validateRemotePayloadBudget(final int requestBytes, final int responseBytes) {
        if (requestBytes < MIN_REMOTE_BYTES || requestBytes > MAX_REMOTE_REQUEST_BYTES
                || responseBytes < MIN_REMOTE_BYTES || responseBytes > MAX_REMOTE_RESPONSE_BYTES) {
            throw new IllegalArgumentException("Remote MCP payload budget outside the supported range");
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

    /** A remote target reference with immutable payload budgets, containing no bearer token. */
    public record Server(String name, URI endpoint, String credentialRef, String credentialVersion,
                         int remoteRequestBytes, int remoteResponseBytes) {

        public Server {
            if (Objects.isNull(name) || !name.matches("[A-Za-z0-9_-]{1,48}")) {
                throw new IllegalArgumentException("Invalid MCP Server target");
            }
            validateEndpoint(endpoint);
            if (Objects.isNull(credentialRef) || !credentialRef.matches("[A-Za-z0-9_./-]{1,96}") || credentialRef.contains("..")
                    || Objects.isNull(credentialVersion) || !credentialVersion.matches("[A-Za-z0-9_-]{1,48}")) {
                throw new IllegalArgumentException("Invalid service credential reference");
            }
            validateRemotePayloadBudget(remoteRequestBytes, remoteResponseBytes);
        }

        /**
         * Legacy target without explicit budgets: the fixed 8 KiB transport limit is retained.
         * @param name stable server name
         * @param endpoint exact configured endpoint
         * @param credentialRef service credential reference
         * @param credentialVersion service credential version
         */
        public Server(final String name, final URI endpoint, final String credentialRef, final String credentialVersion) {
            this(name, endpoint, credentialRef, credentialVersion, DEFAULT_REMOTE_REQUEST_BYTES, DEFAULT_REMOTE_RESPONSE_BYTES);
        }
    }
}
