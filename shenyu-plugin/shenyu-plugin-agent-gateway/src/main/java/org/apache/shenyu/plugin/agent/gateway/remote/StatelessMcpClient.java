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
import io.modelcontextprotocol.common.McpTransportContext;
import io.modelcontextprotocol.spec.McpSchema;
import org.apache.shenyu.common.dto.AgentGatewayAggregationConfig.ProtocolProfile;
import org.apache.shenyu.common.tracing.AgentTraceContext;
import reactor.core.publisher.Mono;

import java.io.ByteArrayInputStream;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

import static org.apache.shenyu.plugin.agent.gateway.remote.RemoteMcpHttpSupport.JSON;

/** Request-owned, tools-only adapter for the stateless MCP 2026-07-28 protocol. */
final class StatelessMcpClient implements RemoteMcpEndpoint {

    private static final String VERSION = ProtocolProfile.NATIVE_2026_07_28.version();

    private static final String META_PREFIX = "io.modelcontextprotocol/";

    private static final String DISCOVER = "server/discover";

    private static final String CALL = "tools/call";

    private static final Set<String> SCHEMA_ANNOTATIONS = Set.of("title", "description", "$comment", "examples", "default", "deprecated", "readOnly", "writeOnly");

    private final RemoteServerBinding binding;

    private final RemoteMcpHttpSupport http;

    private final Consumer<Map<String, Object>> observer;

    private final String prefix = UUID.randomUUID().toString();

    private final AtomicLong sequence = new AtomicLong();

    private final Mono<McpSchema.InitializeResult> discovery;

    private volatile boolean ready;

    StatelessMcpClient(final RemoteServerBinding binding, final Consumer<Map<String, Object>> observer) {
        this(binding, observer, RemoteMcpHttpSupport.newHttpClient(null));
    }

    StatelessMcpClient(final RemoteServerBinding binding, final Consumer<Map<String, Object>> observer, final HttpClient http) {
        this.binding = binding;
        this.observer = observer;
        this.http = new RemoteMcpHttpSupport(http);
        discovery = request(DISCOVER, JSON.createObjectNode())
                .map(this::capabilities)
                .doOnNext(ignored -> ready = true)
                .contextWrite(context -> context.delete(McpTransportContext.KEY).delete(AgentTraceContext.HEADERS_KEY))
                .cache();
    }

    @Override
    public Mono<McpSchema.InitializeResult> initialize() {
        return discovery;
    }

    private McpSchema.InitializeResult capabilities(final ObjectNode result) {
        JsonNode versions = result.path("supportedVersions");
        if (!versions.isArray() || !result.path("capabilities").path("tools").isObject()) {
            throw new IllegalArgumentException("Invalid upstream tools discovery");
        }
        boolean supported = false;
        for (JsonNode version : versions) {
            if (!version.isTextual()) {
                throw new IllegalArgumentException("Invalid upstream protocol version");
            }
            supported |= VERSION.equals(version.textValue());
        }
        if (!supported) {
            throw new IllegalArgumentException("Unsupported upstream protocol version");
        }
        // Advertise only the capabilities the adapter can actually serve to this catalog generation.
        return new McpSchema.InitializeResult(VERSION, McpSchema.ServerCapabilities.builder().tools(false).build(), null, null);
    }

    @Override
    public Mono<McpSchema.ListToolsResult> listTools(final String cursor) {
        ObjectNode params = JSON.createObjectNode();
        if (Objects.nonNull(cursor)) {
            params.put("cursor", cursor);
        }
        return request("tools/list", params).map(this::toolPage);
    }

    private McpSchema.ListToolsResult toolPage(final ObjectNode result) {
        JsonNode tools = result.path("tools");
        if (!tools.isArray() || result.has("nextCursor") && !result.path("nextCursor").isTextual()) {
            throw new IllegalArgumentException("Invalid upstream tool page");
        }
        McpSchema.ListToolsResult page = JSON.convertValue(result, McpSchema.ListToolsResult.class);
        for (int i = 0; i < tools.size(); i++) {
            validateDefinition(tools.get(i), page.tools().get(i));
        }
        return page;
    }

    private void validateDefinition(final JsonNode tool, final McpSchema.Tool definition) {
        JsonNode schema = tool.path("inputSchema");
        if (!tool.path("name").isTextual() || !tool.path("description").isTextual() || !"object".equals(schema.path("type").textValue())) {
            throw new IllegalArgumentException("Invalid upstream tool definition");
        }
        if (requiresParameterHeaders(schema)) {
            throw new IllegalArgumentException("Upstream parameter header mirroring is unsupported");
        }
        // The pinned catalog model represents only a subset of root JSON Schema keywords.
        // Never publish a tool with weaker constraints than those supplied by its upstream.
        JsonNode projected = JSON.valueToTree(definition.inputSchema());
        for (Map.Entry<String, JsonNode> field : schema.properties()) {
            String key = field.getKey();
            boolean defaultDialect = "$schema".equals(key) && "https://json-schema.org/draft/2020-12/schema".equals(field.getValue().textValue());
            if (!defaultDialect && !SCHEMA_ANNOTATIONS.contains(key) && !field.getValue().equals(projected.get(key))) {
                throw new IllegalArgumentException("Unsupported upstream tool input schema");
            }
        }
    }

    private boolean requiresParameterHeaders(final JsonNode schema) {
        if (schema.has("x-mcp-header")) {
            return true;
        }
        for (JsonNode property : schema.path("properties")) {
            if (requiresParameterHeaders(property)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public Mono<McpSchema.CallToolResult> callTool(final McpSchema.CallToolRequest request) {
        return callRaw(request).map(result -> JSON.convertValue(result, McpSchema.CallToolResult.class));
    }

    @Override
    public Mono<ObjectNode> callRaw(final McpSchema.CallToolRequest request) {
        ObjectNode snapshot = JSON.valueToTree(request);
        return request(CALL, snapshot).map(result -> {
            if (!result.path("content").isArray() || result.has("isError") && !result.path("isError").isBoolean()) {
                throw new IllegalArgumentException("Invalid upstream tool result");
            }
            return result;
        });
    }

    private Mono<ObjectNode> request(final String method, final ObjectNode params) {
        return Mono.deferContextual(context -> {
            if (http.isClosed() || !ready && !DISCOVER.equals(method)) {
                return Mono.error(new IllegalStateException("Client not ready"));
            }
            ObjectNode arguments = params.deepCopy();
            RemoteTransportPolicy.callMetadata(arguments);
            ObjectNode meta = arguments.putObject("_meta");
            meta.put(META_PREFIX + "protocolVersion", VERSION);
            meta.putObject(META_PREFIX + "clientCapabilities");
            String id = prefix + "-" + sequence.incrementAndGet();
            ObjectNode message = JSON.createObjectNode().put("jsonrpc", "2.0").put("id", id).put("method", method);
            message.set("params", arguments);
            HttpRequest.Builder builder = HttpRequest.newBuilder(binding.config().endpoint())
                    .header("Authorization", binding.authorization())
                    .header("MCP-Protocol-Version", VERSION)
                    .header("Mcp-Method", method);
            McpTransportContext transport = context.getOrDefault(McpTransportContext.KEY, McpTransportContext.EMPTY);
            if (CALL.equals(method)) {
                builder.header("Mcp-Name", RemoteMcpHttpSupport.encodeMirroredName(arguments.path("name").textValue()));
                AgentTraceContext.headers(transport.get(AgentTraceContext.HEADERS_KEY)).forEach(builder::setHeader);
            }
            return http.post(builder, message, transport, StatelessMcpClient::singleUseBody)
                    .map(response -> result(response, method, id));
        });
    }

    private static HttpRequest.BodyPublisher singleUseBody(final byte[] bytes) {
        // Even if the JVM permits retrying POSTs, a transport retry cannot transmit the same tool invocation twice.
        AtomicReference<byte[]> remaining = new AtomicReference<>(bytes);
        HttpRequest.BodyPublisher body = HttpRequest.BodyPublishers.ofInputStream(() -> {
            byte[] first = remaining.getAndSet(null);
            return Objects.nonNull(first) ? new ByteArrayInputStream(first) : null;
        });
        return HttpRequest.BodyPublishers.fromPublisher(body, bytes.length);
    }

    private ObjectNode result(final HttpResponse<byte[]> response, final String method, final String id) {
        observer.accept(Map.of("event", "http-response", "server", binding.config().name(), "method", method,
                "id", id, "status", response.statusCode(), "bytes", response.body().length));
        if (response.statusCode() < 200 || response.statusCode() >= 300) {
            throw new IllegalStateException("Upstream HTTP " + response.statusCode());
        }
        String contentType = response.headers().firstValue("Content-Type").orElse("").split(";", 2)[0].trim();
        ObjectNode envelope = RemoteMcpHttpSupport.decodeEnvelope(response.body(), contentType, true);
        if (!"2.0".equals(envelope.path("jsonrpc").textValue()) || !id.equals(envelope.path("id").textValue()) || envelope.has("method")) {
            throw new IllegalArgumentException("Invalid upstream response envelope");
        }
        if (envelope.has("error")) {
            throw new IllegalStateException("Upstream JSON-RPC error");
        }
        if (!(envelope.get("result") instanceof ObjectNode result)) {
            throw new IllegalArgumentException("Invalid upstream result shape");
        }
        if (!"complete".equals(result.path("resultType").textValue())) {
            throw new IllegalArgumentException("Unsupported upstream resultType");
        }
        RemoteTransportPolicy.resultMetadata(result);
        return result;
    }

    @Override
    public int pending() {
        return http.pending();
    }

    @Override
    public Mono<Void> closeGracefully() {
        return Mono.fromRunnable(http::closeRequests);
    }
}
