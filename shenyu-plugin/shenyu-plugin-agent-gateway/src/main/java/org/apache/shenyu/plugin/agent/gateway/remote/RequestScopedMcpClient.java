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

import com.fasterxml.jackson.databind.node.ObjectNode;
import io.modelcontextprotocol.common.McpTransportContext;
import io.modelcontextprotocol.spec.McpSchema;
import org.apache.shenyu.common.tracing.AgentTraceContext;
import reactor.core.publisher.Mono;

import java.net.URI;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicLong;

import static org.apache.shenyu.plugin.agent.gateway.remote.RemoteMcpHttpSupport.JSON;

/**
 * Bounded, fixed 2025-06-18 request/one-final-response adapter.
 * This is not a general MCP client: no GET stream/reconnect, notifications, MRTR or OAuth.
 * Uses SDK wire models, but intentionally does not enter its detached sendRequest path.
 */
public final class RequestScopedMcpClient implements RemoteMcpEndpoint {

    private static final String VERSION = "2025-06-18";

    private final RemoteMcpHttpSupport http;

    private final URI endpoint;

    private final String name;

    private final String authorization;

    private final java.util.function.Consumer<Map<String, Object>> observer;

    private final String prefix = UUID.randomUUID().toString();

    private final AtomicLong sequence = new AtomicLong();

    private volatile String session;

    private volatile boolean initialized;

    private final Mono<McpSchema.InitializeResult> initialization;
    // Client shutdown is owned/cached; repeat subscribers see the same cleanup failure, not false success.

    private final Mono<Void> shutdown = Mono.defer(this::terminate).cache();

    /**
     * Request Scoped Mcp Client.
     * @param binding trusted binding value
     * @param observer trusted observer value
     */
    public RequestScopedMcpClient(final RemoteServerBinding binding, final java.util.function.Consumer<Map<String, Object>> observer) {
        this(binding, observer, null);
    }

    RequestScopedMcpClient(final RemoteServerBinding binding, final java.util.function.Consumer<Map<String, Object>> observer, final javax.net.ssl.SSLContext testContext) {
        http = new RemoteMcpHttpSupport(RemoteMcpHttpSupport.newHttpClient(testContext));
        this.endpoint = binding.config().endpoint();
        this.name = binding.config().name();
        this.authorization = binding.authorization();
        this.observer = observer;
        initialization = request("initialize", Map.of("protocolVersion", VERSION, "capabilities", Map.of(),
                "clientInfo", Map.of("name", "shenyu-agent-gateway", "version", "2.7.2-SNAPSHOT")), false)
            .map(result -> {
                if (!VERSION.equals(result.path("protocolVersion").asText())) {
                    throw new IllegalArgumentException("Unsupported negotiated version");
                }
                return JSON.convertValue(result, McpSchema.InitializeResult.class);
            })
            .flatMap(result -> request("notifications/initialized", Map.of(), true).thenReturn(result))
            .doOnSuccess(ignored -> initialized = true)
            // Session handshake uses fixed service configuration, never the first Agent's context.
            .contextWrite(context -> context.delete(McpTransportContext.KEY).delete(AgentTraceContext.HEADERS_KEY))
            .cache();
    }

    @Override
    public Mono<McpSchema.InitializeResult> initialize() {
        return initialization;
    }

    @Override
    public Mono<McpSchema.ListToolsResult> listTools(final String cursor) {
        return ready().then(
            Mono.defer(() ->
                request("tools/list", Objects.isNull(cursor) ? Map.of() : Map.of("cursor", cursor), false).map(result ->
                    JSON.convertValue(result, McpSchema.ListToolsResult.class)
                )
            )
        );
    }

    @Override
    public Mono<McpSchema.CallToolResult> callTool(final McpSchema.CallToolRequest request) {
        return callRaw(request).map(result -> JSON.convertValue(result, McpSchema.CallToolResult.class));
    }

    @Override
    public Mono<ObjectNode> listResources(final String cursor) {
        return ready().then(Mono.defer(() -> request("resources/list", Objects.isNull(cursor) ? Map.of() : Map.of("cursor", cursor), false)));
    }

    @Override
    public Mono<ObjectNode> listResourceTemplates(final String cursor) {
        return ready().then(Mono.defer(() -> request("resources/templates/list", Objects.isNull(cursor) ? Map.of() : Map.of("cursor", cursor), false)));
    }

    @Override
    public Mono<ObjectNode> readResource(final String uri) {
        return ready().then(Mono.defer(() -> request("resources/read", Map.of("uri", uri), false)));
    }

    /**
     * call Raw.
     * @param request trusted request value
     * @return operation result
     */
    public Mono<ObjectNode> callRaw(final McpSchema.CallToolRequest request) {
        // Defensive JSON snapshot at assembly; each subscription gets its own outbound id.
        ObjectNode snapshot = JSON.valueToTree(request);
        return ready().then(
            Mono.defer(() -> {
                RemoteTransportPolicy.callMetadata(snapshot);
                return request("tools/call", snapshot.deepCopy(), false).map(result -> {
                    RemoteTransportPolicy.resultMetadata(result);
                    return result;
                });
            })
        );
    }

    private Mono<Void> ready() {
        return Mono.defer(() -> initialized && !http.isClosed() ? Mono.empty() : Mono.error(new IllegalStateException("Client not ready")));
    }

    private Mono<ObjectNode> request(final String method, final Object params, final boolean notification) {
        return Mono.deferContextual(context -> {
            if (http.isClosed()) {
                return Mono.error(new IllegalStateException("Client closed"));
            }
            String id = prefix + "-" + sequence.incrementAndGet();
            ObjectNode message = JSON.createObjectNode().put("jsonrpc", "2.0").put("method", method);
            if (!notification) {
                message.put("id", id);
            }
            message.set("params", JSON.valueToTree(params));
            McpTransportContext transportContext = context.getOrDefault(McpTransportContext.KEY, McpTransportContext.EMPTY);
            HttpRequest.Builder builder = HttpRequest.newBuilder(endpoint)
                    .header("Authorization", authorization)
                    .header("MCP-Protocol-Version", VERSION);
            if (Objects.nonNull(session) && !"initialize".equals(method)) {
                builder.header("MCP-Session-Id", session);
            }
            // A shared handshake, directory refresh or session cleanup never belongs to an Agent call.
            if ("tools/call".equals(method)) {
                AgentTraceContext.headers(transportContext.get(AgentTraceContext.HEADERS_KEY)).forEach(builder::setHeader);
            }
            return http.post(builder, message, transportContext, HttpRequest.BodyPublishers::ofByteArray)
                    .map(response -> decodeResponse(response, method, id, notification));
        });
    }

    private ObjectNode decodeResponse(final HttpResponse<byte[]> response, final String method, final String id, final boolean notification) {
        observer.accept(Map.of("event", "http-response", "server", name, "method", method, "id", id,
                "status", response.statusCode(), "contentType", response.headers().firstValue("Content-Type").orElse(""), "bytes", response.body().length));
        if (response.statusCode() < 200 || response.statusCode() >= 300) {
            throw new IllegalStateException("Upstream HTTP " + response.statusCode());
        }
        if ("initialize".equals(method)) {
            session = RemoteTransportPolicy.session(response.headers().allValues("MCP-Session-Id"));
        }
        if (notification) {
            if (response.statusCode() != 202 || response.body().length != 0) {
                throw new IllegalArgumentException("Invalid notification acknowledgement");
            }
            return JSON.createObjectNode();
        }
        String contentType = response.headers().firstValue("Content-Type").orElse("").split(";")[0].trim();
        ObjectNode envelope = RemoteMcpHttpSupport.decodeEnvelope(response.body(), contentType, false);
        if (!"2.0".equals(envelope.path("jsonrpc").asText()) || !id.equals(envelope.path("id").asText())) {
            throw new IllegalArgumentException("Mismatched protocol response id");
        }
        if (envelope.has("error")) {
            throw new IllegalStateException("Upstream JSON-RPC error");
        }
        if (!envelope.path("result").isObject()) {
            throw new IllegalArgumentException("Invalid result shape");
        }
        return ((ObjectNode) envelope.get("result")).deepCopy();
    }

    @Override
    public int pending() {
        return http.pending();
    }

    @Override
    public Mono<Void> closeGracefully() {
        return shutdown;
    }

    private Mono<Void> terminate() {
        http.closeRequests();
        if (Objects.isNull(session)) {
            return Mono.empty();
        }
        HttpRequest request = HttpRequest.newBuilder(endpoint)
            .timeout(Duration.ofSeconds(2))
            .header("MCP-Session-Id", session)
            .header("Authorization", authorization)
            .header("MCP-Protocol-Version", VERSION)
            .DELETE()
            // Bound completion of the body as well as connection/response headers. Owned future is cancelled on timeout.
            .build();
        return http.discard(request)
            .flatMap(response ->
                response.statusCode() >= 200 && response.statusCode() < 300
                    ? Mono.<Void>empty()
                    : Mono.error(new IllegalStateException("Session cleanup HTTP " + response.statusCode()))
            );
    }

    static <T> Mono<T> requestSignal(final CompletableFuture<T> request) {
        return RemoteMcpHttpSupport.requestSignal(request);
    }
}
