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

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import io.modelcontextprotocol.common.McpTransportContext;
import io.modelcontextprotocol.spec.McpSchema;
import org.apache.http.HttpRequest;
import org.apache.http.HttpHost;
import org.apache.shenyu.common.tracing.AgentTraceContext;
import reactor.core.publisher.Mono;
import reactor.core.publisher.SignalType;

import java.net.URI;
import java.net.SocketTimeoutException;
import java.nio.ByteBuffer;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.Future;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CompletionException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Bounded, fixed 2025-06-18 request/one-final-response adapter.
 * This is not a general MCP client: no GET stream/reconnect, notifications, MRTR or OAuth.
 * Uses SDK wire models, but intentionally does not enter its detached sendRequest path.
 */
public final class RequestScopedMcpClient implements RemoteMcpEndpoint {

    private static final String VERSION = "2025-06-18";

    private static final ObjectMapper JSON = new ObjectMapper().enable(JsonParser.Feature.STRICT_DUPLICATE_DETECTION).enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS);

    private static final int LIMIT = 8192;

    private static final int MAX_REQUEST_BYTES = 8192;

    private static final int MAX_IN_FLIGHT = 64;

    private final RemoteTransportRoute route;

    private final ApacheRemoteMcpTransport http;

    private final URI endpoint;

    private final String name;

    private final String authorization;

    private final java.util.function.Consumer<Map<String, Object>> observer;

    private final String prefix = UUID.randomUUID().toString();

    private final AtomicLong sequence = new AtomicLong();

    private final AtomicInteger active = new AtomicInteger();

    private final Set<Future<?>> inFlight = ConcurrentHashMap.newKeySet();

    private volatile String session;

    private volatile boolean initialized;

    private volatile boolean closed;

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
        this(binding, RemoteTransportRoute.fixed(binding.config().endpoint()), observer, testContext);
    }

    RequestScopedMcpClient(final RemoteServerBinding binding, final RemoteTransportRoute route,
                           final java.util.function.Consumer<Map<String, Object>> observer, final javax.net.ssl.SSLContext testContext) {
        if (!binding.config().endpoint().equals(route.endpoint())) {
            throw new SecurityException("Route does not match the credential binding");
        }
        this.route = Objects.requireNonNull(route, "route");
        http = ApacheRemoteMcpTransport.create(route, testContext);
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
        return Mono.defer(() -> initialized && !closed ? Mono.empty() : Mono.error(new IllegalStateException("Client not ready")));
    }

    private Mono<ObjectNode> request(final String method, final Object params, final boolean notification) {
        return Mono.deferContextual(context -> {
            if (closed) {
                return Mono.error(new IllegalStateException("Client closed"));
            }
            String id = prefix + "-" + sequence.incrementAndGet();
            ObjectNode message = JSON.createObjectNode().put("jsonrpc", "2.0").put("method", method);
            if (!notification) {
                message.put("id", id);
            }
            message.set("params", JSON.valueToTree(params));
            McpTransportContext transportContext = context.getOrDefault(McpTransportContext.KEY, McpTransportContext.EMPTY);
            Instant deadline = transportContext.get("deadline") instanceof Instant value ? value : Instant.now().plusSeconds(4);
            Duration budget = Duration.between(Instant.now(), deadline);
            if (budget.isNegative() || budget.isZero()) {
                return Mono.error(new TimeoutException("Deadline expired before upstream side effect"));
            }
            byte[] requestBytes = message.toString().getBytes(StandardCharsets.UTF_8);
            if (requestBytes.length > MAX_REQUEST_BYTES) {
                return Mono.error(new IllegalArgumentException("Request byte limit"));
            }
            if (active.incrementAndGet() > MAX_IN_FLIGHT) {
                active.decrementAndGet();
                return Mono.error(new IllegalStateException("Client concurrency limit"));
            }
            var owned = new AtomicReference<ApacheRemoteMcpTransport.AsyncRequestHandle<ApacheRemoteHttpResponse>>();
            return Mono.defer(() -> {
                ApacheRemoteMcpTransport.AsyncRequestHandle<ApacheRemoteHttpResponse> future = http.execute(
                        new HttpHost(route.endpoint().getHost(), route.endpoint().getPort(), route.endpoint().getScheme()),
                        outboundRequest(method, requestBytes,
                                AgentTraceContext.headers(transportContext.get(AgentTraceContext.HEADERS_KEY))), budget.toMillis());
                owned.set(future);
                inFlight.add(future);
                if (closed) {
                    future.cancel(true);
                }
                return requestSignal(future);
            })
                .timeout(budget)
                .onErrorMap(SocketTimeoutException.class, error -> new TimeoutException(error.getMessage()))
                .map(response -> decodeResponse(response, method, id, notification))
                .doFinally(ignored -> {
                    Future<?> future = owned.get();
                    if (Objects.nonNull(future)) {
                        future.cancel(true);
                        inFlight.remove(future);
                    }
                    active.decrementAndGet();
                });
        });
    }

    private HttpRequest outboundRequest(final String method, final byte[] requestBytes, final Map<String, String> traceHeaders) {
        ApacheRemoteMcpTransport.ConfigurableRequest request = new ApacheRemoteMcpTransport.ConfigurableRequest(method, endpoint.getRawPath());
        request.setEntity(new org.apache.http.entity.ByteArrayEntity(requestBytes, org.apache.http.entity.ContentType.APPLICATION_JSON));
        request.setHeader("Host", endpoint.getHost() + ":" + endpoint.getPort());
        request.setHeader("Content-Type", "application/json");
        request.setHeader("Accept", "application/json, text/event-stream");
        request.setHeader("Authorization", authorization);
        request.setHeader("MCP-Protocol-Version", VERSION);
        if (Objects.nonNull(session) && !"initialize".equals(method)) {
            request.setHeader("MCP-Session-Id", session);
        }
        // A shared handshake, directory refresh or session cleanup never belongs to an Agent call.
        if ("tools/call".equals(method)) {
            traceHeaders.forEach(request::setHeader);
        }
        return request;
    }

    private ObjectNode decodeResponse(final ApacheRemoteHttpResponse response, final String method, final String id, final boolean notification) {
        observer.accept(Map.of("event", "http-response", "server", name, "method", method, "id", id,
                "status", response.status(), "contentType", header(response, "Content-Type"), "bytes", response.body().length));
        if (response.status() < 200 || response.status() >= 300) {
            throw new IllegalStateException("Upstream HTTP " + response.status());
        }
        if ("initialize".equals(method)) {
            session = RemoteTransportPolicy.session(response.headers().getOrDefault("mcp-session-id", List.of()));
        }
        if (notification) {
            if (response.status() != 202 || response.body().length != 0) {
                throw new IllegalArgumentException("Invalid notification acknowledgement");
            }
            return JSON.createObjectNode();
        }
        String contentType = header(response, "Content-Type").split(";")[0].trim();
        ObjectNode envelope = decode(response.body(), contentType);
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

    private static String header(final ApacheRemoteHttpResponse response, final String name) {
        return response.headers().getOrDefault(name.toLowerCase(Locale.ROOT), List.of()).stream().findFirst().orElse("");
    }

    private static ObjectNode decode(final byte[] bytes, final String contentType) {
        try {
            String text = StandardCharsets.UTF_8.newDecoder()
                .onMalformedInput(CodingErrorAction.REPORT)
                .onUnmappableCharacter(CodingErrorAction.REPORT)
                .decode(ByteBuffer.wrap(bytes))
                .toString();
            if ("text/event-stream".equals(contentType)) {
                String[] blocks = text.replace("\r\n", "\n").split("\n\n");
                String data = null;
                for (String block : blocks) {
                    StringBuilder candidate = new StringBuilder();
                    for (String line : block.split("\n")) {
                        if (line.startsWith("data:")) {
                            String value = line.substring(5);
                            if (value.startsWith(" ")) {
                                value = value.substring(1);
                            }
                            if (candidate.length() > 0) {
                                candidate.append('\n');
                            }
                            candidate.append(value);
                        }
                    }
                    if (candidate.length() > 0) {
                        if (Objects.nonNull(data)) {
                            throw new IllegalArgumentException("Only one final response supported");
                        }
                        data = candidate.toString();
                    }
                }
                if (Objects.isNull(data)) {
                    throw new IllegalArgumentException("Missing final SSE response");
                }
                text = data;
            } else if (!"application/json".equals(contentType)) {
                throw new IllegalArgumentException("Unsupported upstream content type");
            }
            JsonNode result = JSON.readTree(text);
            if (!result.isObject()) {
                throw new IllegalArgumentException("Invalid envelope");
            }
            return (ObjectNode) result;
        } catch (Exception e) {
            throw new IllegalArgumentException("Invalid upstream response");
        }
    }

    @Override
    public int pending() {
        return active.get();
    }

    @Override
    public Mono<Void> closeGracefully() {
        return shutdown;
    }

    private Mono<Void> terminate() {
        closed = true;
        // Whole-client close is shutdown only, never the individual request cancellation path.
        for (Future<?> future : inFlight) {
            future.cancel(true);
        }
        if (Objects.isNull(session)) {
            return Mono.fromRunnable(this::closeTransport);
        }
        HttpRequest request = outboundRequest("DELETE", new byte[0], Map.of());
        return Mono.defer(() -> requestSignal(http.execute(
                        new HttpHost(route.endpoint().getHost(), route.endpoint().getPort(), route.endpoint().getScheme()), request,
                        Duration.ofSeconds(2).toMillis())))
            .timeout(Duration.ofSeconds(2))
            .flatMap(response -> response.status() >= 200 && response.status() < 300
                    ? Mono.<Void>empty()
                    : Mono.error(new IllegalStateException("Session cleanup HTTP " + response.status())))
            .doFinally(ignored -> closeTransport());
    }

    static <T> Mono<T> requestSignal(final ApacheRemoteMcpTransport.AsyncRequestHandle<T> request) {
        return request.signal()
            .onErrorMap(CompletionException.class, RequestScopedMcpClient::unwrapCompletionException)
            .doFinally(type -> {
                if (type == SignalType.CANCEL) {
                    request.cancel(true);
                }
            });
    }

    private static Throwable unwrapCompletionException(final CompletionException error) {
        return Objects.requireNonNullElseGet(error.getCause(), () -> error);
    }

    private void closeTransport() {
        try {
            http.close();
        } catch (Exception error) {
            throw new IllegalStateException("Remote MCP transport cleanup failed", error);
        }
    }

}
