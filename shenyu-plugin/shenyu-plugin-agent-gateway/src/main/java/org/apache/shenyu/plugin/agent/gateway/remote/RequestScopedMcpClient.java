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
import java.io.ByteArrayOutputStream;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.ByteBuffer;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Flow;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import reactor.core.publisher.Mono;
import reactor.core.publisher.Sinks;
import reactor.core.publisher.SignalType;

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

    private final HttpClient http;

    private final URI endpoint;

    private final String name;

    private final String authorization;

    private final java.util.function.Consumer<Map<String, Object>> observer;

    private final String prefix = UUID.randomUUID().toString();

    private final AtomicLong sequence = new AtomicLong();

    private final AtomicInteger active = new AtomicInteger();

    private final Set<CompletableFuture<?>> inFlight = ConcurrentHashMap.newKeySet();

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
        HttpClient.Builder builder = HttpClient.newBuilder().version(HttpClient.Version.HTTP_1_1).proxy(RemoteTransportPolicy.directOnly())
                .followRedirects(HttpClient.Redirect.NEVER).connectTimeout(Duration.ofSeconds(2));
        if (java.util.Objects.nonNull(testContext)) {
            builder.sslContext(testContext);
        }
        http = builder.build();
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
            .contextWrite(context -> context.delete(McpTransportContext.KEY))
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
                request("tools/list", java.util.Objects.isNull(cursor) ? Map.of() : Map.of("cursor", cursor), false).map(result ->
                    JSON.convertValue(result, McpSchema.ListToolsResult.class)
                )
            )
        );
    }

    @Override
    public Mono<McpSchema.CallToolResult> callTool(final McpSchema.CallToolRequest request) {
        return callRaw(request).map(result -> JSON.convertValue(result, McpSchema.CallToolResult.class));
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
                return Mono.error(new IllegalStateException("Deadline expired before upstream side effect"));
            }
            byte[] requestBytes = message.toString().getBytes(StandardCharsets.UTF_8);
            if (requestBytes.length > MAX_REQUEST_BYTES) {
                return Mono.error(new IllegalArgumentException("Request byte limit"));
            }
            HttpRequest.Builder builder = HttpRequest.newBuilder(endpoint)
                .timeout(budget)
                .header("Content-Type", "application/json")
                .header("Accept", "application/json, text/event-stream")
                .header("Authorization", authorization)
                .header("MCP-Protocol-Version", VERSION)
                .POST(HttpRequest.BodyPublishers.ofByteArray(requestBytes));
            if (java.util.Objects.nonNull(session) && !"initialize".equals(method)) {
                builder.header("MCP-Session-Id", session);
            }
            HttpRequest outbound = builder.build();
            if (active.incrementAndGet() > MAX_IN_FLIGHT) {
                active.decrementAndGet();
                return Mono.error(new IllegalStateException("Client concurrency limit"));
            }
            var owned = new java.util.concurrent.atomic.AtomicReference<CompletableFuture<HttpResponse<byte[]>>>();
            return Mono.defer(() -> {
                CompletableFuture<HttpResponse<byte[]>> future = http.sendAsync(outbound, ignored -> new BoundedBody(LIMIT));
                owned.set(future);
                inFlight.add(future);
                if (closed) {
                    future.cancel(true);
                }
                return requestSignal(future);
            })
                .timeout(budget)
                .map(response -> decodeResponse(response, method, id, notification))
                .doFinally(ignored -> {
                    CompletableFuture<?> future = owned.get();
                    if (java.util.Objects.nonNull(future)) {
                        future.cancel(true);
                        inFlight.remove(future);
                    }
                    active.decrementAndGet();
                });
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
            return JSON.createObjectNode();
        }
        String contentType = response.headers().firstValue("Content-Type").orElse("").split(";")[0].trim();
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
                        if (java.util.Objects.nonNull(data)) {
                            throw new IllegalArgumentException("Only one final response supported");
                        }
                        data = candidate.toString();
                    }
                }
                if (java.util.Objects.isNull(data)) {
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
        for (CompletableFuture<?> future : inFlight) {
            future.cancel(true);
        }
        if (java.util.Objects.isNull(session)) {
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
        return Mono.defer(() -> {
            var future = http.sendAsync(request, HttpResponse.BodyHandlers.discarding());
            return requestSignal(future).doFinally(ignored -> future.cancel(true));
        })
            .timeout(Duration.ofSeconds(2))
            .flatMap(response ->
                response.statusCode() >= 200 && response.statusCode() < 300
                    ? Mono.<Void>empty()
                    : Mono.error(new IllegalStateException("Session cleanup HTTP " + response.statusCode()))
            );
    }

    static <T> Mono<T> requestSignal(final CompletableFuture<T> request) {
        Sinks.One<T> signal = Sinks.one();
        request.whenComplete((response, error) -> {
            Throwable failure = error;
            while (failure instanceof CompletionException && java.util.Objects.nonNull(failure.getCause())) {
                failure = failure.getCause();
            }
            if (java.util.Objects.nonNull(failure)) {
                // Retain active errors, but a detached request has no subscriber to receive late failures.
                signal.tryEmitError(failure);
            } else {
                signal.tryEmitValue(response);
            }
        });
        return signal.asMono().doFinally(type -> {
            if (type == SignalType.CANCEL) {
                request.cancel(true);
            }
        });
    }

    private static final class BoundedBody implements HttpResponse.BodySubscriber<byte[]> {

        private final int limit;

        private final ByteArrayOutputStream bytes = new ByteArrayOutputStream();

        private final CompletableFuture<byte[]> result = new CompletableFuture<>();

        private Flow.Subscription subscription;

        private BoundedBody(final int limit) {
            this.limit = limit;
        }

        @Override
        public CompletionStage<byte[]> getBody() {
            return result;
        }

        @Override
        public void onSubscribe(final Flow.Subscription value) {
            subscription = value;
            value.request(1);
        }

        @Override
        public void onNext(final List<ByteBuffer> parts) {
            long incoming = parts.stream().mapToLong(ByteBuffer::remaining).sum();
            if (incoming > limit - bytes.size()) {
                subscription.cancel();
                result.completeExceptionally(new IllegalArgumentException("Response byte limit"));
                return;
            }
            for (ByteBuffer part : parts) {
                byte[] chunk = new byte[part.remaining()];
                part.get(chunk);
                bytes.writeBytes(chunk);
            }
            subscription.request(1);
        }

        @Override
        public void onError(final Throwable error) {
            result.completeExceptionally(error);
        }

        @Override
        public void onComplete() {
            result.complete(bytes.toByteArray());
        }
    }
}
