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
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import io.modelcontextprotocol.common.McpTransportContext;
import reactor.core.publisher.Mono;
import reactor.core.publisher.SignalType;
import reactor.core.publisher.Sinks;

import javax.net.ssl.SSLContext;
import java.io.ByteArrayOutputStream;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.ByteBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.time.Instant;
import java.util.Base64;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.Flow;
import java.util.concurrent.TimeoutException;
import java.util.function.Function;

/** Bounded HTTP exchanges and JSON/SSE framing shared by the upstream protocol adapters. */
final class RemoteMcpHttpSupport {

    static final ObjectMapper JSON = new ObjectMapper().enable(JsonParser.Feature.STRICT_DUPLICATE_DETECTION)
            .enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS);

    private static final int BYTE_LIMIT = 8192;

    private static final int MAX_IN_FLIGHT = 64;

    private static final String ENCODED_PREFIX = "=?base64?";

    private static final String ENCODED_SUFFIX = "?=";

    private static final int BYTE_ORDER_MARK = 0xFEFF;

    private final HttpClient http;

    private final Set<CompletableFuture<?>> inFlight = new HashSet<>();

    private volatile boolean closed;

    RemoteMcpHttpSupport(final HttpClient http) {
        this.http = http;
    }

    static HttpClient newHttpClient(final SSLContext testContext) {
        HttpClient.Builder builder = HttpClient.newBuilder().version(HttpClient.Version.HTTP_1_1).proxy(RemoteTransportPolicy.directOnly())
                .followRedirects(HttpClient.Redirect.NEVER).connectTimeout(Duration.ofSeconds(2));
        if (Objects.nonNull(testContext)) {
            builder.sslContext(testContext);
        }
        return builder.build();
    }

    Mono<HttpResponse<byte[]>> post(final HttpRequest.Builder builder, final ObjectNode message, final McpTransportContext context,
                                    final Function<byte[], HttpRequest.BodyPublisher> publisher) {
        Instant deadline = context.get("deadline") instanceof Instant value ? value : Instant.now().plusSeconds(4);
        Duration budget = Duration.between(Instant.now(), deadline);
        if (budget.isNegative() || budget.isZero()) {
            return Mono.error(new TimeoutException("Deadline expired before upstream side effect"));
        }
        byte[] bytes = message.toString().getBytes(StandardCharsets.UTF_8);
        if (bytes.length > BYTE_LIMIT) {
            return Mono.error(new IllegalArgumentException("Request byte limit"));
        }
        HttpRequest request = builder.timeout(budget)
                .header("Content-Type", "application/json")
                .header("Accept", "application/json, text/event-stream")
                .POST(publisher.apply(bytes)).build();
        return Mono.defer(() -> {
            CompletableFuture<HttpResponse<byte[]>> future;
            synchronized (inFlight) {
                if (closed) {
                    return Mono.error(new IllegalStateException("Client closed"));
                }
                if (inFlight.size() >= MAX_IN_FLIGHT) {
                    return Mono.error(new IllegalStateException("Client concurrency limit"));
                }
                future = http.sendAsync(request, ignored -> new BoundedBody(BYTE_LIMIT));
                inFlight.add(future);
            }
            return requestSignal(future).timeout(budget).doFinally(ignored -> {
                future.cancel(true);
                synchronized (inFlight) {
                    inFlight.remove(future);
                }
            });
        });
    }

    Mono<HttpResponse<Void>> discard(final HttpRequest request) {
        return Mono.defer(() -> requestSignal(http.sendAsync(request, HttpResponse.BodyHandlers.discarding())))
                .timeout(request.timeout().orElseThrow());
    }

    int pending() {
        synchronized (inFlight) {
            return inFlight.size();
        }
    }

    boolean isClosed() {
        return closed;
    }

    void closeRequests() {
        List<CompletableFuture<?>> pending;
        synchronized (inFlight) {
            closed = true;
            pending = List.copyOf(inFlight);
        }
        pending.forEach(future -> future.cancel(true));
    }

    static String encodeMirroredName(final String value) {
        if (Objects.isNull(value) || value.isEmpty()) {
            throw new IllegalArgumentException("Missing upstream tool name");
        }
        if (value.charAt(0) <= ' ' || value.charAt(value.length() - 1) <= ' '
                || (value.startsWith(ENCODED_PREFIX) && value.endsWith(ENCODED_SUFFIX))
                || value.chars().anyMatch(c -> c > '~' || (c < ' ' && c != '\t'))) {
            if (!StandardCharsets.UTF_8.newEncoder().canEncode(value)) {
                throw new IllegalArgumentException("Invalid upstream tool name");
            }
            return ENCODED_PREFIX + Base64.getEncoder().encodeToString(value.getBytes(StandardCharsets.UTF_8)) + ENCODED_SUFFIX;
        }
        return value;
    }

    /**
     * Decode a UTF-8 JSON-RPC envelope.
     * Callers must pass the normalized media type ({@code application/json} or {@code text/event-stream}),
     * with parameters such as {@code ;charset=...} already stripped.
     *
     * @param bytes response body bytes
     * @param contentType normalized media type without parameters
     * @param requireMessageEvent whether SSE frames must be complete message events
     * @return decoded JSON-RPC envelope
     */
    static ObjectNode decodeEnvelope(final byte[] bytes, final String contentType, final boolean requireMessageEvent) {
        try {
            String text = StandardCharsets.UTF_8.newDecoder()
                .onMalformedInput(CodingErrorAction.REPORT)
                .onUnmappableCharacter(CodingErrorAction.REPORT)
                .decode(ByteBuffer.wrap(bytes))
                .toString();
            if ("text/event-stream".equals(contentType)) {
                text = sseData(text, requireMessageEvent);
            } else if (!"application/json".equals(contentType)) {
                throw new IllegalArgumentException("Unsupported upstream content type");
            }
            JsonNode result = JSON.readTree(text);
            if (Objects.isNull(result) || !result.isObject()) {
                throw new IllegalArgumentException("Invalid envelope");
            }
            return (ObjectNode) result;
        } catch (JsonProcessingException | CharacterCodingException | IllegalArgumentException error) {
            throw new IllegalArgumentException("Invalid upstream response");
        }
    }

    private static String sseData(final String text, final boolean requireMessageEvent) {
        String stream = text.replace("\r\n", "\n");
        if (requireMessageEvent) {
            stream = stream.replace('\r', '\n');
            if (!stream.isEmpty() && stream.charAt(0) == BYTE_ORDER_MARK) {
                stream = stream.substring(1);
            }
        }
        String[] blocks = stream.split("\n\n", -1);
        String result = null;
        for (int i = 0; i < blocks.length; i++) {
            String data = eventData(blocks[i], i < blocks.length - 1, requireMessageEvent);
            if (Objects.nonNull(data)) {
                if (Objects.nonNull(result)) {
                    throw new IllegalArgumentException("Only one final response supported");
                }
                result = data;
            }
        }
        if (Objects.isNull(result)) {
            throw new IllegalArgumentException("Missing final SSE response");
        }
        return result;
    }

    private static String eventData(final String block, final boolean terminated, final boolean requireMessageEvent) {
        StringBuilder data = new StringBuilder();
        String event = "";
        boolean hasData = false;
        for (String line : block.split("\n")) {
            if (line.startsWith("data:") || requireMessageEvent && "data".equals(line)) {
                if (hasData) {
                    data.append('\n');
                }
                data.append(sseValue(line));
                hasData = requireMessageEvent || data.length() > 0;
            } else if (requireMessageEvent && (line.startsWith("event:") || "event".equals(line))) {
                event = sseValue(line);
            }
        }
        if (!hasData) {
            return null;
        }
        if (requireMessageEvent && (!terminated || !event.isEmpty() && !"message".equals(event))) {
            throw new IllegalArgumentException("Expected a complete SSE message event");
        }
        return data.toString();
    }

    private static String sseValue(final String line) {
        int colon = line.indexOf(':');
        if (colon < 0) {
            return "";
        }
        return line.substring(colon + (line.startsWith(" ", colon + 1) ? 2 : 1));
    }

    static <T> Mono<T> requestSignal(final CompletableFuture<T> request) {
        Sinks.One<T> signal = Sinks.one();
        request.whenComplete((response, error) -> {
            Throwable failure = error;
            while (failure instanceof CompletionException && Objects.nonNull(failure.getCause())) {
                failure = failure.getCause();
            }
            if (Objects.nonNull(failure)) {
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
