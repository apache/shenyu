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

package org.apache.shenyu.plugin.ai.token.limiter;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.AiTokenLimiterHandle;
import org.apache.shenyu.common.enums.AiTokenLimiterEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.metrics.AiTokenBudgetObserver;
import org.apache.shenyu.plugin.ai.token.limiter.budget.RedisTokenBudgetStore;
import org.apache.shenyu.plugin.ai.token.limiter.budget.RedisTokenBudgetStore.ReservationResult;
import org.apache.shenyu.plugin.ai.token.limiter.budget.TokenBudgetRequest;
import org.apache.shenyu.plugin.ai.token.limiter.handler.AiTokenLimiterPluginHandler;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.reactivestreams.Publisher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.data.redis.core.ReactiveRedisTemplate;
import org.springframework.data.redis.core.ReactiveValueOperations;
import org.springframework.http.HttpCookie;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import org.springframework.util.Assert;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.util.annotation.NonNull;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Shenyu ai token limiter plugin.
 */
public class AiTokenLimiterPlugin extends AbstractShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(AiTokenLimiterPlugin.class);

    private static final String REDIS_KEY_PREFIX = "SHENYU:AI:TOKENLIMIT:";

    /**
     * The envelope of an OpenAI Responses terminal event, which carries the usage of a response. Kept local because a
     * bare {@code response} name in the shared {@link Constants} would read as an HTTP response.
     */
    private static final String RESPONSES_EVENT_OBJECT = "response";

    /**
     * Output tokens of the Chat and Responses protocols. The leading quote keeps {@code max_output_tokens}, a request
     * echo, from being read as an accounted count.
     */
    private static final Pattern OUTPUT_TOKENS_PATTERN = Pattern.compile("\"(?:completion|output)_tokens\"\\s*:\\s*(\\d+)");

    private static final Pattern SSE_LINE_PATTERN = Pattern.compile("\\r?\\n");

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                   final SelectorData selector, final RuleData rule) {

        AiTokenLimiterHandle aiTokenLimiterHandle = AiTokenLimiterPluginHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));

        if (Objects.isNull(aiTokenLimiterHandle)) {
            return chain.execute(exchange);
        }

        ReactiveRedisTemplate<String, String> reactiveRedisTemplate = AiTokenLimiterPluginHandler.REDIS_CACHED_HANDLE.get().obtainHandle(PluginEnum.AI_TOKEN_LIMITER.getName());
        Assert.notNull(reactiveRedisTemplate, "reactiveRedisTemplate is null");

        // generate redis key - include rule id to scope counters per rule
        String tokenLimitType = aiTokenLimiterHandle.getAiTokenLimitType();
        String keyName = aiTokenLimiterHandle.getKeyName();
        Long tokenLimit = aiTokenLimiterHandle.getTokenLimit();
        Long timeWindowSeconds = aiTokenLimiterHandle.getTimeWindowSeconds();

        String cacheKey = REDIS_KEY_PREFIX + CacheKeyUtils.INST.getKey(rule) + ":" + getCacheKey(exchange, tokenLimitType, keyName);

        if (aiTokenLimiterHandle.isReservationEnabled()) {
            return executeWithReservation(exchange, chain, reactiveRedisTemplate, cacheKey, aiTokenLimiterHandle);
        }

        final AiStatisticServerHttpResponse loggingServerHttpResponse = new AiStatisticServerHttpResponse(exchange, exchange.getResponse(),
                tokens -> recordTokensUsage(reactiveRedisTemplate,
                        cacheKey,
                        tokens,
                        timeWindowSeconds));

        // check if the request is allowed
        return isAllowed(reactiveRedisTemplate, cacheKey, tokenLimit)
                .flatMap(allowed -> {
                    if (!allowed) {
                        return rejectAdmission(exchange);
                    }
                    // record tokens usage
                    ServerWebExchange mutatedExchange = exchange.mutate()
                            .response(loggingServerHttpResponse)
                            .build();

                    return chain.execute(mutatedExchange);
                });

    }

    private Mono<Void> executeWithReservation(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                              final ReactiveRedisTemplate<String, String> redisTemplate, final String scope,
                                              final AiTokenLimiterHandle handle) {
        // Snapshot the rule: a config update must not change an in-flight request's budget contract.
        final long limit = handle.getTokenLimit();
        final long amount = handle.getReservationTokens();
        final Duration windowSize = Duration.ofSeconds(handle.getTimeWindowSeconds());
        final RedisTokenBudgetStore store = new RedisTokenBudgetStore(redisTemplate);
        return Mono.usingWhen(Mono.fromSupplier(() -> {
            final Object callback = exchange.getAttribute(Constants.METRICS_AI_TOKEN_BUDGET);
            return new TokenBudgetRequest(store, callback instanceof AiTokenBudgetObserver observer ? observer : null);
        }),
                request -> request.reserve(scope, windowSize, limit, amount).flatMap(result -> {
                    if (result == ReservationResult.REJECTED) {
                        return rejectAdmission(exchange);
                    }
                    if (result != ReservationResult.RESERVED) {
                        return Mono.error(new IllegalStateException("Unexpected repeated AI token reservation: " + result));
                    }
                    return Mono.defer(() -> {
                        if (!request.beginForwarding()) {
                            return Mono.empty();
                        }
                        final AiStatisticServerHttpResponse response = new AiStatisticServerHttpResponse(exchange, exchange.getResponse(), request::recordUsage, true);
                        return chain.execute(exchange.mutate().response(response).build());
                    });
                }),
                this::finishReservation,
                (request, failure) -> finishReservation(request).onErrorResume(cleanupFailure -> {
                    if (failure != cleanupFailure) {
                        failure.addSuppressed(cleanupFailure);
                    }
                    return Mono.empty();
                }),
                request -> finishReservation(request).onErrorResume(failure -> Mono.empty()));
    }

    private Mono<Void> finishReservation(final TokenBudgetRequest request) {
        return request.finish().timeout(Duration.ofSeconds(5))
                .doOnError(failure -> {
                    request.accountingFailed();
                    LOG.error("Failed to reconcile AI token reservation; window expiry bounds retained capacity", failure);
                });
    }

    private Mono<Void> rejectAdmission(final ServerWebExchange exchange) {
        exchange.getResponse().setStatusCode(HttpStatus.TOO_MANY_REQUESTS);
        final Consumer<HttpStatusCode> consumer = exchange.getAttribute(Constants.METRICS_RATE_LIMITER);
        Optional.ofNullable(consumer).ifPresent(c -> c.accept(exchange.getResponse().getStatusCode()));
        return WebFluxResultUtils.result(exchange, ShenyuResultWrap.error(exchange, ShenyuResultEnum.RUN_OUT_OF_TOKENS));
    }

    /**
     * Check if the request is allowed based on rate limiting rules.
     *
     * @param reactiveRedisTemplate the reactive Redis template
     * @param cacheKey the cache key for the request
     * @param tokenLimit the token limit for the request
     * @return whether the request is allowed
     */
    private Mono<Boolean> isAllowed(final ReactiveRedisTemplate<String, String> reactiveRedisTemplate, final String cacheKey, final Long tokenLimit) {

        return reactiveRedisTemplate.opsForValue().get(cacheKey)
                .defaultIfEmpty("0")
                .flatMap(currentTokens -> {
                    if (Long.parseLong(currentTokens) >= tokenLimit) {
                        return Mono.just(false);
                    }
                    return Mono.just(true);
                });
    }

    /**
     * Get the cache key based on the configured key resolver type.
     *
     * @param exchange the server web exchange
     * @param tokenLimitType the type of token limit
     * @param keyName the name of the key
     * @return the cache key
     */
    private String getCacheKey(final ServerWebExchange exchange, final String tokenLimitType, final String keyName) {
        ServerHttpRequest request = exchange.getRequest();
        String key;
        // Determine the key based on the configured key resolver type
        AiTokenLimiterEnum tokenLimiterEnum = AiTokenLimiterEnum.getByName(tokenLimitType);

        key = switch (tokenLimiterEnum) {
            case IP -> Objects.requireNonNull(request.getRemoteAddress()).getHostString();
            case URI -> request.getURI().getPath();
            case HEADER -> request.getHeaders().getFirst(keyName);
            case PARAMETER -> request.getQueryParams().getFirst(keyName);
            case COOKIE -> {
                HttpCookie cookie = request.getCookies().getFirst(keyName);
                yield Objects.nonNull(cookie) ? cookie.getValue() : "";
            }
            default -> exchange.getAttribute(Constants.CONTEXT_PATH);
        };

        return StringUtils.isBlank(key) ? "" : key;
    }

    private void recordTokensUsage(final ReactiveRedisTemplate<String, String> reactiveRedisTemplate, final String cacheKey, final Long tokens, final Long windowSeconds) {
        // The counter is given its window when it is created: re-issuing the expiration after every increment would
        // push the window forward, so a sustained traffic would never reset the token budget. An existing counter
        // keeps the window it was created with; only one without any expiration (written by an earlier version or
        // by another path) is given one, and just once.
        final Duration window = Duration.ofSeconds(windowSeconds);
        final ReactiveValueOperations<String, String> valueOperations = reactiveRedisTemplate.opsForValue();
        valueOperations.setIfAbsent(cacheKey, "0", window)
                .flatMap(created -> created ? Mono.just(Boolean.TRUE) : reactiveRedisTemplate.getExpire(cacheKey)
                        .filter(timeToLive -> timeToLive.isNegative() || timeToLive.isZero())
                        .flatMap(timeToLive -> reactiveRedisTemplate.expire(cacheKey, window)))
                .then(valueOperations.increment(cacheKey, tokens))
                .subscribe();
    }

    @Override
    public int getOrder() {
        return PluginEnum.AI_TOKEN_LIMITER.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.AI_TOKEN_LIMITER.getName();
    }

    static class AiStatisticServerHttpResponse extends ServerHttpResponseDecorator {
        private static final ObjectMapper MAPPER = new ObjectMapper();

        private final ServerWebExchange exchange;

        private final ServerHttpResponse serverHttpResponse;

        private final Consumer<Long> tokensRecorder;

        private final boolean reservationMode;

        private final AtomicBoolean streamingUsageRecorded = new AtomicBoolean(false);

        AiStatisticServerHttpResponse(final ServerWebExchange exchange, final ServerHttpResponse delegate, final Consumer<Long> tokensRecorder) {
            this(exchange, delegate, tokensRecorder, false);
        }

        AiStatisticServerHttpResponse(final ServerWebExchange exchange, final ServerHttpResponse delegate, final Consumer<Long> tokensRecorder, final boolean reservationMode) {
            super(delegate);
            this.exchange = exchange;
            this.serverHttpResponse = delegate;
            this.tokensRecorder = tokensRecorder;
            this.reservationMode = reservationMode;
        }

        @Override
        @NonNull
        public Mono<Void> writeWith(@NonNull final Publisher<? extends DataBuffer> body) {
            return Mono.defer(() -> {
                final BodyWriter writer = new BodyWriter();
                final GzipStreamDecoder decoder = createDecoder();
                return super.writeWith(appendResponse(body, writer, decoder))
                        .doOnSuccess(unused -> recordResponseUsage(writer))
                        .doFinally(signal -> finishResponse(writer, decoder));
            });
        }

        @Override
        @NonNull
        public Mono<Void> writeAndFlushWith(@NonNull final Publisher<? extends Publisher<? extends DataBuffer>> body) {
            return Mono.defer(() -> {
                final BodyWriter writer = new BodyWriter();
                final GzipStreamDecoder decoder = createDecoder();
                Flux<? extends Publisher<? extends DataBuffer>> intercepted = Flux.from(body)
                        .map(inner -> appendResponse(inner, writer, decoder));
                return super.writeAndFlushWith(intercepted)
                        .doOnSuccess(unused -> recordResponseUsage(writer))
                        .doFinally(signal -> finishResponse(writer, decoder));
            });
        }

        private GzipStreamDecoder createDecoder() {
            HttpHeaders headers = serverHttpResponse.getHeaders();
            boolean isGzip = headers.containsKey(Constants.CONTENT_ENCODING)
                    && headers.getFirst(Constants.CONTENT_ENCODING)
                    .contains(Constants.HTTP_ACCEPT_ENCODING_GZIP);
            return isGzip ? new GzipStreamDecoder() : null;
        }

        @NonNull
        private Flux<? extends DataBuffer> appendResponse(final Publisher<? extends DataBuffer> body,
                                                        final BodyWriter writer, final GzipStreamDecoder decoder) {
            return Flux.<DataBuffer>from(body)
                    .doOnNext(buffer -> {
                        try (DataBuffer.ByteBufferIterator it = buffer.readableByteBuffers()) {
                            it.forEachRemaining(bb -> {
                                ByteBuffer ro = bb.asReadOnlyBuffer();
                                byte[] inBytes = new byte[ro.remaining()];
                                ro.get(inBytes);

                                byte[] processedBytes = Objects.nonNull(decoder) ? decoder.decode(inBytes) : inBytes;
                                if (processedBytes.length > 0) {
                                    processChunk(processedBytes, writer);
                                }
                            });
                        } catch (Exception e) {
                            LOG.error("read dataBuffer error", e);
                        }
                    });
        }

        private void recordResponseUsage(final BodyWriter writer) {
            if (reservationMode) {
                // Capture reliable usage before usingWhen starts reservation settlement.
                String body = writer.output();
                recordKnownUsage(body);
                for (String line : SSE_LINE_PATTERN.split(body)) {
                    if (line.startsWith("data:")) {
                        recordKnownUsage(line.substring("data:".length()).trim());
                    }
                }
            }
        }

        private void finishResponse(final BodyWriter writer, final GzipStreamDecoder decoder) {
            if (Objects.nonNull(decoder)) {
                decoder.close();
            }
            if (!reservationMode && !streamingUsageRecorded.get()) {
                String sse = writer.output();
                long usageTokens = extractUsageTokensFromSse(sse);
                tokensRecorder.accept(usageTokens);
            }
        }

        private void processChunk(final byte[] processedBytes, final BodyWriter writer) {
            String chunk = new String(processedBytes, StandardCharsets.UTF_8);
            for (String line : SSE_LINE_PATTERN.split(chunk)) {
                if (!line.startsWith("data:")) {
                    continue;
                }
                String payload = line.substring("data:".length()).trim();
                if (payload.isEmpty() || "[DONE]".equals(payload)) {
                    continue;
                }
                if (!payload.startsWith("{")) {
                    continue;
                }
                try {
                    JsonNode node = reservationMode ? MAPPER.reader().with(DeserializationFeature.FAIL_ON_TRAILING_TOKENS).readTree(payload) : MAPPER.readTree(payload);
                    JsonNode usage = usageOf(node);
                    JsonNode outputTokens = Objects.isNull(usage) ? null : outputTokensOf(usage);
                    if (Objects.nonNull(outputTokens)) {
                        if (reservationMode && !isKnownUsage(outputTokens)) {
                            continue;
                        }
                        tokensRecorder.accept(outputTokens.asLong());
                        streamingUsageRecorded.set(true);
                    }
                } catch (Exception e) {
                    LOG.error("Failed to parse AI response JSON payload", e);
                }
            }
            writer.write(ByteBuffer.wrap(processedBytes));
        }

        /**
         * Locate the usage object of a payload: Chat payloads carry it at the root, while a Responses terminal event
         * carries it inside its {@code response} object.
         *
         * @param node parsed payload of a stream event or a complete body
         * @return the usage object, or null when the payload reports none
         */
        private JsonNode usageOf(final JsonNode node) {
            JsonNode usage = node.get(Constants.USAGE);
            if (Objects.isNull(usage)) {
                // Chat never uses a top-level "response" field, so the fallback cannot shadow a Chat payload.
                JsonNode responsesEvent = node.get(RESPONSES_EVENT_OBJECT);
                usage = Objects.isNull(responsesEvent) ? null : responsesEvent.get(Constants.USAGE);
            }
            return Objects.nonNull(usage) && usage.isObject() ? usage : null;
        }

        /**
         * Read the output tokens of a usage object under either protocol's field name.
         *
         * @param usage usage object of a Chat or Responses payload
         * @return the reported output tokens, or null when the object has neither field
         */
        private JsonNode outputTokensOf(final JsonNode usage) {
            if (usage.has(Constants.COMPLETION_TOKENS)) {
                return usage.get(Constants.COMPLETION_TOKENS);
            }
            return usage.has(Constants.OUTPUT_TOKENS) ? usage.get(Constants.OUTPUT_TOKENS) : null;
        }

        private void recordKnownUsage(final String body) {
            try {
                final JsonNode node = MAPPER.reader().with(DeserializationFeature.FAIL_ON_TRAILING_TOKENS).readTree(body);
                if (Objects.nonNull(node)) {
                    final JsonNode usage = usageOf(node);
                    final JsonNode tokens = Objects.isNull(usage) ? null : outputTokensOf(usage);
                    if (isKnownUsage(tokens)) {
                        tokensRecorder.accept(tokens.asLong());
                    }
                }
            } catch (Exception ignored) {
                // Partial, malformed or unsupported usage remains unknown; do not turn it into zero.
            }
        }

        private boolean isKnownUsage(final JsonNode tokens) {
            return Objects.nonNull(tokens) && tokens.isIntegralNumber() && tokens.canConvertToLong() && tokens.asLong() >= 0L && tokens.asLong() <= 9_007_199_254_740_991L;
        }

        private long extractUsageTokensFromSse(final String sse) {
            // The usage of a body is reported once, at its end, so the last match of either field is the count.
            Matcher matcher = OUTPUT_TOKENS_PATTERN.matcher(sse);
            long last = 0L;
            while (matcher.find()) {
                last = Long.parseLong(matcher.group(1));
            }
            return last;
        }

    }

    static class BodyWriter {

        private static final int MAX_BUFFER_SIZE = 64 * 1024;

        private final byte[] bytes;

        private int size;

        private final AtomicBoolean isClosed = new AtomicBoolean(false);

        BodyWriter() {
            this(MAX_BUFFER_SIZE);
        }

        BodyWriter(final int maxBufferSize) {
            if (maxBufferSize <= 0) {
                throw new IllegalArgumentException("maxBufferSize must be greater than zero");
            }
            this.bytes = new byte[maxBufferSize];
        }

        void write(final ByteBuffer source) {
            if (isClosed.get() || !source.hasRemaining()) {
                return;
            }
            int incomingSize = source.remaining();
            if (incomingSize >= bytes.length) {
                source.position(source.limit() - bytes.length);
                source.get(bytes);
                size = bytes.length;
                return;
            }
            int overflow = Math.max(0, size + incomingSize - bytes.length);
            if (overflow > 0) {
                System.arraycopy(bytes, overflow, bytes, 0, size - overflow);
                size -= overflow;
            }
            source.get(bytes, size, incomingSize);
            size += incomingSize;
        }

        boolean isEmpty() {
            return size == 0;
        }

        String output() {
            isClosed.compareAndSet(false, true);
            return new String(bytes, 0, size, StandardCharsets.UTF_8);
        }
    }
}
