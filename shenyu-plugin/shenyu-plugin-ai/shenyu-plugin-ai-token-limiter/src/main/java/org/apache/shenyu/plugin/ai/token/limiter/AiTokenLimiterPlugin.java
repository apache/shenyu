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

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.AiTokenLimiterHandle;
import org.apache.shenyu.common.enums.AiTokenLimiterEnum;
import org.apache.shenyu.common.enums.PluginEnum;
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

    private static final Pattern COMPLETION_TOKENS_PATTERN = Pattern.compile("\"completion_tokens\"\\s*:\\s*(\\d+)");

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

        final AiStatisticServerHttpResponse loggingServerHttpResponse = new AiStatisticServerHttpResponse(exchange, exchange.getResponse(),
                tokens -> recordTokensUsage(reactiveRedisTemplate,
                        cacheKey,
                        tokens,
                        timeWindowSeconds));

        // check if the request is allowed
        return isAllowed(reactiveRedisTemplate, cacheKey, tokenLimit)
                .flatMap(allowed -> {
                    if (!allowed) {
                        exchange.getResponse().setStatusCode(HttpStatus.TOO_MANY_REQUESTS);
                        final Consumer<HttpStatusCode> consumer = exchange.getAttribute(Constants.METRICS_RATE_LIMITER);
                        Optional.ofNullable(consumer).ifPresent(c -> c.accept(exchange.getResponse().getStatusCode()));
                        Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.RUN_OUT_OF_TOKENS);
                        return WebFluxResultUtils.result(exchange, error);
                    }
                    // record tokens usage
                    ServerWebExchange mutatedExchange = exchange.mutate()
                            .response(loggingServerHttpResponse)
                            .build();

                    return chain.execute(mutatedExchange);
                });

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

        private final AtomicBoolean streamingUsageRecorded = new AtomicBoolean(false);

        AiStatisticServerHttpResponse(final ServerWebExchange exchange, final ServerHttpResponse delegate, final Consumer<Long> tokensRecorder) {
            super(delegate);
            this.exchange = exchange;
            this.serverHttpResponse = delegate;
            this.tokensRecorder = tokensRecorder;
        }

        @Override
        @NonNull
        public Mono<Void> writeWith(@NonNull final Publisher<? extends DataBuffer> body) {
            return super.writeWith(appendResponse(body));
        }

        @Override
        @NonNull
        public Mono<Void> writeAndFlushWith(@NonNull final Publisher<? extends Publisher<? extends DataBuffer>> body) {
            Flux<? extends Publisher<? extends DataBuffer>> intercepted = Flux.from(body)
                    .map(this::appendResponse);
            return super.writeAndFlushWith(intercepted);
        }

        @NonNull
        private Flux<? extends DataBuffer> appendResponse(final Publisher<? extends DataBuffer> body) {
            BodyWriter writer = new BodyWriter();
            HttpHeaders headers = serverHttpResponse.getHeaders();
            boolean isGzip = headers.containsKey(Constants.CONTENT_ENCODING)
                    && headers.getFirst(Constants.CONTENT_ENCODING)
                    .contains(Constants.HTTP_ACCEPT_ENCODING_GZIP);

            final GzipStreamDecoder decoder = isGzip ? new GzipStreamDecoder() : null;

            return Flux.<DataBuffer>from(body)
                    .doOnNext(buffer -> {
                        try (DataBuffer.ByteBufferIterator it = buffer.readableByteBuffers()) {
                            it.forEachRemaining(bb -> {
                                ByteBuffer ro = bb.asReadOnlyBuffer();
                                byte[] inBytes = new byte[ro.remaining()];
                                ro.get(inBytes);

                                byte[] processedBytes = isGzip ? decoder.decode(inBytes) : inBytes;
                                if (processedBytes.length > 0) {
                                    processChunk(processedBytes, writer);
                                }
                            });
                        } catch (Exception e) {
                            LOG.error("read dataBuffer error", e);
                        }
                    })
                    .doFinally(signal -> {
                        if (Objects.nonNull(decoder)) {
                            decoder.close();
                        }
                        if (!streamingUsageRecorded.get()) {
                            String sse = writer.output();
                            long usageTokens = extractUsageTokensFromSse(sse);
                            tokensRecorder.accept(usageTokens);
                        }
                    });
        }

        private void processChunk(final byte[] processedBytes, final BodyWriter writer) {
            String chunk = new String(processedBytes, StandardCharsets.UTF_8);
            for (String line : chunk.split("\\r?\\n")) {
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
                    JsonNode node = MAPPER.readTree(payload);
                    JsonNode usage = node.get(Constants.USAGE);
                    if (Objects.nonNull(usage) && usage.has(Constants.COMPLETION_TOKENS)) {
                        long c = usage.get(Constants.COMPLETION_TOKENS).asLong();
                        tokensRecorder.accept(c);
                        streamingUsageRecorded.set(true);
                    }
                } catch (Exception e) {
                    LOG.error("Failed to parse AI response JSON payload", e);
                }
            }
            writer.write(ByteBuffer.wrap(processedBytes));
        }

        private long extractUsageTokensFromSse(final String sse) {
            Matcher m = COMPLETION_TOKENS_PATTERN.matcher(sse);
            long last = 0L;
            while (m.find()) {
                last = Long.parseLong(m.group(1));
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
