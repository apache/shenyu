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

import com.fasterxml.jackson.core.JsonParser;
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
import org.springframework.data.redis.core.script.RedisScript;
import org.springframework.http.HttpCookie;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.MediaType;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import org.springframework.util.Assert;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.util.annotation.NonNull;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.OptionalLong;
import java.util.function.Consumer;

/**
 * Shenyu ai token limiter plugin.
 */
public class AiTokenLimiterPlugin extends AbstractShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(AiTokenLimiterPlugin.class);

    private static final String REDIS_KEY_PREFIX = "SHENYU:AI:TOKENLIMIT:";

    private static final RedisScript<Long> TOKEN_USAGE_SCRIPT = RedisScript.of(
            "local ttl = redis.call('PTTL', KEYS[1])\n"
                    + "if ttl == -2 then\n"
                    + "  redis.call('SET', KEYS[1], '0', 'EX', ARGV[2])\n"
                    + "elseif ttl == -1 then\n"
                    + "  redis.call('EXPIRE', KEYS[1], ARGV[2])\n"
                    + "end\n"
                    + "return redis.call('INCRBY', KEYS[1], ARGV[1])", Long.class);

    /**
     * The envelope of an OpenAI Responses terminal event, which carries the usage of a response. Kept local because a
     * bare {@code response} name in the shared {@link Constants} would read as an HTTP response.
     */
    private static final String RESPONSES_EVENT_OBJECT = "response";

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
                        timeWindowSeconds).subscribe(unused -> { }, error -> LOG.error("Failed to record AI token usage", error)));

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

    Mono<Long> recordTokensUsage(final ReactiveRedisTemplate<String, String> reactiveRedisTemplate, final String cacheKey, final Long tokens, final Long windowSeconds) {
        if (windowSeconds <= 0) {
            return Mono.error(new IllegalArgumentException("AI token accounting window must be positive"));
        }
        // Redis executes increment and expiry repair atomically. A live fixed window, including PTTL == 0,
        // keeps its deadline; a new or legacy persistent counter receives an expiry in the same operation.
        return reactiveRedisTemplate.execute(TOKEN_USAGE_SCRIPT, List.of(cacheKey), List.of(tokens.toString(), windowSeconds.toString())).single();
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

        private static final int MAX_USAGE_PAYLOAD_SIZE = 64 * 1024;

        private static final ObjectMapper MAPPER = new ObjectMapper().enable(JsonParser.Feature.STRICT_DUPLICATE_DETECTION)
                .enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS);

        private final ServerWebExchange exchange;

        private final ServerHttpResponse serverHttpResponse;

        private final Consumer<Long> tokensRecorder;

        AiStatisticServerHttpResponse(final ServerWebExchange exchange, final ServerHttpResponse delegate, final Consumer<Long> tokensRecorder) {
            super(delegate);
            this.exchange = exchange;
            this.serverHttpResponse = delegate;
            this.tokensRecorder = tokensRecorder;
        }

        @Override
        @NonNull
        public Mono<Void> writeWith(@NonNull final Publisher<? extends DataBuffer> body) {
            return Mono.defer(() -> {
                final ResponseUsageExtractor extractor = new ResponseUsageExtractor(isEventStream());
                final GzipStreamDecoder decoder = createDecoder();
                return super.writeWith(appendResponse(body, extractor, decoder))
                        .doFinally(signal -> finishResponse(extractor, decoder));
            });
        }

        @Override
        @NonNull
        public Mono<Void> writeAndFlushWith(@NonNull final Publisher<? extends Publisher<? extends DataBuffer>> body) {
            return Mono.defer(() -> {
                final ResponseUsageExtractor extractor = new ResponseUsageExtractor(isEventStream());
                final GzipStreamDecoder decoder = createDecoder();
                Flux<? extends Publisher<? extends DataBuffer>> intercepted = Flux.from(body)
                        .map(inner -> appendResponse(inner, extractor, decoder));
                return super.writeAndFlushWith(intercepted)
                        .doFinally(signal -> finishResponse(extractor, decoder));
            });
        }

        private boolean isEventStream() {
            MediaType contentType = serverHttpResponse.getHeaders().getContentType();
            return Objects.nonNull(contentType) && MediaType.TEXT_EVENT_STREAM.isCompatibleWith(contentType);
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
                                                        final ResponseUsageExtractor extractor, final GzipStreamDecoder decoder) {
            return Flux.<DataBuffer>from(body)
                    .doOnNext(buffer -> {
                        try (DataBuffer.ByteBufferIterator it = buffer.readableByteBuffers()) {
                            it.forEachRemaining(bb -> {
                                ByteBuffer ro = bb.asReadOnlyBuffer();
                                byte[] inBytes = new byte[ro.remaining()];
                                ro.get(inBytes);

                                byte[] processedBytes = Objects.nonNull(decoder) ? decoder.decode(inBytes) : inBytes;
                                if (processedBytes.length > 0) {
                                    extractor.accept(processedBytes);
                                }
                            });
                        } catch (Exception e) {
                            LOG.error("read dataBuffer error", e);
                        }
                    });
        }

        private void finishResponse(final ResponseUsageExtractor extractor, final GzipStreamDecoder decoder) {
            if (Objects.nonNull(decoder)) {
                decoder.close();
            }
            extractor.finish().ifPresent(tokensRecorder::accept);
        }

        /**
         * Locate the usage object of a payload. Chat and complete Responses payloads carry it at the root, while a
         * Responses terminal event carries it inside its {@code response} object.
         *
         * @param node parsed payload of a stream event or a complete body
         * @param eventStream whether the payload is an SSE event
         * @return the usage node, or null when the payload reports none
         */
        private static JsonNode usageOf(final JsonNode node, final boolean eventStream) {
            JsonNode usage = node.get(Constants.USAGE);
            if (Objects.isNull(usage) && eventStream) {
                JsonNode responsesEvent = node.get(RESPONSES_EVENT_OBJECT);
                usage = Objects.nonNull(responsesEvent) && responsesEvent.isObject()
                        ? responsesEvent.get(Constants.USAGE) : null;
            }
            return usage;
        }

        /**
         * Read the output tokens of a usage object under either protocol's field name.
         *
         * @param usage usage object of a Chat or Responses payload
         * @return the reported output tokens, or null when the object has neither field
         */
        private static JsonNode outputTokensOf(final JsonNode usage) {
            if (usage.has(Constants.COMPLETION_TOKENS)) {
                return usage.get(Constants.COMPLETION_TOKENS);
            }
            return usage.has(Constants.OUTPUT_TOKENS) ? usage.get(Constants.OUTPUT_TOKENS) : null;
        }

        /**
         * Bounded response usage extractor for complete JSON bodies and incrementally framed SSE events.
         */
        static final class ResponseUsageExtractor {

            private final boolean eventStream;

            private final ByteArrayOutputStream buffer = new ByteArrayOutputStream();

            private final StringBuilder event = new StringBuilder();

            private boolean carriageReturn;

            private boolean stopped;

            private boolean invalid;

            private boolean finished;

            private int eventBytes;

            private Long outputTokens;

            ResponseUsageExtractor(final boolean eventStream) {
                this.eventStream = eventStream;
            }

            synchronized void accept(final byte[] bytes) {
                if (stopped || invalid || finished) {
                    return;
                }
                try {
                    if (!eventStream) {
                        if (bytes.length > MAX_USAGE_PAYLOAD_SIZE - buffer.size()) {
                            invalidate();
                        } else {
                            buffer.writeBytes(bytes);
                        }
                        return;
                    }
                    for (byte value : bytes) {
                        if (stopped || invalid) {
                            break;
                        }
                        if (carriageReturn && value == '\n') {
                            carriageReturn = false;
                            continue;
                        }
                        carriageReturn = false;
                        if (++eventBytes > MAX_USAGE_PAYLOAD_SIZE) {
                            invalidate();
                            break;
                        }
                        if (value == '\r' || value == '\n') {
                            readLine();
                            carriageReturn = value == '\r';
                        } else {
                            buffer.write(value);
                        }
                    }
                } catch (Exception ignored) {
                    invalidate();
                }
            }

            synchronized OptionalLong finish() {
                if (finished) {
                    return OptionalLong.empty();
                }
                finished = true;
                if (!invalid && !eventStream && buffer.size() > 0) {
                    try {
                        capture(MAPPER.readTree(buffer.toByteArray()));
                    } catch (Exception ignored) {
                        invalidate();
                    }
                }
                OptionalLong result = !invalid && Objects.nonNull(outputTokens)
                        ? OptionalLong.of(outputTokens) : OptionalLong.empty();
                clear();
                return result;
            }

            private void readLine() throws IOException {
                String text = StandardCharsets.UTF_8.newDecoder()
                        .onMalformedInput(CodingErrorAction.REPORT)
                        .onUnmappableCharacter(CodingErrorAction.REPORT)
                        .decode(ByteBuffer.wrap(buffer.toByteArray())).toString();
                buffer.reset();
                if (text.isEmpty()) {
                    dispatch();
                    eventBytes = 0;
                } else if ("data".equals(text) || text.startsWith("data:")) {
                    String value = "data".equals(text) ? "" : text.substring("data:".length());
                    event.append(value.startsWith(" ") ? value.substring(1) : value).append('\n');
                }
            }

            private void dispatch() throws IOException {
                if (event.isEmpty()) {
                    return;
                }
                String payload = event.substring(0, event.length() - 1);
                event.setLength(0);
                if ("[DONE]".equals(payload)) {
                    stopped = true;
                } else {
                    capture(MAPPER.readTree(payload));
                }
            }

            private void capture(final JsonNode root) {
                if (Objects.isNull(root) || !root.isObject()) {
                    invalidate();
                    return;
                }
                JsonNode usage = usageOf(root, eventStream);
                if (Objects.isNull(usage) || usage.isNull()) {
                    return;
                }
                if (!usage.isObject()) {
                    invalidate();
                    return;
                }
                JsonNode outputTokenNode = outputTokensOf(usage);
                if (Objects.isNull(outputTokenNode)) {
                    outputTokens = null;
                    return;
                }
                if (!outputTokenNode.isIntegralNumber() || !outputTokenNode.canConvertToLong()
                        || outputTokenNode.longValue() < 0) {
                    invalidate();
                    return;
                }
                outputTokens = outputTokenNode.longValue();
            }

            private void invalidate() {
                invalid = true;
                outputTokens = null;
                clear();
            }

            private void clear() {
                buffer.reset();
                event.setLength(0);
            }
        }
    }
}
