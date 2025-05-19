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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.AiTokenLimiterHandle;
import org.apache.shenyu.common.enums.AiTokenLimiterEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.ai.common.strategy.AiModel;
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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.Channels;
import java.nio.channels.WritableByteChannel;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;
import java.util.zip.GZIPInputStream;

/**
 * Shenyu ai token limiter plugin.
 */
public class AiTokenLimiterPlugin extends AbstractShenyuPlugin {
    
    private static final Logger LOG = LoggerFactory.getLogger(AiTokenLimiterPlugin.class);
    
    private static final String REDIS_KEY_PREFIX = "SHENYU:AI:TOKENLIMIT:";
    
    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                   final SelectorData selector, final RuleData rule) {
        
        AiTokenLimiterHandle aiTokenLimiterHandle = AiTokenLimiterPluginHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
        
        if (Objects.isNull(aiTokenLimiterHandle)) {
            return chain.execute(exchange);
        }
        
        ReactiveRedisTemplate reactiveRedisTemplate = AiTokenLimiterPluginHandler.REDIS_CACHED_HANDLE.get().obtainHandle(PluginEnum.AI_TOKEN_LIMITER.getName());
        Assert.notNull(reactiveRedisTemplate, "reactiveRedisTemplate is null");
        
        // generate redis key
        String tokenLimitType = aiTokenLimiterHandle.getAiTokenLimitType();
        String keyName = aiTokenLimiterHandle.getKeyName();
        Long tokenLimit = aiTokenLimiterHandle.getTokenLimit();
        Long timeWindowSeconds = aiTokenLimiterHandle.getTimeWindowSeconds();
        
        String cacheKey = REDIS_KEY_PREFIX + getCacheKey(exchange, tokenLimitType, keyName);
        
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
    private Mono<Boolean> isAllowed(final ReactiveRedisTemplate reactiveRedisTemplate, final String cacheKey, final Long tokenLimit) {
        
        return reactiveRedisTemplate.opsForValue().get(cacheKey)
                .defaultIfEmpty(0L)
                .flatMap(currentTokens -> {
                    if (Long.parseLong(currentTokens.toString()) >= tokenLimit) {
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

    private void recordTokensUsage(final ReactiveRedisTemplate reactiveRedisTemplate, final String cacheKey, final Long tokens, final Long windowSeconds) {
        // Record token usage with expiration
        reactiveRedisTemplate.opsForValue()
                .increment(cacheKey, tokens)
                .flatMap(currentValue -> reactiveRedisTemplate.expire(cacheKey, Duration.ofSeconds(windowSeconds)))
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
            return super.writeWith(appendResponse(body));
        }

        @NonNull
        private Flux<? extends DataBuffer> appendResponse(final Publisher<? extends DataBuffer> body) {
            // accumulate all response bytes (compressed or plaintext)
            ByteArrayOutputStream accumulateStream = new ByteArrayOutputStream();
            HttpHeaders headers = serverHttpResponse.getHeaders();

            return Flux.<DataBuffer>from(body)
                    .doOnNext(buffer -> {
                        // add every dataBuffer bytes into accumulateStream
                        try (DataBuffer.ByteBufferIterator it = buffer.readableByteBuffers()) {
                            it.forEachRemaining(bb -> {
                                ByteBuffer ro = bb.asReadOnlyBuffer();
                                byte[] bytes = new byte[ro.remaining()];
                                ro.get(bytes);
                                accumulateStream.write(bytes, 0, bytes.length);
                            });
                        } catch (Exception e) {
                            LOG.error("failed to accumulate bytes", e);
                        }
                    })
                    .doFinally(signal -> {
                        boolean isGzip = headers.containsKey(Constants.CONTENT_ENCODING)
                                && headers.getFirst(Constants.CONTENT_ENCODING)
                                .contains(Constants.HTTP_ACCEPT_ENCODING_GZIP);

                        byte[] allBytes = accumulateStream.toByteArray();
                        String text;
                        if (isGzip) {
                            // fully decompression
                            try (GZIPInputStream gis = new GZIPInputStream(new ByteArrayInputStream(allBytes));
                                 ByteArrayOutputStream out = new ByteArrayOutputStream()) {
                                byte[] buf = new byte[4096];
                                int len;
                                while ((len = gis.read(buf)) > 0) {
                                    out.write(buf, 0, len);
                                }
                                text = out.toString(StandardCharsets.UTF_8);
                            } catch (IOException ex) {
                                LOG.error("bulk GZIP decompress failed", ex);
                                text = new String(allBytes, StandardCharsets.UTF_8);
                            }
                        } else {
                            text = new String(allBytes, StandardCharsets.UTF_8);
                        }

                        AiModel aiModel = exchange.getAttribute(Constants.AI_MODEL);
                        long tokens = Objects.requireNonNull(aiModel).getCompletionTokens(text);
                        tokensRecorder.accept(tokens);
                    });
        }
        
    }
    
    static class BodyWriter {
        
        private final ByteArrayOutputStream stream = new ByteArrayOutputStream();
        
        private final WritableByteChannel channel = Channels.newChannel(stream);
        
        private final AtomicBoolean isClosed = new AtomicBoolean(false);
        
        void write(final ByteBuffer buffer) {
            if (!isClosed.get()) {
                try {
                    channel.write(buffer);
                } catch (IOException e) {
                    isClosed.compareAndSet(false, true);
                    LOG.error("Parse Failed.", e);
                }
            }
        }
        
        boolean isEmpty() {
            return stream.size() == 0;
        }
        
        String output() {
            try {
                isClosed.compareAndSet(false, true);
                return stream.toString(StandardCharsets.UTF_8);
            } catch (Exception e) {
                LOG.error("Write failed: ", e);
                return "Write failed: " + e.getMessage();
            } finally {
                try {
                    stream.close();
                } catch (IOException e) {
                    LOG.error("Close stream error: ", e);
                }
                try {
                    channel.close();
                } catch (IOException e) {
                    LOG.error("Close channel error: ", e);
                }
            }
        }
    }
}
