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
import com.google.common.collect.Lists;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.AiTokenLimiterHandle;
import org.apache.shenyu.common.enums.AiTokenLimiterEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.JsonUtils;
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
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.data.redis.core.ReactiveRedisTemplate;
import org.springframework.data.redis.core.script.DefaultRedisScript;
import org.springframework.http.HttpCookie;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import org.springframework.scripting.support.ResourceScriptSource;
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
import java.util.List;
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
    
    private static final String CHECK_LIMIT_SCRIPT = "check-limit.lua";
    
    private static final String INCREMENT_TOKEN_SCRIPT = "increment-token.lua";
    
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
        String keyResolverName = aiTokenLimiterHandle.getAiTokenLimitType();
        String keyName = aiTokenLimiterHandle.getKeyName();
        Long tokenLimit = aiTokenLimiterHandle.getTokenLimit();
        Long timeWindowSeconds = aiTokenLimiterHandle.getTimeWindowSeconds();
        
        String cacheKey = REDIS_KEY_PREFIX + aiTokenLimiterHandle.getAiTokenLimitType() + ":" + getCacheKey(exchange, keyResolverName, keyName);
        
        final AiStatisticServerHttpResponse loggingServerHttpResponse = new AiStatisticServerHttpResponse(exchange.getResponse(),
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
                        Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.TOO_MANY_REQUESTS);
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
        DefaultRedisScript checkLimitScript = new DefaultRedisScript<>();
        String scriptPath = Constants.SCRIPT_PATH + CHECK_LIMIT_SCRIPT;
        checkLimitScript.setScriptSource(new ResourceScriptSource(new ClassPathResource(scriptPath)));
        checkLimitScript.setResultType(Long.class);
        return Mono.from(reactiveRedisTemplate.execute(
                        checkLimitScript,
                        Lists.newArrayList(cacheKey),
                        Lists.newArrayList(tokenLimit.toString()))
                .map(result -> {
                    // Convert result to String since StatusOutput doesn't support long
                    // Return true if result is "1", false otherwise
                    return (Long) result == 1L;
                }));
    }
    
    /**
     * Get the cache key based on the configured key resolver type.
     *
     * @param exchange the server web exchange
     * @param keyResolverName the name of the key resolver
     * @param keyName the name of the key
     * @return the cache key
     */
    private String getCacheKey(final ServerWebExchange exchange, final String keyResolverName, final String keyName) {
        ServerHttpRequest request = exchange.getRequest();
        String key;
        // Determine the key based on the configured key resolver type
        AiTokenLimiterEnum keyResolverEnum = AiTokenLimiterEnum.getByName(keyResolverName);
        
        switch (keyResolverEnum) {
            case IP:
                key = Objects.requireNonNull(request.getRemoteAddress()).getHostString();
                break;
            case URI:
                key = request.getURI().getPath();
                break;
            case HEADER:
                key = request.getHeaders().getFirst(keyName);
                break;
            case PARAMETER:
                key = request.getQueryParams().getFirst(keyName);
                break;
            case COOKIE:
                HttpCookie cookie = request.getCookies().getFirst(keyName);
                key = Objects.nonNull(cookie) ? cookie.getValue() : "";
                break;
            default:
                key = exchange.getAttribute(Constants.CONTEXT_PATH)
                        + ":"
                        + exchange.getRequest().getURI().getPath();
        }
        
        return StringUtils.isBlank(key) ? "" : key;
    }
    
    private String extractClientId(final ServerWebExchange exchange) {
        ServerHttpRequest request = exchange.getRequest();
        // Get client identifier from request headers first
        String clientId = request.getHeaders().getFirst(Constants.CLIENT_ID);
        
        if (StringUtils.isBlank(clientId)) {
            // If not present, try to get it from the exchange attributes
            if (exchange.getAttributes().containsKey(Constants.CLIENT_ID)) {
                clientId = exchange.getAttributes().get(Constants.CLIENT_ID).toString();
            }
        }
        
        if (StringUtils.isBlank(clientId)) {
            // Finally, try using the combination of IP and User-Agent
            String ip = request.getRemoteAddress().getAddress().getHostAddress();
            String userAgent = request.getHeaders().getFirst(HttpHeaders.USER_AGENT);
            clientId = ip + "|" + (Objects.nonNull(userAgent) ? userAgent : "unknown");
        }
        exchange.getAttributes().put(Constants.CLIENT_ID, clientId);
        
        return clientId;
    }
    
    private void recordTokensUsage(final ReactiveRedisTemplate reactiveRedisTemplate, final String cacheKey, final Long tokens, final Long windowSeconds) {
        DefaultRedisScript incrementTokenScript = new DefaultRedisScript<>();
        String scriptPath = Constants.SCRIPT_PATH + INCREMENT_TOKEN_SCRIPT;
        incrementTokenScript.setScriptSource(new ResourceScriptSource(new ClassPathResource(scriptPath)));
        incrementTokenScript.setResultType(List.class);
        reactiveRedisTemplate.execute(incrementTokenScript,
                Lists.newArrayList(cacheKey),
                Lists.newArrayList(tokens.toString(), windowSeconds.toString())).subscribe();
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
        
        private final ServerHttpResponse serverHttpResponse;
        
        private final Consumer<Long> tokensRecorder;
        
        AiStatisticServerHttpResponse(final ServerHttpResponse delegate, final Consumer<Long> tokensRecorder) {
            super(delegate);
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
            
            BodyWriter writer = new BodyWriter();
            return Flux.from(body).doOnNext(buffer -> {
                try (DataBuffer.ByteBufferIterator bufferIterator = buffer.readableByteBuffers()) {
                    bufferIterator.forEachRemaining(byteBuffer -> {
                        // Handle gzip encoded response
                        if (serverHttpResponse.getHeaders().containsKey(Constants.CONTENT_ENCODING)
                                && serverHttpResponse.getHeaders().getFirst(Constants.CONTENT_ENCODING).contains("gzip")) {
                            try {
                                ByteBuffer readOnlyBuffer = byteBuffer.asReadOnlyBuffer();
                                byte[] compressed = new byte[readOnlyBuffer.remaining()];
                                readOnlyBuffer.get(compressed);
                                
                                // Decompress gzipped content
                                byte[] decompressed = decompressGzip(compressed);
                                writer.write(ByteBuffer.wrap(decompressed));
                                
                            } catch (IOException e) {
                                LOG.error("Failed to decompress gzipped response", e);
                                writer.write(byteBuffer.asReadOnlyBuffer());
                            }
                        } else {
                            writer.write(byteBuffer.asReadOnlyBuffer());
                        }
                    });
                }
            }).doFinally(signal -> {
                String responseBody = writer.output();
                long tokens = extractTokensFromResponse(responseBody);
                tokensRecorder.accept(tokens);
            });
        }
        
        private byte[] decompressGzip(final byte[] compressed) throws IOException {
            try (GZIPInputStream gzipInputStream = new GZIPInputStream(new ByteArrayInputStream(compressed));
                 ByteArrayOutputStream outputStream = new ByteArrayOutputStream()) {
                byte[] buffer = new byte[1024];
                int len;
                while ((len = gzipInputStream.read(buffer)) > 0) {
                    outputStream.write(buffer, 0, len);
                }
                return outputStream.toByteArray();
            }
        }
        
        private long extractTokensFromResponse(final String responseBody) {
            // 根据 OpenAI API 响应格式解析出 tokens 使用量
            // 不同的 API 端点响应格式可能不同，需要相应调整
            try {
                JsonNode root = JsonUtils.toJsonNode(responseBody);
                JsonNode usage = root.get("usage");
                if (Objects.nonNull(usage)) {
                    JsonNode totalTokens = usage.get("total_tokens");
                    if (Objects.nonNull(totalTokens)) {
                        return totalTokens.asLong();
                    }
                }
            } catch (Exception e) {
                // 处理解析异常
                LOG.error("Failed to parse response body: {}", responseBody, e);
            }
            return 0;
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
