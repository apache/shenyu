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

package org.apache.shenyu.plugin.ai.statistic;

import com.fasterxml.jackson.databind.JsonNode;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.reactivestreams.Publisher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.http.HttpHeaders;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
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
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Consumer;
import java.util.zip.GZIPInputStream;

/**
 * Shenyu ai statistic plugin.
 */
public class AiStatisticPlugin extends AbstractShenyuPlugin {
    
    private static final Logger LOG = LoggerFactory.getLogger(AiStatisticPlugin.class);
    
    private static final Map<String, AtomicLong> CLIENT_TOKENS_USAGE = new ConcurrentHashMap<>();
    
    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain,
                                   final SelectorData selector, final RuleData rule) {
        // get clientId
        String clientId = extractClientId(exchange);
        
        Long usedTokensStatistics = getUsedTokensStatistics(clientId);
        
        // store the used tokens in the exchange attributes
        exchange.getAttributes().put(Constants.USED_TOKENS, usedTokensStatistics);
        
        
        final AiStatisticServerHttpResponse loggingServerHttpResponse = new AiStatisticServerHttpResponse(exchange.getResponse(), tokens -> recordTokensUsage(clientId, tokens));
        try {
            return chain.execute(exchange.mutate()
                            .response(loggingServerHttpResponse).build());
        } catch (Exception e) {
            LOG.error("Error occurred while processing request: ", e);
            throw e;
        }
    }
    
    private String extractClientId(ServerWebExchange exchange) {
        ServerHttpRequest request = exchange.getRequest();
        // Get client identifier from request headers first
        String clientId = request.getHeaders().getFirst(Constants.CLIENT_ID);
        
        if (StringUtils.isBlank(clientId)) {
            // If not present, try to get it from the exchange attributes
            clientId = exchange.getAttributes().get(Constants.CLIENT_ID).toString();
        }
        
        if (StringUtils.isBlank(clientId)) {
            // Finally, try using the combination of IP and User-Agent
            String ip = request.getRemoteAddress().getAddress().getHostAddress();
            String userAgent = request.getHeaders().getFirst(HttpHeaders.USER_AGENT);
            clientId = ip + "|" + (userAgent != null ? userAgent : "unknown");
        }
        exchange.getAttributes().put(Constants.CLIENT_ID, clientId);
        
        return clientId;
    }
    
    private void recordTokensUsage(String clientId, long tokens) {
        CLIENT_TOKENS_USAGE.computeIfAbsent(clientId, k -> new AtomicLong(0))
                .addAndGet(tokens);
    }
    
    /**
     * Get the total number of tokens used by a specific client.
     *
     * @param clientId the client ID
     * @return the total number of tokens used
     */
    public Long getUsedTokensStatistics(final String clientId) {
        // TODO get tokens from exchange attributes
        AtomicLong usedTokens = CLIENT_TOKENS_USAGE.computeIfAbsent(clientId, k -> new AtomicLong(0));
        return usedTokens.get();
    }
    
    @Override
    public int getOrder() {
        return PluginEnum.AI_STATISTIC.getCode();
    }
    
    @Override
    public String named() {
        return PluginEnum.AI_STATISTIC.getName();
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
                        if (serverHttpResponse.getHeaders().containsKey("Content-Encoding")
                                && serverHttpResponse.getHeaders().getFirst("Content-Encoding").contains("gzip")) {
                            try {
                                ByteBuffer readOnlyBuffer = byteBuffer.asReadOnlyBuffer();
                                byte[] compressed = new byte[readOnlyBuffer.remaining()];
                                readOnlyBuffer.get(compressed);
                                
                                // Decompress gzipped content
                                try (GZIPInputStream gzipInputStream = new GZIPInputStream(new ByteArrayInputStream(compressed));
                                     ByteArrayOutputStream outputStream = new ByteArrayOutputStream()) {
                                    byte[] buffers = new byte[1024];
                                    int len;
                                    while ((len = gzipInputStream.read(buffers)) > 0) {
                                        outputStream.write(buffers, 0, len);
                                    }
                                    writer.write(ByteBuffer.wrap(outputStream.toByteArray()));
                                }
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
        
        private long extractTokensFromResponse(String responseBody) {
            // 根据 OpenAI API 响应格式解析出 tokens 使用量
            // 不同的 API 端点响应格式可能不同，需要相应调整
            try {
                JsonNode root = JsonUtils.toJsonNode(responseBody);
                JsonNode usage = root.get("usage");
                if (usage != null) {
                    JsonNode totalTokens = usage.get("total_tokens");
                    if (totalTokens != null) {
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
