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

package org.apache.shenyu.plugin.ai.transformer.response;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.plugin.AiResponseTransformerConfig;
import org.apache.shenyu.common.dto.convert.rule.AiResponseTransformerHandle;
import org.apache.shenyu.common.enums.AiModelProviderEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.ai.common.spring.ai.registry.AiModelFactoryRegistry;
import org.apache.shenyu.plugin.ai.common.cache.ChatClientCache;
import org.apache.shenyu.plugin.ai.transformer.response.handler.AiResponseTransformerPluginHandler;
import org.apache.shenyu.plugin.ai.transformer.response.template.AiResponseTransformerTemplate;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.base.utils.ServerWebExchangeUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.model.ChatModel;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.codec.HttpMessageReader;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import org.springframework.lang.NonNull;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;
import org.reactivestreams.Publisher;

import java.nio.charset.StandardCharsets;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Stream;

/**
 * this is ai response transformer plugin.
 */
public class AiResponseTransformerPlugin extends AbstractShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(AiResponseTransformerPlugin.class);

    private final List<HttpMessageReader<?>> messageReaders;

    private final AiModelFactoryRegistry aiModelFactoryRegistry;

    public AiResponseTransformerPlugin(final List<HttpMessageReader<?>> messageReaders, final AiModelFactoryRegistry aiModelFactoryRegistry) {
        this.messageReaders = messageReaders;
        this.aiModelFactoryRegistry = aiModelFactoryRegistry;
    }

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        AiResponseTransformerConfig aiResponseTransformerConfig = Singleton.INST.get(AiResponseTransformerConfig.class);
        if (Objects.isNull(aiResponseTransformerConfig)) {
            aiResponseTransformerConfig = new AiResponseTransformerConfig();
        }

        AiResponseTransformerHandle aiResponseTransformerHandle = AiResponseTransformerPluginHandler.CACHED_HANDLE.get()
                .obtainHandle(CacheKeyUtils.INST.getKey(rule));
        ChatClient client = ChatClientCache.getInstance().getClient("default");
        // Create final config with rule handle taking precedence
        if (Objects.nonNull(aiResponseTransformerHandle)) {
            aiResponseTransformerConfig = new AiResponseTransformerConfig();
            Optional.ofNullable(aiResponseTransformerHandle.getProvider()).ifPresent(aiResponseTransformerConfig::setProvider);
            Optional.ofNullable(aiResponseTransformerHandle.getBaseUrl()).ifPresent(aiResponseTransformerConfig::setBaseUrl);
            Optional.ofNullable(aiResponseTransformerHandle.getApiKey()).ifPresent(aiResponseTransformerConfig::setApiKey);
            Optional.ofNullable(aiResponseTransformerHandle.getModel()).ifPresent(aiResponseTransformerConfig::setModel);
            Optional.ofNullable(aiResponseTransformerHandle.getContent()).ifPresent(aiResponseTransformerConfig::setContent);
            client = ChatClientCache.getInstance().getClient(rule.getId());
        }

        String baseUrl = aiResponseTransformerConfig.getBaseUrl();
        String apiKey = aiResponseTransformerConfig.getApiKey();
        String provider = aiResponseTransformerConfig.getProvider();
        if (Stream.of(baseUrl, apiKey, provider).anyMatch(Objects::isNull)) {
            String missing = "";
            missing += Objects.isNull(baseUrl) ? "baseUrl, " : "";
            missing += Objects.isNull(apiKey) ? "apiKey, " : "";
            missing += Objects.isNull(provider) ? "provider, " : "";

            LOG.error("Missing configurations: {}", missing.substring(0, missing.length() - 2));
            return chain.execute(exchange);
        }

        if (Objects.isNull(client)) {
            ChatModel aiModel = aiModelFactoryRegistry
                    .getFactory(AiModelProviderEnum.getByName(provider))
                    .createAiModel(AiResponseTransformerPluginHandler.convertConfig(aiResponseTransformerConfig));
            client = ChatClientCache.getInstance().init(rule.getId(), aiModel);
        }

        ChatClient finalClient = client;
        AiResponseTransformerTemplate aiResponseTransformerTemplate = new AiResponseTransformerTemplate(aiResponseTransformerConfig.getContent(), exchange.getRequest());
        
        return chain.execute(exchange.mutate()
                .response(new AiResponseTransformerDecorator(exchange, aiResponseTransformerTemplate, finalClient))
                .build());
    }



    @Override
    public int getOrder() {
        return PluginEnum.AI_RESPONSE_TRANSFORMER.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.AI_RESPONSE_TRANSFORMER.getName();
    }

    static class AiResponseTransformerDecorator extends ServerHttpResponseDecorator {

        private final ServerWebExchange exchange;
        private final AiResponseTransformerTemplate aiResponseTransformerTemplate;
        private final ChatClient chatClient;

        AiResponseTransformerDecorator(final ServerWebExchange exchange,
                                       final AiResponseTransformerTemplate aiResponseTransformerTemplate,
                                       final ChatClient chatClient) {
            super(exchange.getResponse());
            this.exchange = exchange;
            this.aiResponseTransformerTemplate = aiResponseTransformerTemplate;
            this.chatClient = chatClient;
        }

        @Override
        @NonNull
        public Mono<Void> writeWith(@NonNull final Publisher<? extends DataBuffer> body) {
            final Mono<DataBuffer> dataBufferMono = DataBufferUtils.join(body);
            return dataBufferMono.flatMap(dataBuffer -> {
                byte[] bytes = new byte[dataBuffer.readableByteCount()];
                dataBuffer.read(bytes);
                DataBufferUtils.release(dataBuffer);
                String originalResponseBody = new String(bytes, StandardCharsets.UTF_8);
                
                return aiResponseTransformerTemplate.assembleMessage(exchange)
                        .flatMap(message -> Mono.fromCallable(() -> chatClient.prompt().user(message).call().content())
                                .subscribeOn(Schedulers.boundedElastic())
                                .flatMap(aiResponse -> {
                                    // 解析AI响应中的HTTP响应内容
                                    HttpHeaders newHeaders = extractHeadersFromAiResponse(aiResponse);
                                    String newBody = extractBodyFromAiResponse(aiResponse);
                                    
                                    // 更新响应头
                                    this.getHeaders().clear();
                                    this.getHeaders().putAll(newHeaders);
                                    
                                    // 返回转换后的响应体
                                    if (Objects.nonNull(newBody)) {
                                        return WebFluxResultUtils.result(this.exchange, newBody.getBytes(StandardCharsets.UTF_8));
                                    } else {
                                        return WebFluxResultUtils.result(this.exchange, originalResponseBody.getBytes(StandardCharsets.UTF_8));
                                    }
                                })
                                .onErrorResume(throwable -> {
                                    LOG.error("AI response transformation failed", throwable);
                                    return WebFluxResultUtils.result(this.exchange, originalResponseBody.getBytes(StandardCharsets.UTF_8));
                                }));
            });
        }

        private String extractBodyFromAiResponse(final String aiResponse) {
            if (Objects.isNull(aiResponse) || aiResponse.isEmpty()) {
                return null;
            }

            String[] lines = aiResponse.split("\\R");

            int emptyLineIndex = -1;
            for (int i = 0; i < lines.length; i++) {
                if (lines[i].trim().isEmpty()) {
                    emptyLineIndex = i;
                    break;
                }
            }

            if (emptyLineIndex == -1 || emptyLineIndex == lines.length - 1) {
                return null;
            }

            StringBuilder bodyBuilder = new StringBuilder();
            for (int i = emptyLineIndex + 1; i < lines.length; i++) {
                bodyBuilder.append(lines[i]);
                if (i < lines.length - 1) {
                    bodyBuilder.append("\n");
                }
            }

            return bodyBuilder.toString().trim();
        }

        private HttpHeaders extractHeadersFromAiResponse(final String aiResponse) {
            HttpHeaders headers = new HttpHeaders();
            if (Objects.isNull(aiResponse) || aiResponse.isEmpty()) {
                return headers;
            }
            
            String[] lines = aiResponse.split("\\R");
            boolean headerSectionStarted = false;
            
            for (String line : lines) {
                if (!headerSectionStarted) {
                    if (line.startsWith("HTTP/1.1") || line.matches("^(GET|POST|PUT|DELETE|PATCH|OPTIONS|HEAD)\\s.*\\sHTTP/1.1$")) {
                        headerSectionStarted = true;
                        continue;
                    }
                } else {
                    if (line.trim().isEmpty()) {
                        break;
                    }
                    int colonIndex = line.indexOf(":");
                    if (colonIndex > 0) {
                        String name = line.substring(0, colonIndex).trim();
                        String value = line.substring(colonIndex + 1).trim();
                        headers.add(name, value);
                    }
                }
            }
            
            return headers;
        }
    }
} 
