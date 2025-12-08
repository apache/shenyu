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

package org.apache.shenyu.plugin.ai.proxy.enhanced;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.AiProxyHandle;
import org.apache.shenyu.common.enums.AiModelProviderEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.common.spring.ai.registry.AiModelFactoryRegistry;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.AiProxyApiKeyCache;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.ChatClientCache;
import org.apache.shenyu.plugin.ai.proxy.enhanced.handler.AiProxyPluginHandler;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyConfigService;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyExecutorService;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.model.ChatModel;
import org.springframework.ai.chat.model.ChatResponse;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.web.server.ServerWebExchange;

import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.nio.charset.StandardCharsets;
import java.util.Objects;
import java.util.Optional;

/** AI proxy plugin. This plugin is used to proxy requests to AI services. */
public class AiProxyPlugin extends AbstractShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(AiProxyPlugin.class);

    /**
     * Maximum request body size: 5MB.
     */
    private static final long MAX_REQUEST_BODY_SIZE_BYTES = 5 * 1024 * 1024L;

    private final AiModelFactoryRegistry aiModelFactoryRegistry;

    private final AiProxyConfigService aiProxyConfigService;

    private final AiProxyExecutorService aiProxyExecutorService;

    private final ChatClientCache chatClientCache;

    private final AiProxyPluginHandler aiProxyPluginHandler;

    public AiProxyPlugin(
            final AiModelFactoryRegistry aiModelFactoryRegistry,
            final AiProxyConfigService aiProxyConfigService,
            final AiProxyExecutorService aiProxyExecutorService,
            final ChatClientCache chatClientCache,
            final AiProxyPluginHandler aiProxyPluginHandler) {
        this.aiModelFactoryRegistry = aiModelFactoryRegistry;
        this.aiProxyConfigService = aiProxyConfigService;
        this.aiProxyExecutorService = aiProxyExecutorService;
        this.chatClientCache = chatClientCache;
        this.aiProxyPluginHandler = aiProxyPluginHandler;
    }

    @Override
    protected Mono<Void> doExecute(
            final ServerWebExchange exchange,
            final ShenyuPluginChain chain,
            final SelectorData selector,
            final RuleData rule) {
        final AiProxyHandle selectorHandle = aiProxyPluginHandler
                .getSelectorCachedHandle()
                .obtainHandle(
                        CacheKeyUtils.INST.getKey(
                                selector.getId(), Constants.DEFAULT_RULE));

        return DataBufferUtils.join(exchange.getRequest().getBody())
                .flatMap(dataBuffer -> {
                    // Validate actual body size after reading, not just Content-Length header
                    final int actualSize = dataBuffer.readableByteCount();
                    if (actualSize > MAX_REQUEST_BODY_SIZE_BYTES) {
                        DataBufferUtils.release(dataBuffer);
                        LOG.warn("[AiProxy] Request body size {} exceeds maximum allowed size {}", 
                                actualSize, MAX_REQUEST_BODY_SIZE_BYTES);
                        exchange.getResponse().setStatusCode(HttpStatus.PAYLOAD_TOO_LARGE);
                        return exchange.getResponse().setComplete();
                    }
                    
                    final String requestBody = dataBuffer.toString(StandardCharsets.UTF_8);
                    DataBufferUtils.release(dataBuffer);

                    final AiCommonConfig primaryConfig = aiProxyConfigService.resolvePrimaryConfig(selectorHandle);

                    // override apiKey by proxy key if provided in header
                    final HttpHeaders headers = exchange.getRequest().getHeaders();
                    final String proxyApiKey = headers.getFirst(Constants.X_API_KEY);
                    final boolean proxyEnabled = Objects.nonNull(selectorHandle)
                            && "true".equalsIgnoreCase(String.valueOf(selectorHandle.getProxyEnabled()));

                    if (proxyEnabled) {
                        // if proxy mode enabled but header missing -> 401
                        if (Objects.isNull(proxyApiKey) || proxyApiKey.isEmpty()) {
                            exchange.getResponse().setStatusCode(HttpStatus.UNAUTHORIZED);
                            return exchange.getResponse().setComplete();
                        }

                        final String realKey = AiProxyApiKeyCache.getInstance().getRealApiKey(selector.getId(),
                                proxyApiKey);
                        if (Objects.nonNull(realKey)) {
                            primaryConfig.setApiKey(realKey);
                            if (LOG.isDebugEnabled()) {
                                LOG.debug("[AiProxy] proxy key hit, selectorId={}, key={}... (masked)",
                                        selector.getId(), proxyApiKey.substring(0, Math.min(6, proxyApiKey.length())));
                            }
                            LOG.info("[AiProxy] proxy key hit, cacheSize={}", AiProxyApiKeyCache.getInstance().size());
                        } else {
                            // shenyu proxy api key is invalid
                            LOG.warn("[AiProxy] proxy key invalid, key={}... (masked), selectorId={}",
                                    proxyApiKey.substring(0, Math.min(6, proxyApiKey.length())), selector.getId());
                            exchange.getResponse().setStatusCode(HttpStatus.UNAUTHORIZED);
                            return exchange.getResponse().setComplete();
                        }
                    }

                    if (Boolean.TRUE.equals(primaryConfig.getStream())) {
                        return handleStreamRequest(exchange, selector, requestBody, primaryConfig, selectorHandle);
                    }
                    return handleNonStreamRequest(exchange, selector, requestBody, primaryConfig, selectorHandle);
                });
    }

    private Mono<Void> handleStreamRequest(
            final ServerWebExchange exchange,
            final SelectorData selector,
            final String requestBody,
            final AiCommonConfig primaryConfig,
            final AiProxyHandle selectorHandle) {
        final ChatClient mainClient = createMainChatClient(selector.getId(), primaryConfig);
        final String prompt = aiProxyConfigService.extractPrompt(requestBody);
        final Optional<ChatClient> fallbackClient = resolveFallbackClient(primaryConfig, selectorHandle,
                selector.getId(), requestBody);
        final ServerHttpResponse response = exchange.getResponse();
        response.getHeaders().setContentType(MediaType.TEXT_EVENT_STREAM);

        final Flux<ChatResponse> chatResponseFlux = aiProxyExecutorService.executeStream(mainClient, fallbackClient,
                prompt);

        final Flux<DataBuffer> sseFlux = chatResponseFlux.map(
                chatResponse -> {
                    final String json = JsonUtils.toJson(chatResponse);
                    final String sseData = "data: " + json + "\n\n";
                    return response.bufferFactory()
                            .wrap(sseData.getBytes(StandardCharsets.UTF_8));
                });

        return response.writeWith(sseFlux);
    }

    private Mono<Void> handleNonStreamRequest(
            final ServerWebExchange exchange,
            final SelectorData selector,
            final String requestBody,
            final AiCommonConfig primaryConfig,
            final AiProxyHandle selectorHandle) {
        final ChatClient mainClient = createMainChatClient(selector.getId(), primaryConfig);
        final String prompt = aiProxyConfigService.extractPrompt(requestBody);
        final Optional<ChatClient> fallbackClient = resolveFallbackClient(primaryConfig, selectorHandle,
                selector.getId(), requestBody);

        return aiProxyExecutorService
                .execute(mainClient, fallbackClient, prompt)
                .flatMap(
                        response -> {
                            byte[] jsonBytes = JsonUtils.toJson(response).getBytes(StandardCharsets.UTF_8);
                            return WebFluxResultUtils.result(exchange, jsonBytes);
                        });
    }

    private Optional<ChatClient> resolveFallbackClient(
            final AiCommonConfig primaryConfig,
            final AiProxyHandle selectorHandle,
            final String selectorId,
            final String requestBody) {
        return aiProxyConfigService
                .resolveDynamicFallbackConfig(primaryConfig, requestBody)
                .map(cfg -> {
                    LOG.info("[AiProxy] use dynamic fallback");
                    if (LOG.isDebugEnabled()) {
                        LOG.debug("[AiProxy] dynamic fallback config: {}", cfg);
                    }
                    return createDynamicFallbackClient(cfg);
                })
                .or(
                        () -> aiProxyConfigService
                                .resolveAdminFallbackConfig(primaryConfig, selectorHandle)
                                .map(adminFallbackConfig -> {
                                    LOG.info("[AiProxy] use admin fallback");
                                    if (LOG.isDebugEnabled()) {
                                        LOG.debug("[AiProxy] admin fallback config: {}", adminFallbackConfig);
                                    }
                                    return createAdminFallbackClient(selectorId, adminFallbackConfig);
                                }));
    }

    /**
     * Generate cache key based on config fields excluding apiKey.
     * This ensures cache consistency even when apiKey is updated at runtime.
     *
     * @param config the config
     * @return cache key hash
     */
    private int generateConfigCacheKey(final AiCommonConfig config) {
        return Objects.hash(
                config.getProvider(),
                config.getBaseUrl(),
                config.getModel(),
                config.getTemperature(),
                config.getMaxTokens(),
                config.getStream()
                // Explicitly exclude apiKey to avoid cache misses when apiKey changes
        );
    }

    private ChatClient createMainChatClient(final String selectorId, final AiCommonConfig config) {
        final int configHash = generateConfigCacheKey(config);
        final String cacheKey = selectorId + "|main_" + configHash;
        return chatClientCache.computeIfAbsent(
                cacheKey,
                () -> {
                    LOG.info("Creating and caching main model for selector: {}, key: {}", selectorId, cacheKey);
                    return createChatModel(config);
                });
    }

    private ChatClient createAdminFallbackClient(
            final String selectorId, final AiCommonConfig fallbackConfig) {
        final int configHash = generateConfigCacheKey(fallbackConfig);
        final String fallbackCacheKey = selectorId + "|adminFallback_" + configHash;
        return chatClientCache.computeIfAbsent(
                fallbackCacheKey,
                () -> {
                    LOG.info(
                            "Creating and caching admin fallback model for selector: {}, key: {}",
                            selectorId, fallbackCacheKey);
                    return createChatModel(fallbackConfig);
                });
    }

    private ChatClient createDynamicFallbackClient(final AiCommonConfig fallbackConfig) {
        LOG.info("Creating non-cached dynamic fallback model.");
        return ChatClient.builder(createChatModel(fallbackConfig)).build();
    }

    private ChatModel createChatModel(final AiCommonConfig config) {
        if (LOG.isDebugEnabled()) {
            LOG.debug("Creating chat model with config: {}", config);
        }
        final AiModelProviderEnum provider = AiModelProviderEnum.getByName(config.getProvider());
        if (Objects.isNull(provider)) {
            throw new IllegalArgumentException(
                    "Invalid AI model provider in config: " + config.getProvider());
        }
        final var factory = aiModelFactoryRegistry.getFactory(provider);
        if (Objects.isNull(factory)) {
            throw new IllegalArgumentException(
                    "AI model factory not found for provider: " + provider.getName());
        }
        return Objects.requireNonNull(
                factory.createAiModel(config),
                "The AI model created by the factory must not be null");
    }

    @Override
    public int getOrder() {
        return PluginEnum.AI_PROXY.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.AI_PROXY.getName();
    }
}
