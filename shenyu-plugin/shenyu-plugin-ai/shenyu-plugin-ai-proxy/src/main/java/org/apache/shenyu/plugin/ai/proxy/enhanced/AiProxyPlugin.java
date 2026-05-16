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
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.common.protocol.OpenAiProtocolAdapter;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.AiProxyApiKeyCache;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.OpenAiApiCache;
import org.apache.shenyu.plugin.ai.proxy.enhanced.handler.AiProxyPluginHandler;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyConfigService;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyExecutorService;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyExecutorService.FallbackContext;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.UpstreamErrorLogger;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.ai.openai.api.OpenAiApi;
import org.springframework.ai.openai.api.OpenAiApi.ChatCompletionChunk;
import org.springframework.ai.openai.api.OpenAiApi.ChatCompletionRequest;
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

    private final AiProxyConfigService aiProxyConfigService;

    private final AiProxyExecutorService aiProxyExecutorService;

    private final AiProxyPluginHandler aiProxyPluginHandler;

    public AiProxyPlugin(
            final AiProxyConfigService aiProxyConfigService,
            final AiProxyExecutorService aiProxyExecutorService,
            final AiProxyPluginHandler aiProxyPluginHandler) {
        this.aiProxyConfigService = aiProxyConfigService;
        this.aiProxyExecutorService = aiProxyExecutorService;
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

                    final boolean stream = OpenAiProtocolAdapter.resolveStream(requestBody, primaryConfig.getStream());
                    if (stream) {
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
        final OpenAiApi mainApi = getCachedOpenAiApi(selector.getId(), "main", primaryConfig);
        final ChatCompletionRequest request = OpenAiProtocolAdapter.toChatCompletionRequest(requestBody, true, primaryConfig);
        final Optional<FallbackContext> fallbackCtx = resolveFallbackContext(
                selector.getId(), primaryConfig, selectorHandle, requestBody);
        final ServerHttpResponse response = exchange.getResponse();
        response.getHeaders().setContentType(MediaType.TEXT_EVENT_STREAM);

        final Flux<ChatCompletionChunk> chunkFlux = aiProxyExecutorService.executeDirectStream(
                mainApi, fallbackCtx, request, requestBody, true);

        final Flux<DataBuffer> sseFlux = chunkFlux.map(
                chunk -> {
                    final String json = JsonUtils.toJson(chunk);
                    final String sseData = "data: " + json + "\n\n";
                    return response.bufferFactory()
                            .wrap(sseData.getBytes(StandardCharsets.UTF_8));
                }).concatWith(Mono.fromSupplier(() ->
                        response.bufferFactory()
                                .wrap("data: [DONE]\n\n".getBytes(StandardCharsets.UTF_8))))
                .doOnError(e -> logUpstreamError(e, "stream"));

        return response.writeWith(sseFlux);
    }

    private Mono<Void> handleNonStreamRequest(
            final ServerWebExchange exchange,
            final SelectorData selector,
            final String requestBody,
            final AiCommonConfig primaryConfig,
            final AiProxyHandle selectorHandle) {
        final OpenAiApi mainApi = getCachedOpenAiApi(selector.getId(), "main", primaryConfig);
        final ChatCompletionRequest request = OpenAiProtocolAdapter.toChatCompletionRequest(requestBody, false, primaryConfig);
        final Optional<FallbackContext> fallbackCtx = resolveFallbackContext(
                selector.getId(), primaryConfig, selectorHandle, requestBody);

        return aiProxyExecutorService
                .executeDirectCall(mainApi, fallbackCtx, request, requestBody)
                .flatMap(
                        responseEntity -> {
                            final String responseJson = JsonUtils.toJson(responseEntity.getBody());
                            byte[] jsonBytes = responseJson.getBytes(StandardCharsets.UTF_8);
                            return WebFluxResultUtils.result(exchange, jsonBytes);
                        })
                .doOnError(e -> logUpstreamError(e, "non-stream"));
    }

    private Optional<FallbackContext> resolveFallbackContext(
            final String selectorId,
            final AiCommonConfig primaryConfig,
            final AiProxyHandle selectorHandle,
            final String requestBody) {
        return aiProxyConfigService
                .resolveDynamicFallbackConfig(primaryConfig, requestBody)
                .map(cfg -> {
                    LOG.info("[AiProxy] use dynamic fallback");
                    if (LOG.isDebugEnabled()) {
                        LOG.debug("[AiProxy] dynamic fallback config: {}", cfg);
                    }
                    return new FallbackContext(createOpenAiApi(cfg), cfg);
                })
                .or(() -> aiProxyConfigService
                        .resolveAdminFallbackConfig(primaryConfig, selectorHandle)
                        .map(adminFallbackConfig -> {
                            LOG.info("[AiProxy] use admin fallback");
                            if (LOG.isDebugEnabled()) {
                                LOG.debug("[AiProxy] admin fallback config: {}", adminFallbackConfig);
                            }
                            return new FallbackContext(
                                    getCachedOpenAiApi(selectorId, "adminFallback", adminFallbackConfig),
                                    adminFallbackConfig);
                        }));
    }

    private OpenAiApi getCachedOpenAiApi(final String selectorId, final String type, final AiCommonConfig config) {
        final String cacheKey = selectorId + "|" + type + "_" + generateConfigCacheKey(config);
        return OpenAiApiCache.getInstance().computeIfAbsent(cacheKey, () -> createOpenAiApi(config));
    }

    private int generateConfigCacheKey(final AiCommonConfig config) {
        return Objects.hash(
                config.getBaseUrl(),
                config.getModel(),
                config.getTemperature(),
                config.getMaxTokens()
        );
    }

    private OpenAiApi createOpenAiApi(final AiCommonConfig config) {
        if (Objects.isNull(config.getBaseUrl()) || config.getBaseUrl().isEmpty()) {
            throw new IllegalArgumentException("baseUrl must not be empty");
        }
        if (Objects.isNull(config.getApiKey()) || config.getApiKey().isEmpty()) {
            throw new IllegalArgumentException("apiKey must not be empty");
        }
        return OpenAiApi.builder()
                .baseUrl(config.getBaseUrl())
                .apiKey(config.getApiKey())
                .build();
    }

    @Override
    public int getOrder() {
        return PluginEnum.AI_PROXY.getCode();
    }

    @Override
    public String named() {
        return PluginEnum.AI_PROXY.getName();
    }

    private void logUpstreamError(final Throwable e, final String mode) {
        UpstreamErrorLogger.logUpstreamError(LOG, e, mode);
    }
}
