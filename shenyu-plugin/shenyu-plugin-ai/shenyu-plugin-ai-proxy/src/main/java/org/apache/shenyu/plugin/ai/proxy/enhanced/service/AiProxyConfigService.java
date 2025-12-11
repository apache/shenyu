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

package org.apache.shenyu.plugin.ai.proxy.enhanced.service;

import com.fasterxml.jackson.databind.JsonNode;
import org.apache.shenyu.common.dto.convert.rule.AiProxyHandle;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;
import java.util.Optional;

/**
 * AI proxy config service.
 */
public class AiProxyConfigService {

    private static final Logger LOG = LoggerFactory.getLogger(AiProxyConfigService.class);

    private static final String FALLBACK_CONFIG = "fallbackConfig";

    /**
     * Resolves the primary configuration for the AI call by merging global and
     * selector-level settings.
     *
     * @param handle the selector handle
     * @return the primary AiCommonConfig
     */
    public AiCommonConfig resolvePrimaryConfig(final AiProxyHandle handle) {
        final AiCommonConfig globalConfig = Singleton.INST.get(AiCommonConfig.class);
        final AiCommonConfig primaryConfig = new AiCommonConfig();

        if (Objects.nonNull(globalConfig)) {
            primaryConfig.mergeWith(globalConfig);
        }
        if (Objects.nonNull(handle)) {
            AiCommonConfig handleConfig = new AiCommonConfig();
            handleConfig.setProvider(handle.getProvider());
            handleConfig.setModel(handle.getModel());
            handleConfig.setApiKey(handle.getApiKey());
            handleConfig.setBaseUrl(handle.getBaseUrl());
            handleConfig.setTemperature(handle.getTemperature());
            handleConfig.setMaxTokens(handle.getMaxTokens());
            handleConfig.setStream(handle.getStream());
            primaryConfig.mergeWith(handleConfig);
        }
        return primaryConfig;
    }

    /**
     * Resolves the dynamic fallback configuration from the request body.
     *
     * @param primaryConfig the primary config to use as a base for supplementation
     * @param requestBody   the request body
     * @return an Optional containing the final fallback AiCommonConfig
     */
    public Optional<AiCommonConfig> resolveDynamicFallbackConfig(final AiCommonConfig primaryConfig,
            final String requestBody) {
        return extractDynamicFallbackConfig(requestBody)
                .map(dynamicConfig -> {
                    LOG.info("Resolved dynamic fallback config: {}", dynamicConfig);
                    return new AiCommonConfig(primaryConfig).mergeWith(dynamicConfig);
                });
    }

    /**
     * Resolves the admin-configured fallback from the selector handle.
     *
     * @param primaryConfig the primary config to use as a base for supplementation
     * @param handle        the selector handle
     * @return an Optional containing the final fallback AiCommonConfig
     */
    public Optional<AiCommonConfig> resolveAdminFallbackConfig(final AiCommonConfig primaryConfig,
            final AiProxyHandle handle) {
        return Optional.ofNullable(handle)
                .map(AiProxyHandle::getFallbackConfig)
                .map(fallback -> {
                    AiCommonConfig config = new AiCommonConfig();
                    config.setProvider(fallback.getProvider());
                    config.setModel(fallback.getModel());
                    config.setApiKey(fallback.getApiKey());
                    config.setBaseUrl(fallback.getBaseUrl());
                    config.setTemperature(fallback.getTemperature());
                    config.setMaxTokens(fallback.getMaxTokens());
                    LOG.info("Resolved admin fallback config: {}", config);
                    return new AiCommonConfig(primaryConfig).mergeWith(config);
                });
    }

    /**
     * Extract prompt from request body when fallback config is present.
     *
     * @param requestBody the request body
     * @return prompt or content value, otherwise original request body
     */
    public String extractPrompt(final String requestBody) {
        if (Objects.isNull(requestBody) || requestBody.isEmpty()) {
            return requestBody;
        }
        try {
            JsonNode jsonNode = JsonUtils.toJsonNode(requestBody);
            if (jsonNode.has("prompt")) {
                return jsonNode.get("prompt").asText();
            }
            if (jsonNode.has("content")) {
                return jsonNode.get("content").asText();
            }
        } catch (Exception e) {
            // ignore parsing errors and fall back to original body
        }
        return requestBody;
    }

    private Optional<AiCommonConfig> extractDynamicFallbackConfig(final String requestBody) {
        if (Objects.isNull(requestBody) || requestBody.isEmpty()) {
            return Optional.empty();
        }
        JsonNode jsonNode = JsonUtils.toJsonNode(requestBody);
        if (jsonNode.has(FALLBACK_CONFIG)) {
            AiProxyHandle.FallbackConfig fallbackConfig = JsonUtils.jsonToObject(
                    jsonNode.get(FALLBACK_CONFIG).toString(), AiProxyHandle.FallbackConfig.class);
            AiCommonConfig config = new AiCommonConfig();
            config.setProvider(fallbackConfig.getProvider());
            config.setModel(fallbackConfig.getModel());
            config.setApiKey(fallbackConfig.getApiKey());
            config.setBaseUrl(fallbackConfig.getBaseUrl());
            config.setTemperature(fallbackConfig.getTemperature());
            config.setMaxTokens(fallbackConfig.getMaxTokens());
            return Optional.of(config);
        }
        return Optional.empty();
    }

}
