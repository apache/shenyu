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

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.shenyu.common.dto.convert.rule.AiProxyHandle;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.handler.AiProxyPluginHandler;

import java.net.URI;
import java.util.Locale;
import java.util.Objects;
import java.util.Optional;

/**
 * AI proxy config service.
 */
public class AiProxyConfigService {

    private static final ObjectMapper REQUEST_MAPPER = new ObjectMapper()
            .enable(DeserializationFeature.FAIL_ON_TRAILING_TOKENS)
            .enable(JsonParser.Feature.STRICT_DUPLICATE_DETECTION);

    private static final String FALLBACK_CONFIG = "fallbackConfig";

    private final AiProxyPluginHandler pluginHandler;

    /**
     * Create a resolver using the same handler that receives plugin configuration updates.
     *
     * @param pluginHandler plugin configuration handler
     */
    public AiProxyConfigService(final AiProxyPluginHandler pluginHandler) {
        this.pluginHandler = pluginHandler;
    }

    /**
     * Resolves the primary configuration for the AI call by merging global and
     * selector-level settings.
     *
     * @param handle the selector handle
     * @return the primary AiCommonConfig
     */
    public AiCommonConfig resolvePrimaryConfig(final AiProxyHandle handle) {
        final AiCommonConfig globalConfig = pluginHandler.getGlobalConfig();
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
            handleConfig.setProtocol(handle.getProtocol());
            handleConfig.setRequestBodyOverrides(handle.getRequestBodyOverrides());
            handleConfig.setRequestBodyForceOverride(handle.getRequestBodyForceOverride());
            primaryConfig.mergeWith(handleConfig);
        }
        return primaryConfig;
    }

    /**
     * Resolve a request without selector authorization. Request-selected fallback is disabled.
     *
     * @param primaryConfig primary configuration
     * @param requestBody request body
     * @return empty when no fallback is requested
     */
    public Optional<AiCommonConfig> resolveDynamicFallbackConfig(final AiCommonConfig primaryConfig, final String requestBody) {
        return resolveDynamicFallbackConfig(primaryConfig, null, requestBody);
    }

    /**
     * Resolve a request-selected target from this selector's trusted configuration.
     * Request bodies may name a target, but may not supply upstream configuration.
     *
     * @param primaryConfig primary configuration, including any resolved proxy credential
     * @param handle selector-owned authorization and target definitions
     * @param requestBody request body
     * @return selected, credential-bound fallback, or empty when none was requested
     */
    public Optional<AiCommonConfig> resolveDynamicFallbackConfig(final AiCommonConfig primaryConfig,
                                                              final AiProxyHandle handle, final String requestBody) {
        JsonNode root = requestObject(requestBody);
        if (Objects.isNull(root) || !root.has(FALLBACK_CONFIG)) {
            return Optional.empty();
        }
        if (Objects.isNull(handle) || !Boolean.TRUE.equals(handle.getDynamicFallbackEnabled())) {
            throw new ShenyuException("Request-selected AI fallback is disabled");
        }
        JsonNode selection = root.get(FALLBACK_CONFIG);
        if (!selection.isObject() || selection.size() != 1 || !selection.has("target")
                || !selection.get("target").isTextual() || !selection.get("target").asText().matches("[A-Za-z0-9][A-Za-z0-9._-]{0,63}")) {
            throw new ShenyuException("AI fallback must contain only a managed target reference");
        }
        AiProxyHandle.FallbackConfig target = Objects.isNull(handle.getFallbackTargets()) ? null
                : handle.getFallbackTargets().get(selection.get("target").asText());
        if (Objects.isNull(target)) {
            throw new ShenyuException("AI fallback target is not authorized for this selector");
        }
        return Optional.of(bindFallback(primaryConfig, target));
    }

    /**
     * Resolve an administrator-configured fallback without implicitly moving credentials.
     *
     * @param primaryConfig primary configuration
     * @param handle selector configuration
     * @return credential-bound fallback, or empty
     */
    public Optional<AiCommonConfig> resolveAdminFallbackConfig(final AiCommonConfig primaryConfig, final AiProxyHandle handle) {
        return Optional.ofNullable(handle).map(AiProxyHandle::getFallbackConfig)
                .map(fallback -> bindFallback(primaryConfig, fallback));
    }

    private JsonNode requestObject(final String requestBody) {
        if (Objects.isNull(requestBody) || requestBody.isEmpty()) {
            return null;
        }
        final JsonNode root;
        try {
            root = REQUEST_MAPPER.readTree(requestBody);
        } catch (JsonProcessingException e) {
            throw new ShenyuException("AI request body must be a valid JSON object");
        }
        if (Objects.isNull(root) || !root.isObject()) {
            throw new ShenyuException("AI request body must be a valid JSON object");
        }
        return root;
    }

    private AiCommonConfig bindFallback(final AiCommonConfig primary, final AiProxyHandle.FallbackConfig target) {
        AiCommonConfig overlay = new AiCommonConfig();
        overlay.setProvider(target.getProvider());
        overlay.setModel(target.getModel());
        overlay.setApiKey(target.getApiKey());
        overlay.setBaseUrl(target.getBaseUrl());
        overlay.setTemperature(target.getTemperature());
        overlay.setMaxTokens(target.getMaxTokens());
        AiCommonConfig resolved = new AiCommonConfig(primary).mergeWith(overlay);
        UpstreamIdentity identity = identity(resolved);
        if (Objects.isNull(target.getApiKey()) && !identity.equals(identity(primary))) {
            throw new ShenyuException("AI fallback to a different upstream requires its own credential");
        }
        return resolved;
    }

    private UpstreamIdentity identity(final AiCommonConfig config) {
        if (Objects.isNull(config.getProvider()) || config.getProvider().isBlank() || Objects.isNull(config.getBaseUrl())) {
            throw new ShenyuException("AI fallback requires an explicit upstream identity");
        }
        final URI uri;
        try {
            uri = URI.create(config.getBaseUrl());
        } catch (IllegalArgumentException e) {
            throw new ShenyuException("AI fallback requires a valid HTTP(S) endpoint");
        }
        if (!("https".equalsIgnoreCase(uri.getScheme()) || "http".equalsIgnoreCase(uri.getScheme()))
                || Objects.isNull(uri.getHost()) || Objects.nonNull(uri.getRawUserInfo())
                || Objects.nonNull(uri.getRawQuery()) || Objects.nonNull(uri.getRawFragment()) || uri.getPort() == 0 || uri.getPort() > 65535) {
            throw new ShenyuException("AI fallback requires a valid HTTP(S) endpoint");
        }
        int port = uri.getPort() < 0 ? ("https".equalsIgnoreCase(uri.getScheme()) ? 443 : 80) : uri.getPort();
        return new UpstreamIdentity(config.getProvider(), uri.getScheme().toLowerCase(Locale.ROOT), uri.getHost().toLowerCase(Locale.ROOT), port, uri.getRawPath());
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
            // Strip internal fallbackConfig from body before forwarding to upstream
            if (jsonNode.has(FALLBACK_CONFIG) && jsonNode.isObject()) {
                ((ObjectNode) jsonNode).remove(FALLBACK_CONFIG);
                return jsonNode.toString();
            }
        } catch (Exception e) {
            // ignore parsing errors and fall back to original body
        }
        return requestBody;
    }

    private record UpstreamIdentity(String provider, String scheme, String host, int port, String path) {
    }

}
