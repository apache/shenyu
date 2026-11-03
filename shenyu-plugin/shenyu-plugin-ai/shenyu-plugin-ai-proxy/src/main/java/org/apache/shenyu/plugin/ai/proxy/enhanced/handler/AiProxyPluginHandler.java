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

package org.apache.shenyu.plugin.ai.proxy.enhanced.handler;

import com.google.gson.JsonParseException;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.AiProxyHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.AiProxyApiKeyCache;
import org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory;
import org.apache.shenyu.plugin.ai.proxy.enhanced.service.AiProxyConfigValidator;
import org.apache.shenyu.plugin.base.cache.CommonHandleCache;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;
import java.util.Set;

/**
 * this is ai proxy plugin handler.
 */
public class AiProxyPluginHandler implements PluginDataHandler {

    private static final Logger LOG = LoggerFactory.getLogger(AiProxyPluginHandler.class);

    private static final Set<String> VALIDATION_REASONS = Set.of("maxTokens.nonPositive", "temperature.notFinite", "temperature.negative",
            "baseUrl.invalidSyntax", "baseUrl.invalidEndpoint", "provider.unregistered", "protocol.unregistered");

    private final CommonHandleCache<String, AiProxyHandle> selectorCachedHandle = new CommonHandleCache<>();

    private final AiProxyProviderFactory providerFactory;

    private final AiProxyProtocolFactory protocolFactory;

    private volatile AiCommonConfig globalConfig;

    /**
     * Create a handler without registries for programmatic compatibility.
     * Provider and protocol names are checked when requests are prepared.
     * Logs one warning per instance about the missing update-time name checks.
     * Use the registry constructor to also check names before publishing defaults.
     */
    public AiProxyPluginHandler() {
        providerFactory = null;
        protocolFactory = null;
        LOG.warn("AI proxy defaults validationMode=withoutRegistries; provider and protocol names are checked only when preparing requests");
    }

    /**
     * Create a handler that validates names against the runtime registries.
     *
     * @param providerFactory registered providers
     * @param protocolFactory registered protocols
     */
    public AiProxyPluginHandler(final AiProxyProviderFactory providerFactory, final AiProxyProtocolFactory protocolFactory) {
        this.providerFactory = Objects.requireNonNull(providerFactory, "providerFactory");
        this.protocolFactory = Objects.requireNonNull(protocolFactory, "protocolFactory");
    }

    @Override
    public void handlerPlugin(final PluginData pluginData) {
        if (Objects.isNull(pluginData)) {
            return;
        }
        if (Boolean.FALSE.equals(pluginData.getEnabled())) {
            globalConfig = null;
            LOG.info("AI proxy defaults action=disabled pluginId={} namespaceId={}", pluginData.getId(), pluginData.getNamespaceId());
            return;
        }
        if (StringUtils.isBlank(pluginData.getConfig())) {
            globalConfig = null;
            LOG.info("AI proxy defaults action=cleared reason=blankConfig pluginId={} namespaceId={}", pluginData.getId(), pluginData.getNamespaceId());
            return;
        }
        final AiCommonConfig candidate;
        try {
            candidate = GsonUtils.getInstance().fromJson(pluginData.getConfig(), AiCommonConfig.class);
        } catch (JsonParseException e) {
            LOG.warn("AI proxy defaults action=rejected reason=malformedConfig pluginId={} namespaceId={}", pluginData.getId(), pluginData.getNamespaceId());
            return;
        }
        if (Objects.isNull(candidate)) {
            globalConfig = null;
            LOG.info("AI proxy defaults action=cleared reason=jsonNull pluginId={} namespaceId={}", pluginData.getId(), pluginData.getNamespaceId());
            return;
        }
        try {
            validateConfig(candidate);
        } catch (RuntimeException e) {
            logRejectedConfig(e, pluginData);
            return;
        }
        globalConfig = candidate;
    }

    private void logRejectedConfig(final RuntimeException error, final PluginData pluginData) {
        String reason = error.getMessage();
        if (error instanceof IllegalArgumentException && Objects.nonNull(reason) && VALIDATION_REASONS.contains(reason)) {
            LOG.warn("AI proxy defaults action=rejected reason={} pluginId={} namespaceId={}", reason, pluginData.getId(), pluginData.getNamespaceId());
            return;
        }
        LOG.error("AI proxy defaults action=rejected reason=unexpectedValidationFailure exceptionType={} pluginId={} namespaceId={}",
                error.getClass().getSimpleName(), pluginData.getId(), pluginData.getNamespaceId());
    }

    private void validateConfig(final AiCommonConfig candidate) {
        AiProxyConfigValidator.validate(candidate);
        if (Objects.nonNull(providerFactory) && StringUtils.isNotBlank(candidate.getProvider())) {
            try {
                providerFactory.getProvider(candidate.getProvider());
            } catch (ShenyuException e) {
                throw new IllegalArgumentException("provider.unregistered");
            }
        }
        if (Objects.nonNull(protocolFactory) && StringUtils.isNotBlank(candidate.getProtocol())) {
            try {
                protocolFactory.getProtocol(candidate.getProtocol());
            } catch (ShenyuException e) {
                throw new IllegalArgumentException("protocol.unregistered");
            }
        }
    }

    @Override
    public void removePlugin(final PluginData pluginData) {
        globalConfig = null;
        LOG.info("AI proxy defaults action=removed pluginId={} namespaceId={}",
                Objects.isNull(pluginData) ? null : pluginData.getId(), Objects.isNull(pluginData) ? null : pluginData.getNamespaceId());
    }

    /**
     * Return a request-local copy of the latest plugin defaults.
     *
     * @return global configuration, or null when cleared or disabled
     */
    public AiCommonConfig getGlobalConfig() {
        AiCommonConfig snapshot = globalConfig;
        return Objects.isNull(snapshot) ? null : new AiCommonConfig(snapshot);
    }

    @Override
    public void handlerSelector(final SelectorData selectorData) {
        // Do NOT remove AiProxyApiKeyCache here. Admin will push updated AI_PROXY_API_KEY events
        // with refreshed realApiKey after selector changes. Removing here introduces a window of misses.
        if (Objects.isNull(selectorData.getHandle()) || selectorData.getHandle().isEmpty()) {
            // Clear stale cached handle when selector handle is removed/cleared
            selectorCachedHandle
                    .removeHandle(CacheKeyUtils.INST.getKey(selectorData.getId(), Constants.DEFAULT_RULE));
            return;
        }
        AiProxyHandle aiProxyHandle = GsonUtils.getInstance().fromJson(selectorData.getHandle(), AiProxyHandle.class);
        aiProxyHandle.normalize();
        selectorCachedHandle
                .cachedHandle(CacheKeyUtils.INST.getKey(selectorData.getId(), Constants.DEFAULT_RULE), aiProxyHandle);
    }

    @Override
    public void removeSelector(final SelectorData selectorData) {
        AiProxyApiKeyCache.getInstance().removeBySelectorId(selectorData.getId());
        selectorCachedHandle
                .removeHandle(CacheKeyUtils.INST.getKey(selectorData.getId(), Constants.DEFAULT_RULE));
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.AI_PROXY.getName();
    }

    /**
     * Gets selector cached handle.
     *
     * @return the selector cached handle
     */
    public CommonHandleCache<String, AiProxyHandle> getSelectorCachedHandle() {
        return selectorCachedHandle;
    }
}
