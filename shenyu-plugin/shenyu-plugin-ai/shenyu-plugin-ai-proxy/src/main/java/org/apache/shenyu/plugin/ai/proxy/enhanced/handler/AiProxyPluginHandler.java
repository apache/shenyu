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
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.rule.AiProxyRuleOverrides;
import org.apache.shenyu.common.dto.convert.rule.AiProxyHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.proxy.enhanced.cache.AiProxyApiKeyCache;
import org.apache.shenyu.plugin.base.cache.CommonHandleCache;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * this is ai proxy plugin handler.
 */
public class AiProxyPluginHandler implements PluginDataHandler {

    private static final Logger LOG = LoggerFactory.getLogger(AiProxyPluginHandler.class);

    private final CommonHandleCache<String, AiProxyHandle> selectorCachedHandle = new CommonHandleCache<>();

    private volatile AiCommonConfig globalConfig;

    private final ConcurrentMap<String, PublishedRule> rules = new ConcurrentHashMap<>();

    public AiProxyPluginHandler() {
    }

    @Override
    public void handlerPlugin(final PluginData pluginData) {
        if (Objects.isNull(pluginData)) {
            return;
        }
        try {
            globalConfig = Boolean.FALSE.equals(pluginData.getEnabled()) || StringUtils.isBlank(pluginData.getConfig())
                    ? null : GsonUtils.getInstance().fromJson(pluginData.getConfig(), AiCommonConfig.class);
        } catch (JsonParseException e) {
            globalConfig = null;
            LOG.warn("Invalid configuration for plugin {}; cleared global defaults", pluginData.getName());
        }
    }

    @Override
    public void removePlugin(final PluginData pluginData) {
        globalConfig = null;
        rules.clear();
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
        rules.entrySet().removeIf(entry -> Objects.equals(entry.getValue().selectorId(), selectorData.getId()));
    }

    @Override
    public void handlerRule(final RuleData ruleData) {
        AiProxyRuleOverrides overrides = AiProxyRuleOverrides.parse(ruleData.getHandle());
        if (Objects.nonNull(ruleData.getId())) {
            rules.put(ruleData.getId(), new PublishedRule(ruleData.getSelectorId(), overrides));
        }
    }

    @Override
    public void removeRule(final RuleData ruleData) {
        if (Objects.nonNull(ruleData.getId())) {
            rules.remove(ruleData.getId());
        }
    }

    /**
     * Read an immutable published Rule value without parsing or copying on the request path.
     * @param selectorId actual selected owner
     * @param ruleId actual matched Rule
     * @return matching generation defaults or no overrides
     */
    public AiProxyRuleOverrides getRuleOverrides(final String selectorId, final String ruleId) {
        PublishedRule value = Objects.isNull(ruleId) ? null : rules.get(ruleId);
        return Objects.nonNull(value) && Objects.equals(selectorId, value.selectorId()) ? value.overrides() : AiProxyRuleOverrides.NONE;
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

    private record PublishedRule(String selectorId, AiProxyRuleOverrides overrides) {
    }
}
