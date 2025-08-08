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

package org.apache.shenyu.plugin.ai.transformer.response.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.plugin.AiResponseTransformerConfig;
import org.apache.shenyu.common.dto.convert.rule.AiResponseTransformerHandle;
import org.apache.shenyu.common.enums.AiModelProviderEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.common.spring.ai.AiModelFactory;
import org.apache.shenyu.plugin.ai.common.spring.ai.registry.AiModelFactoryRegistry;
import org.apache.shenyu.plugin.ai.transformer.response.cache.ChatClientCache;
import org.apache.shenyu.plugin.base.cache.CommonHandleCache;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.base.utils.BeanHolder;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;
import java.util.Optional;
import java.util.function.Supplier;

/**
 * this is ai response transformer plugin.
 */
public class AiResponseTransformerPluginHandler implements PluginDataHandler {
    public static final Supplier<CommonHandleCache<String, AiResponseTransformerHandle>> CACHED_HANDLE = new BeanHolder<>(CommonHandleCache::new);

    private static final Logger LOG = LoggerFactory.getLogger(AiResponseTransformerPluginHandler.class);

    private final AiModelFactoryRegistry aiModelFactoryRegistry;

    public AiResponseTransformerPluginHandler(final AiModelFactoryRegistry aiModelFactoryRegistry) {
        this.aiModelFactoryRegistry = aiModelFactoryRegistry;
    }

    @Override
    public void handlerPlugin(final PluginData pluginData) {
        if (Objects.nonNull(pluginData) && pluginData.getEnabled()) {
            try {
                AiResponseTransformerConfig aiResponseTransformerConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(), AiResponseTransformerConfig.class);
                if (Objects.isNull(aiResponseTransformerConfig)) {
                    return;
                }
                AiModelFactory factory = aiModelFactoryRegistry.getFactory(AiModelProviderEnum.getByName(aiResponseTransformerConfig.getProvider()));
                ChatClientCache.getInstance().init("default", factory.createAiModel(convertConfig(aiResponseTransformerConfig)));
                Singleton.INST.single(AiResponseTransformerConfig.class, aiResponseTransformerConfig);
            } catch (Exception e) {

                return;
            }
        }
    }

    @Override
    public void handlerRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData.getHandle()).ifPresent(s -> {
            try {
                AiResponseTransformerHandle aiResponseTransformerHandle = GsonUtils.getInstance().fromJson(s, AiResponseTransformerHandle.class);
                if (Objects.nonNull(aiResponseTransformerHandle)) {
                    CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), aiResponseTransformerHandle);
                }
            } catch (Exception e) {
                LOG.error("AiResponseTransformerPluginHandler handle rule error", e);
                return;
            }
        });
        ChatClientCache.getInstance().destroyClient(ruleData.getId());
    }

    @Override
    public void removeRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData.getHandle()).ifPresent(s -> CACHED_HANDLE.get().removeHandle(CacheKeyUtils.INST.getKey(ruleData)));
        try {
            AiResponseTransformerHandle aiResponseTransformerHandle = GsonUtils.getInstance().fromJson(ruleData.getHandle(), AiResponseTransformerHandle.class);
            if (Objects.nonNull(aiResponseTransformerHandle) && Objects.nonNull(aiResponseTransformerHandle.getProvider())) {
                ChatClientCache.getInstance().destroyClient(ruleData.getId() + aiResponseTransformerHandle.getProvider());
            }
        } catch (Exception e) {
            LOG.error("AiResponseTransformerPluginHandler remove rule error", e);
            return;
        }
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.AI_RESPONSE_TRANSFORMER.getName();
    }

    public static AiCommonConfig convertConfig(final AiResponseTransformerConfig aiResponseTransformerConfig) {
        AiCommonConfig aiCommonConfig = new AiCommonConfig();
        Optional.ofNullable(aiResponseTransformerConfig.getBaseUrl()).ifPresent(aiCommonConfig::setBaseUrl);
        Optional.ofNullable(aiResponseTransformerConfig.getProvider()).ifPresent(aiCommonConfig::setProvider);
        Optional.ofNullable(aiResponseTransformerConfig.getModel()).ifPresent(aiCommonConfig::setModel);
        Optional.ofNullable(aiResponseTransformerConfig.getApiKey()).ifPresent(aiCommonConfig::setApiKey);
        return aiCommonConfig;
    }
} 
