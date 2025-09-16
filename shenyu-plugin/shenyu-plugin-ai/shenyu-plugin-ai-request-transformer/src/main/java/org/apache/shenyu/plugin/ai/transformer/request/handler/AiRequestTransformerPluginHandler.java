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

package org.apache.shenyu.plugin.ai.transformer.request.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.plugin.AiRequestTransformerConfig;
import org.apache.shenyu.common.dto.convert.rule.AiRequestTransformerHandle;
import org.apache.shenyu.common.enums.AiModelProviderEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.ai.common.config.AiCommonConfig;
import org.apache.shenyu.plugin.ai.common.spring.ai.AiModelFactory;
import org.apache.shenyu.plugin.ai.common.spring.ai.registry.AiModelFactoryRegistry;
import org.apache.shenyu.plugin.ai.transformer.request.cache.ChatClientCache;
import org.apache.shenyu.plugin.base.cache.CommonHandleCache;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.base.utils.BeanHolder;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;

import java.util.Objects;
import java.util.Optional;
import java.util.function.Supplier;

/**
 * this is ai request transformer plugin.
 */
public class AiRequestTransformerPluginHandler implements PluginDataHandler {

    public static final Supplier<CommonHandleCache<String, AiRequestTransformerHandle>> CACHED_HANDLE = new BeanHolder<>(CommonHandleCache::new);

    private final AiModelFactoryRegistry aiModelFactoryRegistry;

    public AiRequestTransformerPluginHandler(final AiModelFactoryRegistry aiModelFactoryRegistry) {
        this.aiModelFactoryRegistry = aiModelFactoryRegistry;
    }

    @Override
    public void handlerPlugin(final PluginData pluginData) {
        if (Objects.nonNull(pluginData) && pluginData.getEnabled()) {
            AiRequestTransformerConfig aiRequestTransformerConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(), AiRequestTransformerConfig.class);
            if (Objects.isNull(aiRequestTransformerConfig)) {
                return;
            }
            AiModelFactory factory = aiModelFactoryRegistry.getFactory(AiModelProviderEnum.getByName(aiRequestTransformerConfig.getProvider()));
            ChatClientCache.getInstance().init("default", factory.createAiModel(convertConfig(aiRequestTransformerConfig)));
            Singleton.INST.single(AiRequestTransformerConfig.class, aiRequestTransformerConfig);
        }
    }

    @Override
    public void handlerRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData.getHandle()).ifPresent(s -> {
            AiRequestTransformerHandle aiRequestTransformerHandle = GsonUtils.getInstance().fromJson(s, AiRequestTransformerHandle.class);
            CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), aiRequestTransformerHandle);
        });
        ChatClientCache.getInstance().destroyClient(ruleData.getId());
    }

    @Override
    public void removeRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData.getHandle()).ifPresent(s -> CACHED_HANDLE.get().removeHandle(CacheKeyUtils.INST.getKey(ruleData)));
        AiRequestTransformerHandle aiRequestTransformerHandle = GsonUtils.getInstance().fromJson(ruleData.getHandle(), AiRequestTransformerHandle.class);
        ChatClientCache.getInstance().destroyClient(ruleData.getId() + aiRequestTransformerHandle.getProvider());
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.AI_REQUEST_TRANSFORMER.getName();
    }

    public static AiCommonConfig convertConfig(final AiRequestTransformerConfig aiRequestTransformerConfig) {
        AiCommonConfig aiCommonConfig = new AiCommonConfig();
        Optional.ofNullable(aiRequestTransformerConfig.getBaseUrl()).ifPresent(aiCommonConfig::setBaseUrl);
        Optional.ofNullable(aiRequestTransformerConfig.getProvider()).ifPresent(aiCommonConfig::setProvider);
        Optional.ofNullable(aiRequestTransformerConfig.getModel()).ifPresent(aiCommonConfig::setModel);
        Optional.ofNullable(aiRequestTransformerConfig.getApiKey()).ifPresent(aiCommonConfig::setApiKey);
        return aiCommonConfig;
    }
}
