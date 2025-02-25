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

package org.apache.shenyu.plugin.ai.proxy.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.plugin.AiProxyConfig;
import org.apache.shenyu.common.dto.convert.rule.AiProxyHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.base.cache.CommonHandleCache;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.base.utils.BeanHolder;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;

import java.util.Objects;
import java.util.Optional;
import java.util.function.Supplier;

/**
 * this is ai proxy plugin.
 */
public class AiProxyPluginHandler implements PluginDataHandler {
    
    public static final Supplier<CommonHandleCache<String, AiProxyHandle>> CACHED_HANDLE = new BeanHolder<>(CommonHandleCache::new);
    
    @Override
    public void handlerPlugin(final PluginData pluginData) {
        if (Objects.nonNull(pluginData) && pluginData.getEnabled()) {
            AiProxyConfig aiProxyConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(), AiProxyConfig.class);
            if (Objects.isNull(aiProxyConfig)) {
                return;
            }
            Singleton.INST.single(AiProxyConfig.class, aiProxyConfig);
        }
    }
    
    @Override
    public void handlerRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData.getHandle())
                .ifPresent(s -> {
                    AiProxyHandle aiProxyHandle = GsonUtils.getInstance().fromJson(s, AiProxyHandle.class);
                    CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), aiProxyHandle);
                });
    }
    
    @Override
    public void removeRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData)
                .ifPresent(s -> CACHED_HANDLE.get().removeHandle(CacheKeyUtils.INST.getKey(ruleData)));
    }
    
    @Override
    public String pluginNamed() {
        return PluginEnum.AI_PROXY.getName();
    }
}
