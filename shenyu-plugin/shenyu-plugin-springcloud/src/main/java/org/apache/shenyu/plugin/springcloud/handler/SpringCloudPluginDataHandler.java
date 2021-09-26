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

package org.apache.shenyu.plugin.springcloud.handler;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.SpringCloudRuleHandle;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.dto.convert.selector.SpringCloudSelectorHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.CollectionUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.base.cache.CommonHandleCache;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.base.utils.BeanHolder;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;

import java.util.List;
import java.util.Optional;
import java.util.function.Supplier;
import java.util.stream.Collectors;

/**
 * The type spring cloud plugin data handler.
 */
public class SpringCloudPluginDataHandler implements PluginDataHandler {
    
    public static final Supplier<CommonHandleCache<String, SpringCloudSelectorHandle>> SELECTOR_CACHED = new BeanHolder<>(CommonHandleCache::new);
    
    public static final Supplier<CommonHandleCache<String, SpringCloudRuleHandle>> RULE_CACHED = new BeanHolder<>(CommonHandleCache::new);
    
    @Override
    public void handlerSelector(final SelectorData selectorData) {
        SpringCloudSelectorHandle springCloudSelectorHandle = GsonUtils.getInstance().fromJson(selectorData.getHandle(), SpringCloudSelectorHandle.class);
        SELECTOR_CACHED.get().cachedHandle(selectorData.getId(), springCloudSelectorHandle);
        if (CollectionUtils.isEmpty(springCloudSelectorHandle.getDivideUpstreams())) {
            UpstreamCacheManager.getInstance().removeByKey(selectorData.getId());
            return;
        }
        UpstreamCacheManager.getInstance().submit(selectorData.getId(), convertUpstreamList(springCloudSelectorHandle.getDivideUpstreams()));
    }

    @Override
    public void removeSelector(final SelectorData selectorData) {
        SELECTOR_CACHED.get().removeHandle(selectorData.getId());
        UpstreamCacheManager.getInstance().removeByKey(selectorData.getId());
    }

    @Override
    public void handlerRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData.getHandle()).ifPresent(s -> {
            SpringCloudRuleHandle springCloudRuleHandle = GsonUtils.getInstance().fromJson(s, SpringCloudRuleHandle.class);
            RULE_CACHED.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), springCloudRuleHandle);
        });
    }

    @Override
    public void removeRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData.getHandle()).ifPresent(s -> RULE_CACHED.get().removeHandle(CacheKeyUtils.INST.getKey(ruleData)));
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.SPRING_CLOUD.getName();
    }

    private List<Upstream> convertUpstreamList(final List<DivideUpstream> upstreamList) {
        return upstreamList.stream().map(u -> Upstream.builder()
                .protocol(u.getProtocol())
                .url(u.getUpstreamUrl())
                .weight(u.getWeight())
                .status(u.isStatus())
                .timestamp(u.getTimestamp())
                .warmup(u.getWarmup())
                .build()).collect(Collectors.toList());
    }
}
