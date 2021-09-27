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

package org.apache.shenyu.plugin.apache.dubbo.handler;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.convert.plugin.DubboRegisterConfig;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.DubboRuleHandle;
import org.apache.shenyu.common.dto.convert.selector.DubboSelectorHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.CollectionUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.apache.dubbo.cache.ApplicationConfigCache;
import org.apache.shenyu.plugin.base.cache.CommonHandleCache;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.base.utils.BeanHolder;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.function.Supplier;
import java.util.stream.Collectors;

/**
 * The type Apache dubbo plugin data handler.
 */
public class ApacheDubboPluginDataHandler implements PluginDataHandler {

    public static final Supplier<CommonHandleCache<String, DubboRuleHandle>> RULE_CACHED_HANDLE = new BeanHolder<>(CommonHandleCache::new);

    public static final Supplier<CommonHandleCache<String, List<DubboSelectorHandle>>> SELECTOR_CACHED_HANDLE = new BeanHolder<>(CommonHandleCache::new);

    @Override
    public void handlerPlugin(final PluginData pluginData) {
        if (null != pluginData && pluginData.getEnabled()) {
            DubboRegisterConfig dubboRegisterConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(), DubboRegisterConfig.class);
            DubboRegisterConfig exist = Singleton.INST.get(DubboRegisterConfig.class);
            if (Objects.isNull(dubboRegisterConfig)) {
                return;
            }
            if (Objects.isNull(exist) || !dubboRegisterConfig.equals(exist)) {
                // If it is null, initialize it
                ApplicationConfigCache.getInstance().init(dubboRegisterConfig);
                ApplicationConfigCache.getInstance().invalidateAll();
            }
            Singleton.INST.single(DubboRegisterConfig.class, dubboRegisterConfig);
        }
    }

    @Override
    public void handlerSelector(final SelectorData selectorData) {
        List<DubboSelectorHandle> dubboSelectorHandles = GsonUtils.getInstance().fromList(selectorData.getHandle(), DubboSelectorHandle.class);
        if (CollectionUtils.isEmpty(dubboSelectorHandles)) {
            return;
        }
        List<DubboSelectorHandle> graySelectorHandle = new ArrayList<>();
        for (DubboSelectorHandle each : dubboSelectorHandles) {
            if (StringUtils.isNotBlank(each.getUpstreamUrl()) && Objects.nonNull(each.isGray()) && each.isGray()) {
                graySelectorHandle.add(each);
            }
        }
        if (CollectionUtils.isNotEmpty(graySelectorHandle)) {
            SELECTOR_CACHED_HANDLE.get().cachedHandle(selectorData.getId(), graySelectorHandle);
            UpstreamCacheManager.getInstance().submit(selectorData.getId(), convertUpstreamList(graySelectorHandle));
        }
    }

    @Override
    public void removeSelector(final SelectorData selectorData) {
        SELECTOR_CACHED_HANDLE.get().removeHandle(selectorData.getId());
        UpstreamCacheManager.getInstance().removeByKey(selectorData.getId());
    }

    @Override
    public void handlerRule(final RuleData ruleData) {
        RULE_CACHED_HANDLE.get().cachedHandle(ruleData.getId(), GsonUtils.getInstance().fromJson(ruleData.getHandle(), DubboRuleHandle.class));
    }

    @Override
    public void removeRule(final RuleData ruleData) {
        RULE_CACHED_HANDLE.get().removeHandle(ruleData.getId());
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.DUBBO.getName();
    }

    private List<Upstream> convertUpstreamList(final List<DubboSelectorHandle> handleList) {
        return handleList.stream().map(u -> Upstream.builder()
                .protocol(u.getProtocol())
                .url(u.getUpstreamUrl())
                .weight(u.getWeight())
                .status(u.isStatus())
                .timestamp(u.getTimestamp())
                .warmup(u.getWarmup())
                .group(u.getGroup())
                .version(u.getVersion())
                .build()).collect(Collectors.toList());
    }
}
