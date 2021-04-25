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

package org.dromara.soul.plugin.springcloud.handler;

import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.dto.convert.rule.impl.SpringCloudRuleHandle;
import org.dromara.soul.common.dto.convert.selector.SpringCloudSelectorHandle;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.plugin.base.handler.PluginDataHandler;
import org.dromara.soul.plugin.springcloud.cache.SpringCloudRuleHandleCache;
import org.dromara.soul.plugin.springcloud.cache.SpringCloudSelectorHandleCache;

import java.util.Optional;

/**
 * The type spring cloud plugin data handler.
 *
 * @author zl
 */
public class SpringCloudPluginDataHandler implements PluginDataHandler {

    @Override
    public void handlerSelector(final SelectorData selectorData) {
        Optional.ofNullable(selectorData.getHandle()).ifPresent(s -> {
            final SpringCloudSelectorHandle springCloudSelectorHandle = GsonUtils.getInstance().fromJson(s, SpringCloudSelectorHandle.class);
            SpringCloudSelectorHandleCache.getInstance().cachedHandle(selectorData.getId(), springCloudSelectorHandle);
        });
    }

    @Override
    public void removeSelector(final SelectorData selectorData) {
        Optional.ofNullable(selectorData.getHandle()).ifPresent(s -> {
            SpringCloudSelectorHandleCache.getInstance().removeHandle(selectorData.getId());
        });
    }

    @Override
    public void handlerRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData.getHandle()).ifPresent(s -> {
            final SpringCloudRuleHandle springCloudRuleHandle = GsonUtils.getInstance().fromJson(s, SpringCloudRuleHandle.class);
            SpringCloudRuleHandleCache.getInstance().cachedHandle(getRuleCacheKey(ruleData), springCloudRuleHandle);
        });
    }

    @Override
    public void removeRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData.getHandle()).ifPresent(s -> {
            SpringCloudRuleHandleCache.getInstance().removeHandle(getRuleCacheKey(ruleData));
        });
    }

    /**
     * return rule handle cache key name.
     *
     * @param ruleData ruleData
     * @return string string
     */
    public static String getRuleCacheKey(final RuleData ruleData) {
        return ruleData.getSelectorId() + "_" + ruleData.getName();
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.SPRING_CLOUD.getName();
    }
}
