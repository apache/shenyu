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

package org.dromara.soul.plugin.redirect.handler;

import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.convert.RedirectHandle;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.plugin.base.handler.PluginDataHandler;
import org.dromara.soul.plugin.redirect.cache.RedirectRuleHandleCache;

import java.util.Optional;

/**
 * The type redirect plugin data subscriber.
 *
 * @author zl
 */
public class RedirectPluginDataHandler implements PluginDataHandler {

    @Override
    public void handlerRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData.getHandle()).ifPresent(s -> {
            final RedirectHandle redirectHandle = GsonUtils.getInstance().fromJson(s, RedirectHandle.class);
            RedirectRuleHandleCache.getInstance().cachedHandle(getCacheKeyName(ruleData), redirectHandle);
        });
    }

    @Override
    public void removeRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData.getHandle()).ifPresent(s -> {
            RedirectRuleHandleCache.getInstance().removeHandle(getCacheKeyName(ruleData));
        });
    }

    /**
     * return rule handle cache key name.
     *
     * @param ruleData ruleData
     * @return string string
     */
    public static String getCacheKeyName(final RuleData ruleData) {
        return ruleData.getSelectorId() + "_" + ruleData.getName();
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.REDIRECT.getName();
    }
}
