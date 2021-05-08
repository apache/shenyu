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

package org.apache.shenyu.plugin.waf.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.WafHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.base.utils.Singleton;
import org.apache.shenyu.plugin.waf.config.WafConfig;
import org.apache.shenyu.plugin.waf.cache.WafRuleHandleCache;

import java.util.Optional;

/**
 * The type Waf plugin data handler.
 *
 * @author xiaoyu
 */
public class WafPluginDataHandler implements PluginDataHandler {

    @Override
    public void handlerPlugin(final PluginData pluginData) {
        WafConfig wafConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(), WafConfig.class);
        Singleton.INST.single(WafConfig.class, wafConfig);
    }

    @Override
    public void handlerRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData.getHandle()).ifPresent(s -> {
            final WafHandle wafHandle = GsonUtils.getInstance().fromJson(s, WafHandle.class);
            WafRuleHandleCache.getInstance().cachedHandle(getCacheKeyName(ruleData), wafHandle);
        });
    }

    @Override
    public void removeRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData.getHandle()).ifPresent(s -> {
            WafRuleHandleCache.getInstance().removeHandle(getCacheKeyName(ruleData));
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
        return PluginEnum.WAF.getName();
    }
}
