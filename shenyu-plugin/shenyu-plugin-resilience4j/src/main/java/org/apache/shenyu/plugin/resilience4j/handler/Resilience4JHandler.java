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

package org.apache.shenyu.plugin.resilience4j.handler;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.Resilience4JHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.resilience4j.cache.Resilience4jRuleHandleCache;
import org.apache.shenyu.plugin.resilience4j.factory.Resilience4JRegistryFactory;

import java.util.Optional;

/**
 * Resilience4J rule handle.
 *
 * @author zhanglei
 */
public class Resilience4JHandler implements PluginDataHandler {

    @Override
    public void handlerRule(final RuleData ruleData) {
        Resilience4JRegistryFactory.remove(getResourceName(ruleData));
        Optional.ofNullable(ruleData.getHandle()).ifPresent(s -> {
            final Resilience4JHandle resilience4JHandle = GsonUtils.getInstance().fromJson(s, Resilience4JHandle.class);
            Resilience4jRuleHandleCache.getInstance().cachedHandle(getResourceName(ruleData), resilience4JHandle);
        });
    }

    @Override
    public void removeRule(final RuleData ruleData) {
        Resilience4JRegistryFactory.remove(getResourceName(ruleData));
        Optional.ofNullable(ruleData.getHandle()).ifPresent(s -> {
            Resilience4jRuleHandleCache.getInstance().removeHandle(getResourceName(ruleData));
        });
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.RESILIENCE4J.getName();
    }

    /**
     * Resource name.
     *
     * @param ruleData the ruleData
     * @return String
     */
    public static String getResourceName(final RuleData ruleData) {
        return ruleData.getSelectorId() + "_" + ruleData.getName();
    }
}
