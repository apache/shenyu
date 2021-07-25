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
import org.apache.shenyu.plugin.base.cache.RuleHandleCache;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.base.utils.BeanHolder;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.resilience4j.factory.Resilience4JRegistryFactory;

import java.util.Optional;
import java.util.function.Supplier;

/**
 * Resilience4J rule handle.
 */
public class Resilience4JHandler implements PluginDataHandler {

    public static final Supplier<RuleHandleCache<String, Resilience4JHandle>> CACHED_HANDLE = new BeanHolder(RuleHandleCache::new);

    @Override
    public void handlerRule(final RuleData ruleData) {
        String key = CacheKeyUtils.INST.getKey(ruleData);
        Resilience4JRegistryFactory.remove(key);
        Optional.ofNullable(ruleData.getHandle()).ifPresent(s -> {
            final Resilience4JHandle resilience4JHandle = GsonUtils.getInstance().fromJson(s, Resilience4JHandle.class);
            CACHED_HANDLE.get().cachedHandle(key, resilience4JHandle);
        });
    }

    @Override
    public void removeRule(final RuleData ruleData) {
        String key = CacheKeyUtils.INST.getKey(ruleData);
        Resilience4JRegistryFactory.remove(key);
        Optional.ofNullable(ruleData.getHandle()).ifPresent(s -> CACHED_HANDLE.get().removeHandle(key));
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.RESILIENCE4J.getName();
    }
}
