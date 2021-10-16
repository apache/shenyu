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

package org.apache.shenyu.plugin.hystrix.handler;

import com.netflix.hystrix.strategy.properties.HystrixPropertiesFactory;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.rule.HystrixHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.base.cache.CommonHandleCache;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.base.utils.BeanHolder;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;

import java.util.Optional;
import java.util.function.Supplier;

/**
 * The type Hystrix plugin data handler.
 */
public class HystrixPluginDataHandler implements PluginDataHandler {

    public static final Supplier<CommonHandleCache<String, HystrixHandle>> CACHED_HANDLE = new BeanHolder<>(CommonHandleCache::new);

    @Override
    public void handlerRule(final RuleData ruleData) {
        HystrixPropertiesFactory.reset();
        Optional.ofNullable(ruleData.getHandle()).ifPresent(rule -> {
            HystrixHandle hystrixHandle = GsonUtils.getInstance().fromJson(rule, HystrixHandle.class);
            CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), hystrixHandle);
        });
    }

    @Override
    public void removeRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData.getHandle()).ifPresent(rule -> CACHED_HANDLE.get().removeHandle(CacheKeyUtils.INST.getKey(ruleData)));
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.HYSTRIX.getName();
    }
}
