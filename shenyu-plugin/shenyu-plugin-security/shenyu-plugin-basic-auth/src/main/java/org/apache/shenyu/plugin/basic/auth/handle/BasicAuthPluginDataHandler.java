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

package org.apache.shenyu.plugin.basic.auth.handle;

import java.util.Map;
import java.util.Optional;
import java.util.function.Supplier;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.base.cache.CommonHandleCache;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.base.utils.BeanHolder;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.basic.auth.config.BasicAuthConfig;
import org.apache.shenyu.plugin.basic.auth.rule.BasicAuthRuleHandle;

/**
 * Configuration data of the basic auth plugin.
 */
public class BasicAuthPluginDataHandler implements PluginDataHandler {

    public static final Supplier<CommonHandleCache<String, BasicAuthRuleHandle>> CACHED_HANDLE = new BeanHolder<>(CommonHandleCache::new);

    @Override
    public void handlerPlugin(final PluginData pluginData) {
        Map<String, String> configMap = GsonUtils.getInstance().toObjectMap(pluginData.getConfig(), String.class);
        String defaultHandleJson = Optional.ofNullable(configMap.get(Constants.DEFAULT_HANDLE_JSON)).orElse("");
        BasicAuthConfig basicAuthConfig = new BasicAuthConfig();
        basicAuthConfig.setDefaultHandleJson(defaultHandleJson);
        Singleton.INST.single(BasicAuthConfig.class, basicAuthConfig);
    }

    @Override
    public void removeRule(final RuleData ruleData) {
        CACHED_HANDLE.get().removeHandle(CacheKeyUtils.INST.getKey(ruleData));
    }

    @Override
    public void handlerRule(final RuleData ruleData) {
        BasicAuthConfig basicAuthConfig = Singleton.INST.get(BasicAuthConfig.class);
        Optional.ofNullable(ruleData.getHandle()).ifPresent(ruleHandle -> {
            BasicAuthRuleHandle basicAuthRuleHandle = BasicAuthRuleHandle.newInstance(StringUtils.defaultString(ruleHandle, basicAuthConfig.getDefaultHandleJson()));
            CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), basicAuthRuleHandle);
        });
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.BASIC_AUTH.getName();
    }
}
