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

package org.apache.shenyu.plugin.ai.pii.handler;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.rule.PiiDetectionHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.base.cache.CommonHandleCache;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.base.utils.BeanHolder;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;

import java.util.Objects;

import java.util.function.Supplier;

/**
 * The pii detection plugin data handler. It compiles every rule handle into
 * an immutable {@link PiiDetectionRule} once per rule update, the request path
 * only reads the compiled result.
 */
public class PiiDetectionPluginDataHandler implements PluginDataHandler {

    /**
     * The plugin name.
     */
    public static final String PLUGIN_NAME = PluginEnum.PII_DETECTION.getName();

    /**
     * Cache the compiled rule, the key is the cache key of the rule.
     */
    public static final Supplier<CommonHandleCache<String, PiiDetectionRule>> CACHED_RULES =
            new BeanHolder<>(CommonHandleCache::new);

    @Override
    public void handlerRule(final RuleData ruleData) {
        String key = CacheKeyUtils.INST.getKey(ruleData);
        PiiDetectionHandle handle = null;
        if (Objects.nonNull(ruleData.getHandle())) {
            handle = GsonUtils.getInstance().fromJson(ruleData.getHandle(), PiiDetectionHandle.class);
        }
        if (Objects.isNull(handle)) {
            // a rule whose handle was cleared must not keep enforcing the previous
            // policy, so the compiled rule is dropped instead of left behind
            CACHED_RULES.get().removeHandle(key);
            return;
        }
        CACHED_RULES.get().cachedHandle(key, PiiDetectionRule.compile(key, handle));
    }

    @Override
    public void removeRule(final RuleData ruleData) {
        CACHED_RULES.get().removeHandle(CacheKeyUtils.INST.getKey(ruleData));
    }

    @Override
    public String pluginNamed() {
        return PLUGIN_NAME;
    }

    /**
     * Look up the compiled rule of a rule, exposed for tests.
     *
     * @param ruleData the rule
     * @return the compiled rule, or null when the rule carries no handle
     */
    public static PiiDetectionRule obtainRule(final RuleData ruleData) {
        Objects.requireNonNull(ruleData);
        return CACHED_RULES.get().obtainHandle(CacheKeyUtils.INST.getKey(ruleData));
    }
}
