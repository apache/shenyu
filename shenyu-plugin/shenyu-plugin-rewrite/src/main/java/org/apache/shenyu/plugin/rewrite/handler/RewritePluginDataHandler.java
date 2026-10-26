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

package org.apache.shenyu.plugin.rewrite.handler;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.rule.RewriteHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.RewriteHandleUtils;
import org.apache.shenyu.plugin.base.cache.CommonHandleCache;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.base.utils.BeanHolder;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;
import java.util.Optional;
import java.util.function.Supplier;

/**
 * The type redirect plugin data subscriber.
 */
public class RewritePluginDataHandler implements PluginDataHandler {

    public static final Supplier<CommonHandleCache<String, RewriteHandle>> CACHED_HANDLE = new BeanHolder<>(CommonHandleCache::new);

    private static final Logger LOG = LoggerFactory.getLogger(RewritePluginDataHandler.class);

    @Override
    public void handlerRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData.getHandle()).ifPresent(handle -> {
            if (StringUtils.isBlank(handle)) {
                removeRule(ruleData);
                return;
            }
            final RewriteHandle rewriteHandle = GsonUtils.getInstance().fromJson(handle, RewriteHandle.class);
            if (Objects.isNull(rewriteHandle) || !canRewrite(rewriteHandle, ruleData)) {
                removeRule(ruleData);
                return;
            }
            CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), rewriteHandle);
        });
    }

    @Override
    public void removeRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData).ifPresent(data -> CACHED_HANDLE.get().removeHandle(CacheKeyUtils.INST.getKey(data)));
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.REWRITE.getName();
    }

    /**
     * Check whether the plugin can apply the rule handle, so that an unusable rule is
     * rejected when it is published instead of failing every matching request.
     *
     * @param rewriteHandle the parsed rule handle
     * @param ruleData the rule that owns the handle
     * @return true when the plugin can apply the handle
     */
    private static boolean canRewrite(final RewriteHandle rewriteHandle, final RuleData ruleData) {
        final String problem = RewriteHandleUtils.applicabilityProblem(rewriteHandle.getRegex(), rewriteHandle.getReplace());
        if (Objects.isNull(problem)) {
            return true;
        }
        LOG.error("invalid rewrite rule [{}] is ignored, regex: {}, replace: {}, reason: {}",
                ruleData.getId(), rewriteHandle.getRegex(), rewriteHandle.getReplace(), problem);
        return false;
    }
}
