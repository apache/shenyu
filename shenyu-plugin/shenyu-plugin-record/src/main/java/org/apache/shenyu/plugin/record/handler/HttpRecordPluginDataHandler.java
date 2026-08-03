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

package org.apache.shenyu.plugin.record.handler;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.HttpRecordHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.base.cache.CommonHandleCache;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.base.utils.BeanHolder;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.record.collector.HttpRecordCollector;
import org.apache.shenyu.plugin.record.config.HttpRecordCollectConfig;

import java.util.Optional;
import java.util.function.Supplier;

/**
 * HttpRecordPluginDataHandler.
 *
 * <p>Handles plugin/selector/rule data updates from the admin server,
 * caching the record handle config and starting/stopping the collector.</p>
 */
public class HttpRecordPluginDataHandler implements PluginDataHandler {

    public static final Supplier<CommonHandleCache<String, HttpRecordHandle>> CACHED_HANDLE =
            new BeanHolder<>(CommonHandleCache::new);

    @Override
    public String pluginNamed() {
        return PluginEnum.HTTP_RECORD.getName();
    }

    @Override
    public void handlerPlugin(final PluginData pluginData) {
        HttpRecordCollectConfig.RecordConfig recordConfig =
                GsonUtils.getGson().fromJson(pluginData.getConfig(), HttpRecordCollectConfig.RecordConfig.class);
        HttpRecordCollectConfig.INSTANCE.setRecordConfig(recordConfig);
        HttpRecordCollector.INSTANCE.start();
    }

    @Override
    public void removePlugin(final PluginData pluginData) {
        HttpRecordCollector.INSTANCE.stop();
        PluginDataHandler.super.removePlugin(pluginData);
    }

    @Override
    public void handlerSelector(final SelectorData selectorData) {
        CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(selectorData.getId(), Constants.DEFAULT_RULE),
                HttpRecordHandle.newDefaultInstance());
    }

    @Override
    public void removeSelector(final SelectorData selectorData) {
        CACHED_HANDLE.get().removeHandle(CacheKeyUtils.INST.getKey(selectorData.getId(), Constants.DEFAULT_RULE));
    }

    @Override
    public void handlerRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData.getHandle()).ifPresent(s -> {
            final HttpRecordHandle httpRecordHandle = GsonUtils.getInstance().fromJson(s, HttpRecordHandle.class);
            httpRecordHandle.setTaskId(String.format("%s_%s_%s",
                    httpRecordHandle.getStartTime(), httpRecordHandle.getEndTime(), ruleData.getId()));
            CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), httpRecordHandle);
        });
    }

    @Override
    public void removeRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData.getHandle())
                .ifPresent(s -> CACHED_HANDLE.get().removeHandle(CacheKeyUtils.INST.getKey(ruleData)));
    }
}
