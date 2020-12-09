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

package org.dromara.soul.sync.data.nacos.handler;

import com.alibaba.nacos.api.config.ConfigService;
import com.alibaba.nacos.api.config.listener.Listener;
import com.google.common.collect.Maps;
import com.google.gson.JsonParseException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.Executor;
import lombok.Getter;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.sync.data.api.AuthDataSubscriber;
import org.dromara.soul.sync.data.api.MetaDataSubscriber;
import org.dromara.soul.sync.data.api.PluginDataSubscriber;

/**
 * Nacos cache handler.
 *
 * @author Chenxjx
 * @author xiaoyu
 */
@Slf4j
public class NacosCacheHandler {

    protected static final String GROUP = "DEFAULT_GROUP";

    protected static final String PLUGIN_DATA_ID = "soul.plugin.json";

    protected static final String SELECTOR_DATA_ID = "soul.selector.json";

    protected static final String RULE_DATA_ID = "soul.rule.json";

    protected static final String AUTH_DATA_ID = "soul.auth.json";

    protected static final String META_DATA_ID = "soul.meta.json";

    protected static final Map<String, List<Listener>> LISTENERS = Maps.newConcurrentMap();

    @Getter
    private final ConfigService configService;

    private final PluginDataSubscriber pluginDataSubscriber;

    private final List<MetaDataSubscriber> metaDataSubscribers;

    private final List<AuthDataSubscriber> authDataSubscribers;

    public NacosCacheHandler(final ConfigService configService, final PluginDataSubscriber pluginDataSubscriber,
                             final List<MetaDataSubscriber> metaDataSubscribers,
                             final List<AuthDataSubscriber> authDataSubscribers) {
        this.configService = configService;
        this.pluginDataSubscriber = pluginDataSubscriber;
        this.metaDataSubscribers = metaDataSubscribers;
        this.authDataSubscribers = authDataSubscribers;
    }

    protected void updatePluginMap(final String configInfo) {
        try {
            List<PluginData> pluginDataList = GsonUtils.getInstance().fromList(configInfo, PluginData.class);
            pluginDataList.forEach(pluginData -> Optional.ofNullable(pluginDataSubscriber).ifPresent(e -> e.unSubscribe(pluginData)));
            pluginDataList.forEach(pluginData -> Optional.ofNullable(pluginDataSubscriber).ifPresent(e -> e.onSubscribe(pluginData)));
        } catch (JsonParseException e) {
            log.error("sync plugin data have error:", e);
        }
    }

    protected void updateSelectorMap(final String configInfo) {
        try {
            List<SelectorData> selectorDataList = GsonUtils.getInstance().fromList(configInfo, SelectorData.class);
            selectorDataList.forEach(selectorData -> Optional.ofNullable(pluginDataSubscriber).ifPresent(e -> e.unSelectorSubscribe(selectorData)));
            selectorDataList.forEach(selectorData -> Optional.ofNullable(pluginDataSubscriber).ifPresent(e -> e.onSelectorSubscribe(selectorData)));
        } catch (JsonParseException e) {
            log.error("sync selector data have error:", e);
        }
    }

    protected void updateRuleMap(final String configInfo) {
        try {
            List<RuleData> ruleDataList = GsonUtils.getInstance().fromList(configInfo, RuleData.class);
            ruleDataList.forEach(ruleData -> Optional.ofNullable(pluginDataSubscriber).ifPresent(e -> e.unRuleSubscribe(ruleData)));
            ruleDataList.forEach(ruleData -> Optional.ofNullable(pluginDataSubscriber).ifPresent(e -> e.onRuleSubscribe(ruleData)));
        } catch (JsonParseException e) {
            log.error("sync rule data have error:", e);
        }
    }

    protected void updateMetaDataMap(final String configInfo) {
        try {
            List<MetaData> metaDataList = GsonUtils.getInstance().fromList(configInfo, MetaData.class);
            metaDataList.forEach(metaData -> metaDataSubscribers.forEach(subscriber -> subscriber.unSubscribe(metaData)));
            metaDataList.forEach(metaData -> metaDataSubscribers.forEach(subscriber -> subscriber.onSubscribe(metaData)));
        } catch (JsonParseException e) {
            log.error("sync meta data have error:", e);
        }
    }

    protected void updateAuthMap(final String configInfo) {
        try {
            List<AppAuthData> appAuthDataList = GsonUtils.getInstance().fromList(configInfo, AppAuthData.class);
            appAuthDataList.forEach(authData -> authDataSubscribers.forEach(subscriber -> subscriber.unSubscribe(authData)));
            appAuthDataList.forEach(authData -> authDataSubscribers.forEach(subscriber -> subscriber.onSubscribe(authData)));
        } catch (JsonParseException e) {
            log.error("sync auth data have error:", e);
        }
    }

    @SneakyThrows
    private String getConfigAndSignListener(final String dataId, final Listener listener) {
        return configService.getConfigAndSignListener(dataId, GROUP, 6000, listener);
    }

    protected void watcherData(final String dataId, final OnChange oc) {
        Listener listener = new Listener() {
            @Override
            public void receiveConfigInfo(final String configInfo) {
                oc.change(configInfo);
            }

            @Override
            public Executor getExecutor() {
                return null;
            }
        };
        oc.change(getConfigAndSignListener(dataId, listener));
        LISTENERS.getOrDefault(dataId, new ArrayList<>()).add(listener);
    }

    protected interface OnChange {

        void change(String changeData);
    }
}
