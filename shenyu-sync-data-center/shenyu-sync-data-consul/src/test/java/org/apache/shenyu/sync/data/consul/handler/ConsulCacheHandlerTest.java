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

package org.apache.shenyu.sync.data.consul.handler;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.junit.Assert;
import org.junit.Test;

import java.util.List;
import java.util.ArrayList;
import java.util.Collections;

/**
 * test case for {@link ConsulCacheHandler}.
 */
@Slf4j
@SuppressWarnings("all")
public final class ConsulCacheHandlerTest {

    @SneakyThrows
    @Test
    public void testUpdatePluginMap() {
        String pluginName1 = "PLUGIN_NAME_1";
        String pluginName2 = "PLUGIN_NAME_2";
        PluginData pluginData1 =
                PluginData.builder().name(pluginName1).id("plugin_1").config("config_1").build();
        PluginData pluginData2 =
                PluginData.builder().name(pluginName2).id("plugin_2").config("config_2").build();
        String pluginData = GsonUtils.getInstance()
                .toJson(ImmutableMap.of(pluginName2, pluginData2, pluginName1, pluginData1));

        final List<PluginData> onSubscribeList = new ArrayList<>();
        final List<PluginData> unsubscribeList = new ArrayList<>();
        ConsulCacheHandler consulCacheHandler = new ConsulCacheHandler(new PluginDataSubscriber() {
            @Override
            public void onSubscribe(final PluginData pluginData) {
                onSubscribeList.add(pluginData);
            }

            @Override
            public void unSubscribe(final PluginData pluginData) {
                unsubscribeList.add(pluginData);
            }
        }, Collections.emptyList(), Collections.emptyList());
        consulCacheHandler.updatePluginData(pluginData);
        Assert.assertEquals(2, onSubscribeList.size());
        Assert.assertEquals(2, unsubscribeList.size());
    }

    @SneakyThrows
    @Test
    public void testUpdateSelectorMap() {
        String selectorDataPluginName1 = "SELECTOR_DATA_1";
        String selectorDataPluginName2 = "SELECTOR_DATA_2";
        SelectorData selectorData1 =
                SelectorData.builder()
                        .pluginName(selectorDataPluginName1)
                        .id("select_1")
                        .name("SELECT_DATA_NAME_1")
                        .build();
        SelectorData selectorData2 =
                SelectorData.builder()
                        .pluginName(selectorDataPluginName2)
                        .id("select_2")
                        .name("SELECT_DATA_NAME_2")
                        .build();

        String selectorDataParam = GsonUtils.getInstance()
                .toJson(ImmutableMap.of(selectorDataPluginName2, ImmutableList.of(selectorData2),
                        selectorDataPluginName1, ImmutableList.of(selectorData1)));
        final List<SelectorData> subscribeList = new ArrayList<>();
        final List<SelectorData> unsubscribeList = new ArrayList<>();
        ConsulCacheHandler consulCacheHandler = new ConsulCacheHandler(new PluginDataSubscriber() {
            @Override
            public void onSelectorSubscribe(final SelectorData selectorData) {
                subscribeList.add(selectorData);
            }

            @Override
            public void unSelectorSubscribe(final SelectorData selectorData) {
                unsubscribeList.add(selectorData);
            }
        }, Collections.emptyList(), Collections.emptyList());
        consulCacheHandler.updateSelectorMap(selectorDataParam);
        Assert.assertEquals(2, subscribeList.size());
        Assert.assertEquals(2, unsubscribeList.size());
    }

    @SneakyThrows
    @Test
    public void testUpdateRuleMap() {
        String ruleDataId1 = "RULE_DATA_1";
        String ruleDataId2 = "RULE_DATA_2";
        String selectorId1 = "ID_1";
        String selectorId2 = "ID_2";
        RuleData ruleData1 = RuleData.builder().selectorId(selectorId1).id(ruleDataId1).build();
        RuleData ruleData2 = RuleData.builder().selectorId(selectorId2).id(ruleDataId2).build();
        String ruleDataParam = GsonUtils.getInstance()
                .toJson(
                        ImmutableMap.of(
                                selectorId2,
                                ImmutableList.of(ruleData2),
                                selectorId1,
                                ImmutableList.of(ruleData1)));
        final List<RuleData> subscribeList = new ArrayList<>();
        final List<RuleData> unsubscribeList = new ArrayList<>();
        ConsulCacheHandler consulCacheHandler = new ConsulCacheHandler(new PluginDataSubscriber() {
            @Override
            public void onRuleSubscribe(final RuleData ruleData) {
                subscribeList.add(ruleData);
            }

            @Override
            public void unRuleSubscribe(final RuleData ruleData) {
                unsubscribeList.add(ruleData);
            }
        }, Collections.emptyList(), Collections.emptyList());
        consulCacheHandler.updateRuleMap(ruleDataParam);
        Assert.assertEquals(2, subscribeList.size());
        Assert.assertEquals(2, unsubscribeList.size());
    }

    @SneakyThrows
    @Test
    public void testUpdateMetaDataMap() {
        String metadataPath1 = "METADATA_PATH_1";
        String metadataPath2 = "METADATA_PATH_2";
        MetaData metaData1 = MetaData.builder().path(metadataPath1).id("meta_1").build();
        MetaData metaData2 = MetaData.builder().path(metadataPath2).id("meta_2").build();

        String metaDataParam = GsonUtils.getInstance()
                .toJson(ImmutableMap.of(metadataPath1, metaData1, metadataPath2, metaData2));
        final List<MetaData> subscribeList = new ArrayList<>();
        final List<MetaData> unsubscribeList = new ArrayList<>();
        MetaDataSubscriber metaDataSubscriber = new MetaDataSubscriber() {
            @Override
            public void onSubscribe(final MetaData metaData) {
                subscribeList.add(metaData);
            }

            @Override
            public void unSubscribe(final MetaData metaData) {
                unsubscribeList.add(metaData);
            }
        };
        ConsulCacheHandler consulCacheHandler = new ConsulCacheHandler(null, Lists.newArrayList(metaDataSubscriber),
                Collections.emptyList());
        consulCacheHandler.updateMetaDataMap(metaDataParam);
        Assert.assertEquals(2, subscribeList.size());
        Assert.assertEquals(2, unsubscribeList.size());
    }

    @SneakyThrows
    @Test
    public void testUpdateAuthMap() {
        String mockAppKey = "MOCK_APP_KEY";
        String mockAppKey2 = "MOCK_APP_KEY2";
        String mockAppSecret = "MOCK_APP_SECRET";
        AppAuthData appAuthData =
                AppAuthData.builder().appKey(mockAppKey).appSecret(mockAppSecret).enabled(true).build();
        AppAuthData appAuthData2 =
                AppAuthData.builder().appKey(mockAppKey2).appSecret(mockAppSecret).enabled(true).build();

        String appAuthDataParam = GsonUtils.getInstance()
                .toJson(ImmutableMap.of(mockAppKey2, appAuthData2, mockAppKey, appAuthData));
        final List<AppAuthData> subscribeList = new ArrayList<>();
        final List<AppAuthData> unsubscribeList = new ArrayList<>();

        AuthDataSubscriber authDataSubscriber = new AuthDataSubscriber() {
            @Override
            public void onSubscribe(final AppAuthData appAuthData) {
                subscribeList.add(appAuthData);
            }

            @Override
            public void unSubscribe(final AppAuthData appAuthData) {
                unsubscribeList.add(appAuthData);
            }
        };
        ConsulCacheHandler consulCacheHandler = new ConsulCacheHandler(null,
                Collections.emptyList(), Lists.newArrayList(authDataSubscriber));

        consulCacheHandler.updateAuthMap(appAuthDataParam);
        Assert.assertEquals(2, subscribeList.size());
        Assert.assertEquals(2, unsubscribeList.size());
    }
}
