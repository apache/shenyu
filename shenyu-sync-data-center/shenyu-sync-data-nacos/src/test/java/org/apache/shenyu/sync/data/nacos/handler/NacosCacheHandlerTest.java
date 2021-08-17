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

package org.apache.shenyu.sync.data.nacos.handler;

import com.alibaba.nacos.api.config.ConfigService;
import com.alibaba.nacos.api.exception.NacosException;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.CountDownLatch;
import java.util.stream.Collectors;

/**
 * add test case for {@link NacosCacheHandler}.
 */
@SuppressWarnings("all")
public final class NacosCacheHandlerTest {
    private static final ConcurrentMap<String, PluginData> PLUGIN_MAP = Maps.newConcurrentMap();

    private static final ConcurrentMap<String, List<SelectorData>> SELECTOR_MAP = Maps.newConcurrentMap();

    private static final ConcurrentMap<String, List<RuleData>> RULE_MAP = Maps.newConcurrentMap();

    private static final ConcurrentMap<String, AppAuthData> AUTH_MAP = Maps.newConcurrentMap();

    private static final ConcurrentMap<String, MetaData> META_DATA = Maps.newConcurrentMap();

    private static final Comparator<SelectorData> SELECTOR_DATA_COMPARATOR = Comparator.comparing(SelectorData::getSort);

    private static final Comparator<RuleData> RULE_DATA_COMPARATOR = Comparator.comparing(RuleData::getSort);

    private static final String GROUP = "DEFAULT_GROUP";

    private static final String PLUGIN_DATA_ID = "shenyu.plugin.json";

    private static final String SELECTOR_DATA_ID = "shenyu.selector.json";

    private static final String RULE_DATA_ID = "shenyu.rule.json";

    private static final String AUTH_DATA_ID = "shenyu.auth.json";

    private static final String META_DATA_ID = "shenyu.meta.json";

    private NacosCacheHandler nacosCacheHandlerService;

    private ConfigService configService;

    @Before
    public void setup() {
        configService = new NacosMockConfigService();
    }

    @Test
    public void testUpdatePluginMap() throws NacosException {
        String pluginName1 = "PLUGIN_NAME_1";
        String pluginName2 = "PLUGIN_NAME_2";
        PluginData pluginData1 =
                PluginData.builder().name(pluginName1).id("plugin_1").config("config_1").build();
        PluginData pluginData2 =
                PluginData.builder().name(pluginName2).id("plugin_2").config("config_2").build();
        String pluginData = GsonUtils.getInstance()
                .toJson(ImmutableMap.of(pluginName2, pluginData2, pluginName1, pluginData1));

        changePluginData(ImmutableList.of(pluginData1, pluginData2));

        final CountDownLatch latch = new CountDownLatch(2);
        final List<PluginData> onSubscribeList = new ArrayList<>();
        final List<PluginData> unsubscribeList = new ArrayList<>();
        nacosCacheHandlerService = new NacosCacheHandler(configService, new PluginDataSubscriber() {
            @Override
            public void onSubscribe(final PluginData pluginData) {
                onSubscribeList.add(pluginData);
                latch.countDown();
            }

            @Override
            public void unSubscribe(final PluginData pluginData) {
                unsubscribeList.add(pluginData);
                latch.countDown();
            }
        }, Collections.emptyList(), Collections.emptyList());
        nacosCacheHandlerService.updatePluginMap(pluginData);
        Assert.assertEquals(2, onSubscribeList.size());
        Assert.assertEquals(2, unsubscribeList.size());
        Assert.assertEquals(
                configService.getConfig(PLUGIN_DATA_ID, GROUP, 1),
                GsonUtils.getInstance()
                        .toJson(ImmutableMap.of(pluginName2, pluginData2, pluginName1, pluginData1)));

    }

    @Test
    public void testUpdateSelectorMap() throws NacosException {
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

        changeSelectorData(ImmutableList.of(selectorData1, selectorData2));
        String selectorDataParam = GsonUtils.getInstance()
                .toJson(ImmutableMap.of(selectorDataPluginName2, ImmutableList.of(selectorData2),
                        selectorDataPluginName1, ImmutableList.of(selectorData1)));
        final CountDownLatch latch = new CountDownLatch(2);
        final List<SelectorData> subscribeList = new ArrayList<>();
        final List<SelectorData> unsubscribeList = new ArrayList<>();
        nacosCacheHandlerService = new NacosCacheHandler(configService, new PluginDataSubscriber() {
            @Override
            public void onSelectorSubscribe(final SelectorData selectorData) {
                subscribeList.add(selectorData);
                latch.countDown();
            }

            @Override
            public void unSelectorSubscribe(final SelectorData selectorData) {
                unsubscribeList.add(selectorData);
                latch.countDown();
            }
        }, Collections.emptyList(), Collections.emptyList());
        nacosCacheHandlerService.updateSelectorMap(selectorDataParam);
        Assert.assertEquals(2, subscribeList.size());
        Assert.assertEquals(2, unsubscribeList.size());
        Assert.assertEquals(
                configService.getConfig(SELECTOR_DATA_ID, GROUP, 1),
                GsonUtils.getInstance()
                        .toJson(
                                ImmutableMap.of(
                                        selectorDataPluginName2,
                                        ImmutableList.of(selectorData2),
                                        selectorDataPluginName1,
                                        ImmutableList.of(selectorData1))));
    }

    @Test
    public void testUpdateRuleMap() throws NacosException {
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
        changeRuleData(ImmutableList.of(ruleData1, ruleData2));
        final CountDownLatch latch = new CountDownLatch(2);
        final List<RuleData> subscribeList = new ArrayList<>();
        final List<RuleData> unsubscribeList = new ArrayList<>();
        nacosCacheHandlerService = new NacosCacheHandler(configService, new PluginDataSubscriber() {
            @Override
            public void onRuleSubscribe(final RuleData ruleData) {
                subscribeList.add(ruleData);
                latch.countDown();
            }

            @Override
            public void unRuleSubscribe(final RuleData ruleData) {
                unsubscribeList.add(ruleData);
                latch.countDown();
            }
        }, Collections.emptyList(), Collections.emptyList());
        nacosCacheHandlerService.updateRuleMap(ruleDataParam);
        Assert.assertEquals(2, subscribeList.size());
        Assert.assertEquals(2, unsubscribeList.size());
        Assert.assertEquals(
                configService.getConfig(RULE_DATA_ID, GROUP, 1),
                GsonUtils.getInstance()
                        .toJson(
                                ImmutableMap.of(
                                        selectorId2,
                                        ImmutableList.of(ruleData2),
                                        selectorId1,
                                        ImmutableList.of(ruleData1))));
    }

    @Test
    public void testUpdateMetaDataMap() throws NacosException {
        String metadataPath1 = "METADATA_PATH_1";
        String metadataPath2 = "METADATA_PATH_2";
        MetaData metaData1 = MetaData.builder().path(metadataPath1).id("meta_1").build();
        MetaData metaData2 = MetaData.builder().path(metadataPath2).id("meta_2").build();

        changeMetaData(ImmutableList.of(metaData1, metaData2));
        String metaDataParam = GsonUtils.getInstance()
                .toJson(ImmutableMap.of(metadataPath1, metaData1, metadataPath2, metaData2));
        final CountDownLatch latch = new CountDownLatch(2);
        final List<MetaData> subscribeList = new ArrayList<>();
        final List<MetaData> unsubscribeList = new ArrayList<>();
        MetaDataSubscriber metaDataSubscriber = new MetaDataSubscriber() {
            @Override
            public void onSubscribe(final MetaData metaData) {
                subscribeList.add(metaData);
                latch.countDown();
            }

            @Override
            public void unSubscribe(final MetaData metaData) {
                unsubscribeList.add(metaData);
                latch.countDown();
            }
        };
        nacosCacheHandlerService = new NacosCacheHandler(configService, null, Lists.newArrayList(metaDataSubscriber),
                Collections.emptyList());
        nacosCacheHandlerService.updateMetaDataMap(metaDataParam);
        Assert.assertEquals(2, subscribeList.size());
        Assert.assertEquals(2, unsubscribeList.size());
        Assert.assertEquals(
                configService.getConfig(META_DATA_ID, GROUP, 1),
                GsonUtils.getInstance()
                        .toJson(ImmutableMap.of(metadataPath1, metaData1, metadataPath2, metaData2)));
    }

    @Test
    public void testUpdateAuthMap() throws NacosException {
        String mockAppKey = "MOCK_APP_KEY";
        String mockAppKey2 = "MOCK_APP_KEY2";
        String mockAppSecret = "MOCK_APP_SECRET";
        AppAuthData appAuthData =
                AppAuthData.builder().appKey(mockAppKey).appSecret(mockAppSecret).enabled(true).build();
        AppAuthData appAuthData2 =
                AppAuthData.builder().appKey(mockAppKey2).appSecret(mockAppSecret).enabled(true).build();

        changeAuthData(ImmutableList.of(appAuthData, appAuthData2));
        String appAuthDataParam = GsonUtils.getInstance()
                .toJson(ImmutableMap.of(mockAppKey2, appAuthData2, mockAppKey, appAuthData));
        final CountDownLatch latch = new CountDownLatch(2);
        final List<AppAuthData> subscribeList = new ArrayList<>();
        final List<AppAuthData> unsubscribeList = new ArrayList<>();

        AuthDataSubscriber authDataSubscriber = new AuthDataSubscriber() {
            @Override
            public void onSubscribe(final AppAuthData appAuthData) {
                subscribeList.add(appAuthData);
                latch.countDown();
            }

            @Override
            public void unSubscribe(final AppAuthData appAuthData) {
                unsubscribeList.add(appAuthData);
                latch.countDown();
            }
        };
        nacosCacheHandlerService = new NacosCacheHandler(configService, null,
                Collections.emptyList(), Lists.newArrayList(authDataSubscriber));

        nacosCacheHandlerService.updateAuthMap(appAuthDataParam);
        Assert.assertEquals(2, subscribeList.size());
        Assert.assertEquals(2, unsubscribeList.size());
        Assert.assertEquals(
                configService.getConfig(AUTH_DATA_ID, GROUP, 100),
                GsonUtils.getInstance()
                        .toJson(ImmutableMap.of(mockAppKey2, appAuthData2, mockAppKey, appAuthData)));
    }

    @Test
    public void testWatcherData() throws NacosException {
        String mockAppKey = "MOCK_APP_KEY";
        String mockAppKey2 = "MOCK_APP_KEY2";
        String mockAppSecret = "MOCK_APP_SECRET";
        AppAuthData appAuthData =
                AppAuthData.builder().appKey(mockAppKey).appSecret(mockAppSecret).enabled(true).build();
        AppAuthData appAuthData2 =
                AppAuthData.builder().appKey(mockAppKey2).appSecret(mockAppSecret).enabled(true).build();

        changeAuthData(ImmutableList.of(appAuthData, appAuthData2));
        String appAuthDataParam = GsonUtils.getInstance()
                .toJson(ImmutableMap.of(mockAppKey2, appAuthData2, mockAppKey, appAuthData));
        final CountDownLatch latch = new CountDownLatch(2);
        final List<AppAuthData> subscribeList = new ArrayList<>();
        final List<AppAuthData> unsubscribeList = new ArrayList<>();

        AuthDataSubscriber authDataSubscriber = new AuthDataSubscriber() {
            @Override
            public void onSubscribe(final AppAuthData appAuthData) {
                subscribeList.add(appAuthData);
                latch.countDown();
            }

            @Override
            public void unSubscribe(final AppAuthData appAuthData) {
                unsubscribeList.add(appAuthData);
                latch.countDown();
            }
        };
        nacosCacheHandlerService = new NacosCacheHandler(configService, null,
                Collections.emptyList(), Lists.newArrayList(authDataSubscriber));

        NacosCacheHandler.OnChange oc = nacosCacheHandlerService::updateAuthMap;
        nacosCacheHandlerService.watcherData(AUTH_DATA_ID, oc);
    }

    private void changePluginData(final List<PluginData> changed) throws NacosException {
        updateDataMap(getConfig(PLUGIN_DATA_ID));

        changed.forEach(plugin -> PLUGIN_MAP.put(plugin.getName(), plugin));

        publishConfig(PLUGIN_DATA_ID, PLUGIN_MAP);
    }

    private void changeSelectorData(final List<SelectorData> changed) throws NacosException {
        changeSelectorDataMap(getConfig(SELECTOR_DATA_ID));

        changed.forEach(selector -> {
            List<SelectorData> ls = SELECTOR_MAP
                    .getOrDefault(selector.getPluginName(), new ArrayList<>())
                    .stream()
                    .filter(s -> !s.getId().equals(selector.getId()))
                    .sorted(SELECTOR_DATA_COMPARATOR)
                    .collect(Collectors.toList());
            ls.add(selector);
            SELECTOR_MAP.put(selector.getPluginName(), ls);
        });

        publishConfig(SELECTOR_DATA_ID, SELECTOR_MAP);
    }

    private void changeRuleData(final List<RuleData> changed) throws NacosException {
        changeRuleDataMap(getConfig(RULE_DATA_ID));

        changed.forEach(rule -> {
            List<RuleData> ls = RULE_MAP
                    .getOrDefault(rule.getSelectorId(), new ArrayList<>())
                    .stream()
                    .filter(s -> !s.getId().equals(rule.getSelectorId()))
                    .sorted(RULE_DATA_COMPARATOR)
                    .collect(Collectors.toList());
            ls.add(rule);
            RULE_MAP.put(rule.getSelectorId(), ls);
        });

        publishConfig(RULE_DATA_ID, RULE_MAP);
    }

    private void changeMetaData(final List<MetaData> changed) throws NacosException {
        changeMetaDataMap(getConfig(META_DATA_ID));

        changed.forEach(meta -> {
            META_DATA
                    .values()
                    .stream()
                    .filter(md -> Objects.equals(md.getId(), meta.getId()))
                    .forEach(md -> META_DATA.remove(md.getPath()));

            META_DATA.put(meta.getPath(), meta);
        });
        publishConfig(META_DATA_ID, META_DATA);
    }

    private void changeAuthData(final List<AppAuthData> changed) throws NacosException {
        changeAppAuthDataMap(getConfig(AUTH_DATA_ID));

        changed.forEach(appAuth -> AUTH_MAP.put(appAuth.getAppKey(), appAuth));

        publishConfig(AUTH_DATA_ID, AUTH_MAP);
    }

    private void changeAppAuthDataMap(final String configInfo) {
        JsonObject jo = GsonUtils.getInstance().fromJson(configInfo, JsonObject.class);
        Set<String> set = new HashSet<>(AUTH_MAP.keySet());
        for (Map.Entry<String, JsonElement> e : jo.entrySet()) {
            set.remove(e.getKey());
            AUTH_MAP.put(e.getKey(), GsonUtils.getInstance().fromJson(e.getValue(), AppAuthData.class));
        }
        AUTH_MAP.keySet().removeAll(set);

    }

    private void changeMetaDataMap(final String configInfo) {
        JsonObject jo = GsonUtils.getInstance().fromJson(configInfo, JsonObject.class);
        Set<String> set = new HashSet<>(META_DATA.keySet());
        for (Map.Entry<String, JsonElement> e : jo.entrySet()) {
            set.remove(e.getKey());
            META_DATA.put(e.getKey(), GsonUtils.getInstance().fromJson(e.getValue(), MetaData.class));
        }
        META_DATA.keySet().removeAll(set);
    }

    private void changeRuleDataMap(final String configInfo) {
        JsonObject jo = GsonUtils.getInstance().fromJson(configInfo, JsonObject.class);
        Set<String> set = new HashSet<>(RULE_MAP.keySet());
        for (Map.Entry<String, JsonElement> e : jo.entrySet()) {
            set.remove(e.getKey());
            List<RuleData> ls = new ArrayList<>();
            e.getValue().getAsJsonArray().forEach(je -> ls.add(GsonUtils.getInstance().fromJson(je, RuleData.class)));
            RULE_MAP.put(e.getKey(), ls);
        }
        RULE_MAP.keySet().removeAll(set);
    }

    private void changeSelectorDataMap(final String config) {
        JsonObject jo = GsonUtils.getInstance().fromJson(config, JsonObject.class);
        Set<String> set = new HashSet<>(SELECTOR_MAP.keySet());
        for (Map.Entry<String, JsonElement> e : jo.entrySet()) {
            set.remove(e.getKey());
            List<SelectorData> ls = new ArrayList<>();
            e.getValue().getAsJsonArray().forEach(je -> ls.add(GsonUtils.getInstance().fromJson(je, SelectorData.class)));
            SELECTOR_MAP.put(e.getKey(), ls);
        }
        SELECTOR_MAP.keySet().removeAll(set);
    }

    private void updateDataMap(final String configInfo) {
        JsonObject jo = GsonUtils.getInstance().fromJson(configInfo, JsonObject.class);
        Set<String> set = new HashSet<>(PLUGIN_MAP.keySet());
        for (Map.Entry<String, JsonElement> e : jo.entrySet()) {
            set.remove(e.getKey());
            PLUGIN_MAP.put(e.getKey(), GsonUtils.getInstance().fromJson(e.getValue(), PluginData.class));
        }
        PLUGIN_MAP.keySet().removeAll(set);
    }

    private String getConfig(final String dataId) throws NacosException {
        return configService.getConfig(dataId, GROUP, 6000);
    }

    private void publishConfig(final String dataId, final Object data) throws NacosException {
        configService.publishConfig(dataId, GROUP, GsonUtils.getInstance().toJson(data));
    }
}
