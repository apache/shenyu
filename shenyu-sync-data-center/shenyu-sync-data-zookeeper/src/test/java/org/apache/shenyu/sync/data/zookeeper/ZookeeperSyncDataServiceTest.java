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

package org.apache.shenyu.sync.data.zookeeper;

import com.google.common.collect.Lists;
import org.apache.curator.test.TestingServer;
import org.apache.shenyu.common.constant.DefaultPathConstants;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.zookeeper.CreateMode;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static junit.framework.TestCase.assertTrue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.Is.is;

public final class ZookeeperSyncDataServiceTest {

    private static final String MOCK_PLUGIN_PARENT_PATH = "/shenyu/plugin";

    private static final String MOCK_PLUGIN_PATH = "/shenyu/plugin/divide";

    private static final String MOCK_PLUGIN_NAME = "divide";

    private static final String MOCK_SELECTOR_PARENT_PATH = "/shenyu/selector/divide";

    private static final String MOCK_SELECTOR_PATH = "/shenyu/selector/divide/test";

    private static final String MOCK_SELECTOR_NAME = "test";

    private static final String MOCK_RULE_PARENT_PATH = "/shenyu/rule/divide";

    private static final String MOCK_RULE_PATH = "/shenyu/rule/divide/test-test";

    private static final String MOCK_RULE_NAME = "test-test";

    private static final String MOCK_APP_AUTH_PARENT_PATH = "/shenyu/auth";

    private static final String MOCK_APP_AUTH_PATH = "/shenyu/auth/test";

    private static final String MOCK_APP_AUTH_KEY = "test";

    private static final String MOCK_META_DATA_PARENT_PATH = "/shenyu/metaData";

    private static final String MOCK_META_DATA_PATH = "/shenyu/metaData/test";

    private static final String MOCK_META_DATA_ID = "test";

    private ZookeeperClient zkClient;

    private ZookeeperSyncDataService syncDataService;

    @BeforeEach
    public void setUp() throws Exception {
        TestingServer server = new TestingServer();
        ZookeeperConfig config = new ZookeeperConfig(server.getConnectString());
        zkClient = new ZookeeperClient(config);
        zkClient.start();
    }

    @Test
    public void testWatchPluginWhenInit() throws InterruptedException {
        final List<PluginData> subscribeList = new ArrayList<>(1);
        PluginData pluginData = PluginData.builder().name(MOCK_PLUGIN_NAME).enabled(Boolean.FALSE).build();
        zkClient.createOrUpdate(MOCK_PLUGIN_PATH, pluginData, CreateMode.PERSISTENT);

        syncDataService = new ZookeeperSyncDataService(zkClient, new PluginDataSubscriber() {
            @Override
            public void onSubscribe(final PluginData pluginData) {
                subscribeList.add(pluginData);
            }
        }, Collections.emptyList(), Collections.emptyList());
        // wait for listener taking action
        Thread.sleep(200);
        assertThat(subscribeList.size(), is(1));
        assertThat(subscribeList.get(0).getName(), is("divide"));
    }

    @Test
    public void testWatchPluginWhenDataChange() throws Exception {
        final List<PluginData> subscribeList = new ArrayList<>(1);
        syncDataService = new ZookeeperSyncDataService(zkClient, new PluginDataSubscriber() {
            @Override
            public void onSubscribe(final PluginData pluginData) {
                subscribeList.add(pluginData);
            }
        }, Collections.emptyList(), Collections.emptyList());

        PluginData pluginData = PluginData.builder().name(MOCK_PLUGIN_NAME).enabled(Boolean.FALSE).build();
        zkClient.createOrUpdate(MOCK_PLUGIN_PATH, pluginData, CreateMode.PERSISTENT);
        // wait for listener taking action
        Thread.sleep(200);
        assertThat(subscribeList.size(), is(1));
        assertThat(subscribeList.get(0).getName(), is("divide"));
    }

    @Test
    public void testWatchPluginWhenDataDeleted() throws Exception {
        final List<PluginData> unSubscribeList = new ArrayList<>(1);
        PluginData pluginData = PluginData.builder().name(MOCK_PLUGIN_NAME).enabled(Boolean.FALSE).build();
        zkClient.createOrUpdate(MOCK_PLUGIN_PATH, pluginData, CreateMode.PERSISTENT);

        syncDataService = new ZookeeperSyncDataService(zkClient, new PluginDataSubscriber() {
            @Override
            public void unSubscribe(final PluginData pluginData) {
                unSubscribeList.add(pluginData);
            }
        }, Collections.emptyList(), Collections.emptyList());
        Thread.sleep(200);
        zkClient.delete(MOCK_PLUGIN_PATH);
        // wait for listener taking action
        Thread.sleep(200);
        assertThat(unSubscribeList.size(), is(1));
        assertThat(unSubscribeList.get(0).getName(), is("divide"));
    }

    @Test
    public void testWatchSelectorWhenInit() throws InterruptedException {
        final List<SelectorData> subscribeList = new ArrayList<>(1);
        SelectorData selectorData = SelectorData.builder().name(MOCK_SELECTOR_NAME).enabled(Boolean.FALSE).build();
        zkClient.createOrUpdate(MOCK_SELECTOR_PATH, selectorData, CreateMode.PERSISTENT);
        syncDataService = new ZookeeperSyncDataService(zkClient, new PluginDataSubscriber() {
            @Override
            public void onSelectorSubscribe(final SelectorData selectorData) {
                subscribeList.add(selectorData);
            }
        }, Collections.emptyList(), Collections.emptyList());
        Thread.sleep(200);
        assertThat(subscribeList.size(), is(1));
        assertThat(subscribeList.get(0).getName(), is("test"));
    }

    @Test
    public void testWatchSelectorWhenDataChange() throws Exception {
        final List<SelectorData> subscribeList = new ArrayList<>(1);
        SelectorData selectorData = SelectorData.builder().name(MOCK_SELECTOR_NAME).enabled(Boolean.FALSE).build();
        zkClient.createOrUpdate(MOCK_SELECTOR_PATH, selectorData, CreateMode.PERSISTENT);
        syncDataService = new ZookeeperSyncDataService(zkClient, new PluginDataSubscriber() {
            @Override
            public void onSelectorSubscribe(final SelectorData selectorData) {
                subscribeList.add(selectorData);
            }
        }, Collections.emptyList(), Collections.emptyList());
        Thread.sleep(200);
        zkClient.createOrUpdate(MOCK_SELECTOR_PATH, selectorData, CreateMode.PERSISTENT);
        Thread.sleep(200);
        assertThat(subscribeList.size(), is(2));
        assertThat(subscribeList.get(0).getName(), is("test"));
    }

    @Test
    public void testWatchSelectorWhenDataDeleted() throws Exception {
        final List<SelectorData> unSubscribeList = new ArrayList<>(1);
        SelectorData selectorData = SelectorData.builder().name(MOCK_SELECTOR_NAME).enabled(Boolean.FALSE).build();
        zkClient.createOrUpdate(MOCK_SELECTOR_PATH, selectorData, CreateMode.PERSISTENT);
        syncDataService = new ZookeeperSyncDataService(zkClient, new PluginDataSubscriber() {
            @Override
            public void unSelectorSubscribe(final SelectorData selectorData) {
                unSubscribeList.add(selectorData);
            }
        }, Collections.emptyList(), Collections.emptyList());
        Thread.sleep(200);
        zkClient.delete(MOCK_SELECTOR_PATH);
        Thread.sleep(200);
        assertThat(unSubscribeList.size(), is(1));
        assertThat(unSubscribeList.get(0).getId(), is("test"));
    }

    @Test
    public void testWatchRuleWhenInit() throws InterruptedException {
        final List<RuleData> subscribeList = new ArrayList<>(1);
        RuleData ruleData = RuleData.builder().name(MOCK_RULE_NAME).enabled(Boolean.FALSE).build();
        zkClient.createOrUpdate(MOCK_RULE_PATH, ruleData, CreateMode.PERSISTENT);
        syncDataService = new ZookeeperSyncDataService(zkClient, new PluginDataSubscriber() {
            @Override
            public void onRuleSubscribe(final RuleData data) {
                subscribeList.add(data);
            }
        }, Collections.emptyList(), Collections.emptyList());
        Thread.sleep(200);
        assertThat(subscribeList.size(), is(1));
        assertThat(subscribeList.get(0).getName(), is(MOCK_RULE_NAME));
    }

    @Test
    public void testWatchRuleWhenDataChange() throws Exception {
        final List<RuleData> subscribeList = new ArrayList<>(1);
        RuleData ruleData = RuleData.builder().name(MOCK_RULE_NAME).enabled(Boolean.FALSE).build();
        zkClient.createOrUpdate(MOCK_RULE_PATH, ruleData, CreateMode.PERSISTENT);
        syncDataService = new ZookeeperSyncDataService(zkClient, new PluginDataSubscriber() {
            @Override
            public void onRuleSubscribe(final RuleData data) {
                subscribeList.add(data);
            }
        }, Collections.emptyList(), Collections.emptyList());
        Thread.sleep(200);
        zkClient.createOrUpdate(MOCK_RULE_PATH, ruleData, CreateMode.PERSISTENT);
        Thread.sleep(200);
        assertThat(subscribeList.size(), is(2));
        assertThat(subscribeList.get(0).getName(), is(MOCK_RULE_NAME));
    }

    @Test
    public void testWatchRuleWhenDataDeleted() throws Exception {
        final List<RuleData> unSubscribeList = new ArrayList<>(1);
        RuleData ruleData = RuleData.builder().name(MOCK_RULE_NAME).enabled(Boolean.FALSE).build();
        zkClient.createOrUpdate(MOCK_RULE_PATH, ruleData, CreateMode.PERSISTENT);
        syncDataService = new ZookeeperSyncDataService(zkClient, new PluginDataSubscriber() {
            @Override
            public void unRuleSubscribe(final RuleData data) {
                unSubscribeList.add(data);
            }
        }, Collections.emptyList(), Collections.emptyList());
        Thread.sleep(200);
        zkClient.delete(MOCK_RULE_PATH);
        Thread.sleep(200);
        assertThat(unSubscribeList.size(), is(1));
        assertThat(unSubscribeList.get(0).getSelectorId() + DefaultPathConstants.SELECTOR_JOIN_RULE + unSubscribeList.get(0).getId(), is(MOCK_RULE_NAME));
    }

    @Test
    public void testWatchAppAuthWhenInit() throws InterruptedException {
        final List<AppAuthData> subscribeList = new ArrayList<>(1);
        AppAuthData appAuthData = AppAuthData.builder().appKey(MOCK_APP_AUTH_KEY).enabled(Boolean.FALSE).build();
        zkClient.createOrUpdate(MOCK_APP_AUTH_PATH, appAuthData, CreateMode.PERSISTENT);
        AuthDataSubscriber authDataSubscriber = new AuthDataSubscriber() {
            @Override
            public void onSubscribe(final AppAuthData appAuthData) {
                subscribeList.add(appAuthData);
            }

            @Override
            public void unSubscribe(final AppAuthData appAuthData) {
            }
        };
        syncDataService = new ZookeeperSyncDataService(zkClient,
                null, Collections.emptyList(), Lists.newArrayList(authDataSubscriber));
        Thread.sleep(200);
        assertThat(subscribeList.size(), is(1));
    }

    @Test
    public void testWatchAppAuthWhenDataChange() throws Exception {
        final List<AppAuthData> subscribeList = new ArrayList<>(1);
        AppAuthData appAuthData = AppAuthData.builder().appKey(MOCK_APP_AUTH_KEY).enabled(Boolean.FALSE).build();
        zkClient.createOrUpdate(MOCK_APP_AUTH_PATH, appAuthData, CreateMode.PERSISTENT);
        AuthDataSubscriber authDataSubscriber = new AuthDataSubscriber() {
            @Override
            public void onSubscribe(final AppAuthData appAuthData) {
                subscribeList.add(appAuthData);
            }

            @Override
            public void unSubscribe(final AppAuthData appAuthData) {
            }
        };
        syncDataService = new ZookeeperSyncDataService(zkClient,
                null, Collections.emptyList(), Lists.newArrayList(authDataSubscriber));
        Thread.sleep(200);
        appAuthData.setEnabled(true);
        zkClient.createOrUpdate(MOCK_APP_AUTH_PATH, appAuthData, CreateMode.PERSISTENT);
        Thread.sleep(200);
        assertThat(subscribeList.size(), is(2));
        assertTrue(subscribeList.get(1).getEnabled());
    }

    @Test
    public void testWatchAppAuthWhenDataDeleted() throws Exception {
        final List<AppAuthData> unSubscribeList = new ArrayList<>(1);
        AppAuthData appAuthData = AppAuthData.builder().appKey(MOCK_APP_AUTH_KEY).enabled(Boolean.FALSE).build();
        zkClient.createOrUpdate(MOCK_APP_AUTH_PATH, appAuthData, CreateMode.PERSISTENT);
        AuthDataSubscriber authDataSubscriber = new AuthDataSubscriber() {
            @Override
            public void onSubscribe(final AppAuthData appAuthData) {
            }

            @Override
            public void unSubscribe(final AppAuthData appAuthData) {
                unSubscribeList.add(appAuthData);
            }
        };
        syncDataService = new ZookeeperSyncDataService(zkClient,
                null, Collections.emptyList(), Lists.newArrayList(authDataSubscriber));
        Thread.sleep(200);
        zkClient.delete(MOCK_APP_AUTH_PATH);
        Thread.sleep(200);
        assertThat(unSubscribeList.size(), is(1));
        assertThat(unSubscribeList.get(0).getAppKey(), is(MOCK_APP_AUTH_KEY));
    }

    @Test
    public void testWatchMetaDataWhenInit() throws InterruptedException {
        final List<MetaData> subscribeList = new ArrayList<>(1);
        MetaData metaData = MetaData.builder().id(MOCK_META_DATA_ID).enabled(Boolean.FALSE).build();
        zkClient.createOrUpdate(MOCK_META_DATA_PATH, metaData, CreateMode.PERSISTENT);
        MetaDataSubscriber metaDataSubscriber = new MetaDataSubscriber() {
            @Override
            public void onSubscribe(final MetaData metaData) {
                subscribeList.add(metaData);
            }

            @Override
            public void unSubscribe(final MetaData metaData) {
            }
        };
        syncDataService = new ZookeeperSyncDataService(zkClient,
                null, Lists.newArrayList(metaDataSubscriber), Collections.emptyList());
        Thread.sleep(200);
        assertThat(subscribeList.size(), is(1));
    }

    @Test
    public void testWatchMetaDataWhenDataChange() throws Exception {
        final List<MetaData> subscribeList = new ArrayList<>(1);
        MetaData metaData = MetaData.builder().id(MOCK_META_DATA_ID).enabled(Boolean.FALSE).build();
        zkClient.createOrUpdate(MOCK_META_DATA_PATH, metaData, CreateMode.PERSISTENT);
        MetaDataSubscriber metaDataSubscriber = new MetaDataSubscriber() {
            @Override
            public void onSubscribe(final MetaData metaData) {
                subscribeList.add(metaData);
            }

            @Override
            public void unSubscribe(final MetaData metaData) {
            }
        };
        syncDataService = new ZookeeperSyncDataService(zkClient,
                null, Lists.newArrayList(metaDataSubscriber), Collections.emptyList());
        Thread.sleep(200);
        metaData.setEnabled(true);
        zkClient.createOrUpdate(MOCK_META_DATA_PATH, metaData, CreateMode.PERSISTENT);
        Thread.sleep(200);
        assertThat(subscribeList.size(), is(2));
        assertTrue(subscribeList.get(1).getEnabled());
    }

    @Test
    public void testWatchMetaDataWhenDataDeleted() throws Exception {
        final List<MetaData> unSubscribeList = new ArrayList<>(1);
        MetaData metaData = MetaData.builder().id(MOCK_META_DATA_ID).enabled(Boolean.FALSE).build();
        zkClient.createOrUpdate(MOCK_META_DATA_PATH, metaData, CreateMode.PERSISTENT);
        MetaDataSubscriber metaDataSubscriber = new MetaDataSubscriber() {
            @Override
            public void onSubscribe(final MetaData metaData) {
            }

            @Override
            public void unSubscribe(final MetaData metaData) {
                unSubscribeList.add(metaData);
            }
        };
        syncDataService = new ZookeeperSyncDataService(zkClient,
                null, Lists.newArrayList(metaDataSubscriber), Collections.emptyList());
        Thread.sleep(200);
        zkClient.delete(MOCK_META_DATA_PATH);
        Thread.sleep(200);
        assertThat(unSubscribeList.size(), is(1));
        assertThat(unSubscribeList.get(0).getPath(), is(MOCK_META_DATA_ID));
    }

    @AfterEach
    public void tearDown() {
        syncDataService.close();
    }
}
