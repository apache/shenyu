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
import org.I0Itec.zkclient.IZkDataListener;
import org.I0Itec.zkclient.ZkClient;
import org.apache.shenyu.common.constant.DefaultPathConstants;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.junit.MockitoJUnitRunner;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static junit.framework.TestCase.assertTrue;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@RunWith(MockitoJUnitRunner.class)
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

    private ZkClient zkClient;

    private ZookeeperSyncDataService syncDataService;

    @Before
    public void setUp() {
        zkClient = mock(ZkClient.class);
        //mock plugin data & method
        PluginData pluginData = PluginData.builder().name(MOCK_PLUGIN_NAME).enabled(Boolean.FALSE).build();
        when(zkClient.exists(anyString())).thenReturn(Boolean.FALSE);
        when(zkClient.readData(MOCK_PLUGIN_PATH)).thenReturn(GsonUtils.getInstance().toJson(pluginData));
        when(zkClient.getChildren(MOCK_PLUGIN_PARENT_PATH)).thenReturn(Lists.newArrayList(MOCK_PLUGIN_NAME));
        //mock selector data & method
        SelectorData selectorData = SelectorData.builder().name(MOCK_SELECTOR_NAME).enabled(Boolean.FALSE).build();
        when(zkClient.readData(MOCK_SELECTOR_PATH)).thenReturn(GsonUtils.getInstance().toJson(selectorData));
        when(zkClient.getChildren(MOCK_SELECTOR_PARENT_PATH)).thenReturn(Lists.newArrayList(MOCK_SELECTOR_NAME));
        //mock rule data & method
        RuleData ruleData = RuleData.builder().name(MOCK_RULE_NAME).enabled(Boolean.FALSE).build();
        when(zkClient.readData(MOCK_RULE_PATH)).thenReturn(GsonUtils.getInstance().toJson(ruleData));
        when(zkClient.getChildren(MOCK_RULE_PARENT_PATH)).thenReturn(Lists.newArrayList(MOCK_RULE_NAME));
        //mock auth data & method
        AppAuthData appAuthData = AppAuthData.builder().appKey(MOCK_APP_AUTH_KEY).enabled(Boolean.FALSE).build();
        when(zkClient.readData(MOCK_APP_AUTH_PATH)).thenReturn(GsonUtils.getInstance().toJson(appAuthData));
        when(zkClient.getChildren(MOCK_APP_AUTH_PARENT_PATH)).thenReturn(Lists.newArrayList(MOCK_APP_AUTH_KEY));
        //mock meta data & method
        MetaData metaData = MetaData.builder().id(MOCK_META_DATA_ID).enabled(Boolean.FALSE).build();
        when(zkClient.readData(MOCK_META_DATA_PATH)).thenReturn(GsonUtils.getInstance().toJson(metaData));
        when(zkClient.getChildren(MOCK_META_DATA_PARENT_PATH)).thenReturn(Lists.newArrayList(MOCK_META_DATA_ID));
    }

    @Test
    public void testWatchPluginWhenInit() {
        final List<PluginData> subscribeList = new ArrayList<>(1);
        syncDataService = new ZookeeperSyncDataService(zkClient, new PluginDataSubscriber() {
            @Override
            public void onSubscribe(final PluginData pluginData) {
                subscribeList.add(pluginData);
            }
        }, Collections.emptyList(), Collections.emptyList());
        assertThat(subscribeList.size(), is(1));
        assertThat(subscribeList.get(0).getName(), is("divide"));
    }

    @Test
    public void testWatchPluginWhenDataChange() throws Exception {
        final PluginData changedPluginData = PluginData.builder().name(MOCK_PLUGIN_NAME).enabled(Boolean.TRUE).build();
        final List<PluginData> subscribeList = new ArrayList<>(2);
        syncDataService = new ZookeeperSyncDataService(zkClient, new PluginDataSubscriber() {
            @Override
            public void onSubscribe(final PluginData pluginData) {
                subscribeList.add(pluginData);
            }
        }, Collections.emptyList(), Collections.emptyList());
        ArgumentCaptor<IZkDataListener> captor = ArgumentCaptor.forClass(IZkDataListener.class);
        verify(zkClient).subscribeDataChanges(eq(MOCK_PLUGIN_PATH), captor.capture());
        captor.getValue().handleDataChange(MOCK_PLUGIN_PATH, GsonUtils.getInstance().toJson(changedPluginData));
        assertThat(subscribeList.size(), is(2));
        assertTrue(subscribeList.get(1).getEnabled());
    }

    @Test
    public void testWatchPluginWhenDataDeleted() throws Exception {
        final List<PluginData> unSubscribeList = new ArrayList<>(1);
        syncDataService = new ZookeeperSyncDataService(zkClient, new PluginDataSubscriber() {
            @Override
            public void unSubscribe(final PluginData pluginData) {
                unSubscribeList.add(pluginData);
            }
        }, Collections.emptyList(), Collections.emptyList());
        ArgumentCaptor<IZkDataListener> captor = ArgumentCaptor.forClass(IZkDataListener.class);
        verify(zkClient).subscribeDataChanges(eq(MOCK_PLUGIN_PATH), captor.capture());
        captor.getValue().handleDataDeleted(MOCK_PLUGIN_PATH);
        assertThat(unSubscribeList.size(), is(1));
        assertThat(unSubscribeList.get(0).getName(), is("divide"));
    }

    @Test
    public void testWatchSelectorWhenInit() {
        final List<SelectorData> subscribeList = new ArrayList<>(1);
        syncDataService = new ZookeeperSyncDataService(zkClient, new PluginDataSubscriber() {
            @Override
            public void onSelectorSubscribe(final SelectorData selectorData) {
                subscribeList.add(selectorData);
            }
        }, Collections.emptyList(), Collections.emptyList());
        assertThat(subscribeList.size(), is(1));
        assertThat(subscribeList.get(0).getName(), is("test"));
    }

    @Test
    public void testWatchSelectorWhenDataChange() throws Exception {
        final SelectorData changedSelectorData = SelectorData.builder().name(MOCK_SELECTOR_NAME).enabled(Boolean.TRUE).build();
        final List<SelectorData> subscribeList = new ArrayList<>(2);
        syncDataService = new ZookeeperSyncDataService(zkClient, new PluginDataSubscriber() {
            @Override
            public void onSelectorSubscribe(final SelectorData selectorData) {
                subscribeList.add(selectorData);
            }
        }, Collections.emptyList(), Collections.emptyList());
        ArgumentCaptor<IZkDataListener> captor = ArgumentCaptor.forClass(IZkDataListener.class);
        verify(zkClient).subscribeDataChanges(eq(MOCK_SELECTOR_PATH), captor.capture());
        captor.getValue().handleDataChange(MOCK_SELECTOR_PATH, GsonUtils.getInstance().toJson(changedSelectorData));
        assertThat(subscribeList.size(), is(2));
        assertTrue(subscribeList.get(1).getEnabled());
    }

    @Test
    public void testWatchSelectorWhenDataDeleted() throws Exception {
        final List<SelectorData> unSubscribeList = new ArrayList<>(1);
        syncDataService = new ZookeeperSyncDataService(zkClient, new PluginDataSubscriber() {
            @Override
            public void unSelectorSubscribe(final SelectorData selectorData) {
                unSubscribeList.add(selectorData);
            }
        }, Collections.emptyList(), Collections.emptyList());
        ArgumentCaptor<IZkDataListener> captor = ArgumentCaptor.forClass(IZkDataListener.class);
        verify(zkClient).subscribeDataChanges(eq(MOCK_SELECTOR_PATH), captor.capture());
        captor.getValue().handleDataDeleted(MOCK_SELECTOR_PATH);
        assertThat(unSubscribeList.size(), is(1));
        assertThat(unSubscribeList.get(0).getId(), is("test"));
    }

    @Test
    public void testWatchRuleWhenInit() {
        final List<RuleData> subscribeList = new ArrayList<>(1);
        syncDataService = new ZookeeperSyncDataService(zkClient, new PluginDataSubscriber() {
            @Override
            public void onRuleSubscribe(final RuleData ruleData) {
                subscribeList.add(ruleData);
            }
        }, Collections.emptyList(), Collections.emptyList());
        assertThat(subscribeList.size(), is(1));
        assertThat(subscribeList.get(0).getName(), is(MOCK_RULE_NAME));
    }

    @Test
    public void testWatchRuleWhenDataChange() throws Exception {
        final RuleData changedRuleData = RuleData.builder().name(MOCK_RULE_NAME).enabled(Boolean.TRUE).build();
        final List<RuleData> subscribeList = new ArrayList<>(2);
        syncDataService = new ZookeeperSyncDataService(zkClient, new PluginDataSubscriber() {
            @Override
            public void onRuleSubscribe(final RuleData ruleData) {
                subscribeList.add(ruleData);
            }
        }, Collections.emptyList(), Collections.emptyList());
        ArgumentCaptor<IZkDataListener> captor = ArgumentCaptor.forClass(IZkDataListener.class);
        verify(zkClient).subscribeDataChanges(eq(MOCK_RULE_PATH), captor.capture());
        captor.getValue().handleDataChange(MOCK_RULE_PATH, GsonUtils.getInstance().toJson(changedRuleData));
        assertThat(subscribeList.size(), is(2));
        Assert.assertTrue(subscribeList.get(1).getEnabled());
    }

    @Test
    public void testWatchRuleWhenDataDeleted() throws Exception {
        final List<RuleData> unSubscribeList = new ArrayList<>(1);
        syncDataService = new ZookeeperSyncDataService(zkClient, new PluginDataSubscriber() {
            @Override
            public void unRuleSubscribe(final RuleData ruleData) {
                unSubscribeList.add(ruleData);
            }
        }, Collections.emptyList(), Collections.emptyList());
        ArgumentCaptor<IZkDataListener> captor = ArgumentCaptor.forClass(IZkDataListener.class);
        verify(zkClient).subscribeDataChanges(eq(MOCK_RULE_PATH), captor.capture());
        captor.getValue().handleDataDeleted(MOCK_RULE_PATH);
        assertThat(unSubscribeList.size(), is(1));
        assertThat(unSubscribeList.get(0).getSelectorId() + DefaultPathConstants.SELECTOR_JOIN_RULE + unSubscribeList.get(0).getId(), is(MOCK_RULE_NAME));
    }

    @Test
    public void testWatchAppAuthWhenInit() {
        final List<AppAuthData> subscribeList = new ArrayList<>(1);
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
        assertThat(subscribeList.size(), is(1));
    }

    @Test
    public void testWatchAppAuthWhenDataChange() throws Exception {
        final AppAuthData changedAppAuthData = AppAuthData.builder().appKey("test").enabled(Boolean.TRUE).build();
        final List<AppAuthData> subscribeList = new ArrayList<>(1);
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
        ArgumentCaptor<IZkDataListener> captor = ArgumentCaptor.forClass(IZkDataListener.class);
        verify(zkClient).subscribeDataChanges(eq(MOCK_APP_AUTH_PATH), captor.capture());
        captor.getValue().handleDataChange(MOCK_APP_AUTH_PATH, GsonUtils.getInstance().toJson(changedAppAuthData));
        assertThat(subscribeList.size(), is(2));
        assertTrue(subscribeList.get(1).getEnabled());
    }

    @Test
    public void testWatchAppAuthWhenDataDeleted() throws Exception {
        final List<AppAuthData> unSubscribeList = new ArrayList<>(1);
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
        ArgumentCaptor<IZkDataListener> captor = ArgumentCaptor.forClass(IZkDataListener.class);
        verify(zkClient).subscribeDataChanges(eq(MOCK_APP_AUTH_PATH), captor.capture());
        captor.getValue().handleDataDeleted(MOCK_APP_AUTH_PATH);
        assertThat(unSubscribeList.size(), is(1));
        assertThat(unSubscribeList.get(0).getAppKey(), is(MOCK_APP_AUTH_KEY));
    }

    @Test
    public void testWatchMetaDataWhenInit() {
        final List<MetaData> subscribeList = new ArrayList<>(1);
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
        assertThat(subscribeList.size(), is(1));
    }

    @Test
    public void testWatchMetaDataWhenDataChange() throws Exception {
        final MetaData changedMetaData = MetaData.builder().id(MOCK_META_DATA_ID).enabled(Boolean.TRUE).build();
        final List<MetaData> subscribeList = new ArrayList<>(2);
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
        Assert.assertEquals(1, subscribeList.size());
        ArgumentCaptor<IZkDataListener> captor = ArgumentCaptor.forClass(IZkDataListener.class);
        verify(zkClient).subscribeDataChanges(eq(MOCK_META_DATA_PATH), captor.capture());
        captor.getValue().handleDataChange(MOCK_META_DATA_PATH, GsonUtils.getInstance().toJson(changedMetaData));
        assertThat(subscribeList.size(), is(2));
        assertTrue(subscribeList.get(1).getEnabled());
    }

    @Test
    public void testWatchMetaDataWhenDataDeleted() throws Exception {
        final List<MetaData> unSubscribeList = new ArrayList<>(1);
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
        ArgumentCaptor<IZkDataListener> captor = ArgumentCaptor.forClass(IZkDataListener.class);
        verify(zkClient).subscribeDataChanges(eq(MOCK_META_DATA_PATH), captor.capture());
        captor.getValue().handleDataDeleted(MOCK_META_DATA_PATH);
        assertThat(unSubscribeList.size(), is(1));
        assertThat(unSubscribeList.get(0).getPath(), is(MOCK_META_DATA_ID));
    }

    @After
    public void tearDown() {
        syncDataService.close();
    }
}
