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

package org.apache.shenyu.admin.listener.zookeeper;

import com.google.common.collect.ImmutableList;
import org.apache.shenyu.common.constant.DefaultPathConstants;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.register.client.server.zookeeper.ZookeeperClient;
import org.apache.zookeeper.CreateMode;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * The testCase for ZookeeperDataChangedListener.
 */
@ExtendWith(MockitoExtension.class)
public final class ZookeeperDataChangedListenerTest {

    private static final String MOCK_APP_KEY = "MOCK_APP_KEY";

    private static final String MOCK_APP_SECRET = "MOCK_APP_SECRET";

    private static final String MOCK_ID = "MOCK_ID";

    private static final String MOCK_PATH = "MOCK_PATH";

    private static final String MOCK_APP_NAME = "MOCK_APP_NAME";

    private static final String MOCK_NAME = "MOCK_NAME";

    private static final String MOCK_CONFIG = "MOCK_CONFIG";

    private static final String MOCK_PLUGIN_NAME = "MOCK_PLUGIN_NAME";

    private static final String MOCK_SELECTOR_ID = "MOCK_SELECTOR_ID";

    @InjectMocks
    private ZookeeperDataChangedListener zookeeperDataChangedListener;

    @Mock
    private ZookeeperClient zkClient;

    /**
     * test case onAppAuthChanged create event.
     */
    @Test
    public void testOnAppAuthChangedCreate() {
        AppAuthData appAuthData = AppAuthData.builder().appKey(MOCK_APP_KEY).appSecret(MOCK_APP_SECRET).build();
        String appAuthPath = DefaultPathConstants.buildAppAuthPath(appAuthData.getAppKey());

        zookeeperDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthData), DataEventTypeEnum.CREATE);
        verify(zkClient, times(1)).createOrUpdate(appAuthPath, appAuthData, CreateMode.PERSISTENT);
    }

    /**
     * test case onAppAuthChanged update event.
     */
    @Test
    public void testOnAppAuthChangedUpdate() {
        AppAuthData appAuthData = AppAuthData.builder().appKey(MOCK_APP_KEY).appSecret(MOCK_APP_SECRET).build();
        String appAuthPath = DefaultPathConstants.buildAppAuthPath(appAuthData.getAppKey());

        zookeeperDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthData), DataEventTypeEnum.UPDATE);
        verify(zkClient, times(1)).createOrUpdate(appAuthPath, appAuthData, CreateMode.PERSISTENT);
    }

    /**
     * test case onAppAuthChanged delete event.
     */
    @Test
    public void testOnAppAuthChangedDelete() {
        AppAuthData appAuthData = AppAuthData.builder().appKey(MOCK_APP_KEY).appSecret(MOCK_APP_SECRET).build();
        String appAuthPath = DefaultPathConstants.buildAppAuthPath(appAuthData.getAppKey());

        when(zkClient.isExist(appAuthPath)).thenReturn(true);
        zookeeperDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthData), DataEventTypeEnum.DELETE);
        verify(zkClient, times(1)).delete(appAuthPath);
    }

    /**
     * test case onMetaDataChanged create event.
     */
    @Test
    public void testOnMetaDataChangedCreate() throws UnsupportedEncodingException {
        MetaData metaData = MetaData.builder().id(MOCK_ID).path(MOCK_PATH).appName(MOCK_APP_NAME).build();
        String metaDataPath = DefaultPathConstants.buildMetaDataPath(URLEncoder.encode(metaData.getPath(), "UTF-8"));

        zookeeperDataChangedListener.onMetaDataChanged(ImmutableList.of(metaData), DataEventTypeEnum.CREATE);
        verify(zkClient, times(1)).createOrUpdate(metaDataPath, metaData, CreateMode.PERSISTENT);
    }

    /**
     * test case onMetaDataChanged update event.
     */
    @Test
    public void testOnMetaDataChangedUpdate() throws UnsupportedEncodingException {
        MetaData metaData = MetaData.builder().id(MOCK_ID).path(MOCK_PATH).appName(MOCK_APP_NAME).build();
        String metaDataPath = DefaultPathConstants.buildMetaDataPath(URLEncoder.encode(metaData.getPath(), "UTF-8"));

        zookeeperDataChangedListener.onMetaDataChanged(ImmutableList.of(metaData), DataEventTypeEnum.UPDATE);
        verify(zkClient, times(1)).createOrUpdate(metaDataPath, metaData, CreateMode.PERSISTENT);
    }

    /**
     * test case onMetaDataChanged delete event.
     */
    @Test
    public void testOnMetaDataChangedDelete() throws UnsupportedEncodingException {
        MetaData metaData = MetaData.builder().id(MOCK_ID).path(MOCK_PATH).appName(MOCK_APP_NAME).build();
        String metaDataPath = DefaultPathConstants.buildMetaDataPath(URLEncoder.encode(metaData.getPath(), "UTF-8"));

        when(zkClient.isExist(metaDataPath)).thenReturn(true);
        zookeeperDataChangedListener.onMetaDataChanged(ImmutableList.of(metaData), DataEventTypeEnum.DELETE);
        verify(zkClient, times(1)).delete(metaDataPath);
    }

    /**
     * test case onPluginChanged create event.
     */
    @Test
    public void testOnPluginChangedCreate() {
        PluginData pluginData = PluginData.builder().id(MOCK_ID).name(MOCK_NAME).config(MOCK_CONFIG).build();
        String pluginPath = DefaultPathConstants.buildPluginPath(pluginData.getName());

        zookeeperDataChangedListener.onPluginChanged(ImmutableList.of(pluginData), DataEventTypeEnum.CREATE);
        verify(zkClient, times(1)).createOrUpdate(pluginPath, pluginData, CreateMode.PERSISTENT);
    }

    /**
     * test case onPluginChanged update event.
     */
    @Test
    public void testOnPluginChangedUpdate() {
        PluginData pluginData = PluginData.builder().id(MOCK_ID).name(MOCK_NAME).config(MOCK_CONFIG).build();
        String pluginPath = DefaultPathConstants.buildPluginPath(pluginData.getName());

        zookeeperDataChangedListener.onPluginChanged(ImmutableList.of(pluginData), DataEventTypeEnum.UPDATE);
        verify(zkClient, times(1)).createOrUpdate(pluginPath, pluginData, CreateMode.PERSISTENT);
    }

    /**
     * test case onPluginChanged delete event.
     */
    @Test
    public void testOnPluginChangedDelete() {
        PluginData pluginData = PluginData.builder().id(MOCK_ID).name(MOCK_NAME).config(MOCK_CONFIG).build();
        String pluginPath = DefaultPathConstants.buildPluginPath(pluginData.getName());
        String selectorParentPath = DefaultPathConstants.buildSelectorParentPath(pluginData.getName());
        String ruleParentPath = DefaultPathConstants.buildRuleParentPath(pluginData.getName());

        when(zkClient.isExist(pluginPath)).thenReturn(true);
        when(zkClient.isExist(selectorParentPath)).thenReturn(true);
        when(zkClient.isExist(ruleParentPath)).thenReturn(true);

        zookeeperDataChangedListener.onPluginChanged(ImmutableList.of(pluginData), DataEventTypeEnum.DELETE);
        verify(zkClient, times(1)).delete(pluginPath);
        verify(zkClient, times(1)).delete(selectorParentPath);
        verify(zkClient, times(1)).delete(ruleParentPath);
    }

    /**
     * test case onSelectorChanged create event.
     */
    @Test
    public void testOnSelectorChangedCreate() {
        SelectorData selectorData = SelectorData.builder().id(MOCK_ID).name(MOCK_NAME).pluginName(MOCK_PLUGIN_NAME).build();

        String selectorRealPath = DefaultPathConstants.buildSelectorRealPath(selectorData.getPluginName(), selectorData.getId());

        zookeeperDataChangedListener.onSelectorChanged(ImmutableList.of(selectorData), DataEventTypeEnum.CREATE);
        verify(zkClient, times(1)).createOrUpdate(selectorRealPath, selectorData, CreateMode.PERSISTENT);
    }

    /**
     * test case onSelectorChanged update event.
     */
    @Test
    public void testOnSelectorChangedUpdate() {
        SelectorData selectorData = SelectorData.builder().id(MOCK_ID).name(MOCK_NAME).pluginName(MOCK_PLUGIN_NAME).build();

        String selectorRealPath = DefaultPathConstants.buildSelectorRealPath(selectorData.getPluginName(), selectorData.getId());

        zookeeperDataChangedListener.onSelectorChanged(ImmutableList.of(selectorData), DataEventTypeEnum.UPDATE);
        verify(zkClient, times(1)).createOrUpdate(selectorRealPath, selectorData, CreateMode.PERSISTENT);
    }

    /**
     * test case onSelectorChanged refresh event.
     */
    @Test
    public void testOnSelectorChangedRefresh() {
        SelectorData selectorData = SelectorData.builder().id(MOCK_ID).name(MOCK_NAME).pluginName(MOCK_PLUGIN_NAME).build();
        String selectorParentPath = DefaultPathConstants.buildSelectorParentPath(selectorData.getPluginName());

        when(zkClient.isExist(selectorParentPath)).thenReturn(true);
        zookeeperDataChangedListener.onSelectorChanged(ImmutableList.of(selectorData), DataEventTypeEnum.REFRESH);
        verify(zkClient, times(1)).delete(selectorParentPath);
    }

    /**
     * test case onSelectorChanged delete event.
     */
    @Test
    public void testOnSelectorChangedDelete() {
        SelectorData selectorData = SelectorData.builder().id(MOCK_ID).name(MOCK_NAME).pluginName(MOCK_PLUGIN_NAME).build();
        String selectorRealPath = DefaultPathConstants.buildSelectorRealPath(selectorData.getPluginName(), selectorData.getId());

        when(zkClient.isExist(selectorRealPath)).thenReturn(true);
        zookeeperDataChangedListener.onSelectorChanged(ImmutableList.of(selectorData), DataEventTypeEnum.DELETE);
        verify(zkClient, times(1)).delete(selectorRealPath);
    }

    /**
     * test case onRuleChanged create event.
     */
    @Test
    public void testOnRuleChangedCreate() {
        RuleData ruleData = RuleData.builder()
                .id(MOCK_ID)
                .name(MOCK_NAME)
                .pluginName(MOCK_PLUGIN_NAME)
                .selectorId(MOCK_SELECTOR_ID)
                .build();
        String ruleRealPath = DefaultPathConstants.buildRulePath(ruleData.getPluginName(), ruleData.getSelectorId(), ruleData.getId());

        zookeeperDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.CREATE);
        verify(zkClient, times(1)).createOrUpdate(ruleRealPath, ruleData, CreateMode.PERSISTENT);
    }

    /**
     * test case onRuleChanged update event.
     */
    @Test
    public void testOnRuleChangedUpdate() {
        RuleData ruleData = RuleData.builder()
                .id(MOCK_ID)
                .name(MOCK_NAME)
                .pluginName(MOCK_PLUGIN_NAME)
                .selectorId(MOCK_SELECTOR_ID)
                .build();
        String ruleRealPath = DefaultPathConstants.buildRulePath(ruleData.getPluginName(), ruleData.getSelectorId(),
                ruleData.getId());

        zookeeperDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.UPDATE);
        verify(zkClient, times(1)).createOrUpdate(ruleRealPath, ruleData, CreateMode.PERSISTENT);
    }

    /**
     * test case onRuleChanged refresh event.
     */
    @Test
    public void testOnRuleChangedRefresh() {
        RuleData ruleData = RuleData.builder()
                .id(MOCK_ID)
                .name(MOCK_NAME)
                .pluginName(MOCK_PLUGIN_NAME)
                .selectorId(MOCK_SELECTOR_ID)
                .build();
        String ruleParentPath = DefaultPathConstants.buildRuleParentPath(ruleData.getPluginName());

        when(zkClient.isExist(ruleParentPath)).thenReturn(true);
        zookeeperDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.REFRESH);
        verify(zkClient, times(1)).delete(ruleParentPath);
    }

    /**
     * test case onRuleChanged delete event.
     */
    @Test
    public void testOnRuleChangedDelete() {
        RuleData ruleData = RuleData.builder()
                .id(MOCK_ID)
                .name(MOCK_NAME)
                .pluginName(MOCK_PLUGIN_NAME)
                .selectorId(MOCK_SELECTOR_ID)
                .build();
        String ruleRealPath = DefaultPathConstants.buildRulePath(ruleData.getPluginName(), ruleData.getSelectorId(), ruleData.getId());

        when(zkClient.isExist(ruleRealPath)).thenReturn(true);
        zookeeperDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.DELETE);
        verify(zkClient, times(1)).delete(ruleRealPath);
    }

}
