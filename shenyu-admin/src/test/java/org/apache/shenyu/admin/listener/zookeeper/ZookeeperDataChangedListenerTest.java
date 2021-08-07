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

import static org.mockito.Mockito.atMostOnce;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.net.URLEncoder;

import lombok.SneakyThrows;

import org.I0Itec.zkclient.ZkClient;
import org.apache.shenyu.common.constant.DefaultPathConstants;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import com.google.common.collect.ImmutableList;

/**
 * The testCase for ZookeeperDataChangedListener.
 */
@RunWith(MockitoJUnitRunner.class)
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
    private ZkClient zkClient;

    /**
     * test case onAppAuthChanged create event.
     */
    @Test
    public void testOnAppAuthChangedCreate() {
        AppAuthData appAuthData = new AppAuthData();
        appAuthData.setAppKey(MOCK_APP_KEY);
        appAuthData.setAppSecret(MOCK_APP_SECRET);
        String appAuthPath = DefaultPathConstants.buildAppAuthPath(appAuthData.getAppKey());

        when(zkClient.exists(appAuthPath)).thenReturn(false);
        zookeeperDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthData), DataEventTypeEnum.CREATE);
        verify(zkClient, times(1)).createPersistent(appAuthPath, true);
        verify(zkClient, times(1)).writeData(appAuthPath, GsonUtils.getInstance().toJson(appAuthData));
    }

    /**
     * test case onAppAuthChanged update event.
     */
    @Test
    public void testOnAppAuthChangedUpdate() {
        AppAuthData appAuthData = new AppAuthData();
        appAuthData.setAppKey(MOCK_APP_KEY);
        appAuthData.setAppSecret(MOCK_APP_SECRET);
        String appAuthPath = DefaultPathConstants.buildAppAuthPath(appAuthData.getAppKey());

        when(zkClient.exists(appAuthPath)).thenReturn(true);
        zookeeperDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthData), DataEventTypeEnum.UPDATE);
        verify(zkClient, atMostOnce()).createPersistent(appAuthPath, true);
        verify(zkClient, times(1)).writeData(appAuthPath, GsonUtils.getInstance().toJson(appAuthData));
    }

    /**
     * test case onAppAuthChanged delete event.
     */
    @Test
    public void testOnAppAuthChangedDelete() {
        AppAuthData appAuthData = new AppAuthData();
        appAuthData.setAppKey(MOCK_APP_KEY);
        appAuthData.setAppSecret(MOCK_APP_SECRET);
        String appAuthPath = DefaultPathConstants.buildAppAuthPath(appAuthData.getAppKey());

        when(zkClient.exists(appAuthPath)).thenReturn(true);
        zookeeperDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthData), DataEventTypeEnum.DELETE);
        verify(zkClient, times(1)).delete(appAuthPath);
    }

    /**
     * test case onMetaDataChanged create event.
     */
    @SneakyThrows
    @Test
    public void testOnMetaDataChangedCreate() {
        MetaData metaData = new MetaData();
        metaData.setId(MOCK_ID);
        metaData.setPath(MOCK_PATH);
        metaData.setAppName(MOCK_APP_NAME);
        String metaDataPath = DefaultPathConstants.buildMetaDataPath(URLEncoder.encode(metaData.getPath(), "UTF-8"));

        when(zkClient.exists(metaDataPath)).thenReturn(false);
        zookeeperDataChangedListener.onMetaDataChanged(ImmutableList.of(metaData), DataEventTypeEnum.CREATE);
        verify(zkClient, times(1)).createPersistent(metaDataPath, true);
        verify(zkClient, times(1)).writeData(metaDataPath, GsonUtils.getInstance().toJson(metaData));
    }

    /**
     * test case onMetaDataChanged update event.
     */
    @SneakyThrows
    @Test
    public void testOnMetaDataChangedUpdate() {
        MetaData metaData = new MetaData();
        metaData.setId(MOCK_ID);
        metaData.setPath(MOCK_PATH);
        metaData.setAppName(MOCK_APP_NAME);
        String metaDataPath = DefaultPathConstants.buildMetaDataPath(URLEncoder.encode(metaData.getPath(), "UTF-8"));

        when(zkClient.exists(metaDataPath)).thenReturn(true);
        zookeeperDataChangedListener.onMetaDataChanged(ImmutableList.of(metaData), DataEventTypeEnum.UPDATE);
        verify(zkClient, atMostOnce()).createPersistent(metaDataPath, true);
        verify(zkClient, times(1)).writeData(metaDataPath, GsonUtils.getInstance().toJson(metaData));
    }

    /**
     * test case onMetaDataChanged delete event.
     */
    @SneakyThrows
    @Test
    public void testOnMetaDataChangedDelete() {
        MetaData metaData = new MetaData();
        metaData.setId(MOCK_ID);
        metaData.setPath(MOCK_PATH);
        metaData.setAppName(MOCK_APP_NAME);
        String metaDataPath = DefaultPathConstants.buildMetaDataPath(URLEncoder.encode(metaData.getPath(), "UTF-8"));

        when(zkClient.exists(metaDataPath)).thenReturn(true);
        zookeeperDataChangedListener.onMetaDataChanged(ImmutableList.of(metaData), DataEventTypeEnum.DELETE);
        verify(zkClient, times(1)).delete(metaDataPath);
    }

    /**
     * test case onPluginChanged create event.
     */
    @Test
    public void testOnPluginChangedCreate() {
        PluginData pluginData = new PluginData(MOCK_ID, MOCK_NAME, MOCK_CONFIG, null, null);
        String pluginPath = DefaultPathConstants.buildPluginPath(pluginData.getName());

        when(zkClient.exists(pluginPath)).thenReturn(false);
        zookeeperDataChangedListener.onPluginChanged(ImmutableList.of(pluginData), DataEventTypeEnum.CREATE);
        verify(zkClient, times(1)).createPersistent(pluginPath, true);
        verify(zkClient, times(1)).writeData(pluginPath, GsonUtils.getInstance().toJson(pluginData));
    }

    /**
     * test case onPluginChanged update event.
     */
    @Test
    public void testOnPluginChangedUpdate() {
        PluginData pluginData = new PluginData(MOCK_ID, MOCK_NAME, MOCK_CONFIG, null, null);
        String pluginPath = DefaultPathConstants.buildPluginPath(pluginData.getName());

        when(zkClient.exists(pluginPath)).thenReturn(true);
        zookeeperDataChangedListener.onPluginChanged(ImmutableList.of(pluginData), DataEventTypeEnum.UPDATE);
        verify(zkClient, atMostOnce()).createPersistent(pluginPath, true);
        verify(zkClient, times(1)).writeData(pluginPath, GsonUtils.getInstance().toJson(pluginData));
    }

    /**
     * test case onPluginChanged delete event.
     */
    @Test
    public void testOnPluginChangedDelete() {
        PluginData pluginData = new PluginData(MOCK_ID, MOCK_NAME, MOCK_CONFIG, null, null);
        String pluginPath = DefaultPathConstants.buildPluginPath(pluginData.getName());
        String selectorParentPath = DefaultPathConstants.buildSelectorParentPath(pluginData.getName());
        String ruleParentPath = DefaultPathConstants.buildRuleParentPath(pluginData.getName());

        when(zkClient.exists(pluginPath)).thenReturn(true);
        when(zkClient.exists(selectorParentPath)).thenReturn(true);
        when(zkClient.exists(ruleParentPath)).thenReturn(true);

        zookeeperDataChangedListener.onPluginChanged(ImmutableList.of(pluginData), DataEventTypeEnum.DELETE);
        verify(zkClient, times(1)).deleteRecursive(pluginPath);
        verify(zkClient, times(1)).deleteRecursive(selectorParentPath);
        verify(zkClient, times(1)).deleteRecursive(ruleParentPath);
    }

    /**
     * test case onSelectorChanged create event.
     */
    @Test
    public void testOnSelectorChangedCreate() {
        SelectorData selectorData = new SelectorData();
        selectorData.setId(MOCK_ID);
        selectorData.setName(MOCK_NAME);
        selectorData.setPluginName(MOCK_PLUGIN_NAME);

        String selectorRealPath = DefaultPathConstants.buildSelectorRealPath(selectorData.getPluginName(), selectorData.getId());
        String selectorParentPath = DefaultPathConstants.buildSelectorParentPath(selectorData.getPluginName());

        when(zkClient.exists(selectorRealPath)).thenReturn(false);
        when(zkClient.exists(selectorParentPath)).thenReturn(false);

        zookeeperDataChangedListener.onSelectorChanged(ImmutableList.of(selectorData), DataEventTypeEnum.CREATE);
        verify(zkClient, times(1)).createPersistent(selectorRealPath, true);
        verify(zkClient, times(1)).createPersistent(selectorParentPath, true);
        verify(zkClient, times(1)).writeData(selectorRealPath, GsonUtils.getInstance().toJson(selectorData));
    }

    /**
     * test case onSelectorChanged update event.
     */
    @Test
    public void testOnSelectorChangedUpdate() {
        SelectorData selectorData = new SelectorData();
        selectorData.setId(MOCK_ID);
        selectorData.setName(MOCK_NAME);
        selectorData.setPluginName(MOCK_PLUGIN_NAME);

        String selectorRealPath = DefaultPathConstants.buildSelectorRealPath(selectorData.getPluginName(), selectorData.getId());
        String selectorParentPath = DefaultPathConstants.buildSelectorParentPath(selectorData.getPluginName());

        when(zkClient.exists(selectorRealPath)).thenReturn(true);
        when(zkClient.exists(selectorParentPath)).thenReturn(true);

        zookeeperDataChangedListener.onSelectorChanged(ImmutableList.of(selectorData), DataEventTypeEnum.UPDATE);
        verify(zkClient, atMostOnce()).createPersistent(selectorRealPath, true);
        verify(zkClient, atMostOnce()).createPersistent(selectorParentPath, true);
        verify(zkClient, times(1)).writeData(selectorRealPath, GsonUtils.getInstance().toJson(selectorData));
    }

    /**
     * test case onSelectorChanged refresh event.
     */
    @Test
    public void testOnSelectorChangedRefresh() {
        SelectorData selectorData = new SelectorData();
        selectorData.setId(MOCK_ID);
        selectorData.setName(MOCK_NAME);
        selectorData.setPluginName(MOCK_PLUGIN_NAME);
        String selectorParentPath = DefaultPathConstants.buildSelectorParentPath(selectorData.getPluginName());

        when(zkClient.exists(selectorParentPath)).thenReturn(true);
        zookeeperDataChangedListener.onSelectorChanged(ImmutableList.of(selectorData), DataEventTypeEnum.REFRESH);
        verify(zkClient, times(1)).deleteRecursive(selectorParentPath);
    }

    /**
     * test case onSelectorChanged delete event.
     */
    @Test
    public void testOnSelectorChangedDelete() {
        SelectorData selectorData = new SelectorData();
        selectorData.setId(MOCK_ID);
        selectorData.setName(MOCK_NAME);
        selectorData.setPluginName(MOCK_PLUGIN_NAME);
        String selectorRealPath = DefaultPathConstants.buildSelectorRealPath(selectorData.getPluginName(), selectorData.getId());

        when(zkClient.exists(selectorRealPath)).thenReturn(true);
        zookeeperDataChangedListener.onSelectorChanged(ImmutableList.of(selectorData), DataEventTypeEnum.DELETE);
        verify(zkClient, times(1)).delete(selectorRealPath);
    }

    /**
     * test case onRuleChanged create event.
     */
    @Test
    public void testOnRuleChangedCreate() {
        RuleData ruleData = new RuleData();
        ruleData.setId(MOCK_ID);
        ruleData.setName(MOCK_NAME);
        ruleData.setPluginName(MOCK_PLUGIN_NAME);
        ruleData.setSelectorId(MOCK_SELECTOR_ID);
        String ruleRealPath = DefaultPathConstants.buildRulePath(ruleData.getPluginName(), ruleData.getSelectorId(), ruleData.getId());
        String ruleParentPath = DefaultPathConstants.buildRuleParentPath(ruleData.getPluginName());

        when(zkClient.exists(ruleRealPath)).thenReturn(false);
        when(zkClient.exists(ruleParentPath)).thenReturn(false);

        zookeeperDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.CREATE);
        verify(zkClient, times(1)).createPersistent(ruleRealPath, true);
        verify(zkClient, times(1)).createPersistent(ruleParentPath, true);
        verify(zkClient, times(1)).writeData(ruleRealPath, GsonUtils.getInstance().toJson(ruleData));
    }

    /**
     * test case onRuleChanged update event.
     */
    @Test
    public void testOnRuleChangedUpdate() {
        RuleData ruleData = new RuleData();
        ruleData.setId(MOCK_ID);
        ruleData.setName(MOCK_NAME);
        ruleData.setPluginName(MOCK_PLUGIN_NAME);
        ruleData.setSelectorId(MOCK_SELECTOR_ID);
        String ruleRealPath = DefaultPathConstants.buildRulePath(ruleData.getPluginName(), ruleData.getSelectorId(),
                ruleData.getId());
        String ruleParentPath = DefaultPathConstants.buildRuleParentPath(ruleData.getPluginName());

        when(zkClient.exists(ruleRealPath)).thenReturn(true);
        when(zkClient.exists(ruleParentPath)).thenReturn(true);

        zookeeperDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.UPDATE);
        verify(zkClient, atMostOnce()).createPersistent(ruleRealPath, true);
        verify(zkClient, atMostOnce()).createPersistent(ruleParentPath, true);
        verify(zkClient, times(1)).writeData(ruleRealPath, GsonUtils.getInstance().toJson(ruleData));
    }

    /**
     * test case onRuleChanged refresh event.
     */
    @Test
    public void testOnRuleChangedRefresh() {
        RuleData ruleData = new RuleData();
        ruleData.setId(MOCK_ID);
        ruleData.setName(MOCK_NAME);
        ruleData.setPluginName(MOCK_PLUGIN_NAME);
        ruleData.setSelectorId(MOCK_SELECTOR_ID);
        String ruleParentPath = DefaultPathConstants.buildRuleParentPath(ruleData.getPluginName());

        when(zkClient.exists(ruleParentPath)).thenReturn(true);
        zookeeperDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.REFRESH);
        verify(zkClient, times(1)).deleteRecursive(ruleParentPath);
    }

    /**
     * test case onRuleChanged delete event.
     */
    @Test
    public void testOnRuleChangedDelete() {
        RuleData ruleData = new RuleData();
        ruleData.setId(MOCK_ID);
        ruleData.setName(MOCK_NAME);
        ruleData.setPluginName(MOCK_PLUGIN_NAME);
        ruleData.setSelectorId(MOCK_SELECTOR_ID);
        String ruleRealPath = DefaultPathConstants.buildRulePath(ruleData.getPluginName(), ruleData.getSelectorId(), ruleData.getId());

        when(zkClient.exists(ruleRealPath)).thenReturn(true);
        zookeeperDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.DELETE);
        verify(zkClient, times(1)).delete(ruleRealPath);
    }

}
