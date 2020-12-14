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

package org.dromara.soul.admin.listener.zookeeper;

import com.google.common.collect.ImmutableList;
import lombok.SneakyThrows;
import org.I0Itec.zkclient.ZkClient;
import org.apache.curator.test.TestingServer;
import org.dromara.soul.common.constant.ZkPathConstants;
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.enums.DataEventTypeEnum;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;

import java.net.URLEncoder;

/**
 * The testCase for ZookeeperDataChangedListener.
 *
 * @author tyjwan
 */
@RunWith(MockitoJUnitRunner.class)
public final class ZookeeperDataChangedListenerTest {

    private static TestingServer zkServer;

    private ZookeeperDataChangedListener zookeeperDataChangedListener;

    private ZkClient zkClient;

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        zkServer = new TestingServer(21810, true);
    }

    @Before
    public void setUp() {
        zkClient = new ZkClient("127.0.0.1:21810");
        this.zookeeperDataChangedListener = new ZookeeperDataChangedListener(zkClient);
    }

    @After
    public void after() {
        zkClient.close();
    }

    @AfterClass
    public static void tearDown() throws Exception {
        zkServer.stop();
    }

    /**
     * test case onAppAuthChanged.
     */
    @Test
    public void testOnAppAuthChanged() {
        String mockAppKey = "MOCK_APP_KEY";
        String mockAppSecret = "MOCK_APP_SECRET";
        AppAuthData appAuthData = new AppAuthData();
        appAuthData.setAppKey(mockAppKey);
        appAuthData.setAppSecret(mockAppSecret);
        appAuthData.setEnabled(true);

        AppAuthData appAuthDataUpdate = new AppAuthData();
        String mockAppSecretUpdate = "MOCK_APP_SECRET2";
        appAuthDataUpdate.setAppKey(mockAppKey);
        appAuthDataUpdate.setAppSecret(mockAppSecretUpdate);
        appAuthDataUpdate.setEnabled(true);

        zookeeperDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthData), DataEventTypeEnum.CREATE);
        Assert.assertEquals(zkClient.readData(ZkPathConstants.buildAppAuthPath(mockAppKey)), appAuthData);

        zookeeperDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthDataUpdate), DataEventTypeEnum.UPDATE);
        Assert.assertEquals(zkClient.readData(ZkPathConstants.buildAppAuthPath(mockAppKey)), appAuthDataUpdate);

        zookeeperDataChangedListener.onAppAuthChanged(ImmutableList.of(appAuthDataUpdate), DataEventTypeEnum.DELETE);
        Assert.assertNull(zkClient.readData(ZkPathConstants.buildAppAuthPath(mockAppKey), Boolean.TRUE));
    }

    /**
     * test case onMetaDataChanged.
     */
    @SneakyThrows
    @Test
    public void testOnMetaDataChanged() {
        String mockId = "MOCK_ID";
        String mockAppName = "MOCK_APP_NAME";
        String mockPath = "MOCK_PATH";
        MetaData metaData = new MetaData();
        metaData.setId(mockId);
        metaData.setAppName(mockAppName);
        metaData.setPath(mockPath);

        MetaData metaDataUpdate = new MetaData();
        String mockAppNameUpdate = "MOCK_APP_NAME2";
        metaDataUpdate.setId(mockId);
        metaDataUpdate.setAppName(mockAppNameUpdate);
        metaDataUpdate.setPath(mockPath);

        zookeeperDataChangedListener.onMetaDataChanged(ImmutableList.of(metaData), DataEventTypeEnum.CREATE);
        Assert.assertEquals(zkClient.readData(
                ZkPathConstants.buildMetaDataPath(URLEncoder.encode(mockPath, "UTF-8"))), metaData);

        zookeeperDataChangedListener.onMetaDataChanged(ImmutableList.of(metaDataUpdate), DataEventTypeEnum.UPDATE);
        Assert.assertEquals(zkClient.readData(
                ZkPathConstants.buildMetaDataPath(URLEncoder.encode(mockPath, "UTF-8"))), metaDataUpdate);

        zookeeperDataChangedListener.onMetaDataChanged(ImmutableList.of(metaDataUpdate), DataEventTypeEnum.DELETE);
        Assert.assertNull(zkClient.readData(
                ZkPathConstants.buildMetaDataPath(URLEncoder.encode(mockPath, "UTF-8")), Boolean.TRUE));
    }

    /**
     * test case onPluginChanged.
     */
    @Test
    public void testOnPluginChanged() {
        String mockId = "MOCK_ID";
        String mockName = "MOCK_NAME";
        String mockConfig = "MOCK_CONFIG";
        PluginData pluginData = new PluginData();
        pluginData.setId(mockId);
        pluginData.setName(mockName);
        pluginData.setConfig(mockConfig);

        PluginData pluginDataUpdate = new PluginData();
        String mockConfigUpdate = "MOCK_CONFIG2";
        pluginDataUpdate.setId(mockId);
        pluginDataUpdate.setName(mockName);
        pluginDataUpdate.setConfig(mockConfigUpdate);

        zookeeperDataChangedListener.onPluginChanged(ImmutableList.of(pluginData), DataEventTypeEnum.CREATE);
        Assert.assertEquals(zkClient.readData(ZkPathConstants.buildPluginPath(mockName)), pluginData);

        zookeeperDataChangedListener.onPluginChanged(ImmutableList.of(pluginDataUpdate), DataEventTypeEnum.UPDATE);
        Assert.assertEquals(zkClient.readData(ZkPathConstants.buildPluginPath(mockName)), pluginDataUpdate);

        zookeeperDataChangedListener.onPluginChanged(ImmutableList.of(pluginDataUpdate), DataEventTypeEnum.DELETE);
        Assert.assertNull(zkClient.readData(ZkPathConstants.buildPluginPath(mockName), Boolean.TRUE));
    }

    /**
     * test case onSelectorChanged.
     */
    @Test
    public void testOnSelectorChanged() {
        String mockId = "MOCK_ID";
        String mockName = "MOCK_NAME";
        String mockPluginName = "MOCK_PLUGIN_NAME";
        SelectorData selectorData = new SelectorData();
        selectorData.setId(mockId);
        selectorData.setName(mockName);
        selectorData.setPluginName(mockPluginName);

        SelectorData selectorDataUpdate = new SelectorData();
        String mockNameUpdate = "MOCK_NAME2";
        selectorDataUpdate.setId(mockId);
        selectorDataUpdate.setName(mockNameUpdate);
        selectorDataUpdate.setPluginName(mockPluginName);

        zookeeperDataChangedListener.onSelectorChanged(ImmutableList.of(selectorData), DataEventTypeEnum.CREATE);
        Assert.assertEquals(zkClient.readData(ZkPathConstants.buildSelectorRealPath(mockPluginName, mockId)), selectorData);

        zookeeperDataChangedListener.onSelectorChanged(ImmutableList.of(selectorDataUpdate), DataEventTypeEnum.UPDATE);
        Assert.assertEquals(zkClient.readData(ZkPathConstants.buildSelectorRealPath(mockPluginName, mockId)), selectorDataUpdate);

        zookeeperDataChangedListener.onSelectorChanged(ImmutableList.of(selectorDataUpdate), DataEventTypeEnum.DELETE);
        Assert.assertNull(zkClient.readData(ZkPathConstants.buildSelectorRealPath(mockPluginName, mockId),
                Boolean.TRUE));
    }

    /**
     * test case onRuleChanged.
     */
    @Test
    public void testOnRuleChanged() {
        String mockId = "MOCK_ID";
        String mockName = "MOCK_NAME";
        String mockPluginName = "MOCK_PLUGIN_NAME";
        String mockSelectorId = "MOCK_SELECTOR_ID";
        RuleData ruleData = new RuleData();
        ruleData.setId(mockId);
        ruleData.setName(mockName);
        ruleData.setPluginName(mockPluginName);
        ruleData.setSelectorId(mockSelectorId);

        zookeeperDataChangedListener.onRuleChanged(ImmutableList.of(ruleData), DataEventTypeEnum.CREATE);
        Assert.assertEquals(zkClient.readData(ZkPathConstants.buildRulePath(mockPluginName, mockSelectorId, mockId)), ruleData);

        String mockNameUpdate = "MOCK_NAME2";
        RuleData ruleDataUpdate = new RuleData();
        ruleDataUpdate.setId(mockId).setName(mockNameUpdate).setPluginName(mockPluginName).setSelectorId(mockSelectorId);
        zookeeperDataChangedListener.onRuleChanged(ImmutableList.of(ruleDataUpdate), DataEventTypeEnum.UPDATE);
        Assert.assertEquals(zkClient.readData(ZkPathConstants.buildRulePath(mockPluginName, mockSelectorId, mockId)), ruleDataUpdate);

        zookeeperDataChangedListener.onRuleChanged(ImmutableList.of(ruleDataUpdate), DataEventTypeEnum.DELETE);
        Assert.assertNull(zkClient.readData(ZkPathConstants.buildRulePath(mockPluginName, mockSelectorId, mockId),
                Boolean.TRUE));
    }
}
