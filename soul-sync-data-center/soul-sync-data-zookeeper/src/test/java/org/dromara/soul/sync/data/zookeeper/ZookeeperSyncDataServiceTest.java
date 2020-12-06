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

package org.dromara.soul.sync.data.zookeeper;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.I0Itec.zkclient.ZkClient;
import org.apache.curator.test.TestingServer;
import org.dromara.soul.common.constant.ZkPathConstants;
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.ConditionData;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.enums.MatchModeEnum;
import org.dromara.soul.common.enums.OperatorEnum;
import org.dromara.soul.common.enums.ParamTypeEnum;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.SelectorTypeEnum;
import org.dromara.soul.sync.data.api.AuthDataSubscriber;
import org.dromara.soul.sync.data.api.MetaDataSubscriber;
import org.dromara.soul.sync.data.api.PluginDataSubscriber;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

/**
 * Test cases for ZookeeperSyncDataService.
 *
 * @author zendwang
 */
@Slf4j
@SuppressWarnings("all")
public final class ZookeeperSyncDataServiceTest {
    
    private static final Map<String, PluginData> PLUGIN_MAP = Maps.newConcurrentMap();
    
    private static TestingServer zkServer;
    
    private ZkClient zkClient;
    
    private ZookeeperSyncDataService syncDataService;
    
    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        PLUGIN_MAP.put(PluginEnum.DIVIDE.getName(), new PluginData("6", PluginEnum.DIVIDE.getName(), "", 0, Boolean.TRUE));
        zkServer = new TestingServer(31810, true);
    }
    
    @Before
    public void setUp() throws Exception {
        zkClient = new ZkClient("127.0.0.1:31810");
    }
    
    @After
    public void after() {
        zkClient.close();
        syncDataService.close();
    }
    
    @AfterClass
    public static void tearDown() throws Exception {
        zkServer.stop();
    }
    
    @SneakyThrows
    @Test
    public void testWatcherPlugin() {
        //init plugin data on Zookeeper
        initZkPluginData();
        final CountDownLatch latch = new CountDownLatch(2);
        final List<PluginData> onSubscribeList = new ArrayList<>();
        final List<PluginData> unsubscribeList = new ArrayList<>();
        syncDataService = new ZookeeperSyncDataService(zkClient, new PluginDataSubscriber() {
            @Override
            public void onSubscribe(final PluginData pluginData) {
                latch.countDown();
                onSubscribeList.add(pluginData);
            }
    
            @Override
            public void unSubscribe(final PluginData pluginData) {
                latch.countDown();
                unsubscribeList.add(pluginData);
            }
        }, Collections.emptyList(), Collections.emptyList());
        PluginData pluginData = PLUGIN_MAP.get(PluginEnum.DIVIDE.getName());
        final String pluginPath = ZkPathConstants.buildPluginPath(pluginData.getName());
        zkClient.delete(pluginPath);
        latch.await(500, TimeUnit.MILLISECONDS);
        Assert.assertEquals(1, onSubscribeList.size());
        Assert.assertEquals(pluginData, onSubscribeList.get(0));
        Assert.assertEquals(1, unsubscribeList.size());
        Assert.assertEquals(pluginData.getName(), unsubscribeList.get(0).getName());
    }
    
    @SneakyThrows
    @Test
    public void testWatcherSelector() {
        //init plugin data on zk
        initZkPluginData();
        //init one selector data on zk
        final SelectorData selectorData = buildSelectorData("xxx", "aaa", PluginEnum.DIVIDE.getName());
        writeSelector(selectorData);
        final CountDownLatch latch = new CountDownLatch(3);
        final List<SelectorData> subscribeList = new ArrayList<>();
        final List<SelectorData> unsubscribeList = new ArrayList<>();
        syncDataService = new ZookeeperSyncDataService(zkClient, new PluginDataSubscriber() {
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
        //add new selector data on zk
        final SelectorData otherSelectorData = buildSelectorData("ddd", "bbb", PluginEnum.DIVIDE.getName());
        writeSelector(otherSelectorData);
        final String selectorPath = ZkPathConstants.buildSelectorRealPath(selectorData.getPluginName(), selectorData.getId());
        zkClient.delete(selectorPath);
        latch.await(10, TimeUnit.SECONDS);
        Assert.assertEquals(2, subscribeList.size());
        Assert.assertEquals(1, unsubscribeList.size());
        Assert.assertEquals(selectorData, subscribeList.get(0));
        Assert.assertEquals(otherSelectorData, subscribeList.get(1));
        Assert.assertEquals(selectorData.getId(), unsubscribeList.get(0).getId());
    }
    
    @SneakyThrows
    @Test
    public void testWatcherRule() {
        //init plugin data on zk
        initZkPluginData();
        //init one rule data on zk
        final RuleData ruleData = buildRuleDTO("aaa", "xxx", PluginEnum.DIVIDE.getName());
        writeRule(ruleData);
        final CountDownLatch latch = new CountDownLatch(3);
        final List<RuleData> subscribeList = new ArrayList<>();
        final List<RuleData> unsubscribeList = new ArrayList<>();
        syncDataService = new ZookeeperSyncDataService(zkClient, new PluginDataSubscriber() {
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
        //add new rule data on zk
        final RuleData otherRuleData = buildRuleDTO("bbb", "xxx", PluginEnum.DIVIDE.getName());
        writeRule(otherRuleData);
        final String rulePath = ZkPathConstants.buildRulePath(ruleData.getPluginName(), ruleData.getSelectorId(), ruleData.getId());
        zkClient.delete(rulePath);
        latch.await(20, TimeUnit.SECONDS);
        Assert.assertEquals(2, subscribeList.size());
        Assert.assertEquals(1, unsubscribeList.size());
        Assert.assertEquals(ruleData, subscribeList.get(0));
        Assert.assertEquals(otherRuleData, subscribeList.get(1));
        Assert.assertEquals(ruleData.getId(), unsubscribeList.get(0).getId());
    }
    
    @SneakyThrows
    @Test
    public void testWatcherAppAuth() {
        final CountDownLatch latch = new CountDownLatch(3);
        final List<AppAuthData> subscribeList = new ArrayList<>();
        final List<AppAuthData> unsubscribeList = new ArrayList<>();
        final AppAuthData appAuthData = buildAppAuthData("7sdfdfx", "dfd#434");
        writeAppAuth(appAuthData);
        AuthDataSubscriber authDataSubscriber = new AuthDataSubscriber() {
            @Override
            public void onSubscribe(final AppAuthData appAuthData) {
                latch.countDown();
                subscribeList.add(appAuthData);
            }
    
            @Override
            public void unSubscribe(final AppAuthData appAuthData) {
                latch.countDown();
                unsubscribeList.add(appAuthData);
            }
        };
        syncDataService = new ZookeeperSyncDataService(zkClient, null,
                Collections.emptyList(), Lists.newArrayList(authDataSubscriber));
        final AppAuthData otherAppAuthData = buildAppAuthData("8sdfdfx", "efd#434");
        writeAppAuth(otherAppAuthData);
        final String appAuthPath = ZkPathConstants.buildAppAuthPath(appAuthData.getAppKey());
        zkClient.delete(appAuthPath);
        latch.await(500, TimeUnit.MILLISECONDS);
        Assert.assertEquals(2, subscribeList.size());
        Assert.assertEquals(1, unsubscribeList.size());
        Assert.assertEquals(appAuthData, subscribeList.get(0));
        Assert.assertEquals(otherAppAuthData, subscribeList.get(1));
    }
    
    @SneakyThrows
    @Test
    public void testWatcherMetaData() {
        final CountDownLatch latch = new CountDownLatch(3);
        final List<MetaData> subscribeList = new ArrayList<>();
        final List<MetaData> unsubscribeList = new ArrayList<>();
        final MetaData metaData = buildMetaData("dz", "httptest", "http");
        writeMetaData(metaData);
        MetaDataSubscriber metaDataSubscriber = new MetaDataSubscriber() {
            @Override
            public void onSubscribe(final MetaData metaData) {
                latch.countDown();
                subscribeList.add(metaData);
            }
    
            @Override
            public void unSubscribe(final MetaData metaData) {
                latch.countDown();
                unsubscribeList.add(metaData);
            }
        };
        syncDataService = new ZookeeperSyncDataService(zkClient, null,
                 Lists.newArrayList(metaDataSubscriber), Collections.emptyList());
        final MetaData otherMetaData = buildMetaData("dz2", "httptest2", "http2");
        writeMetaData(otherMetaData);
        final String metaDataPath = ZkPathConstants.buildMetaDataPath(metaData.getPath());
        zkClient.delete(metaDataPath);
        latch.await(500, TimeUnit.MILLISECONDS);
        Assert.assertEquals(2, subscribeList.size());
        Assert.assertEquals(1, unsubscribeList.size());
        Assert.assertEquals(metaData, subscribeList.get(0));
        Assert.assertEquals(otherMetaData, subscribeList.get(1));
    }
    
    private void initZkPluginData() {
        PLUGIN_MAP.forEach((k, v) -> writePlugin(v));
    }
    
    private MetaData buildMetaData(final String id, final String appName, final String path) {
        final MetaData metaData = new MetaData();
        metaData.setId(id);
        metaData.setAppName(appName);
        metaData.setPath(path);
        metaData.setEnabled(Boolean.TRUE);
        return metaData;
    }
    
    private AppAuthData buildAppAuthData(final String appKey, final String appSecret) {
        final AppAuthData appAuthData = new AppAuthData();
        appAuthData.setAppKey(appKey);
        appAuthData.setAppSecret(appSecret);
        appAuthData.setEnabled(Boolean.TRUE);
        return appAuthData;
    }
    
    private SelectorData buildSelectorData(final String id, final String name, final String pluginName) {
        SelectorData selectorData = new SelectorData();
        selectorData.setId(id);
        selectorData.setName(name);
        selectorData.setSort(1);
        selectorData.setContinued(Boolean.TRUE);
        selectorData.setLoged(Boolean.TRUE);
        selectorData.setEnabled(Boolean.TRUE);
        selectorData.setPluginName(pluginName);
        selectorData.setType(SelectorTypeEnum.FULL_FLOW.getCode());
        selectorData.setMatchMode(MatchModeEnum.AND.getCode());
        final ConditionData conditionZkDTO = buildConditionZkDTO();
        selectorData.setConditionList(Collections.singletonList(conditionZkDTO));
        return selectorData;
    }
    
    private ConditionData buildConditionZkDTO() {
        ConditionData condition = new ConditionData();
        condition.setOperator(OperatorEnum.EQ.getAlias());
        condition.setParamName("module");
        condition.setParamValue("pdm");
        condition.setParamType(ParamTypeEnum.POST.getName());
        return condition;
    }
    
    private RuleData buildRuleDTO(final String id, final String selectorId, final String pluginName) {
        RuleData dto = new RuleData();
        dto.setId(id);
        dto.setSelectorId(selectorId);
        dto.setName(pluginName + " rule name");
        dto.setConditionDataList(Collections.singletonList(buildConditionZkDTO()));
        dto.setEnabled(Boolean.TRUE);
        dto.setLoged(Boolean.TRUE);
        dto.setMatchMode(MatchModeEnum.AND.getCode());
        dto.setPluginName(pluginName);
        dto.setSort(120);
        return dto;
    }
    
    private void writePlugin(final PluginData pluginData) {
        String pluginPath = ZkPathConstants.buildPluginPath(pluginData.getName());
        if (!zkClient.exists(pluginPath)) {
            zkClient.createPersistent(pluginPath, true);
        }
        zkClient.writeData(pluginPath, pluginData);
    }
    
    private void writeSelector(final SelectorData selectorData) {
        final String selectorRealPath =
                ZkPathConstants.buildSelectorRealPath(selectorData.getPluginName(), selectorData.getId());
        if (!zkClient.exists(selectorRealPath)) {
            zkClient.createPersistent(selectorRealPath, true);
        }
        zkClient.writeData(selectorRealPath, selectorData);
    }
    
    private void writeRule(final RuleData ruleData) {
        final String rulePath = ZkPathConstants
                .buildRulePath(ruleData.getPluginName(), ruleData.getSelectorId(), ruleData.getId());
        if (!zkClient.exists(rulePath)) {
            zkClient.createPersistent(rulePath, true);
        }
        zkClient.writeData(rulePath, ruleData);
    }
    
    private void writeAppAuth(final AppAuthData appAuthData) {
        final String appAuthPath = ZkPathConstants
                .buildAppAuthPath(appAuthData.getAppKey());
        if (!zkClient.exists(appAuthPath)) {
            zkClient.createPersistent(appAuthPath, true);
        }
        zkClient.writeData(appAuthPath, appAuthData);
    }
    
    private void writeMetaData(final MetaData metaData) {
        final String metaDataPath = ZkPathConstants
                .buildMetaDataPath(metaData.getPath());
        if (!zkClient.exists(metaDataPath)) {
            zkClient.createPersistent(metaDataPath, true);
        }
        zkClient.writeData(metaDataPath, metaData);
    }
}
