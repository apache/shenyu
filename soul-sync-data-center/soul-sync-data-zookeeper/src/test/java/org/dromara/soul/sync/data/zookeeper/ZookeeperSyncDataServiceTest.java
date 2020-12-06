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
import java.util.Map.Entry;
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
    
    private static TestingServer zkServer;
    
    private ZkClient zkClient;
    
    private ZookeeperSyncDataService syncDataService;
    
    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        zkServer = new TestingServer(21810, true);
    }
    
    @Before
    public void setUp() throws Exception {
        zkClient = new ZkClient("127.0.0.1:21810");
        buildZkData();
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
        final CountDownLatch latch = new CountDownLatch(2);
        final List<PluginData> actuals = new ArrayList<>();
        syncDataService = new ZookeeperSyncDataService(zkClient, new PluginDataSubscriber() {
            @Override
            public void onSubscribe(final PluginData pluginData) {
                latch.countDown();
                actuals.add(pluginData);
            }
    
            @Override
            public void unSubscribe(final PluginData pluginData) {
                latch.countDown();
                actuals.add(pluginData);
            }
        }, Collections.emptyList(), Collections.emptyList());
        PluginData pluginData = new PluginData("6", PluginEnum.DIVIDE.getName(), "", 0, Boolean.TRUE);
        final String pluginPath = ZkPathConstants.buildPluginPath(pluginData.getName());
        zkClient.delete(pluginPath);
        latch.await(5, TimeUnit.SECONDS);
        Assert.assertEquals(2, actuals.size());
        Assert.assertEquals(pluginData, actuals.get(0));
    }
    
    @SneakyThrows
    @Test
    public void testWatcherSelector() {
        final CountDownLatch latch = new CountDownLatch(2);
        final List<SelectorData> actuals = new ArrayList<>();
        syncDataService = new ZookeeperSyncDataService(zkClient, new PluginDataSubscriber() {
            @Override
            public void onSelectorSubscribe(final SelectorData selectorData) {
                latch.countDown();
                actuals.add(selectorData);
            }
    
            @Override
            public void unSelectorSubscribe(final SelectorData selectorData) {
                latch.countDown();
                actuals.add(selectorData);
            }
        }, Collections.emptyList(), Collections.emptyList());
        final SelectorData selectorData = buildSelectorData("xxx", "aaa", PluginEnum.DIVIDE.getName());
        final String selectorPath = ZkPathConstants.buildSelectorRealPath(selectorData.getPluginName(), selectorData.getId());
        zkClient.delete(selectorPath);
        latch.await(5, TimeUnit.SECONDS);
        Assert.assertEquals(2, actuals.size());
        Assert.assertEquals(selectorData, actuals.get(0));
    }
    
    @SneakyThrows
    @Test
    public void testWatcherRule() {
        final CountDownLatch latch = new CountDownLatch(1);
        final List<RuleData> actuals = new ArrayList<>();
        syncDataService = new ZookeeperSyncDataService(zkClient, new PluginDataSubscriber() {
            @Override
            public void unRuleSubscribe(final RuleData ruleData) {
                latch.countDown();
                actuals.add(ruleData);
            }
        }, Collections.emptyList(), Collections.emptyList());
        final RuleData ruleData = buildRuleDTO("aaa", "xxx", PluginEnum.DIVIDE.getName());
        final String rulePath = ZkPathConstants.buildRulePath(ruleData.getPluginName(), ruleData.getSelectorId(), ruleData.getId());
        zkClient.delete(rulePath);
        latch.await(5, TimeUnit.SECONDS);
        Assert.assertEquals(1, actuals.size());
        Assert.assertEquals(ruleData.getId(), actuals.get(0).getId());
    }
    
    @SneakyThrows
    @Test
    public void testWatcherAppAuth() {
        final CountDownLatch latch = new CountDownLatch(2);
        final List<AppAuthData> actuals = new ArrayList<>();
        AuthDataSubscriber authDataSubscriber = new AuthDataSubscriber() {
            @Override
            public void onSubscribe(final AppAuthData appAuthData) {
                latch.countDown();
                actuals.add(appAuthData);
            }
    
            @Override
            public void unSubscribe(final AppAuthData appAuthData) {
                latch.countDown();
                actuals.add(appAuthData);
            }
        };
        final AppAuthData appAuthData = buildAppAuthData("7sdfdfx", "dfd#434");
        final String appAuthPath = ZkPathConstants.buildAppAuthPath(appAuthData.getAppKey());
        syncDataService = new ZookeeperSyncDataService(zkClient, null,
                Collections.emptyList(), Lists.newArrayList(authDataSubscriber));
        zkClient.delete(appAuthPath);
        latch.await(5, TimeUnit.SECONDS);
        Assert.assertEquals(2, actuals.size());
        Assert.assertEquals(appAuthData, actuals.get(0));
    }
    
    @SneakyThrows
    @Test
    public void testWatcherMetaData() {
        final CountDownLatch latch = new CountDownLatch(2);
        final List<MetaData> actuals = new ArrayList<>();
        MetaDataSubscriber metaDataSubscriber = new MetaDataSubscriber() {
            @Override
            public void onSubscribe(final MetaData metaData) {
                latch.countDown();
                actuals.add(metaData);
            }
    
            @Override
            public void unSubscribe(final MetaData metaData) {
                latch.countDown();
                actuals.add(metaData);
            }
        };
        final MetaData metaData = buildMetaData("dz", "httptest", "http");
        final String metaDataPath = ZkPathConstants.buildMetaDataPath(metaData.getPath());
        syncDataService = new ZookeeperSyncDataService(zkClient, null,
                 Lists.newArrayList(metaDataSubscriber), Collections.emptyList());
        zkClient.delete(metaDataPath);
        latch.await(1000, TimeUnit.MILLISECONDS);
        Assert.assertEquals(2, actuals.size());
        Assert.assertEquals(metaData, actuals.get(0));
    }
    
    private void buildZkData() {
        Map<String, PluginData> pluginMap = Maps.newHashMap();
        pluginMap.put(PluginEnum.DIVIDE.getName(), new PluginData("6", PluginEnum.DIVIDE.getName(), "", 0, Boolean.TRUE));
        for (Entry<String, PluginData> entry : pluginMap.entrySet()) {
            final PluginData pluginData = entry.getValue();
            writePlugin(pluginData);
            final SelectorData selectorZkDTO = buildSelectorData("xxx", "aaa", pluginData.getName());
            writeSelector(selectorZkDTO);
            final RuleData ruleZkDTO = buildRuleDTO("aaa", selectorZkDTO.getId(), selectorZkDTO.getPluginName());
            writeRule(ruleZkDTO);
        }
        final AppAuthData appAuthData = buildAppAuthData("7sdfdfx", "dfd#434");
        writeAppAuth(appAuthData);
        final MetaData metaData = buildMetaData("dz", "httptest", "http");
        writeMetaData(metaData);
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
    
    private void writeRule(final RuleData ruleZkDTO) {
        final String rulePath = ZkPathConstants
                .buildRulePath(ruleZkDTO.getPluginName(), ruleZkDTO.getSelectorId(), ruleZkDTO.getId());
        if (!zkClient.exists(rulePath)) {
            zkClient.createPersistent(rulePath, true);
        }
        zkClient.writeData(rulePath, ruleZkDTO);
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
