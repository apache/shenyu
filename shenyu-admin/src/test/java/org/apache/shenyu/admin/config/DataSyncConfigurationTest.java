/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.admin.config;

import org.I0Itec.zkclient.ZkClient;
import org.apache.curator.test.TestingServer;
import org.apache.shenyu.admin.AbstractConfigurationTest;
import org.apache.shenyu.admin.config.properties.HttpSyncProperties;
import org.apache.shenyu.admin.service.MetaDataService;
import org.apache.shenyu.admin.service.PluginService;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.service.SyncDataService;
import org.apache.shenyu.admin.service.impl.AppAuthServiceImpl;
import org.apache.shenyu.admin.service.impl.SyncDataServiceImpl;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.ApplicationEventPublisher;
import static org.junit.Assert.assertNotNull;

/**
 * The TestCase for DataSyncConfiguration.
 */
@RunWith(MockitoJUnitRunner.class)
@EnableConfigurationProperties(HttpSyncProperties.class)
public final class DataSyncConfigurationTest extends AbstractConfigurationTest {

    private static TestingServer zkServer;

    private final ZkClient zkClient = new ZkClient("127.0.0.1:21810");

    @InjectMocks
    private AppAuthServiceImpl appAuthService;

    @Mock
    private PluginService pluginService;

    @Mock
    private SelectorService selectorService;

    @Mock
    private RuleService ruleService;

    @Mock
    private ApplicationEventPublisher eventPublisher;

    @Mock
    private MetaDataService metaDataService;

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        zkServer = new TestingServer(21810, true);
    }

    @Test
    public void testHttpLongPollingDataChangedListener() {
        final HttpSyncProperties httpSyncProperties = new HttpSyncProperties();
        DataSyncConfiguration.HttpLongPollingListener httpLongPollingListener = new DataSyncConfiguration.HttpLongPollingListener();
        assertNotNull(httpLongPollingListener.httpLongPollingDataChangedListener(httpSyncProperties));
    }

    @Test
    public void testZookeeperDataChangedListener() {
        DataSyncConfiguration.ZookeeperListener zookeeperListener = new DataSyncConfiguration.ZookeeperListener();
        assertNotNull(zookeeperListener.zookeeperDataChangedListener(zkClient));
    }

    @Test
    public void testZookeeperDataInit() {
        final SyncDataService syncDataService = new SyncDataServiceImpl(appAuthService, pluginService, selectorService,
                ruleService, eventPublisher, metaDataService);
        DataSyncConfiguration.ZookeeperListener zookeeperListener = new DataSyncConfiguration.ZookeeperListener();
        assertNotNull(zookeeperListener.zookeeperDataInit(zkClient, syncDataService));
    }

    @Test
    public void testWebsocketDataChangedListener() {
        DataSyncConfiguration.WebsocketListener websocketListener = new DataSyncConfiguration.WebsocketListener();
        assertNotNull(websocketListener.websocketDataChangedListener());
    }

    @Test
    public void testWebsocketCollector() {
        DataSyncConfiguration.WebsocketListener websocketListener = new DataSyncConfiguration.WebsocketListener();
        assertNotNull(websocketListener.websocketCollector());
    }

    @Test
    public void testServerEndpointExporter() {
        DataSyncConfiguration.WebsocketListener websocketListener = new DataSyncConfiguration.WebsocketListener();
        assertNotNull(websocketListener.serverEndpointExporter());
    }

    @After
    public void after() {
        zkClient.close();
    }

    @AfterClass
    public static void tearDown() throws Exception {
        zkServer.stop();
    }
}
