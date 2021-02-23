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

package org.dromara.soul.admin.config;

import com.alibaba.nacos.api.config.ConfigService;
import org.I0Itec.zkclient.ZkClient;
import org.apache.curator.test.TestingServer;
import org.dromara.soul.admin.AbstractConfigurationTest;
import org.dromara.soul.admin.config.properties.HttpSyncProperties;
import org.dromara.soul.admin.listener.nacos.NacosMockConfigService;
import org.dromara.soul.admin.service.MetaDataService;
import org.dromara.soul.admin.service.PluginService;
import org.dromara.soul.admin.service.RuleService;
import org.dromara.soul.admin.service.SelectorService;
import org.dromara.soul.admin.service.SyncDataService;
import org.dromara.soul.admin.service.impl.AppAuthServiceImpl;
import org.dromara.soul.admin.service.sync.SyncDataServiceImpl;
import org.junit.Before;
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
 *
 * @author xwzwmt
 */
@RunWith(MockitoJUnitRunner.class)
@EnableConfigurationProperties(HttpSyncProperties.class)
public final class DataSyncConfigurationTest extends AbstractConfigurationTest {

    private static TestingServer zkServer;

    private final ZkClient zkClient = new ZkClient("127.0.0.1:21810");

    private ConfigService configService;

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

    @Before
    public void setUp() {
        configService = new NacosMockConfigService();
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
    public void testNacosDataChangedListener() {
        DataSyncConfiguration.NacosListener nacosListener = new DataSyncConfiguration.NacosListener();
        assertNotNull(nacosListener.nacosDataChangedListener(configService));
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
