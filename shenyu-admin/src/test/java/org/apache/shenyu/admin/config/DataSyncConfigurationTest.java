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

import com.alibaba.nacos.client.config.NacosConfigService;
import com.ecwid.consul.v1.ConsulClient;
import org.I0Itec.zkclient.ZkClient;
import org.apache.curator.test.TestingServer;
import org.apache.shenyu.admin.AbstractConfigurationTest;
import org.apache.shenyu.admin.config.properties.ConsulProperties;
import org.apache.shenyu.admin.config.properties.HttpSyncProperties;
import org.apache.shenyu.admin.listener.etcd.EtcdClient;
import org.apache.shenyu.admin.service.MetaDataService;
import org.apache.shenyu.admin.service.PluginService;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.service.SyncDataService;
import org.apache.shenyu.admin.service.impl.AppAuthServiceImpl;
import org.apache.shenyu.admin.service.impl.SyncDataServiceImpl;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.ApplicationEventPublisher;

import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The TestCase for {@link DataSyncConfiguration}.
 */
@ExtendWith(MockitoExtension.class)
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

    @BeforeAll
    public static void setUpBeforeClass() throws Exception {
        zkServer = new TestingServer(21810, true);
    }

    @AfterAll
    public static void tearDown() throws Exception {
        zkServer.stop();
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

    @Test
    public void testNacosDataChangedListener() {
        DataSyncConfiguration.NacosListener nacosListener = new DataSyncConfiguration.NacosListener();
        NacosConfigService configService = mock(NacosConfigService.class);
        assertNotNull(nacosListener.nacosDataChangedListener(configService));
    }

    @Test
    public void testNacosDataInit() {
        DataSyncConfiguration.NacosListener nacosListener = new DataSyncConfiguration.NacosListener();
        NacosConfigService configService = mock(NacosConfigService.class);
        SyncDataService syncDataService = mock(SyncDataService.class);
        assertNotNull(nacosListener.nacosDataInit(configService, syncDataService));
    }

    @Test
    public void testEtcdDataChangedListener() {
        DataSyncConfiguration.EtcdListener etcdListener = new DataSyncConfiguration.EtcdListener();
        EtcdClient client = mock(EtcdClient.class);
        assertNotNull(etcdListener.etcdDataChangedListener(client));
    }

    @Test
    public void testEtcdDataInit() {
        DataSyncConfiguration.EtcdListener etcdListener = new DataSyncConfiguration.EtcdListener();
        EtcdClient client = mock(EtcdClient.class);
        SyncDataService syncDataService = mock(SyncDataService.class);
        assertNotNull(etcdListener.etcdDataInit(client, syncDataService));
    }

    @Test
    public void testConsulClient() {
        DataSyncConfiguration.ConsulListener consulListener = new DataSyncConfiguration.ConsulListener();
        ConsulProperties consulProperties = mock(ConsulProperties.class);
        when(consulProperties.getUrl()).thenReturn("127.0.0.1");
        assertNotNull(consulListener.consulClient(consulProperties));
    }

    @Test
    public void testConsulDataChangedListener() {
        DataSyncConfiguration.ConsulListener consulListener = new DataSyncConfiguration.ConsulListener();
        ConsulClient consulClient = mock(ConsulClient.class);
        assertNotNull(consulListener.consulDataChangedListener(consulClient));
    }

    @Test
    public void testConsulDataInit() {
        DataSyncConfiguration.ConsulListener consulListener = new DataSyncConfiguration.ConsulListener();
        ConsulClient consulClient = mock(ConsulClient.class);
        SyncDataService syncDataService = mock(SyncDataService.class);
        assertNotNull(consulListener.consulDataInit(consulClient, syncDataService));
    }

    @AfterEach
    public void after() {
        zkClient.close();
    }
}
