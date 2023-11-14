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

package org.apache.shenyu.admin.config;

import com.alibaba.nacos.api.NacosFactory;
import com.alibaba.nacos.api.config.ConfigService;
import com.alibaba.nacos.client.config.NacosConfigService;
import com.ecwid.consul.v1.ConsulClient;
import com.tencent.polaris.configuration.api.core.ConfigFilePublishService;
import com.tencent.polaris.configuration.api.core.ConfigFileService;
import org.apache.curator.test.TestingServer;
import org.apache.shenyu.admin.AbstractConfigurationTest;
import org.apache.shenyu.admin.config.properties.ConsulProperties;
import org.apache.shenyu.admin.config.properties.HttpSyncProperties;
import org.apache.shenyu.admin.config.properties.NacosProperties;
import org.apache.shenyu.admin.config.properties.PolarisProperties;
import org.apache.shenyu.admin.config.properties.ZookeeperProperties;
import org.apache.shenyu.admin.listener.etcd.EtcdClient;
import org.apache.shenyu.admin.service.DiscoveryService;
import org.apache.shenyu.admin.service.MetaDataService;
import org.apache.shenyu.admin.service.PluginService;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.service.SyncDataService;
import org.apache.shenyu.admin.service.impl.AppAuthServiceImpl;
import org.apache.shenyu.admin.service.impl.SyncDataServiceImpl;
import org.apache.shenyu.register.client.server.zookeeper.ZookeeperClient;
import org.apache.shenyu.register.client.server.zookeeper.ZookeeperConfig;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.ApplicationEventPublisher;

import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * The TestCase for {@link DataSyncConfiguration}.
 */
@ExtendWith(MockitoExtension.class)
@EnableConfigurationProperties(HttpSyncProperties.class)
public final class DataSyncConfigurationTest extends AbstractConfigurationTest {

    private static TestingServer zkServer;

    private static ZookeeperClient zkClient;

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

    @Mock
    private DiscoveryService discoveryService;

    @BeforeAll
    public static void setUpBeforeClass() throws Exception {
        zkServer = new TestingServer();
        ZookeeperConfig config = new ZookeeperConfig(zkServer.getConnectString());
        zkClient = new ZookeeperClient(config);
    }

    @AfterAll
    public static void tearDown() throws Exception {
        zkClient.close();
        zkServer.stop();
    }

    @Test
    public void testHttpLongPollingDataChangedListener() {
        final HttpSyncProperties httpSyncProperties = new HttpSyncProperties();
        HttpLongPollingSyncConfiguration httpLongPollingListener = new HttpLongPollingSyncConfiguration();
        assertNotNull(httpLongPollingListener.httpLongPollingDataChangedListener(httpSyncProperties));
    }

    @Test
    public void zookeeperClientTest() {
        try (MockedConstruction<ZookeeperClient> zookeeperClientMockedConstruction = mockConstruction(ZookeeperClient.class)) {
            final ZookeeperProperties zookeeperProperties = new ZookeeperProperties();
            ZookeeperSyncConfiguration zookeeperListener = new ZookeeperSyncConfiguration();
            assertNotNull(zookeeperListener.zookeeperClient(zookeeperProperties));
            zookeeperProperties.setSessionTimeout(3000);
            zookeeperProperties.setConnectionTimeout(3000);
            assertNotNull(zookeeperListener.zookeeperClient(zookeeperProperties));
        }
    }

    @Test
    public void testZookeeperDataChangedListener() {
        ZookeeperSyncConfiguration zookeeperListener = new ZookeeperSyncConfiguration();
        assertNotNull(zookeeperListener.zookeeperDataChangedListener(zkClient));
    }

    @Test
    public void testZookeeperDataInit() {
        final SyncDataService syncDataService = new SyncDataServiceImpl(appAuthService, pluginService, selectorService,
                ruleService, eventPublisher, metaDataService, discoveryService);
        ZookeeperSyncConfiguration zookeeperListener = new ZookeeperSyncConfiguration();
        assertNotNull(zookeeperListener.zookeeperDataChangedInit(zkClient));
    }

    @Test
    public void testWebsocketDataChangedListener() {
        WebSocketSyncConfiguration websocketListener = new WebSocketSyncConfiguration();
        assertNotNull(websocketListener.websocketDataChangedListener());
    }

    @Test
    public void testWebsocketCollector() {
        WebSocketSyncConfiguration websocketListener = new WebSocketSyncConfiguration();
        assertNotNull(websocketListener.websocketCollector());
    }

    @Test
    public void testServerEndpointExporter() {
        WebSocketSyncConfiguration websocketListener = new WebSocketSyncConfiguration();
        assertNotNull(websocketListener.serverEndpointExporter());
    }

    @Test
    public void testNacosDataChangedListener() {
        NacosSyncConfiguration nacosListener = new NacosSyncConfiguration();
        NacosConfigService configService = mock(NacosConfigService.class);
        assertNotNull(nacosListener.nacosDataChangedListener(configService));
    }

    @Test
    public void testNacosDataInit() {
        NacosSyncConfiguration nacosListener = new NacosSyncConfiguration();
        NacosConfigService configService = mock(NacosConfigService.class);
        assertNotNull(nacosListener.nacosDataChangedInit(configService));
    }

    @Test
    public void nacosConfigServiceTest() {
        try (MockedStatic<NacosFactory> nacosFactoryMockedStatic = mockStatic(NacosFactory.class)) {
            final NacosProperties nacosProperties = new NacosProperties();
            final NacosProperties.NacosACMProperties nacosACMProperties = new NacosProperties.NacosACMProperties();
            nacosProperties.setAcm(nacosACMProperties);
            nacosFactoryMockedStatic.when(() -> NacosFactory.createConfigService(any(Properties.class))).thenReturn(mock(ConfigService.class));
            NacosSyncConfiguration nacosListener = new NacosSyncConfiguration();
            nacosProperties.setUrl("url");
            Assertions.assertDoesNotThrow(() -> nacosListener.nacosConfigService(nacosProperties));
            nacosProperties.setNamespace("url");
            nacosProperties.setUsername("username");
            nacosProperties.setPassword("password");
            Assertions.assertDoesNotThrow(() -> nacosListener.nacosConfigService(nacosProperties));
            nacosACMProperties.setEnabled(true);
            nacosACMProperties.setEndpoint("acm.aliyun.com");
            nacosACMProperties.setAccessKey("accessKey");
            nacosACMProperties.setNamespace("namespace");
            nacosACMProperties.setSecretKey("secretKey");
            Assertions.assertDoesNotThrow(() -> nacosListener.nacosConfigService(nacosProperties));
        }
    }

    @Test
    public void testPolarisDataChangedListener() {
        PolarisSyncConfiguration polarisListener = new PolarisSyncConfiguration();
        PolarisProperties polarisProperties = mock(PolarisProperties.class);
        ConfigFileService polarisConfigFileService = mock(ConfigFileService.class);
        ConfigFilePublishService polarisConfigFilePublishService = mock(ConfigFilePublishService.class);
        assertNotNull(polarisListener.polarisDataChangedListener(polarisProperties, polarisConfigFileService, polarisConfigFilePublishService));
    }

    @Test
    public void testPolarisDataInit() {
        PolarisSyncConfiguration polarisListener = new PolarisSyncConfiguration();
        PolarisProperties polarisProperties = mock(PolarisProperties.class);
        ConfigFileService polarisConfigFileService = mock(ConfigFileService.class);
        assertNotNull(polarisListener.polarisDataChangedInit(polarisProperties, polarisConfigFileService));
    }

    @Test
    public void polarisConfigServiceTest() {
        final PolarisProperties polarisProperties = new PolarisProperties();
        polarisProperties.setUrl("127.0.0.1:8093");
        polarisProperties.setNamespace("namespace");
        PolarisSyncConfiguration polarisListener = new PolarisSyncConfiguration();
        assertNotNull(polarisListener.polarisConfigFileService(polarisProperties));
        assertNotNull(polarisListener.polarisConfigFilePublishService(polarisProperties));
    }

    @Test
    public void testEtcdDataChangedListener() {
        EtcdSyncConfiguration etcdListener = new EtcdSyncConfiguration();
        EtcdClient client = mock(EtcdClient.class);
        assertNotNull(etcdListener.etcdDataChangedListener(client));
    }

    @Test
    public void testEtcdDataInit() {
        EtcdSyncConfiguration etcdListener = new EtcdSyncConfiguration();
        EtcdClient client = mock(EtcdClient.class);
        SyncDataService syncDataService = mock(SyncDataService.class);
        assertNotNull(etcdListener.etcdDataChangedInit(client));
    }

    @Test
    public void testConsulClient() {
        ConsulSyncConfiguration consulListener = new ConsulSyncConfiguration();
        ConsulProperties consulProperties = mock(ConsulProperties.class);
        when(consulProperties.getUrl()).thenReturn("http://127.0.0.1:8500");
        assertNotNull(consulListener.consulClient(consulProperties));
    }

    @Test
    public void testConsulDataChangedListener() {
        ConsulSyncConfiguration consulListener = new ConsulSyncConfiguration();
        ConsulClient consulClient = mock(ConsulClient.class);
        assertNotNull(consulListener.consulDataChangedListener(consulClient));
    }

    @Test
    public void testConsulDataInit() {
        ConsulSyncConfiguration consulListener = new ConsulSyncConfiguration();
        ConsulClient consulClient = mock(ConsulClient.class);
        SyncDataService syncDataService = mock(SyncDataService.class);
        assertNotNull(consulListener.consulDataChangedInit(consulClient));
    }

    @AfterEach
    public void after() {
        zkClient.close();
    }
}
