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

package org.apache.shenyu.admin.listener;

import org.apache.shenyu.admin.config.properties.ClusterProperties;
import org.apache.shenyu.admin.listener.http.HttpLongPollingDataChangedListener;
import org.apache.shenyu.admin.listener.nacos.NacosDataChangedListener;
import org.apache.shenyu.admin.listener.websocket.WebsocketDataChangedListener;
import org.apache.shenyu.admin.listener.zookeeper.ZookeeperDataChangedListener;
import org.apache.shenyu.admin.mode.cluster.service.ClusterSelectMasterService;
import org.apache.shenyu.admin.service.manager.LoadServiceDocEntry;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ApplicationContext;
import org.springframework.test.util.ReflectionTestUtils;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link DataChangedEventDispatcher}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class DataChangedEventDispatcherTest {

    @InjectMocks
    private DataChangedEventDispatcher dataChangedEventDispatcher;

    @Mock
    private ApplicationContext applicationContext;

    @Mock
    private HttpLongPollingDataChangedListener httpLongPollingDataChangedListener;

    @Mock
    private NacosDataChangedListener nacosDataChangedListener;

    @Mock
    private WebsocketDataChangedListener websocketDataChangedListener;

    @Mock
    private ZookeeperDataChangedListener zookeeperDataChangedListener;

    @Mock
    private LoadServiceDocEntry loadServiceDocEntry;
    
    @Mock
    private ClusterProperties clusterProperties;
    
    @Mock
    private ClusterSelectMasterService shenyuClusterSelectMasterService;

    @BeforeEach
    public void setUp() throws NoSuchFieldException, IllegalAccessException {
        Map<String, DataChangedListener> listenerMap = new HashMap<>();
        listenerMap.put("httpLongPollingDataChangedListener", httpLongPollingDataChangedListener);
        listenerMap.put("nacosDataChangedListener", nacosDataChangedListener);
        listenerMap.put("websocketDataChangedListener", websocketDataChangedListener);
        listenerMap.put("zookeeperDataChangedListener", zookeeperDataChangedListener);
        when(applicationContext.getBeansOfType(DataChangedListener.class)).thenReturn(listenerMap);

        when(applicationContext.getBean(LoadServiceDocEntry.class)).thenReturn(loadServiceDocEntry);
        applicationContext.getBean(LoadServiceDocEntry.class);
        
        Field shenyuClusterSelectMasterServiceField = DataChangedEventDispatcher.class.getDeclaredField("shenyuClusterSelectMasterService");
        shenyuClusterSelectMasterServiceField.setAccessible(true);
        shenyuClusterSelectMasterServiceField.set(dataChangedEventDispatcher, shenyuClusterSelectMasterService);
        shenyuClusterSelectMasterServiceField.setAccessible(false);
        
        Field clusterPropertiesField = DataChangedEventDispatcher.class.getDeclaredField("clusterProperties");
        clusterPropertiesField.setAccessible(true);
        clusterPropertiesField.set(dataChangedEventDispatcher, clusterProperties);
        clusterPropertiesField.setAccessible(false);
        
        dataChangedEventDispatcher.afterPropertiesSet();
    }

    /**
     * onApplicationEvent APP_AUTH configGroupEnum test case.
     */
    @Test
    public void onApplicationEventWithAppAuthConfigGroupTest() {
        when(clusterProperties.isEnabled()).thenReturn(true);
        when(shenyuClusterSelectMasterService.isMaster()).thenReturn(true);
        ConfigGroupEnum configGroupEnum = ConfigGroupEnum.APP_AUTH;
        DataChangedEvent dataChangedEvent = new DataChangedEvent(configGroupEnum, null, new ArrayList<>());
        dataChangedEventDispatcher.onApplicationEvent(dataChangedEvent);
        verify(httpLongPollingDataChangedListener, times(1)).onAppAuthChanged(anyList(), any());
        verify(nacosDataChangedListener, times(1)).onAppAuthChanged(anyList(), any());
        verify(websocketDataChangedListener, times(1)).onAppAuthChanged(anyList(), any());
        verify(zookeeperDataChangedListener, times(1)).onAppAuthChanged(anyList(), any());
    }

    /**
     * onApplicationEvent PLUGIN configGroupEnum test case.
     */
    @Test
    public void onApplicationEventWithPluginConfigGroupTest() {
        when(clusterProperties.isEnabled()).thenReturn(true);
        when(shenyuClusterSelectMasterService.isMaster()).thenReturn(true);
        ConfigGroupEnum configGroupEnum = ConfigGroupEnum.PLUGIN;
        DataChangedEvent dataChangedEvent = new DataChangedEvent(configGroupEnum, null, new ArrayList<>());
        dataChangedEventDispatcher.onApplicationEvent(dataChangedEvent);
        verify(httpLongPollingDataChangedListener, times(1)).onPluginChanged(anyList(), any());
        verify(nacosDataChangedListener, times(1)).onPluginChanged(anyList(), any());
        verify(websocketDataChangedListener, times(1)).onPluginChanged(anyList(), any());
        verify(zookeeperDataChangedListener, times(1)).onPluginChanged(anyList(), any());
    }

    /**
     * onApplicationEvent RULE configGroupEnum test case.
     */
    @Test
    public void onApplicationEventWithRuleConfigGroupTest() {
        when(clusterProperties.isEnabled()).thenReturn(true);
        when(shenyuClusterSelectMasterService.isMaster()).thenReturn(true);
        ConfigGroupEnum configGroupEnum = ConfigGroupEnum.RULE;
        DataChangedEvent dataChangedEvent = new DataChangedEvent(configGroupEnum, null, new ArrayList<>());
        dataChangedEventDispatcher.onApplicationEvent(dataChangedEvent);
        verify(httpLongPollingDataChangedListener, times(1)).onRuleChanged(anyList(), any());
        verify(nacosDataChangedListener, times(1)).onRuleChanged(anyList(), any());
        verify(websocketDataChangedListener, times(1)).onRuleChanged(anyList(), any());
        verify(zookeeperDataChangedListener, times(1)).onRuleChanged(anyList(), any());
    }

    /**
     * onApplicationEvent SELECTOR configGroupEnum test case.
     */
    @Test
    public void onApplicationEventWithSelectorConfigGroupTest() {
        when(clusterProperties.isEnabled()).thenReturn(true);
        when(shenyuClusterSelectMasterService.isMaster()).thenReturn(true);
        ConfigGroupEnum configGroupEnum = ConfigGroupEnum.SELECTOR;
        DataChangedEvent dataChangedEvent = new DataChangedEvent(configGroupEnum, null, new ArrayList<>());
        dataChangedEventDispatcher.onApplicationEvent(dataChangedEvent);
        verify(httpLongPollingDataChangedListener, times(1)).onSelectorChanged(anyList(), any());
        verify(nacosDataChangedListener, times(1)).onSelectorChanged(anyList(), any());
        verify(websocketDataChangedListener, times(1)).onSelectorChanged(anyList(), any());
        verify(zookeeperDataChangedListener, times(1)).onSelectorChanged(anyList(), any());
    }

    /**
     * onApplicationEvent META_DATA configGroupEnum test case.
     */
    @Test
    public void onApplicationEventWithMetaDataConfigGroupTest() {
        when(clusterProperties.isEnabled()).thenReturn(true);
        when(shenyuClusterSelectMasterService.isMaster()).thenReturn(true);
        ConfigGroupEnum configGroupEnum = ConfigGroupEnum.META_DATA;
        DataChangedEvent dataChangedEvent = new DataChangedEvent(configGroupEnum, null, new ArrayList<>());
        dataChangedEventDispatcher.onApplicationEvent(dataChangedEvent);
        verify(httpLongPollingDataChangedListener, times(1)).onMetaDataChanged(anyList(), any());
        verify(nacosDataChangedListener, times(1)).onMetaDataChanged(anyList(), any());
        verify(websocketDataChangedListener, times(1)).onMetaDataChanged(anyList(), any());
        verify(zookeeperDataChangedListener, times(1)).onMetaDataChanged(anyList(), any());
    }

    /**
     * onApplicationEvent null configGroupEnum test case.
     */
    @Test
    public void onApplicationEventWithNullTest() {
        when(clusterProperties.isEnabled()).thenReturn(true);
        when(shenyuClusterSelectMasterService.isMaster()).thenReturn(true);
        NullPointerException exception = assertThrows(NullPointerException.class, () -> {
            DataChangedEvent dataChangedEvent = new DataChangedEvent(null, null, new ArrayList<>());
            dataChangedEventDispatcher.onApplicationEvent(dataChangedEvent);
        });
        assertNotNull(exception);
    }

    /**
     * afterPropertiesSet listener init check test case.
     */
    @Test
    @SuppressWarnings("unchecked")
    public void afterPropertiesSetTest() {
        List<DataChangedListener> listeners = (List<DataChangedListener>) ReflectionTestUtils.getField(dataChangedEventDispatcher, "listeners");
        assertTrue(listeners.contains(httpLongPollingDataChangedListener));
        assertTrue(listeners.contains(nacosDataChangedListener));
        assertTrue(listeners.contains(websocketDataChangedListener));
        assertTrue(listeners.contains(zookeeperDataChangedListener));
    }

    /**
     * onApplicationEvent PROXY_SELECTOR configGroupEnum test case.
     */
    @Test
    void onApplicationEventWithProxySelectorConfigGroupTest() {
        when(clusterProperties.isEnabled()).thenReturn(true);
        when(shenyuClusterSelectMasterService.isMaster()).thenReturn(true);
        DataChangedEvent dataChangedEvent = new DataChangedEvent(ConfigGroupEnum.PROXY_SELECTOR, null, new ArrayList<>());
        dataChangedEventDispatcher.onApplicationEvent(dataChangedEvent);
        verify(httpLongPollingDataChangedListener, times(1)).onProxySelectorChanged(anyList(), any());
        verify(nacosDataChangedListener, times(1)).onProxySelectorChanged(anyList(), any());
        verify(websocketDataChangedListener, times(1)).onProxySelectorChanged(anyList(), any());
        verify(zookeeperDataChangedListener, times(1)).onProxySelectorChanged(anyList(), any());
    }

    /**
     * onApplicationEvent AI_PROXY_API_KEY configGroupEnum test case.
     */
    @Test
    void onApplicationEventWithAiProxyApiKeyConfigGroupTest() {
        when(clusterProperties.isEnabled()).thenReturn(true);
        when(shenyuClusterSelectMasterService.isMaster()).thenReturn(true);
        DataChangedEvent dataChangedEvent = new DataChangedEvent(ConfigGroupEnum.AI_PROXY_API_KEY, null, new ArrayList<>());
        dataChangedEventDispatcher.onApplicationEvent(dataChangedEvent);
        verify(httpLongPollingDataChangedListener, times(1)).onAiProxyApiKeyChanged(anyList(), any());
        verify(nacosDataChangedListener, times(1)).onAiProxyApiKeyChanged(anyList(), any());
        verify(websocketDataChangedListener, times(1)).onAiProxyApiKeyChanged(anyList(), any());
        verify(zookeeperDataChangedListener, times(1)).onAiProxyApiKeyChanged(anyList(), any());
    }

    /**
     * onApplicationEvent DISCOVER_UPSTREAM configGroupEnum test case — also triggers loadDocOnUpstreamChanged.
     */
    @Test
    void onApplicationEventWithDiscoverUpstreamConfigGroupTest() {
        when(clusterProperties.isEnabled()).thenReturn(true);
        when(shenyuClusterSelectMasterService.isMaster()).thenReturn(true);
        DataChangedEvent dataChangedEvent = new DataChangedEvent(ConfigGroupEnum.DISCOVER_UPSTREAM, null, new ArrayList<>());
        dataChangedEventDispatcher.onApplicationEvent(dataChangedEvent);
        verify(httpLongPollingDataChangedListener, times(1)).onDiscoveryUpstreamChanged(anyList(), any());
        verify(nacosDataChangedListener, times(1)).onDiscoveryUpstreamChanged(anyList(), any());
        verify(websocketDataChangedListener, times(1)).onDiscoveryUpstreamChanged(anyList(), any());
        verify(zookeeperDataChangedListener, times(1)).onDiscoveryUpstreamChanged(anyList(), any());
        verify(loadServiceDocEntry, atLeastOnce()).loadDocOnUpstreamChanged(anyList(), any());
    }

    /**
     * When cluster is disabled, all listeners receive the event regardless of master status.
     */
    @Test
    void onApplicationEventClusterDisabledAllListenersNotifiedTest() {
        when(clusterProperties.isEnabled()).thenReturn(false);
        DataChangedEvent dataChangedEvent = new DataChangedEvent(ConfigGroupEnum.PLUGIN, null, new ArrayList<>());
        dataChangedEventDispatcher.onApplicationEvent(dataChangedEvent);
        verify(httpLongPollingDataChangedListener, times(1)).onPluginChanged(anyList(), any());
        verify(nacosDataChangedListener, times(1)).onPluginChanged(anyList(), any());
        verify(websocketDataChangedListener, times(1)).onPluginChanged(anyList(), any());
        verify(zookeeperDataChangedListener, times(1)).onPluginChanged(anyList(), any());
    }

    /**
     * When cluster enabled and not master, non-AbstractDataChangedListener is skipped.
     */
    @Test
    void onApplicationEventNotMasterSkipsNonAbstractListenersTest() {
        when(clusterProperties.isEnabled()).thenReturn(true);
        when(shenyuClusterSelectMasterService.isMaster()).thenReturn(false);
        List<DataChangedListener> orderedListeners = new ArrayList<>();
        orderedListeners.add(nacosDataChangedListener);
        ReflectionTestUtils.setField(dataChangedEventDispatcher, "listeners", Collections.unmodifiableList(orderedListeners));
        DataChangedEvent dataChangedEvent = new DataChangedEvent(ConfigGroupEnum.PLUGIN, null, new ArrayList<>());
        dataChangedEventDispatcher.onApplicationEvent(dataChangedEvent);
        verify(nacosDataChangedListener, never()).onPluginChanged(anyList(), any());
    }

    /**
     * When shenyuClusterSelectMasterService is null, all listeners still receive the event.
     */
    @Test
    void onApplicationEventNullMasterServiceDispatchesAllTest() throws NoSuchFieldException, IllegalAccessException {
        when(clusterProperties.isEnabled()).thenReturn(true);
        Field field = DataChangedEventDispatcher.class.getDeclaredField("shenyuClusterSelectMasterService");
        field.setAccessible(true);
        field.set(dataChangedEventDispatcher, null);
        DataChangedEvent dataChangedEvent = new DataChangedEvent(ConfigGroupEnum.PLUGIN, null, new ArrayList<>());
        dataChangedEventDispatcher.onApplicationEvent(dataChangedEvent);
        verify(nacosDataChangedListener, times(1)).onPluginChanged(anyList(), any());
        verify(httpLongPollingDataChangedListener, times(1)).onPluginChanged(anyList(), any());
        verify(websocketDataChangedListener, times(1)).onPluginChanged(anyList(), any());
        verify(zookeeperDataChangedListener, times(1)).onPluginChanged(anyList(), any());
    }
}
