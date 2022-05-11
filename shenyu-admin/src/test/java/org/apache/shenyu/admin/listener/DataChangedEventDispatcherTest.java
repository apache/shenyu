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

import org.apache.shenyu.admin.listener.http.HttpLongPollingDataChangedListener;
import org.apache.shenyu.admin.listener.nacos.NacosDataChangedListener;
import org.apache.shenyu.admin.listener.websocket.WebsocketDataChangedListener;
import org.apache.shenyu.admin.listener.zookeeper.ZookeeperDataChangedListener;
import org.apache.shenyu.admin.service.manager.LoadServiceDocEntry;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ApplicationContext;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link DataChangedEventDispatcher}.
 */
@ExtendWith(MockitoExtension.class)
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

    @BeforeEach
    public void setUp() {
        Map<String, DataChangedListener> listenerMap = new HashMap<>();
        listenerMap.put("httpLongPollingDataChangedListener", httpLongPollingDataChangedListener);
        listenerMap.put("nacosDataChangedListener", nacosDataChangedListener);
        listenerMap.put("websocketDataChangedListener", websocketDataChangedListener);
        listenerMap.put("zookeeperDataChangedListener", zookeeperDataChangedListener);
        when(applicationContext.getBeansOfType(DataChangedListener.class)).thenReturn(listenerMap);

        when(applicationContext.getBean(LoadServiceDocEntry.class)).thenReturn(loadServiceDocEntry);
        applicationContext.getBean(LoadServiceDocEntry.class);
        dataChangedEventDispatcher.afterPropertiesSet();
    }

    /**
     * onApplicationEvent APP_AUTH configGroupEnum test case.
     */
    @Test
    public void onApplicationEventWithAppAuthConfigGroupTest() {
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
}
