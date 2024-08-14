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

package org.apache.shenyu.admin.discovery;

import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.DiscoveryHandlerMapper;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.model.dto.DiscoveryHandlerDTO;
import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.model.entity.DiscoveryHandlerDO;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.discovery.api.ShenyuDiscoveryService;
import org.apache.shenyu.discovery.api.config.DiscoveryConfig;
import org.apache.shenyu.discovery.api.listener.DataChangedEventListener;
import org.apache.shenyu.spi.ExtensionLoader;
import org.junit.Before;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ApplicationEventPublisher;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class DefaultDiscoveryProcessorTest {

    @InjectMocks
    private DefaultDiscoveryProcessor defaultDiscoveryProcessor;

    @Mock
    private DiscoveryUpstreamMapper discoveryUpstreamMapper;

    @Mock
    private DiscoveryHandlerMapper discoveryHandlerMapper;

    @Mock
    private ShenyuDiscoveryService shenyuDiscoveryService;

    @Mock
    private ApplicationEventPublisher eventPublisher;

    @Before
    public void setUp() {
        defaultDiscoveryProcessor = new DefaultDiscoveryProcessor(discoveryUpstreamMapper);
    }

    @BeforeEach
    public void testCreateDiscovery() {
        defaultDiscoveryProcessor.setApplicationEventPublisher(eventPublisher);
        DiscoveryDO discoveryDO = new DiscoveryDO();
        final String id = "id";
        final String type = "zookeeper";
        final String props = "{\"serverList\":\"localhost:2181\"}";
        Map<String, String> map = new HashMap<>();
        map.put("serverList", "localhost:2181");
        Properties properties = new Properties();
        properties.putAll(map);
        discoveryDO.setId(id);
        discoveryDO.setType(type);
        discoveryDO.setProps(props);
        discoveryDO.setServerList("localhost:2181");
        MockedStatic<ExtensionLoader> mocked = mockStatic(ExtensionLoader.class);
        ExtensionLoader extensionLoader = mock(ExtensionLoader.class);
        mocked.when(() -> ExtensionLoader.getExtensionLoader(ShenyuDiscoveryService.class)).thenReturn(extensionLoader);
        when(extensionLoader.getJoin(anyString())).thenReturn(shenyuDiscoveryService);
        doNothing().when(shenyuDiscoveryService).init(any(DiscoveryConfig.class));
        defaultDiscoveryProcessor.createDiscovery(discoveryDO);
        verify(ExtensionLoader.getExtensionLoader(ShenyuDiscoveryService.class), times(1)).getJoin(type);
        try {
            final Field field = defaultDiscoveryProcessor.getClass().getSuperclass().getDeclaredField(
                    "discoveryServiceCache");
            field.setAccessible(true);
            Map<String, ShenyuDiscoveryService> actual = (Map<String, ShenyuDiscoveryService>) field.get(defaultDiscoveryProcessor);
            Map<String, ShenyuDiscoveryService> expected = new HashMap<>();
            expected.put(id, shenyuDiscoveryService);
            assertEquals(expected, actual);

        } catch (NoSuchFieldException | IllegalAccessException e) {
            throw new RuntimeException(e);
        }
        try {
            final Field field = defaultDiscoveryProcessor.getClass().getSuperclass().getDeclaredField(
                    "dataChangedEventListenerCache");
            field.setAccessible(true);
            Map<String, Set> actual = (Map<String, Set>) field.get(defaultDiscoveryProcessor);
            Map<String, Set> expected = new HashMap<>();
            expected.put(id, new HashSet<>());
            assertEquals(expected, actual);

        } catch (NoSuchFieldException | IllegalAccessException e) {
            throw new RuntimeException(e);
        }
        mocked.close();
    }

    @Test
    public void testCreateProxySelector() {
        DiscoveryHandlerDTO discoveryHandlerDTO = new DiscoveryHandlerDTO();
        ProxySelectorDTO proxySelectorDTO = new ProxySelectorDTO();
        Assertions.assertThrows(NullPointerException.class, () -> defaultDiscoveryProcessor.createProxySelector(discoveryHandlerDTO, proxySelectorDTO));
        discoveryHandlerDTO.setDiscoveryId("id");

        Assertions.assertThrows(ShenyuAdminException.class, () -> defaultDiscoveryProcessor.createProxySelector(discoveryHandlerDTO, proxySelectorDTO));
        when(shenyuDiscoveryService.exists(anyString())).thenReturn(true);
        doNothing().when(shenyuDiscoveryService).watch(anyString(), any(DataChangedEventListener.class));
        doNothing().when(eventPublisher).publishEvent(any(Object.class));
        defaultDiscoveryProcessor.createProxySelector(discoveryHandlerDTO, proxySelectorDTO);
        verify(eventPublisher).publishEvent(any(DataChangedEvent.class));
    }

    @Test
    public void testChangeUpstream() {
        doNothing().when(eventPublisher).publishEvent(any(DataChangedEvent.class));
        defaultDiscoveryProcessor.changeUpstream(new ProxySelectorDTO(), new ArrayList<>());
        verify(eventPublisher).publishEvent(any(DataChangedEvent.class));
    }

    @Test
    public void testFetchAll() {
        DiscoveryHandlerDO discoveryHandlerDO = new DiscoveryHandlerDO();
        discoveryHandlerDO.setDiscoveryId("id");
        when(discoveryHandlerMapper.selectById(anyString())).thenReturn(discoveryHandlerDO);

        List<DiscoveryUpstreamDO> discoveryUpstreamDOS = new ArrayList<>();
        DiscoveryUpstreamDO discoveryUpstreamDO = new DiscoveryUpstreamDO();
//        List<String> childData =new ArrayList<>();

        discoveryUpstreamDOS.add(discoveryUpstreamDO);
//        childData.add("1");
        when(discoveryUpstreamMapper.selectByDiscoveryHandlerId(anyString())).thenReturn(discoveryUpstreamDOS);
//        when(shenyuDiscoveryService.getRegisterData(anyString())).thenReturn(childData);
//        when(discoveryUpstreamMapper.insert(any(DiscoveryUpstreamDO.class))).thenReturn(1);
        doNothing().when(eventPublisher).publishEvent(any(Object.class));
        DiscoveryHandlerDTO discoveryHandlerDTO = new DiscoveryHandlerDTO();
        discoveryHandlerDTO.setId("1");
        discoveryHandlerDTO.setDiscoveryId("id");
        ProxySelectorDTO proxySelectorDTO = new ProxySelectorDTO();
        defaultDiscoveryProcessor.fetchAll(discoveryHandlerDTO, proxySelectorDTO);
        verify(eventPublisher).publishEvent(any(DataChangedEvent.class));
    }

    @Test
    public void testRemoveProxySelector() {
        DiscoveryHandlerDTO discoveryHandlerDTO = new DiscoveryHandlerDTO();
        final ProxySelectorDTO proxySelectorDTO = new ProxySelectorDTO();
        discoveryHandlerDTO.setDiscoveryId("id");
        doNothing().when(shenyuDiscoveryService).unwatch(anyString());
        doNothing().when(eventPublisher).publishEvent(any(Object.class));
        defaultDiscoveryProcessor.removeProxySelector(discoveryHandlerDTO, proxySelectorDTO);
        verify(eventPublisher).publishEvent(any(DataChangedEvent.class));
    }

    @Test
    public void testRemoveDiscovery() {
        DiscoveryDO discoveryDO = new DiscoveryDO();
        discoveryDO.setId("id");
        doNothing().when(shenyuDiscoveryService).shutdown();
        defaultDiscoveryProcessor.removeDiscovery(discoveryDO);
        verify(shenyuDiscoveryService).shutdown();

    }

}
