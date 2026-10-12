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
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.model.dto.DiscoveryHandlerDTO;
import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.registry.api.config.RegisterConfig;
import org.apache.shenyu.registry.api.entity.InstanceEntity;
import org.apache.shenyu.registry.api.event.ChangedEventListener;
import org.apache.shenyu.spi.ExtensionLoader;
import org.junit.Before;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ApplicationEventPublisher;

import java.lang.reflect.Field;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
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
    private ShenyuInstanceRegisterRepository shenyuDiscoveryService;

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
        discoveryDO.setDiscoveryType(type);
        discoveryDO.setProps(props);
        discoveryDO.setServerList("localhost:2181");
        MockedStatic<ExtensionLoader> mocked = mockStatic(ExtensionLoader.class);
        ExtensionLoader extensionLoader = mock(ExtensionLoader.class);
        mocked.when(() -> ExtensionLoader.getExtensionLoader(ShenyuInstanceRegisterRepository.class)).thenReturn(extensionLoader);
        when(extensionLoader.getJoin(anyString())).thenReturn(shenyuDiscoveryService);
        doNothing().when(shenyuDiscoveryService).init(any(RegisterConfig.class));
        defaultDiscoveryProcessor.createDiscovery(discoveryDO);
        verify(ExtensionLoader.getExtensionLoader(ShenyuInstanceRegisterRepository.class), times(1)).getJoin(type);
        try {
            final Field field = defaultDiscoveryProcessor.getClass().getSuperclass().getDeclaredField(
                    "discoveryServiceCache");
            field.setAccessible(true);
            Map<String, ShenyuInstanceRegisterRepository> actual = (Map<String, ShenyuInstanceRegisterRepository>) field.get(defaultDiscoveryProcessor);
            Map<String, ShenyuInstanceRegisterRepository> expected = new HashMap<>();
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
        doNothing().when(shenyuDiscoveryService).watchInstances(anyString(), any(ChangedEventListener.class));
        doNothing().when(eventPublisher).publishEvent(any(Object.class));
    }

    @Test
    public void testChangeUpstream() {
        doNothing().when(eventPublisher).publishEvent(any(DataChangedEvent.class));
        defaultDiscoveryProcessor.changeUpstream(new ProxySelectorDTO(), new ArrayList<>());
        verify(eventPublisher).publishEvent(any(DataChangedEvent.class));
    }

    @Test
    public void testFetchAllPublishesEmptyListAfterRemovingMissingUpstreams() {
        final DiscoveryUpstreamDO removed = DiscoveryUpstreamDO.builder().id("removed").url("10.0.0.2:8080").build();
        when(shenyuDiscoveryService.selectInstances(anyString())).thenReturn(Collections.emptyList());
        when(discoveryUpstreamMapper.selectByDiscoveryHandlerId("handler-id"))
                .thenReturn(Collections.singletonList(removed), Collections.emptyList());
        final DiscoveryHandlerDTO handler = new DiscoveryHandlerDTO();
        handler.setId("handler-id");
        handler.setDiscoveryId("id");

        defaultDiscoveryProcessor.fetchAll(handler, new ProxySelectorDTO());

        verify(discoveryUpstreamMapper).deleteByIds(Collections.singletonList("removed"));
        verify(discoveryUpstreamMapper, never()).insert(any(DiscoveryUpstreamDO.class));
        final ArgumentCaptor<DataChangedEvent> eventCaptor = ArgumentCaptor.forClass(DataChangedEvent.class);
        verify(eventPublisher).publishEvent(eventCaptor.capture());
        final DiscoverySyncData published = (DiscoverySyncData) ((List<?>) eventCaptor.getValue().getSource()).get(0);
        assertTrue(published.getUpstreamDataList().isEmpty());
    }

    @Test
    public void testFetchAllRetainsWatchIpAddressAndConfiguration() {
        final InstanceEntity selected = InstanceEntity.builder().appName("SHENYU-INSTANCES").host("10.0.0.1").port(8080).build();
        when(shenyuDiscoveryService.selectInstances(anyString())).thenReturn(Collections.singletonList(selected));
        final DiscoveryUpstreamDO watched = DiscoveryUpstreamDO.builder().id("watch-row").discoveryHandlerId("handler-id")
                .url("10.0.0.1:8080").protocol("https://").weight(20).props("{\"warmup\":\"30\",\"az\":\"east\"}")
                .namespaceId("namespace-id").dateCreated(new Timestamp(123)).dateUpdated(new Timestamp(456)).build();
        when(discoveryUpstreamMapper.selectByDiscoveryHandlerId("handler-id")).thenReturn(Collections.singletonList(watched));
        final DiscoveryHandlerDTO handler = new DiscoveryHandlerDTO();
        handler.setId("handler-id");
        handler.setDiscoveryId("id");
        handler.setListenerNode("/SHENYU-INSTANCES");
        final ProxySelectorDTO selector = new ProxySelectorDTO();
        selector.setId("selector-id");
        selector.setNamespaceId("namespace-id");

        defaultDiscoveryProcessor.fetchAll(handler, selector);

        verify(discoveryUpstreamMapper, never()).deleteByIds(anyList());
        verify(discoveryUpstreamMapper, never()).insert(any(DiscoveryUpstreamDO.class));
        final ArgumentCaptor<DataChangedEvent> eventCaptor = ArgumentCaptor.forClass(DataChangedEvent.class);
        verify(eventPublisher).publishEvent(eventCaptor.capture());
        final DiscoverySyncData published = (DiscoverySyncData) ((List<?>) eventCaptor.getValue().getSource()).get(0);
        final DiscoveryUpstreamData upstream = published.getUpstreamDataList().get(0);
        assertEquals(watched.getUpstreamUrl(), upstream.getUrl());
        assertEquals(watched.getProtocol(), upstream.getProtocol());
        assertEquals(watched.getWeight(), upstream.getWeight());
        assertEquals(watched.getProps(), upstream.getProps());
        assertEquals(watched.getNamespaceId(), upstream.getNamespaceId());
    }

    @Test
    public void testFetchAllPublishesReconciledRecordsWithNewInstanceDefaults() {
        final InstanceEntity retained = InstanceEntity.builder().host("10.0.0.1").port(8080).build();
        final InstanceEntity added = InstanceEntity.builder().host("10.0.0.3").port(8080).build();
        added.setWeight(12);
        when(shenyuDiscoveryService.selectInstances(anyString())).thenReturn(List.of(retained, added));
        final DiscoveryUpstreamDO existing = DiscoveryUpstreamDO.builder().id("retained").url("10.0.0.1:8080").status(1).build();
        final DiscoveryUpstreamDO removed = DiscoveryUpstreamDO.builder().id("removed").url("10.0.0.2:8080").build();
        final List<DiscoveryUpstreamDO> reconciled = new ArrayList<>(List.of(existing));
        when(discoveryUpstreamMapper.selectByDiscoveryHandlerId("handler-id")).thenReturn(List.of(existing, removed), reconciled);
        when(discoveryUpstreamMapper.insert(any(DiscoveryUpstreamDO.class))).thenAnswer(invocation -> {
            reconciled.add(invocation.getArgument(0));
            return 1;
        });
        final DiscoveryHandlerDTO handler = new DiscoveryHandlerDTO();
        handler.setId("handler-id");
        handler.setDiscoveryId("id");
        final ProxySelectorDTO selector = new ProxySelectorDTO();
        selector.setId("selector-id");
        selector.setNamespaceId("namespace-id");

        defaultDiscoveryProcessor.fetchAll(handler, selector);

        verify(discoveryUpstreamMapper).deleteByIds(Collections.singletonList("removed"));
        verify(discoveryUpstreamMapper).insert(any(DiscoveryUpstreamDO.class));
        final ArgumentCaptor<DataChangedEvent> eventCaptor = ArgumentCaptor.forClass(DataChangedEvent.class);
        verify(eventPublisher).publishEvent(eventCaptor.capture());
        final DiscoverySyncData published = (DiscoverySyncData) ((List<?>) eventCaptor.getValue().getSource()).get(0);
        final List<DiscoveryUpstreamData> upstreams = published.getUpstreamDataList();
        assertEquals(List.of("10.0.0.1:8080", "10.0.0.3:8080"), upstreams.stream().map(DiscoveryUpstreamData::getUrl).toList());
        assertEquals(1, upstreams.get(0).getStatus());
        final DiscoveryUpstreamData newUpstream = upstreams.get(1);
        assertEquals("http://", newUpstream.getProtocol());
        assertEquals(12, newUpstream.getWeight());
        assertEquals("handler-id", newUpstream.getDiscoveryHandlerId());
        assertEquals("namespace-id", newUpstream.getNamespaceId());
        assertNotNull(newUpstream.getId());
        assertNotNull(newUpstream.getDateCreated());
    }

    @Test
    public void testRemoveProxySelector() {
        DiscoveryHandlerDTO discoveryHandlerDTO = new DiscoveryHandlerDTO();
        final ProxySelectorDTO proxySelectorDTO = new ProxySelectorDTO();
        discoveryHandlerDTO.setDiscoveryId("id");
        doNothing().when(shenyuDiscoveryService).unWatchInstances(anyString());
        doNothing().when(eventPublisher).publishEvent(any(Object.class));
        defaultDiscoveryProcessor.removeProxySelector(discoveryHandlerDTO, proxySelectorDTO);
        verify(eventPublisher).publishEvent(any(DataChangedEvent.class));
    }

    @Test
    public void testRemoveDiscovery() {
        DiscoveryDO discoveryDO = new DiscoveryDO();
        discoveryDO.setId("id");
        doNothing().when(shenyuDiscoveryService).close();
        defaultDiscoveryProcessor.removeDiscovery(discoveryDO);
        verify(shenyuDiscoveryService).close();

    }

    @Test
    @SuppressWarnings("unchecked")
    public void testRemoveProxySelectorAfterDiscoveryRemoved() throws NoSuchFieldException, IllegalAccessException {
        defaultDiscoveryProcessor.setApplicationEventPublisher(eventPublisher);
        // simulate removeDiscovery: it drops only the discoveryServiceCache entry and leaves the
        // listener-cache key registered by createProxySelector behind
        final Field serviceCacheField = defaultDiscoveryProcessor.getClass().getSuperclass().getDeclaredField("discoveryServiceCache");
        serviceCacheField.setAccessible(true);
        Map<String, ShenyuInstanceRegisterRepository> serviceCache = (Map<String, ShenyuInstanceRegisterRepository>) serviceCacheField.get(defaultDiscoveryProcessor);
        serviceCache.remove("id");
        final Field listenerCacheField = defaultDiscoveryProcessor.getClass().getSuperclass().getDeclaredField("dataChangedEventListenerCache");
        listenerCacheField.setAccessible(true);
        Map<String, Set<String>> listenerCache = (Map<String, Set<String>>) listenerCacheField.get(defaultDiscoveryProcessor);
        listenerCache.put("id", new HashSet<>(Collections.singleton("/shenyu/discovery")));

        doNothing().when(eventPublisher).publishEvent(any(DataChangedEvent.class));
        DiscoveryHandlerDTO discoveryHandlerDTO = new DiscoveryHandlerDTO();
        discoveryHandlerDTO.setDiscoveryId("id");
        discoveryHandlerDTO.setListenerNode("/shenyu/discovery");
        defaultDiscoveryProcessor.removeProxySelector(discoveryHandlerDTO, new ProxySelectorDTO());
        verify(eventPublisher).publishEvent(any(DataChangedEvent.class));
        // the emptied set must not linger as a tombstone entry
        assertTrue(listenerCache.isEmpty());
        // and re-registering the selector must get a usable set again instead of an NPE
        assertFalse(defaultDiscoveryProcessor.getCacheKey("id").contains("/shenyu/discovery"));
    }

}
