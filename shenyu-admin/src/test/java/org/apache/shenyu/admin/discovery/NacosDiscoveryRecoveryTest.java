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

import com.alibaba.nacos.api.exception.NacosException;
import com.alibaba.nacos.api.naming.NamingFactory;
import com.alibaba.nacos.api.naming.NamingService;
import com.alibaba.nacos.api.naming.listener.EventListener;
import com.alibaba.nacos.api.naming.listener.NamingEvent;
import com.alibaba.nacos.api.naming.pojo.Instance;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.DiscoveryHandlerMapper;
import org.apache.shenyu.admin.mapper.DiscoveryMapper;
import org.apache.shenyu.admin.mapper.DiscoveryRelMapper;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.mapper.ProxySelectorMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.DiscoveryHandlerDTO;
import org.apache.shenyu.admin.model.dto.ProxySelectorDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.model.entity.DiscoveryHandlerDO;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.admin.model.entity.ProxySelectorDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.service.impl.DiscoveryServiceImpl;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.transaction.PlatformTransactionManager;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Exercises startup synchronization and recovery through the real Nacos registry provider.
 */
@ExtendWith(MockitoExtension.class)
class NacosDiscoveryRecoveryTest {

    private static final String SERVICE = "shenyu-examples-http";

    private static final String GROUP = "DEFAULT_GROUP";

    private static final String NAMESPACE = "recovery-namespace";

    @Mock
    private NamingService namingService;

    @Mock
    private DiscoveryUpstreamMapper upstreamMapper;

    @Mock
    private ApplicationEventPublisher publisher;

    private DefaultDiscoveryProcessor processor;

    private DiscoveryDO discovery;

    private MockedStatic<NamingFactory> namingFactory;

    private final List<Instance> instances = new ArrayList<>();

    private final List<DataChangedEvent> events = new ArrayList<>();

    @BeforeEach
    void setUp() throws NacosException {
        when(namingService.selectInstances(SERVICE, GROUP, true)).thenAnswer(invocation -> new ArrayList<>(instances));
        doAnswer(invocation -> events.add(invocation.getArgument(0))).when(publisher).publishEvent(any(DataChangedEvent.class));
        processor = new DefaultDiscoveryProcessor(upstreamMapper);
        processor.setApplicationEventPublisher(publisher);
        discovery = DiscoveryDO.builder().id("discovery-id").discoveryType("nacos")
                .pluginName("divide").namespaceId(NAMESPACE).serverList("127.0.0.1:8848").props("{}").build();
        namingFactory = mockStatic(NamingFactory.class);
        namingFactory.when(() -> NamingFactory.createNamingService(any(Properties.class))).thenReturn(namingService);
        processor.createDiscovery(discovery);
    }

    @AfterEach
    void tearDown() {
        try {
            processor.removeDiscovery(discovery);
        } finally {
            namingFactory.close();
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void testRestoreOfflineServiceAndProcessLaterChanges(final boolean proxySelector) throws NacosException {
        final List<DiscoveryUpstreamDO> rows = mockUpstreamStorage();
        DiscoveryMapper discoveryMapper = mock(DiscoveryMapper.class);
        when(discoveryMapper.selectAll()).thenReturn(Collections.singletonList(discovery));
        when(discoveryMapper.selectAllByNamespaceId(NAMESPACE)).thenReturn(Collections.singletonList(discovery));
        DiscoveryProcessorHolder holder = mock(DiscoveryProcessorHolder.class);
        when(holder.chooseProcessor("nacos")).thenReturn(processor);
        ProxySelectorMapper proxyMapper = mock(ProxySelectorMapper.class);
        SelectorMapper selectorMapper = mock(SelectorMapper.class);
        DiscoveryHandlerMapper handlerMapper = mock(DiscoveryHandlerMapper.class);
        final DiscoveryHandlerDO handler = DiscoveryHandlerDO.builder().id("handler-id").discoveryId(discovery.getId())
                .listenerNode(SERVICE).handler("{}").build();
        if (proxySelector) {
            when(proxyMapper.selectByDiscoveryId(discovery.getId())).thenReturn(Collections.singletonList(
                    ProxySelectorDO.builder().id("selector-id").name("recovery-selector").pluginName("divide").namespaceId(NAMESPACE).build()));
            when(handlerMapper.selectByProxySelectorId("selector-id")).thenReturn(handler);
        } else {
            when(selectorMapper.selectByDiscoveryId(discovery.getId())).thenReturn(Collections.singletonList(
                    SelectorDO.builder().id("selector-id").selectorName("recovery-selector").namespaceId(NAMESPACE).build()));
            when(handlerMapper.selectBySelectorId("selector-id")).thenReturn(handler);
        }
        DiscoveryServiceImpl service = new DiscoveryServiceImpl(discoveryMapper, proxyMapper, mock(DiscoveryRelMapper.class),
                handlerMapper, mock(SelectorService.class), selectorMapper, holder, mock(PlatformTransactionManager.class));

        assertFalse(processor.getShenyuDiscoveryService(discovery.getId()).serviceExists(SERVICE));
        assertDoesNotThrow(() -> service.syncData());
        assertTrue(rows.isEmpty(), "The initial empty snapshot must remove stale upstreams");
        assertTrue(lastSnapshot().getUpstreamDataList().isEmpty());
        assertDoesNotThrow(() -> service.syncDataByNamespaceId(NAMESPACE));
        ArgumentCaptor<EventListener> listener = ArgumentCaptor.forClass(EventListener.class);
        verify(namingService).subscribe(eq(SERVICE), eq(GROUP), listener.capture());
        assertTrue(processor.getCacheKey(discovery.getId()).contains(SERVICE));

        Instance instance = new Instance();
        instance.setIp("127.0.0.1");
        instance.setPort(8080);
        instance.setServiceName(SERVICE);
        instances.add(instance);
        listener.getValue().onEvent(new NamingEvent(SERVICE, new ArrayList<>(instances)));

        assertEquals(1, rows.size());
        assertEquals("127.0.0.1:8080", rows.get(0).getUpstreamUrl());
        assertEquals("handler-id", rows.get(0).getDiscoveryHandlerId());
        assertEquals(NAMESPACE, rows.get(0).getNamespaceId());
        assertEquals(NAMESPACE, lastSnapshot().getNamespaceId());
        assertEquals("selector-id", lastSnapshot().getSelectorId());
        assertEquals("127.0.0.1:8080", lastSnapshot().getUpstreamDataList().get(0).getUrl());

        instances.clear();
        listener.getValue().onEvent(new NamingEvent(SERVICE, Collections.emptyList()));
        assertTrue(rows.isEmpty());
        assertTrue(lastSnapshot().getUpstreamDataList().isEmpty());
    }

    @Test
    void testSubscriptionFailureRemainsVisibleAndCanBeRetried() throws NacosException {
        final NacosException failure = new NacosException(500, "temporary subscription failure");
        doThrow(failure).doNothing().when(namingService).subscribe(eq(SERVICE), eq(GROUP), any(EventListener.class));
        DiscoveryHandlerDTO handler = new DiscoveryHandlerDTO();
        handler.setId("handler-id");
        handler.setDiscoveryId(discovery.getId());
        handler.setListenerNode(SERVICE);
        handler.setHandler("{}");
        ProxySelectorDTO selector = new ProxySelectorDTO();
        selector.setId("selector-id");
        selector.setPluginName("divide");
        selector.setNamespaceId(NAMESPACE);

        ShenyuException exception = assertThrows(ShenyuException.class, () -> processor.createProxySelector(handler, selector));
        assertSame(failure, exception.getCause());
        assertFalse(processor.getCacheKey(discovery.getId()).contains(SERVICE));
        assertDoesNotThrow(() -> processor.createProxySelector(handler, selector));
        verify(namingService, times(2)).subscribe(eq(SERVICE), eq(GROUP), any(EventListener.class));
        assertTrue(processor.getCacheKey(discovery.getId()).contains(SERVICE));
    }

    private List<DiscoveryUpstreamDO> mockUpstreamStorage() {
        final List<DiscoveryUpstreamDO> rows = new ArrayList<>();
        rows.add(DiscoveryUpstreamDO.builder().id("stale-id").discoveryHandlerId("handler-id").url("127.0.0.1:8081").build());
        when(upstreamMapper.selectByDiscoveryHandlerId("handler-id")).thenAnswer(invocation -> new ArrayList<>(rows));
        when(upstreamMapper.deleteByIds(Collections.singletonList("stale-id"))).thenAnswer(invocation -> {
            rows.clear();
            return 1;
        });
        when(upstreamMapper.insert(any(DiscoveryUpstreamDO.class))).thenAnswer(invocation -> {
            rows.add(invocation.getArgument(0));
            return 1;
        });
        when(upstreamMapper.deleteByUrl("handler-id", "127.0.0.1:8080")).thenAnswer(invocation -> {
            rows.clear();
            return 1;
        });
        return rows;
    }

    private DiscoverySyncData lastSnapshot() {
        return events.stream().filter(event -> event.getGroupKey() == ConfigGroupEnum.DISCOVER_UPSTREAM)
                .map(event -> (DiscoverySyncData) event.getSource().get(0)).reduce((first, second) -> second).orElseThrow();
    }
}
