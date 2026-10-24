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

import org.apache.shenyu.admin.config.properties.ClusterProperties;
import org.apache.shenyu.admin.discovery.listener.DiscoveryDataChangedEvent;
import org.apache.shenyu.admin.discovery.parse.KeyValueParser;
import org.apache.shenyu.admin.listener.ClusterDataChangedEventForwarder;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.listener.DataChangedEventDispatcher;
import org.apache.shenyu.admin.listener.DataChangedListener;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.apache.shenyu.admin.model.event.discovery.DiscoveryStreamUpdatedEvent;
import org.apache.shenyu.admin.service.manager.LoadServiceDocEntry;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationEvent;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class DiscoveryDataChangedEventSyncListenerTest {

    @InjectMocks
    private DiscoveryDataChangedEventSyncListener discoveryDataChangedEventSyncListener;

    @Mock
    private KeyValueParser keyValueParser;

    @Mock
    private ApplicationEventPublisher eventPublisher;

    @Mock
    private DiscoveryUpstreamMapper discoveryUpstreamMapper;

    @Mock
    private DiscoverySyncData contextInfo;

    @BeforeEach
    public void setUp() {
        String discoveryHandlerId = "discoveryHandlerId";
        discoveryDataChangedEventSyncListener = new DiscoveryDataChangedEventSyncListener(eventPublisher, discoveryUpstreamMapper, keyValueParser, contextInfo, discoveryHandlerId);
    }

    @AfterEach
    void clearTransaction() {
        if (TransactionSynchronizationManager.isSynchronizationActive()) {
            TransactionSynchronizationManager.clearSynchronization();
        }
        TransactionSynchronizationManager.setActualTransactionActive(false);
    }

    @Test
    void testCommittedRegistryChangeRefreshesCompleteHealthSnapshot() {
        prepareRegistryChange();
        DiscoveryUpstreamDO old = new DiscoveryUpstreamDO();
        old.setUpstreamUrl("127.0.0.1:8080");
        DiscoveryUpstreamDO replacement = new DiscoveryUpstreamDO();
        replacement.setUpstreamUrl("127.0.0.1:8081");
        when(discoveryUpstreamMapper.selectByDiscoveryHandlerId("discoveryHandlerId")).thenReturn(List.of(old, replacement));
        TransactionSynchronizationManager.setActualTransactionActive(true);
        TransactionSynchronizationManager.initSynchronization();

        discoveryDataChangedEventSyncListener.onChange(new DiscoveryDataChangedEvent("key", "value", DiscoveryDataChangedEvent.Event.ADDED));

        verify(eventPublisher, never()).publishEvent(any(DiscoveryStreamUpdatedEvent.class));
        TransactionSynchronizationManager.getSynchronizations().forEach(TransactionSynchronization::afterCommit);
        ArgumentCaptor<DiscoveryStreamUpdatedEvent> captor = ArgumentCaptor.forClass(DiscoveryStreamUpdatedEvent.class);
        verify(eventPublisher).publishEvent(captor.capture());
        DiscoverySyncData snapshot = captor.getValue().getDiscoverySyncData();
        Assertions.assertEquals("selector-1", snapshot.getSelectorId());
        Assertions.assertEquals(2, snapshot.getUpstreamDataList().size());
        Assertions.assertEquals("127.0.0.1:8081", snapshot.getUpstreamDataList().get(1).getUrl());
    }

    @Test
    void testRolledBackRegistryChangeDoesNotRefreshHealthSnapshot() {
        prepareRegistryChange();
        TransactionSynchronizationManager.setActualTransactionActive(true);
        TransactionSynchronizationManager.initSynchronization();

        discoveryDataChangedEventSyncListener.onChange(new DiscoveryDataChangedEvent("key", "value", DiscoveryDataChangedEvent.Event.ADDED));
        TransactionSynchronizationManager.getSynchronizations()
                .forEach(synchronization -> synchronization.afterCompletion(TransactionSynchronization.STATUS_ROLLED_BACK));

        verify(eventPublisher, never()).publishEvent(any(DiscoveryStreamUpdatedEvent.class));
    }

    @Test
    void testHealthRefreshPrecedesCommittedGatewayDispatch() {
        prepareRegistryChange();
        List<String> delivered = new ArrayList<>();
        TransactionSynchronizationManager.setActualTransactionActive(true);
        TransactionSynchronizationManager.initSynchronization();
        connectGatewayDispatcher(delivered);

        discoveryDataChangedEventSyncListener.onChange(new DiscoveryDataChangedEvent("key", "value", DiscoveryDataChangedEvent.Event.ADDED));
        Assertions.assertTrue(delivered.isEmpty());
        TransactionSynchronizationManager.getSynchronizations().forEach(TransactionSynchronization::afterCommit);

        Assertions.assertEquals(List.of("health", "gateway"), delivered);
    }

    @Test
    void testHealthRefreshFailureDoesNotSuppressCommittedGatewayDispatch() {
        prepareRegistryChange();
        List<String> delivered = new ArrayList<>();
        TransactionSynchronizationManager.setActualTransactionActive(true);
        TransactionSynchronizationManager.initSynchronization();
        connectGatewayDispatcher(delivered);
        doThrow(new IllegalArgumentException("invalid discovery properties"))
                .when(eventPublisher).publishEvent(any(DiscoveryStreamUpdatedEvent.class));

        discoveryDataChangedEventSyncListener.onChange(new DiscoveryDataChangedEvent("key", "value", DiscoveryDataChangedEvent.Event.ADDED));

        Assertions.assertDoesNotThrow(() -> TransactionSynchronizationManager.getSynchronizations().forEach(TransactionSynchronization::afterCommit));
        Assertions.assertEquals(Collections.singletonList("gateway"), delivered);
    }

    @Test
    void testHealthRefreshFailureDoesNotSuppressNonTransactionalGatewayEvent() {
        prepareRegistryChange();
        doThrow(new IllegalArgumentException("invalid discovery properties"))
                .when(eventPublisher).publishEvent(any(DiscoveryStreamUpdatedEvent.class));

        Assertions.assertDoesNotThrow(() -> discoveryDataChangedEventSyncListener.onChange(
                new DiscoveryDataChangedEvent("key", "value", DiscoveryDataChangedEvent.Event.UPDATED)));

        verify(eventPublisher).publishEvent(any(DataChangedEvent.class));
    }

    @SuppressWarnings("unchecked")
    private void connectGatewayDispatcher(final List<String> delivered) {
        ApplicationContext context = mock(ApplicationContext.class);
        DataChangedListener gateway = mock(DataChangedListener.class);
        when(context.getBeansOfType(DataChangedListener.class)).thenReturn(Map.of("gateway", gateway));
        when(context.getBean(LoadServiceDocEntry.class)).thenReturn(mock(LoadServiceDocEntry.class));
        ObjectProvider<ClusterDataChangedEventForwarder> forwarder = mock(ObjectProvider.class);
        when(context.getBeanProvider(ClusterDataChangedEventForwarder.class)).thenReturn(forwarder);
        DataChangedEventDispatcher dispatcher = new DataChangedEventDispatcher(context);
        ReflectionTestUtils.setField(dispatcher, "clusterProperties", new ClusterProperties());
        dispatcher.afterPropertiesSet();
        doAnswer(invocation -> {
            delivered.add("gateway");
            return null;
        }).when(gateway).onDiscoveryUpstreamChanged(org.mockito.ArgumentMatchers.anyList(), any());
        doAnswer(invocation -> {
            ApplicationEvent event = invocation.getArgument(0);
            if (event instanceof DataChangedEvent) {
                dispatcher.onApplicationEvent((DataChangedEvent) event);
            } else if (event instanceof DiscoveryStreamUpdatedEvent) {
                delivered.add("health");
            }
            return null;
        }).when(eventPublisher).publishEvent(any(ApplicationEvent.class));
    }

    @Test
    void testDeletingLastRegistryUpstreamRefreshesEmptySnapshot() {
        prepareRegistryChange();

        discoveryDataChangedEventSyncListener.onChange(new DiscoveryDataChangedEvent("key", "value", DiscoveryDataChangedEvent.Event.DELETED));

        ArgumentCaptor<DiscoveryStreamUpdatedEvent> captor = ArgumentCaptor.forClass(DiscoveryStreamUpdatedEvent.class);
        org.mockito.InOrder order = inOrder(eventPublisher);
        order.verify(eventPublisher).publishEvent(captor.capture());
        order.verify(eventPublisher).publishEvent(any(DataChangedEvent.class));
        Assertions.assertTrue(captor.getValue().getDiscoverySyncData().getUpstreamDataList().isEmpty());
        Assertions.assertEquals(EventTypeEnum.UPDATE, captor.getValue().getType());
    }

    @Test
    void testRegistryUpdateRefreshesHealthSnapshotWithoutTransaction() {
        prepareRegistryChange();

        discoveryDataChangedEventSyncListener.onChange(new DiscoveryDataChangedEvent("key", "value", DiscoveryDataChangedEvent.Event.UPDATED));

        verify(eventPublisher).publishEvent(any(DiscoveryStreamUpdatedEvent.class));
    }

    private void prepareRegistryChange() {
        DiscoveryUpstreamData upstream = new DiscoveryUpstreamData();
        upstream.setUrl("127.0.0.1:8081");
        when(keyValueParser.parseValue("value")).thenReturn(Collections.singletonList(upstream));
        when(contextInfo.getNamespaceId()).thenReturn(SYS_DEFAULT_NAMESPACE_ID);
        when(contextInfo.getPluginName()).thenReturn("divide");
        when(contextInfo.getSelectorId()).thenReturn("selector-1");
        when(contextInfo.getDiscoveryHandlerId()).thenReturn("discoveryHandlerId");
    }

    @Test
    public void testOnChange() {
        final List<DiscoveryUpstreamData> discoveryUpstreamDTOS = new ArrayList<>();
        DiscoveryUpstreamData discoveryUpstreamData = new DiscoveryUpstreamData();
        discoveryUpstreamData.setProtocol("http");
        discoveryUpstreamData.setUrl("1111");
        discoveryUpstreamData.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        discoveryUpstreamData.setDiscoveryHandlerId("discoveryHandlerId");
        discoveryUpstreamDTOS.add(discoveryUpstreamData);
        doNothing().when(eventPublisher).publishEvent(any(DataChangedEvent.class));
        when(keyValueParser.parseValue(anyString())).thenReturn(discoveryUpstreamDTOS);
        when(contextInfo.getNamespaceId()).thenReturn(SYS_DEFAULT_NAMESPACE_ID);
        when(contextInfo.getDiscoveryHandlerId()).thenReturn("discoveryHandlerId");
        DiscoveryDataChangedEvent event = new DiscoveryDataChangedEvent("key", "value", DiscoveryDataChangedEvent.Event.ADDED);
        discoveryDataChangedEventSyncListener.onChange(event);
        verify(discoveryUpstreamMapper).insert(any(DiscoveryUpstreamDO.class));
        DiscoveryDataChangedEvent event2 = new DiscoveryDataChangedEvent("key", "value", DiscoveryDataChangedEvent.Event.UPDATED);
        discoveryDataChangedEventSyncListener.onChange(event2);
        verify(discoveryUpstreamMapper).updateDiscoveryHandlerIdAndUrl(any(DiscoveryUpstreamDO.class));
        DiscoveryDataChangedEvent event3 = new DiscoveryDataChangedEvent("key", "value", DiscoveryDataChangedEvent.Event.DELETED);

        discoveryDataChangedEventSyncListener.onChange(event3);
        verify(discoveryUpstreamMapper).deleteByUrl(anyString(), anyString());
    }

    @Test
    public void testOnChangeShouldUseDiscoveryNamespaceWhenUpstreamNamespaceBlank() {
        final String namespaceId = "namespace-test";
        final DiscoveryUpstreamData discoveryUpstreamData = new DiscoveryUpstreamData();
        discoveryUpstreamData.setProtocol("http://");
        discoveryUpstreamData.setUrl("127.0.0.1:8080");
        when(keyValueParser.parseValue(anyString())).thenReturn(Collections.singletonList(discoveryUpstreamData));
        when(contextInfo.getNamespaceId()).thenReturn(namespaceId);
        when(contextInfo.getDiscoveryHandlerId()).thenReturn("discoveryHandlerId");

        DiscoveryDataChangedEvent event = new DiscoveryDataChangedEvent("key", "value", DiscoveryDataChangedEvent.Event.ADDED);
        discoveryDataChangedEventSyncListener.onChange(event);

        ArgumentCaptor<DiscoveryUpstreamDO> discoveryUpstreamCaptor = ArgumentCaptor.forClass(DiscoveryUpstreamDO.class);
        verify(discoveryUpstreamMapper).insert(discoveryUpstreamCaptor.capture());
        Assertions.assertEquals(namespaceId, discoveryUpstreamCaptor.getValue().getNamespaceId());
    }

    @ParameterizedTest
    @EnumSource(value = DiscoveryDataChangedEvent.Event.class, names = {"ADDED", "UPDATED", "DELETED"})
    public void testRegistryEventsPublishCompleteSnapshot(final DiscoveryDataChangedEvent.Event eventType) {
        prepareRegistryEvent();
        DiscoveryUpstreamDO remaining = new DiscoveryUpstreamDO();
        remaining.setUpstreamUrl("remaining:8080");
        List<DiscoveryUpstreamDO> storedUpstreams = new ArrayList<>();
        storedUpstreams.add(remaining);
        if (eventType != DiscoveryDataChangedEvent.Event.DELETED) {
            DiscoveryUpstreamDO changed = new DiscoveryUpstreamDO();
            changed.setUpstreamUrl("changed:8080");
            storedUpstreams.add(changed);
        }
        when(discoveryUpstreamMapper.selectByDiscoveryHandlerId("handler")).thenReturn(storedUpstreams);

        discoveryDataChangedEventSyncListener.onChange(new DiscoveryDataChangedEvent("key", "value", eventType));

        ArgumentCaptor<DataChangedEvent> captor = ArgumentCaptor.forClass(DataChangedEvent.class);
        verify(eventPublisher).publishEvent(captor.capture());
        DiscoverySyncData snapshot = (DiscoverySyncData) ((List<?>) captor.getValue().getSource()).get(0);
        Assertions.assertEquals(storedUpstreams.size(), snapshot.getUpstreamDataList().size());
        Assertions.assertEquals("remaining:8080", snapshot.getUpstreamDataList().get(0).getUrl());
        Assertions.assertEquals("selector", snapshot.getSelectorId());
        Assertions.assertEquals(SYS_DEFAULT_NAMESPACE_ID, snapshot.getNamespaceId());
    }

    @Test
    public void testDeletingLastInstancePublishesEmptySnapshot() {
        prepareRegistryEvent();
        when(discoveryUpstreamMapper.selectByDiscoveryHandlerId("handler")).thenReturn(Collections.emptyList());

        discoveryDataChangedEventSyncListener.onChange(new DiscoveryDataChangedEvent("key", "value", DiscoveryDataChangedEvent.Event.DELETED));

        verify(discoveryUpstreamMapper).deleteByUrl("handler", "changed:8080");
        ArgumentCaptor<DataChangedEvent> captor = ArgumentCaptor.forClass(DataChangedEvent.class);
        verify(eventPublisher).publishEvent(captor.capture());
        DiscoverySyncData snapshot = (DiscoverySyncData) ((List<?>) captor.getValue().getSource()).get(0);
        Assertions.assertTrue(snapshot.getUpstreamDataList().isEmpty());
    }

    private void prepareRegistryEvent() {
        DiscoveryUpstreamData changed = new DiscoveryUpstreamData();
        changed.setUrl("changed:8080");
        changed.setProtocol("http://");
        changed.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        when(keyValueParser.parseValue("value")).thenReturn(Collections.singletonList(changed));
        when(contextInfo.getNamespaceId()).thenReturn(SYS_DEFAULT_NAMESPACE_ID);
        when(contextInfo.getDiscoveryHandlerId()).thenReturn("handler");
        when(contextInfo.getSelectorId()).thenReturn("selector");
    }

    @Test
    public void testOnChangeIsSafeWhenListenerIsAddedConcurrently() throws Exception {
        DiscoverySyncData additionalContext = org.mockito.Mockito.mock(DiscoverySyncData.class);
        when(contextInfo.getNamespaceId()).thenReturn(SYS_DEFAULT_NAMESPACE_ID);
        when(contextInfo.getDiscoveryHandlerId()).thenReturn("discoveryHandlerId");
        when(contextInfo.getSelectorId()).thenReturn("selector-1");
        when(additionalContext.getNamespaceId()).thenReturn(SYS_DEFAULT_NAMESPACE_ID);
        when(additionalContext.getDiscoveryHandlerId()).thenReturn("discoveryHandlerId");
        when(additionalContext.getSelectorId()).thenReturn("selector-2");
        DiscoveryUpstreamData upstreamData = new DiscoveryUpstreamData();
        upstreamData.setProtocol("http://");
        upstreamData.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        upstreamData.setUrl("127.0.0.1:8080");
        final CountDownLatch processingStarted = new CountDownLatch(1);
        final CountDownLatch continueProcessing = new CountDownLatch(1);
        org.mockito.Mockito.doAnswer(invocation -> {
            processingStarted.countDown();
            continueProcessing.await();
            return Collections.singletonList(upstreamData);
        }).when(keyValueParser).parseValue(anyString());

        ExecutorService executor = Executors.newSingleThreadExecutor();
        try {
            final java.util.concurrent.Future<?> change = executor.submit(() -> discoveryDataChangedEventSyncListener.onChange(
                    new DiscoveryDataChangedEvent("key", "value", DiscoveryDataChangedEvent.Event.ADDED)));
            Assertions.assertTrue(processingStarted.await(1, TimeUnit.SECONDS));
            discoveryDataChangedEventSyncListener.addListener(additionalContext);
            continueProcessing.countDown();
            Assertions.assertDoesNotThrow(() -> change.get(1, TimeUnit.SECONDS));
        } finally {
            continueProcessing.countDown();
            executor.shutdownNow();
        }
        verify(discoveryUpstreamMapper).insert(any(DiscoveryUpstreamDO.class));
    }

    private void setUpCommonMocks() {
        doNothing().when(eventPublisher).publishEvent(any(DataChangedEvent.class));
        when(contextInfo.getNamespaceId()).thenReturn(SYS_DEFAULT_NAMESPACE_ID);
        when(contextInfo.getDiscoveryHandlerId()).thenReturn("discoveryHandlerId");
        when(contextInfo.getPluginName()).thenReturn("divide");
    }

    private List<DiscoveryUpstreamData> buildUpstreamDataList(final String url) {
        DiscoveryUpstreamData data = new DiscoveryUpstreamData();
        data.setProtocol("http");
        data.setUrl(url);
        data.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        data.setDiscoveryHandlerId("discoveryHandlerId");
        List<DiscoveryUpstreamData> list = new ArrayList<>();
        list.add(data);
        return list;
    }

    @Test
    public void testAddedFallbackMatchMigratesOldUrl() {
        setUpCommonMocks();
        String normalizedUrl = "192.168.1.1:80";
        when(keyValueParser.parseValue(anyString())).thenReturn(buildUpstreamDataList("192.168.1.1"));
        when(discoveryUpstreamMapper.selectByDiscoveryHandlerIdAndUrl("discoveryHandlerId", normalizedUrl)).thenReturn(null);

        DiscoveryUpstreamDO oldRecord = new DiscoveryUpstreamDO();
        oldRecord.setId("old-id");
        oldRecord.setUpstreamUrl("192.168.1.1");
        when(discoveryUpstreamMapper.selectByDiscoveryHandlerId("discoveryHandlerId"))
                .thenReturn(Collections.singletonList(oldRecord));

        DiscoveryDataChangedEvent event = new DiscoveryDataChangedEvent("key", "value", DiscoveryDataChangedEvent.Event.ADDED);
        discoveryDataChangedEventSyncListener.onChange(event);

        verify(discoveryUpstreamMapper, never()).insert(any(DiscoveryUpstreamDO.class));
        ArgumentCaptor<DiscoveryUpstreamDO> captor = ArgumentCaptor.forClass(DiscoveryUpstreamDO.class);
        verify(discoveryUpstreamMapper).updateSelective(captor.capture());
        assertEquals(normalizedUrl, captor.getValue().getUpstreamUrl());
    }

    @Test
    public void testUpdatedFallbackMatchMigratesOldUrl() {
        setUpCommonMocks();
        when(keyValueParser.parseValue(anyString())).thenReturn(buildUpstreamDataList("192.168.1.1"));
        when(discoveryUpstreamMapper.updateDiscoveryHandlerIdAndUrl(any(DiscoveryUpstreamDO.class))).thenReturn(0);

        DiscoveryUpstreamDO oldRecord = new DiscoveryUpstreamDO();
        oldRecord.setId("old-id");
        oldRecord.setUpstreamUrl("192.168.1.1");
        when(discoveryUpstreamMapper.selectByDiscoveryHandlerId("discoveryHandlerId"))
                .thenReturn(Collections.singletonList(oldRecord));

        DiscoveryDataChangedEvent event = new DiscoveryDataChangedEvent("key", "value", DiscoveryDataChangedEvent.Event.UPDATED);
        discoveryDataChangedEventSyncListener.onChange(event);

        ArgumentCaptor<DiscoveryUpstreamDO> captor = ArgumentCaptor.forClass(DiscoveryUpstreamDO.class);
        verify(discoveryUpstreamMapper).updateSelective(captor.capture());
        assertEquals("192.168.1.1:80", captor.getValue().getUpstreamUrl());
    }

    @Test
    public void testDeletedFallbackMatchDeletesById() {
        setUpCommonMocks();
        String normalizedUrl = "[2001:db8::1]:80";
        when(keyValueParser.parseValue(anyString())).thenReturn(buildUpstreamDataList("[2001:db8::1]"));
        when(discoveryUpstreamMapper.deleteByUrl("discoveryHandlerId", normalizedUrl)).thenReturn(0);

        DiscoveryUpstreamDO oldRecord = new DiscoveryUpstreamDO();
        oldRecord.setId("old-id");
        oldRecord.setUpstreamUrl("[2001:db8::1]");
        when(discoveryUpstreamMapper.selectByDiscoveryHandlerId("discoveryHandlerId"))
                .thenReturn(Collections.singletonList(oldRecord));

        DiscoveryDataChangedEvent event = new DiscoveryDataChangedEvent("key", "value", DiscoveryDataChangedEvent.Event.DELETED);
        discoveryDataChangedEventSyncListener.onChange(event);

        verify(discoveryUpstreamMapper).deleteByIds(Collections.singletonList("old-id"));
    }

    @Test
    public void testDeletedExactMatchSucceeds() {
        setUpCommonMocks();
        String normalizedUrl = "192.168.1.1:8080";
        when(keyValueParser.parseValue(anyString())).thenReturn(buildUpstreamDataList("192.168.1.1:8080"));
        when(discoveryUpstreamMapper.deleteByUrl("discoveryHandlerId", normalizedUrl)).thenReturn(1);

        DiscoveryDataChangedEvent event = new DiscoveryDataChangedEvent("key", "value", DiscoveryDataChangedEvent.Event.DELETED);
        discoveryDataChangedEventSyncListener.onChange(event);

        verify(discoveryUpstreamMapper).deleteByUrl("discoveryHandlerId", normalizedUrl);
        // only the post-update reload of the sync payload may read the upstream list;
        // the host/port fallback lookup must not run
        verify(discoveryUpstreamMapper, times(1)).selectByDiscoveryHandlerId(anyString());
        verify(discoveryUpstreamMapper, never()).deleteByIds(anyList());
    }
}
