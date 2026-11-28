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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
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
    public void testUpdatedFreshUpstreamSetsHandlerAndTimestampWithoutDatabaseId() {
        DiscoveryUpstreamData upstreamData = new DiscoveryUpstreamData();
        upstreamData.setProtocol("http://");
        upstreamData.setUrl("127.0.0.1:8080");
        upstreamData.setWeight(50);
        when(keyValueParser.parseValue(anyString())).thenReturn(Collections.singletonList(upstreamData));
        when(contextInfo.getNamespaceId()).thenReturn(SYS_DEFAULT_NAMESPACE_ID);
        when(contextInfo.getDiscoveryHandlerId()).thenReturn("discoveryHandlerId");
        final long beforeUpdate = System.currentTimeMillis();

        discoveryDataChangedEventSyncListener.onChange(new DiscoveryDataChangedEvent("key", "value", DiscoveryDataChangedEvent.Event.UPDATED));

        ArgumentCaptor<DiscoveryUpstreamDO> upstreamCaptor = ArgumentCaptor.forClass(DiscoveryUpstreamDO.class);
        verify(discoveryUpstreamMapper).updateDiscoveryHandlerIdAndUrl(upstreamCaptor.capture());
        DiscoveryUpstreamDO updated = upstreamCaptor.getValue();
        Assertions.assertNull(updated.getId());
        Assertions.assertEquals("discoveryHandlerId", updated.getDiscoveryHandlerId());
        Assertions.assertEquals(50, updated.getWeight());
        Assertions.assertNotNull(updated.getDateUpdated());
        Assertions.assertTrue(updated.getDateUpdated().getTime() >= beforeUpdate);
        Assertions.assertTrue(updated.getDateUpdated().getTime() <= System.currentTimeMillis());
        verify(eventPublisher).publishEvent(any(DataChangedEvent.class));
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
}
