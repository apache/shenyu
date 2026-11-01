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

package org.apache.shenyu.admin.service;

import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.SelectorConditionMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.apache.shenyu.admin.model.event.discovery.DiscoveryStreamUpdatedEvent;
import org.apache.shenyu.admin.service.converter.DivideSelectorHandleConverter;
import org.apache.shenyu.admin.service.converter.SelectorHandleConverterFactor;
import org.apache.shenyu.admin.service.impl.UpstreamCheckService;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.dto.convert.selector.CommonUpstream;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.dto.convert.selector.ZombieUpstream;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.UpstreamCheckUtils;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
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
import org.springframework.test.util.ReflectionTestUtils;

import java.sql.Timestamp;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * Tests the discovery snapshot published after upstream health changes.
 */
@ExtendWith(MockitoExtension.class)
class UpstreamCheckServiceSyncTest {

    private static final String SELECTOR_ID = "sync-selector";

    @Mock
    private SelectorMapper selectorMapper;

    @Mock
    private PluginMapper pluginMapper;

    @Mock
    private SelectorConditionMapper selectorConditionMapper;

    @Mock
    private ApplicationEventPublisher eventPublisher;

    @Mock
    private DiscoveryUpstreamService discoveryUpstreamService;

    private UpstreamCheckService service;

    @BeforeEach
    void setUp() {
        SelectorHandleConverterFactor converters = new SelectorHandleConverterFactor(
                Collections.singletonMap(PluginEnum.DIVIDE.getName(), new DivideSelectorHandleConverter()));
        ShenyuRegisterCenterConfig config = new ShenyuRegisterCenterConfig();
        config.setRegisterType("http");
        config.getProps().setProperty("checked", "true");
        service = new UpstreamCheckService(selectorMapper, eventPublisher, pluginMapper, selectorConditionMapper,
                config, converters, discoveryUpstreamService);
        lenient().when(selectorMapper.selectById(SELECTOR_ID)).thenReturn(SelectorDO.builder().id(SELECTOR_ID)
                .selectorName("/divide").namespaceId("namespace").pluginId("divide-id").handle("[]").build());
        lenient().when(pluginMapper.selectById("divide-id"))
                .thenReturn(PluginDO.builder().id("divide-id").name(PluginEnum.DIVIDE.getName()).build());
    }

    @AfterEach
    void tearDown() {
        service.close();
        UpstreamCheckService.removeByKey(SELECTOR_ID);
        ((Collection<?>) ReflectionTestUtils.getField(UpstreamCheckService.class, "PENDING_SYNC")).clear();
        ((Collection<?>) ReflectionTestUtils.getField(UpstreamCheckService.class, "ZOMBIE_SET")).clear();
    }

    @Test
    void testCrashedBackendIsEvictedWithoutAnOfflineNotification() throws Exception {
        try (Socket crashed = new Socket(); ServerSocket healthy = new ServerSocket(0)) {
            // Reserve the dead port without listening, so another process cannot reuse it.
            crashed.bind(new InetSocketAddress("127.0.0.1", 0));
            String failedUrl = "127.0.0.1:" + crashed.getLocalPort();
            assertFalse(UpstreamCheckUtils.checkUrl(failedUrl));
            String healthyUrl = "127.0.0.1:" + healthy.getLocalPort();
            CommonUpstream dead = DivideUpstream.builder().upstreamUrl(failedUrl).protocol("http://").status(true).healthCheckEnabled(true).build();
            CommonUpstream live = DivideUpstream.builder().upstreamUrl(healthyUrl).protocol("http://").status(true).healthCheckEnabled(true).build();
            when(discoveryUpstreamService.findBySelectorId(SELECTOR_ID)).thenReturn(List.of(
                    DiscoveryUpstreamData.builder().url(failedUrl).protocol("http://").status(0).dateCreated(new Timestamp(1)).build(),
                    DiscoveryUpstreamData.builder().url(healthyUrl).protocol("http://").status(0).dateCreated(new Timestamp(1)).build()));
            ReflectionTestUtils.setField(service, "invokeExecutor", new ScheduledThreadPoolExecutor(2));
            service.replace(SELECTOR_ID, List.of(dead, live));
            Map<?, ?> upstreams = (Map<?, ?>) ReflectionTestUtils.getField(UpstreamCheckService.class, "UPSTREAM_MAP");

            ReflectionTestUtils.invokeMethod(service, "check", SELECTOR_ID, upstreams.get(SELECTOR_ID));
            ReflectionTestUtils.invokeMethod(service, "waitFinish");

            verify(discoveryUpstreamService).changeStatusBySelectorIdAndUrl(SELECTOR_ID, failedUrl, Boolean.FALSE);
            DiscoverySyncData snapshot = publishedSnapshot();
            assertEquals(1, snapshot.getUpstreamDataList().size());
            assertEquals(healthyUrl, snapshot.getUpstreamDataList().get(0).getUrl());
            assertEquals(Collections.singletonList(live), upstreams.get(SELECTOR_ID));
            ArgumentCaptor<SelectorDO> selector = ArgumentCaptor.forClass(SelectorDO.class);
            verify(selectorMapper).updateSelective(selector.capture());
            assertFalse(selector.getValue().getHandle().contains(failedUrl));
            Collection<?> zombies = (Collection<?>) ReflectionTestUtils.getField(UpstreamCheckService.class, "ZOMBIE_SET");
            assertTrue(zombies.stream().map(ZombieUpstream.class::cast).anyMatch(zombie -> SELECTOR_ID.equals(zombie.getSelectorId())
                    && failedUrl.equals(zombie.getCommonUpstream().getUpstreamUrl()) && zombie.getZombieCheckTimes() > 0));
        }
    }

    @Test
    void testUncheckedHealthyDiscoveryUpstreamIsPreserved() {
        DiscoveryUpstreamData offline = DiscoveryUpstreamData.builder().url("127.0.0.1:8080").status(1).build();
        DiscoveryUpstreamData replacement = DiscoveryUpstreamData.builder().id("replacement")
                .url("127.0.0.1:8081").protocol("http://").dateCreated(new Timestamp(1)).status(0).weight(80).props("{}").build();
        when(discoveryUpstreamService.findBySelectorId(SELECTOR_ID)).thenReturn(new ArrayList<>(List.of(offline, replacement)));

        ReflectionTestUtils.invokeMethod(service, "updateSelectorHandler", SELECTOR_ID, Collections.emptyList(), Collections.emptySet());

        DiscoverySyncData snapshot = publishedSnapshot();
        assertEquals(1, snapshot.getUpstreamDataList().size());
        assertEquals("replacement", snapshot.getUpstreamDataList().get(0).getId());
        assertEquals(80, snapshot.getUpstreamDataList().get(0).getWeight());
        verify(discoveryUpstreamService, never()).changeStatusBySelectorIdAndUrl(SELECTOR_ID, replacement.getUrl(), Boolean.TRUE);
        ArgumentCaptor<SelectorDO> selectorCaptor = ArgumentCaptor.forClass(SelectorDO.class);
        verify(selectorMapper).updateSelective(selectorCaptor.capture());
        assertTrue(selectorCaptor.getValue().getHandle().contains(replacement.getUrl()));
    }

    @Test
    void testPartiallyStaleResultPreservesUncheckedUpstream() {
        DiscoveryUpstreamData checked = DiscoveryUpstreamData.builder().url("127.0.0.1:8080").protocol("http://").status(0).build();
        DiscoveryUpstreamData replacement = DiscoveryUpstreamData.builder().url("127.0.0.1:8081").protocol("http://").dateCreated(new Timestamp(1)).status(0).build();
        when(discoveryUpstreamService.findBySelectorId(SELECTOR_ID)).thenReturn(new ArrayList<>(List.of(checked, replacement)));
        DivideUpstream alive = DivideUpstream.builder().upstreamUrl(checked.getUrl()).protocol("http://").status(true).build();

        ReflectionTestUtils.invokeMethod(service, "updateSelectorHandler", SELECTOR_ID, Collections.singletonList(alive), Collections.emptySet());

        assertEquals(2, publishedSnapshot().getUpstreamDataList().size());
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void testLateHealthCheckCannotOverwriteDiscoveryRefresh(final boolean deleted) {
        ScheduledThreadPoolExecutor executor = mock(ScheduledThreadPoolExecutor.class);
        List<Runnable> requests = new ArrayList<>();
        doAnswer(invocation -> requests.add(invocation.getArgument(0))).when(executor).execute(any(Runnable.class));
        ReflectionTestUtils.setField(service, "invokeExecutor", executor);
        CommonUpstream old = DivideUpstream.builder().upstreamUrl("127.0.0.1:8080")
                .protocol("http://").status(true).healthCheckEnabled(true).build();
        service.replace(SELECTOR_ID, Collections.singletonList(old));
        Map<?, ?> upstreams = (Map<?, ?>) ReflectionTestUtils.getField(UpstreamCheckService.class, "UPSTREAM_MAP");
        ReflectionTestUtils.invokeMethod(service, "check", SELECTOR_ID, upstreams.get(SELECTOR_ID));
        CommonUpstream replacement = DivideUpstream.builder().upstreamUrl("127.0.0.1:8081")
                .protocol("http://").status(true).build();
        List<CommonUpstream> latest = deleted ? Collections.emptyList() : Collections.singletonList(replacement);
        service.replace(SELECTOR_ID, latest);

        try (MockedStatic<UpstreamCheckUtils> checks = mockStatic(UpstreamCheckUtils.class)) {
            checks.when(() -> UpstreamCheckUtils.checkUrl(old.getUpstreamUrl())).thenReturn(false);
            requests.forEach(Runnable::run);
        }
        ReflectionTestUtils.invokeMethod(service, "waitFinish");

        assertEquals(latest, upstreams.get(SELECTOR_ID));
        verifyNoInteractions(eventPublisher);
    }

    @Test
    void testAuthoritativeEmptySnapshotDoesNotResurrectCheckedUpstream() {
        when(discoveryUpstreamService.hasDiscoveryHandler(SELECTOR_ID)).thenReturn(true);
        DivideUpstream alive = DivideUpstream.builder().upstreamUrl("127.0.0.1:8080").protocol("http://").status(true).build();

        ReflectionTestUtils.invokeMethod(service, "updateSelectorHandler", SELECTOR_ID, Collections.singletonList(alive), Collections.emptySet());

        assertTrue(publishedSnapshot().getUpstreamDataList().isEmpty());
        verify(discoveryUpstreamService, never()).changeStatusBySelectorIdAndUrl(SELECTOR_ID, alive.getUpstreamUrl(), Boolean.TRUE);
    }

    @Test
    void testReplacementDiscoveredByAnotherAdminRemainsInHealthCache() {
        DiscoveryUpstreamData replacement = DiscoveryUpstreamData.builder().url("127.0.0.1:8081").protocol("http://")
                .dateCreated(new Timestamp(1)).status(0).build();
        when(discoveryUpstreamService.findBySelectorId(SELECTOR_ID)).thenReturn(Collections.singletonList(replacement));
        CommonUpstream old = DivideUpstream.builder().upstreamUrl("127.0.0.1:8080").protocol("http://").status(false).build();
        service.replace(SELECTOR_ID, Collections.singletonList(old));

        ReflectionTestUtils.invokeMethod(service, "updateHandler", SELECTOR_ID, Collections.singletonList(old), Collections.emptyList());

        assertEquals(replacement.getUrl(), publishedSnapshot().getUpstreamDataList().get(0).getUrl());
        @SuppressWarnings("unchecked")
        Map<String, List<CommonUpstream>> upstreams = (Map<String, List<CommonUpstream>>) ReflectionTestUtils.getField(UpstreamCheckService.class, "UPSTREAM_MAP");
        assertEquals(1, upstreams.get(SELECTOR_ID).size());
        assertEquals(replacement.getUrl(), upstreams.get(SELECTOR_ID).get(0).getUpstreamUrl());
    }

    @Test
    void testFailedCheckOverridesPersistedHealthyStatus() {
        DiscoveryUpstreamData failed = DiscoveryUpstreamData.builder().url("127.0.0.1:8080").protocol("http://").status(0).build();
        DiscoveryUpstreamData replacement = DiscoveryUpstreamData.builder().url("127.0.0.1:8081").protocol("http://")
                .dateCreated(new Timestamp(1)).status(0).build();
        when(discoveryUpstreamService.findBySelectorId(SELECTOR_ID)).thenReturn(List.of(failed, replacement));
        CommonUpstream old = DivideUpstream.builder().upstreamUrl(failed.getUrl()).protocol("http://").status(false).build();
        service.replace(SELECTOR_ID, Collections.singletonList(old));

        ReflectionTestUtils.invokeMethod(service, "updateHandler", SELECTOR_ID, Collections.singletonList(old), Collections.emptyList());

        List<DiscoveryUpstreamData> outgoing = publishedSnapshot().getUpstreamDataList();
        assertEquals(1, outgoing.size());
        assertEquals(replacement.getUrl(), outgoing.get(0).getUrl());
    }

    @Test
    void testRecoveredUpstreamPublishesCurrentStatus() {
        DiscoveryUpstreamData recovered = DiscoveryUpstreamData.builder().id("recovered")
                .url("127.0.0.1:8080").protocol("http://").status(1).weight(80).props("{}").build();
        DiscoveryUpstreamData offline = DiscoveryUpstreamData.builder().id("offline").url("127.0.0.1:8081").status(1).build();
        when(discoveryUpstreamService.findBySelectorId(SELECTOR_ID)).thenReturn(new ArrayList<>(List.of(recovered, offline)));
        DivideUpstream alive = DivideUpstream.builder().upstreamUrl(recovered.getUrl()).protocol("http://").status(true).build();

        ReflectionTestUtils.invokeMethod(service, "updateSelectorHandler", SELECTOR_ID, Collections.singletonList(alive), Collections.emptySet());

        verify(discoveryUpstreamService).changeStatusBySelectorIdAndUrl(SELECTOR_ID, recovered.getUrl(), Boolean.TRUE);
        DiscoverySyncData snapshot = publishedSnapshot();
        assertEquals(1, snapshot.getUpstreamDataList().size());
        DiscoveryUpstreamData upstream = snapshot.getUpstreamDataList().get(0);
        assertEquals(0, upstream.getStatus(), "The published status must match the database update");
        assertEquals("recovered", upstream.getId());
        assertEquals(80, upstream.getWeight());
        assertEquals("{}", upstream.getProps());
    }

    @Test
    void testAllOfflinePublishesEmptySnapshot() {
        DiscoveryUpstreamData offline = DiscoveryUpstreamData.builder().url("127.0.0.1:8080").status(1).build();
        when(discoveryUpstreamService.findBySelectorId(SELECTOR_ID)).thenReturn(new ArrayList<>(Collections.singletonList(offline)));

        ReflectionTestUtils.invokeMethod(service, "updateSelectorHandler", SELECTOR_ID, Collections.emptyList(), Collections.emptySet());

        assertTrue(publishedSnapshot().getUpstreamDataList().isEmpty());
    }

    @Test
    void testUpstreamWithoutDiscoveryRecordPublishesHealthyStatus() {
        DivideUpstream alive = DivideUpstream.builder().upstreamUrl("127.0.0.1:8080").protocol("http://").status(true).build();

        ReflectionTestUtils.invokeMethod(service, "updateSelectorHandler", SELECTOR_ID, Collections.singletonList(alive), Collections.emptySet());

        DiscoverySyncData snapshot = publishedSnapshot();
        assertEquals(1, snapshot.getUpstreamDataList().size());
        assertEquals(alive.getUpstreamUrl(), snapshot.getUpstreamDataList().get(0).getUrl());
        assertEquals(0, snapshot.getUpstreamDataList().get(0).getStatus());
    }

    @Test
    void testRegistrationCanProceedDuringSlowEventDispatch() throws Exception {
        CommonUpstream old = DivideUpstream.builder().upstreamUrl("127.0.0.1:8080").protocol("http://").status(false).build();
        CommonUpstream added = DivideUpstream.builder().upstreamUrl("127.0.0.1:8081").protocol("http://").status(true).build();
        service.replace(SELECTOR_ID, Collections.singletonList(old));
        CountDownLatch publishing = new CountDownLatch(1);
        CountDownLatch release = new CountDownLatch(1);
        doAnswer(invocation -> {
            DataChangedEvent event = invocation.getArgument(0);
            if (event.getGroupKey() == ConfigGroupEnum.SELECTOR) {
                publishing.countDown();
                assertTrue(release.await(5, TimeUnit.SECONDS));
            }
            return null;
        }).when(eventPublisher).publishEvent(any(DataChangedEvent.class));
        ExecutorService executor = Executors.newFixedThreadPool(2);
        try {
            final Future<?> check = executor.submit(() -> ReflectionTestUtils.invokeMethod(service, "updateHandler", SELECTOR_ID,
                    Collections.singletonList(old), Collections.emptyList()));
            assertTrue(publishing.await(5, TimeUnit.SECONDS));
            Future<?> registration = executor.submit(() -> ReflectionTestUtils.invokeMethod(service, "submitJust", SELECTOR_ID, added));
            registration.get(2, TimeUnit.SECONDS);
            release.countDown();
            check.get(5, TimeUnit.SECONDS);
            Map<?, ?> upstreams = (Map<?, ?>) ReflectionTestUtils.getField(UpstreamCheckService.class, "UPSTREAM_MAP");
            assertTrue(upstreams.containsKey(SELECTOR_ID));
            assertTrue(((List<?>) upstreams.get(SELECTOR_ID)).contains(added));
        } finally {
            release.countDown();
            executor.shutdownNow();
            assertTrue(executor.awaitTermination(5, TimeUnit.SECONDS));
        }
    }

    @Test
    void testQueuedRegistrationCannotPublishSupersededList() {
        ScheduledThreadPoolExecutor executor = mock(ScheduledThreadPoolExecutor.class);
        List<Runnable> updates = new ArrayList<>();
        doAnswer(invocation -> updates.add(invocation.getArgument(0))).when(executor).execute(any(Runnable.class));
        ReflectionTestUtils.setField(service, "executor", executor);
        CommonUpstream first = DivideUpstream.builder().upstreamUrl("127.0.0.1:8080").protocol("http://").status(true).build();
        CommonUpstream second = DivideUpstream.builder().upstreamUrl("127.0.0.1:8081").protocol("http://").status(true).build();
        service.submit(SELECTOR_ID, first);
        service.submit(SELECTOR_ID, second);

        updates.get(0).run();
        verifyNoInteractions(eventPublisher);
        updates.get(1).run();

        assertEquals(2, publishedSnapshot().getUpstreamDataList().size());
    }

    @Test
    void testReentrantDiscoveryRefreshDuringPublicationIsRetained() {
        CommonUpstream old = DivideUpstream.builder().upstreamUrl("127.0.0.1:8080").protocol("http://").status(false).build();
        CommonUpstream replacement = DivideUpstream.builder().upstreamUrl("127.0.0.1:8081").protocol("http://").status(true).build();
        service.replace(SELECTOR_ID, Collections.singletonList(old));
        doAnswer(invocation -> {
            DataChangedEvent event = invocation.getArgument(0);
            if (event.getGroupKey() == ConfigGroupEnum.DISCOVER_UPSTREAM) {
                service.replace(SELECTOR_ID, Collections.singletonList(replacement));
            }
            return null;
        }).when(eventPublisher).publishEvent(any(DataChangedEvent.class));

        ReflectionTestUtils.invokeMethod(service, "updateHandler", SELECTOR_ID, Collections.singletonList(old), Collections.emptyList());

        Map<?, ?> upstreams = (Map<?, ?>) ReflectionTestUtils.getField(UpstreamCheckService.class, "UPSTREAM_MAP");
        assertEquals(Collections.singletonList(replacement), upstreams.get(SELECTOR_ID));
    }

    @Test
    void testMixedHttpRegistrationDoesNotOverrideAuthoritativeDiscovery() {
        CommonUpstream http = DivideUpstream.builder().upstreamUrl("127.0.0.1:8080").protocol("http://").status(true).build();
        DiscoveryUpstreamData discovered = DiscoveryUpstreamData.builder().url("127.0.0.1:8081").protocol("http://")
                .dateCreated(new Timestamp(1)).status(0).build();
        when(discoveryUpstreamService.findBySelectorId(SELECTOR_ID)).thenReturn(Collections.singletonList(discovered));

        ReflectionTestUtils.invokeMethod(service, "updateSelectorHandler", SELECTOR_ID, Collections.singletonList(http), Collections.emptySet());

        List<DiscoveryUpstreamData> outgoing = publishedSnapshot().getUpstreamDataList();
        assertEquals(1, outgoing.size());
        assertEquals(discovered.getUrl(), outgoing.get(0).getUrl());
        ArgumentCaptor<SelectorDO> selector = ArgumentCaptor.forClass(SelectorDO.class);
        verify(selectorMapper).updateSelective(selector.capture());
        assertTrue(selector.getValue().getHandle().contains(discovered.getUrl()));
        org.junit.jupiter.api.Assertions.assertFalse(selector.getValue().getHandle().contains(http.getUpstreamUrl()));
    }

    @Test
    void testDiscoveryRefreshPreservesPersistedHealthState() {
        DiscoveryUpstreamData healthy = DiscoveryUpstreamData.builder().url("127.0.0.1:8080").protocol("http://")
                .dateCreated(new Timestamp(1)).status(0).build();
        DiscoveryUpstreamData unhealthy = DiscoveryUpstreamData.builder().url("127.0.0.1:8081").protocol("http://")
                .dateCreated(new Timestamp(1)).status(1).build();
        DiscoverySyncData snapshot = new DiscoverySyncData();
        snapshot.setSelectorId(SELECTOR_ID);
        snapshot.setPluginName(PluginEnum.DIVIDE.getName());
        snapshot.setUpstreamDataList(List.of(healthy, unhealthy));

        service.onDiscoveryUpstreamUpdated(new DiscoveryStreamUpdatedEvent(snapshot, EventTypeEnum.UPDATE));

        @SuppressWarnings("unchecked")
        Map<String, List<CommonUpstream>> upstreams = (Map<String, List<CommonUpstream>>) ReflectionTestUtils.getField(UpstreamCheckService.class, "UPSTREAM_MAP");
        assertTrue(upstreams.get(SELECTOR_ID).get(0).isStatus());
        org.junit.jupiter.api.Assertions.assertFalse(upstreams.get(SELECTOR_ID).get(1).isStatus());
    }

    /**
     * Central probe mode (the default): a successful admin probe keeps the legacy behavior and
     * revives the upstream in the db status, the persisted selector handle and the published
     * events, see <a href="https://github.com/apache/shenyu/issues/8056">issue #8056</a>.
     */
    @Test
    void testCentralModeProbeSuccessWritesBackAliveAsLegacy() throws Exception {
        try (ServerSocket healthy = new ServerSocket(0)) {
            String aliveUrl = "127.0.0.1:" + healthy.getLocalPort();
            DiscoveryUpstreamData disabled = DiscoveryUpstreamData.builder().url(aliveUrl).protocol("http://").status(1)
                    .dateCreated(new Timestamp(1)).build();
            when(discoveryUpstreamService.findBySelectorId(SELECTOR_ID)).thenReturn(new ArrayList<>(List.of(disabled)));
            CommonUpstream recovered = DivideUpstream.builder().upstreamUrl(aliveUrl).protocol("http://").status(false).healthCheckEnabled(true).build();
            ReflectionTestUtils.setField(service, "invokeExecutor", new ScheduledThreadPoolExecutor(2));
            service.replace(SELECTOR_ID, List.of(recovered));
            Map<?, ?> upstreams = (Map<?, ?>) ReflectionTestUtils.getField(UpstreamCheckService.class, "UPSTREAM_MAP");

            ReflectionTestUtils.invokeMethod(service, "check", SELECTOR_ID, upstreams.get(SELECTOR_ID));
            ReflectionTestUtils.invokeMethod(service, "waitFinish");

            verify(discoveryUpstreamService).changeStatusBySelectorIdAndUrl(SELECTOR_ID, aliveUrl, Boolean.TRUE);
            ArgumentCaptor<SelectorDO> selector = ArgumentCaptor.forClass(SelectorDO.class);
            verify(selectorMapper).updateSelective(selector.capture());
            List<DivideUpstream> handle = GsonUtils.getInstance().fromCurrentList(selector.getValue().getHandle(), DivideUpstream.class);
            assertEquals(1, handle.size());
            assertEquals(aliveUrl, handle.get(0).getUpstreamUrl());
            assertTrue(handle.get(0).isStatus());
            DiscoverySyncData snapshot = publishedSnapshot();
            assertEquals(1, snapshot.getUpstreamDataList().size());
            assertEquals(aliveUrl, snapshot.getUpstreamDataList().get(0).getUrl());
            assertEquals(0, snapshot.getUpstreamDataList().get(0).getStatus());
        }
    }

    /**
     * Local probe mode: the scheduled check entry is inert, so a probe success can never reach
     * the write-back of updateSelectorHandler; the explicitly disabled discovery row, the selector
     * handle and the published events all stay untouched,
     * see <a href="https://github.com/apache/shenyu/issues/8056">issue #8056</a>.
     */
    @Test
    void testLocalModeScheduledCheckDoesNotReviveDisabledUpstream() throws Exception {
        service = buildServiceWithProbeMode(Constants.LOCAL_PROBE_MODE_VALUE);
        try (Socket crashed = new Socket()) {
            crashed.bind(new InetSocketAddress("127.0.0.1", 0));
            String deadUrl = "127.0.0.1:" + crashed.getLocalPort();
            CommonUpstream upstream = DivideUpstream.builder().upstreamUrl(deadUrl).protocol("http://").status(false).healthCheckEnabled(true).build();
            service.replace(SELECTOR_ID, List.of(upstream));

            ReflectionTestUtils.invokeMethod(service, "doCheck");

            verify(discoveryUpstreamService, never()).changeStatusBySelectorIdAndUrl(any(), any(), any());
            verify(selectorMapper, never()).updateSelective(any());
            verify(eventPublisher, never()).publishEvent(any());
        }
    }

    /**
     * Local probe mode: even when the probe-execution path below the scheduled entry is exercised
     * directly and the probe really succeeds, the defensive write-side gate in updateSelectorHandler
     * keeps the explicitly disabled upstream disabled in the db and the selector handle, and no
     * selector/discovery event is published, see <a href="https://github.com/apache/shenyu/issues/8056">issue #8056</a>.
     */
    @Test
    void testLocalModeCheckPipelineDoesNotReviveDisabledUpstream() throws Exception {
        service = buildServiceWithProbeMode(Constants.LOCAL_PROBE_MODE_VALUE);
        try (ServerSocket healthy = new ServerSocket(0)) {
            String aliveUrl = "127.0.0.1:" + healthy.getLocalPort();
            DiscoveryUpstreamData disabled = DiscoveryUpstreamData.builder().url(aliveUrl).protocol("http://").status(1).build();
            lenient().when(discoveryUpstreamService.findBySelectorId(SELECTOR_ID)).thenReturn(new ArrayList<>(List.of(disabled)));
            CommonUpstream upstream = DivideUpstream.builder().upstreamUrl(aliveUrl).protocol("http://").status(false).healthCheckEnabled(true).build();
            ReflectionTestUtils.setField(service, "invokeExecutor", new ScheduledThreadPoolExecutor(2));
            service.replace(SELECTOR_ID, List.of(upstream));
            Map<?, ?> upstreams = (Map<?, ?>) ReflectionTestUtils.getField(UpstreamCheckService.class, "UPSTREAM_MAP");

            ReflectionTestUtils.invokeMethod(service, "check", SELECTOR_ID, upstreams.get(SELECTOR_ID));
            ReflectionTestUtils.invokeMethod(service, "waitFinish");

            verify(discoveryUpstreamService, never()).changeStatusBySelectorIdAndUrl(any(), any(), any());
            verify(selectorMapper, never()).updateSelective(any());
            verify(eventPublisher, never()).publishEvent(any());
            assertEquals(1, disabled.getStatus());
        }
    }

    private UpstreamCheckService buildServiceWithProbeMode(final String probeMode) {
        ShenyuRegisterCenterConfig config = new ShenyuRegisterCenterConfig();
        config.setRegisterType("http");
        config.getProps().setProperty(Constants.IS_CHECKED, "true");
        config.getProps().setProperty(Constants.PROBE_MODE, probeMode);
        SelectorHandleConverterFactor converters = new SelectorHandleConverterFactor(
                Collections.singletonMap(PluginEnum.DIVIDE.getName(), new DivideSelectorHandleConverter()));
        return new UpstreamCheckService(selectorMapper, eventPublisher, pluginMapper, selectorConditionMapper,
                config, converters, discoveryUpstreamService);
    }

    private DiscoverySyncData publishedSnapshot() {
        ArgumentCaptor<DataChangedEvent> captor = ArgumentCaptor.forClass(DataChangedEvent.class);
        verify(eventPublisher, times(2)).publishEvent(captor.capture());
        DataChangedEvent event = captor.getAllValues().stream()
                .filter(value -> value.getGroupKey() == ConfigGroupEnum.DISCOVER_UPSTREAM).findFirst().orElseThrow();
        DiscoverySyncData snapshot = GsonUtils.getInstance().fromJson(GsonUtils.getInstance().toJson(event.getSource().get(0)), DiscoverySyncData.class);
        assertEquals(SELECTOR_ID, snapshot.getSelectorId());
        assertEquals("/divide", snapshot.getSelectorName());
        assertEquals("namespace", snapshot.getNamespaceId());
        assertEquals(PluginEnum.DIVIDE.getName(), snapshot.getPluginName());
        return snapshot;
    }
}
