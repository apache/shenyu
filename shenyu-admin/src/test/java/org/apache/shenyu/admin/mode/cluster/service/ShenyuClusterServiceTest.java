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

package org.apache.shenyu.admin.mode.cluster.service;

import org.apache.shenyu.admin.config.properties.ClusterProperties;
import org.apache.shenyu.admin.service.InstanceInfoService;
import org.apache.shenyu.admin.service.impl.InstanceCheckService;
import org.apache.shenyu.admin.service.impl.UpstreamCheckService;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InOrder;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Collections;
import java.util.Objects;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link ShenyuClusterService} shutdown lifecycle.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class ShenyuClusterServiceTest {

    private static final long AWAIT_MILLIS = 5000L;

    @Mock
    private ClusterSelectMasterService clusterSelectMasterService;

    @Mock
    private UpstreamCheckService upstreamCheckService;

    @Mock
    private InstanceCheckService instanceCheckService;

    private ShenyuClusterService clusterService;

    @BeforeEach
    public void setUp() {
        final ClusterProperties properties = new ClusterProperties();
        properties.setSelectPeriod(1L);
        clusterService = new ShenyuClusterService(clusterSelectMasterService, upstreamCheckService, instanceCheckService, properties);
    }

    @AfterEach
    void tearDown() {
        clusterService.shutdown();
    }

    @Test
    void testMasterErrorClosesBothCheckersAndPreservesCauseWhenCleanupFails() {
        IllegalStateException original = new IllegalStateException("Master renewal failed");
        when(clusterSelectMasterService.selectMaster(anyString(), anyString(), anyString())).thenReturn(true);
        when(clusterSelectMasterService.checkMasterStatus()).thenThrow(original);
        doThrow(new IllegalStateException("Upstream cleanup failed")).when(upstreamCheckService).close();
        doThrow(new IllegalStateException("Instance cleanup failed")).when(instanceCheckService).close();

        ShenyuException failure = assertThrows(ShenyuException.class,
                () -> ReflectionTestUtils.invokeMethod(clusterService, "doSelectMaster", "127.0.0.1", "9095", "/"));

        assertSame(original, failure.getCause());
        InOrder order = inOrder(clusterSelectMasterService, upstreamCheckService, instanceCheckService);
        order.verify(upstreamCheckService).setup();
        order.verify(instanceCheckService).setup();
        order.verify(clusterSelectMasterService).checkMasterStatus();
        order.verify(upstreamCheckService).close();
        order.verify(instanceCheckService).close();
        order.verify(clusterSelectMasterService).releaseMaster();
    }

    @Test
    void testMasterErrorTerminatesRealInstanceCheckerEvenWhenFinalPersistenceFails() throws InterruptedException {
        InstanceInfoService persistence = mock(InstanceInfoService.class);
        InstanceCheckService checker = new InstanceCheckService(persistence);
        ClusterProperties properties = new ClusterProperties();
        clusterService = new ShenyuClusterService(clusterSelectMasterService, upstreamCheckService, checker, properties);
        when(persistence.list()).thenReturn(Collections.emptyList());
        doThrow(new IllegalStateException("Database unavailable")).when(persistence).createOrUpdate(any());
        InstanceBeatInfoDTO heartbeat = new InstanceBeatInfoDTO();
        heartbeat.setInstanceIp("127.0.0.1");
        heartbeat.setInstancePort("8080");
        heartbeat.setInstanceType("http");
        heartbeat.setNamespaceId("ns");
        checker.handleBeatInfo(heartbeat);
        IllegalStateException original = new IllegalStateException("Master renewal failed");
        when(clusterSelectMasterService.selectMaster(anyString(), anyString(), anyString())).thenReturn(true);
        when(clusterSelectMasterService.checkMasterStatus()).thenThrow(original);

        try {
            ShenyuException failure = assertThrows(ShenyuException.class,
                    () -> ReflectionTestUtils.invokeMethod(clusterService, "doSelectMaster", "127.0.0.1", "9095", "/"));

            assertSame(original, failure.getCause());
            ScheduledThreadPoolExecutor executor = (ScheduledThreadPoolExecutor) ReflectionTestUtils.getField(checker, "executor");
            assertTrue(executor.isTerminated());
            InOrder order = inOrder(upstreamCheckService, persistence, clusterSelectMasterService);
            order.verify(upstreamCheckService).close();
            order.verify(persistence).createOrUpdate(any());
            order.verify(clusterSelectMasterService).releaseMaster();
        } finally {
            ScheduledThreadPoolExecutor executor = (ScheduledThreadPoolExecutor) ReflectionTestUtils.getField(checker, "executor");
            if (Objects.nonNull(executor)) {
                executor.shutdownNow();
                assertTrue(executor.awaitTermination(2, TimeUnit.SECONDS));
            }
        }
    }

    @Test
    public void testShutdownBeforeStartDoesNotThrow() {
        assertDoesNotThrow(clusterService::shutdown);
        assertTrue(scheduler().isShutdown());
    }

    @Test
    public void testShutdownStopsElectionScheduler() throws Exception {
        final AtomicInteger attempts = new AtomicInteger();
        when(clusterSelectMasterService.selectMaster(anyString(), anyString(), anyString()))
                .thenAnswer(invocation -> {
                    attempts.incrementAndGet();
                    return false;
                });

        clusterService.start("127.0.0.1", 9095, "/");
        awaitAttempts(attempts, 1);

        clusterService.shutdown();

        assertTrue(scheduler().isShutdown());
        final int afterShutdown = attempts.get();
        Thread.sleep(2500L);
        assertEquals(afterShutdown, attempts.get(), "no election attempt may happen after completed shutdown");

        assertDoesNotThrow(clusterService::shutdown);
    }

    @Test
    public void testShutdownInterruptsRenewalAndReleasesLeadership() throws Exception {
        when(clusterSelectMasterService.selectMaster(anyString(), anyString(), anyString())).thenReturn(true);
        when(clusterSelectMasterService.checkMasterStatus()).thenReturn(true);

        clusterService.start("127.0.0.1", 9095, "/");
        // the first election succeeded and the renewal loop is running
        org.mockito.Mockito.verify(clusterSelectMasterService, timeout(AWAIT_MILLIS).atLeastOnce())
                .selectMaster(anyString(), anyString(), anyString());

        clusterService.shutdown();

        // the interrupted renewal loop releases leadership through its own finally block
        org.mockito.Mockito.verify(clusterSelectMasterService, timeout(3000L)).releaseMaster();
        // and the existing error path closes the owned check services
        org.mockito.Mockito.verify(upstreamCheckService, timeout(3000L)).close();
        org.mockito.Mockito.verify(instanceCheckService, timeout(3000L)).close();
    }

    private ScheduledExecutorService scheduler() {
        return (ScheduledExecutorService) ReflectionTestUtils.getField(clusterService, "executorService");
    }

    private void awaitAttempts(final AtomicInteger attempts, final int expected) throws Exception {
        final long deadline = System.currentTimeMillis() + AWAIT_MILLIS;
        while (attempts.get() < expected && System.currentTimeMillis() < deadline) {
            Thread.sleep(50L);
        }
        assertTrue(attempts.get() >= expected, "election attempt did not happen in time");
    }
}
