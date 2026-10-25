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
import org.apache.shenyu.admin.scale.scaler.ScaleLifecycleService;
import org.apache.shenyu.admin.service.impl.InstanceCheckService;
import org.apache.shenyu.admin.service.impl.UpstreamCheckService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InOrder;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
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

    @Mock
    private ScaleLifecycleService scaleLifecycleService;

    private ShenyuClusterService clusterService;

    @BeforeEach
    public void setUp() {
        final ClusterProperties properties = new ClusterProperties();
        properties.setSelectPeriod(1L);
        clusterService = new ShenyuClusterService(
                clusterSelectMasterService, upstreamCheckService, instanceCheckService, scaleLifecycleService, properties);
    }

    @Test
    public void testShutdownBeforeStartDoesNotThrow() {
        assertDoesNotThrow(clusterService::shutdown);
        assertTrue(scheduler().isShutdown());
    }

    @Test
    public void testMasterLifecycleStartsAndStopsScaling() {
        when(clusterSelectMasterService.selectMaster(anyString(), anyString(), anyString())).thenReturn(true);
        when(clusterSelectMasterService.checkMasterStatus()).thenReturn(false);

        ReflectionTestUtils.invokeMethod(clusterService, "doSelectMaster", "127.0.0.1", "9095", "/");

        final InOrder order = inOrder(upstreamCheckService, instanceCheckService, scaleLifecycleService);
        order.verify(upstreamCheckService).setup();
        order.verify(instanceCheckService).setup();
        order.verify(scaleLifecycleService).start();
        order.verify(scaleLifecycleService).stop();
        verify(clusterSelectMasterService).releaseMaster();
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
        verify(scaleLifecycleService, never()).start();

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
        verify(scaleLifecycleService).start();
        verify(scaleLifecycleService, times(2)).stop();
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

    @Test
    public void testMasterSelectionContinuesAfterFailure() throws InterruptedException {
        ClusterSelectMasterService localSelectMasterService = mock(ClusterSelectMasterService.class);
        UpstreamCheckService localUpstreamCheckService = mock(UpstreamCheckService.class);
        InstanceCheckService localInstanceCheckService = mock(InstanceCheckService.class);
        ScaleLifecycleService localScaleLifecycleService = mock(ScaleLifecycleService.class);
        ClusterProperties clusterProperties = new ClusterProperties();
        clusterProperties.setSelectPeriod(1L);
        ShenyuClusterService localClusterService = new ShenyuClusterService(localSelectMasterService, localUpstreamCheckService,
                localInstanceCheckService, localScaleLifecycleService, clusterProperties);
        AtomicInteger attempts = new AtomicInteger();
        CountDownLatch attemptsFinished = new CountDownLatch(2);
        when(localSelectMasterService.selectMaster("127.0.0.1", "9195", "/shenyu")).thenAnswer(invocation -> {
            if (attempts.incrementAndGet() == 1) {
                throw new IllegalStateException("temporary lock failure");
            }
            return false;
        });
        when(localSelectMasterService.releaseMaster()).thenAnswer(invocation -> {
            attemptsFinished.countDown();
            return true;
        });

        ScheduledExecutorService executorService = (ScheduledExecutorService) ReflectionTestUtils.getField(localClusterService, "executorService");
        try {
            localClusterService.startSelectMasterTask("127.0.0.1", "9195", "/shenyu");

            assertTrue(attemptsFinished.await(5, TimeUnit.SECONDS));
            verify(localSelectMasterService, atLeast(2)).selectMaster("127.0.0.1", "9195", "/shenyu");
            verify(localSelectMasterService, atLeast(2)).releaseMaster();
            verify(localUpstreamCheckService, times(1)).close();
            verify(localInstanceCheckService, times(1)).close();
        } finally {
            executorService.shutdownNow();
        }
    }
}
