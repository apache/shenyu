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

package org.apache.shenyu.admin.service.impl;

import org.apache.shenyu.admin.model.vo.InstanceInfoVO;
import org.apache.shenyu.admin.service.InstanceInfoService;
import org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.InOrder;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * Tests for draining instance checker work and releasing resources during shutdown.
 */
@ExtendWith(MockitoExtension.class)
final class InstanceCheckServiceShutdownTest {

    @Mock
    private InstanceInfoService instanceInfoService;

    private InstanceCheckService instanceCheckService;

    private InstanceBeatInfoDTO heartbeat;

    private final List<ExecutorService> ownedExecutors = new ArrayList<>();

    @BeforeEach
    void setUp() {
        instanceCheckService = new InstanceCheckService(instanceInfoService);
        heartbeat = new InstanceBeatInfoDTO();
        heartbeat.setInstanceIp("127.0.0.1");
        heartbeat.setInstancePort("8080");
        heartbeat.setInstanceType("http");
        heartbeat.setNamespaceId("ns");
        heartbeat.setInstanceInfo("{}");
    }

    @AfterEach
    void tearDown() throws InterruptedException {
        ownedExecutors.forEach(ExecutorService::shutdownNow);
        for (ExecutorService executor : ownedExecutors) {
            assertTrue(executor.awaitTermination(2, TimeUnit.SECONDS), "test-owned executor must terminate");
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void testCloseDrainsExecutorBeforeFinalFlushEvenWhenPersistenceFails(final boolean failPersistence) throws Exception {
        ScheduledThreadPoolExecutor executor = startChecker();
        Thread worker = executor.submit(Thread::currentThread).get(2, TimeUnit.SECONDS);
        instanceCheckService.handleBeatInfo(heartbeat);
        InstanceInfoVO cached = instanceCheckService.getInstanceHealthBeatInfo(heartbeat);
        assertFalse(worker.isDaemon());
        doAnswer(invocation -> {
            if (failPersistence) {
                throw new IllegalStateException("Database unavailable");
            }
            assertTrue(executor.isTerminated(), "stop scheduled work before final persistence");
            assertSame(cached, instanceCheckService.getInstanceHealthBeatInfo(heartbeat));
            return null;
        }).when(instanceInfoService).createOrUpdate(any(InstanceInfoVO.class));

        assertAll(
                () -> assertDoesNotThrow(instanceCheckService::close),
                () -> assertTrue(executor.isShutdown()),
                () -> assertTrue(executor.isTerminated()),
                () -> assertNull(instanceCheckService.getInstanceHealthBeatInfo(heartbeat))
        );
        worker.join(1000L);
        assertFalse(worker.isAlive(), "no checker worker may remain after completed shutdown");
        assertDoesNotThrow(instanceCheckService::close);
        verify(instanceInfoService).createOrUpdate(cached);
    }

    @Test
    void testCloseBeforeSetupFlushesOnceAndRejectsLaterWork() {
        instanceCheckService.handleBeatInfo(heartbeat);
        final InstanceInfoVO cached = instanceCheckService.getInstanceHealthBeatInfo(heartbeat);

        assertDoesNotThrow(instanceCheckService::close);
        assertDoesNotThrow(instanceCheckService::close);
        instanceCheckService.handleBeatInfo(heartbeat);
        instanceCheckService.fetchInstanceData();
        instanceCheckService.syncDB();

        assertNull(instanceCheckService.getInstanceHealthBeatInfo(heartbeat));
        verify(instanceInfoService).createOrUpdate(cached);
        verify(instanceInfoService, never()).list();
    }

    @Test
    void testSetupAfterCloseRestartsHeartbeatProcessingWithoutLeakingExecutors() {
        final ScheduledThreadPoolExecutor first = startChecker();
        instanceCheckService.close();
        instanceCheckService.handleBeatInfo(heartbeat);
        assertNull(instanceCheckService.getInstanceHealthBeatInfo(heartbeat));

        instanceCheckService.setup();
        ScheduledThreadPoolExecutor second = checkerExecutor();
        ownedExecutors.add(second);
        assertNotSame(first, second);
        assertTrue(first.isTerminated());
        instanceCheckService.handleBeatInfo(heartbeat);
        assertNotNull(instanceCheckService.getInstanceHealthBeatInfo(heartbeat));
        instanceCheckService.setup();
        assertSame(second, checkerExecutor(), "repeated setup must not leak another executor");

        instanceCheckService.close();

        assertTrue(second.isTerminated());
        verify(instanceInfoService, times(2)).list();
        verify(instanceInfoService).createOrUpdate(any(InstanceInfoVO.class));
    }

    @Test
    void testCloseWaitsForRunningSyncBeforeFinalFlushAndCacheClear() throws Exception {
        ScheduledThreadPoolExecutor executor = startChecker();
        instanceCheckService.handleBeatInfo(heartbeat);
        InstanceInfoVO cached = instanceCheckService.getInstanceHealthBeatInfo(heartbeat);
        CountDownLatch syncStarted = new CountDownLatch(1);
        CountDownLatch releaseSync = new CountDownLatch(1);
        AtomicInteger persistenceCalls = new AtomicInteger();
        doAnswer(invocation -> {
            if (persistenceCalls.incrementAndGet() == 1) {
                syncStarted.countDown();
                assertTrue(releaseSync.await(2, TimeUnit.SECONDS));
                assertSame(cached, instanceCheckService.getInstanceHealthBeatInfo(heartbeat));
            } else {
                assertTrue(executor.isTerminated());
            }
            return null;
        }).when(instanceInfoService).createOrUpdate(any(InstanceInfoVO.class));
        ExecutorService closingExecutor = Executors.newSingleThreadExecutor();
        ownedExecutors.add(closingExecutor);
        try {
            final Future<?> runningSync = executor.submit(instanceCheckService::syncDB);
            assertTrue(syncStarted.await(2, TimeUnit.SECONDS));
            Future<?> closing = closingExecutor.submit(instanceCheckService::close);
            await().atMost(2, TimeUnit.SECONDS).until(executor::isShutdown);
            assertFalse(closing.isDone());
            assertSame(cached, instanceCheckService.getInstanceHealthBeatInfo(heartbeat));

            releaseSync.countDown();
            runningSync.get(2, TimeUnit.SECONDS);
            closing.get(2, TimeUnit.SECONDS);

            assertTrue(executor.isTerminated());
            assertNull(instanceCheckService.getInstanceHealthBeatInfo(heartbeat));
            verify(instanceInfoService, times(2)).createOrUpdate(cached);
        } finally {
            releaseSync.countDown();
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void testGracefulTimeoutInterruptsWorkAndOnlyFlushesAfterTermination(final boolean terminatesAfterInterrupt) throws InterruptedException {
        ScheduledThreadPoolExecutor executor = mock(ScheduledThreadPoolExecutor.class);
        ReflectionTestUtils.setField(instanceCheckService, "executor", executor);
        when(executor.awaitTermination(5L, TimeUnit.SECONDS)).thenReturn(false, terminatesAfterInterrupt);
        instanceCheckService.handleBeatInfo(heartbeat);
        InstanceInfoVO cached = instanceCheckService.getInstanceHealthBeatInfo(heartbeat);

        assertDoesNotThrow(instanceCheckService::close);

        InOrder order = inOrder(executor, instanceInfoService);
        order.verify(executor).shutdown();
        order.verify(executor).awaitTermination(5L, TimeUnit.SECONDS);
        order.verify(executor).shutdownNow();
        order.verify(executor).awaitTermination(5L, TimeUnit.SECONDS);
        if (terminatesAfterInterrupt) {
            order.verify(instanceInfoService).createOrUpdate(cached);
            assertNull(instanceCheckService.getInstanceHealthBeatInfo(heartbeat));
        } else {
            verifyNoInteractions(instanceInfoService);
            assertSame(cached, instanceCheckService.getInstanceHealthBeatInfo(heartbeat));
            assertThrows(IllegalStateException.class, instanceCheckService::setup);
        }
        assertDoesNotThrow(instanceCheckService::close);
        verify(executor, times(2)).awaitTermination(5L, TimeUnit.SECONDS);
    }

    @Test
    void testInterruptedCloseRestoresInterruptAndRetainsStateWhileWorkIsRunning() throws InterruptedException {
        ScheduledThreadPoolExecutor executor = mock(ScheduledThreadPoolExecutor.class);
        ReflectionTestUtils.setField(instanceCheckService, "executor", executor);
        when(executor.awaitTermination(5L, TimeUnit.SECONDS)).thenThrow(new InterruptedException("Shutdown interrupted"));
        instanceCheckService.handleBeatInfo(heartbeat);
        InstanceInfoVO cached = instanceCheckService.getInstanceHealthBeatInfo(heartbeat);

        try {
            assertDoesNotThrow(instanceCheckService::close);

            assertTrue(Thread.currentThread().isInterrupted());
            verify(executor).shutdown();
            verify(executor).shutdownNow();
            assertSame(cached, instanceCheckService.getInstanceHealthBeatInfo(heartbeat));
            verifyNoInteractions(instanceInfoService);
        } finally {
            Thread.interrupted();
        }
    }

    private ScheduledThreadPoolExecutor startChecker() {
        when(instanceInfoService.list()).thenReturn(Collections.emptyList());
        instanceCheckService.setup();
        ScheduledThreadPoolExecutor executor = checkerExecutor();
        ownedExecutors.add(executor);
        return executor;
    }

    private ScheduledThreadPoolExecutor checkerExecutor() {
        return (ScheduledThreadPoolExecutor) ReflectionTestUtils.getField(instanceCheckService, "executor");
    }
}
