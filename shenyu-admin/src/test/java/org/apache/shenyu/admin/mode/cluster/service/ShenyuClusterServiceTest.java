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
import org.apache.shenyu.admin.service.impl.InstanceCheckService;
import org.apache.shenyu.admin.service.impl.UpstreamCheckService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test for {@link ShenyuClusterService}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class ShenyuClusterServiceTest {

    static {
        System.setProperty("net.bytebuddy.experimental", "true");
    }

    @Mock
    private ClusterSelectMasterService selectMasterService;

    @Mock
    private UpstreamCheckService upstreamCheckService;

    @Mock
    private InstanceCheckService instanceCheckService;

    private ClusterProperties clusterProperties;

    private ShenyuClusterService clusterService;

    @BeforeEach
    public void setUp() {
        clusterProperties = new ClusterProperties();
        clusterProperties.setSelectPeriod(1L);
        clusterService = new ShenyuClusterService(
                selectMasterService,
                upstreamCheckService,
                instanceCheckService,
                clusterProperties);
    }

    @AfterEach
    public void tearDown() {
        clusterService.shutdown();
    }

    @Test
    public void testMasterSelectionContinuesAfterException() throws InterruptedException {
        CountDownLatch attempts = new CountDownLatch(2);
        when(selectMasterService.selectMaster(anyString(), anyString(), anyString())).thenAnswer(invocation -> {
            attempts.countDown();
            if (attempts.getCount() == 1) {
                throw new RuntimeException("simulated select error");
            }
            return false;
        });

        clusterService.start("127.0.0.1", 9195, "/shenyu");

        assertTrue(attempts.await(5, TimeUnit.SECONDS), "Periodic master selection must continue after an exception");
        clusterService.shutdown();

        verify(selectMasterService, atLeast(2)).selectMaster("127.0.0.1", "9195", "/shenyu");
        verify(selectMasterService, atLeast(2)).releaseMaster();
        verify(upstreamCheckService, never()).setup();
        verify(instanceCheckService, never()).setup();
    }

    @Test
    public void testMasterRenewalExceptionCleansUpAndRetries() throws InterruptedException {
        CountDownLatch attempts = new CountDownLatch(2);
        AtomicInteger invocationCount = new AtomicInteger();

        when(selectMasterService.selectMaster(anyString(), anyString(), anyString())).thenAnswer(invocation -> {
            attempts.countDown();
            int count = invocationCount.incrementAndGet();
            return count == 1;
        });

        when(selectMasterService.checkMasterStatus()).thenAnswer(invocation -> {
            throw new IllegalStateException("simulated lock lost");
        });

        clusterService.start("127.0.0.1", 9195, "/shenyu");

        assertTrue(attempts.await(5, TimeUnit.SECONDS), "Master selection should retry on next period after renewal failure");
        clusterService.shutdown();

        verify(upstreamCheckService).setup();
        verify(instanceCheckService).setup();
        verify(upstreamCheckService).close();
        verify(instanceCheckService).close();
        verify(selectMasterService, atLeast(2)).releaseMaster();
    }

    @Test
    public void testSelectMasterFailureWaitsForNextPeriod() throws InterruptedException {
        CountDownLatch attempts = new CountDownLatch(2);
        when(selectMasterService.selectMaster(anyString(), anyString(), anyString())).thenAnswer(invocation -> {
            attempts.countDown();
            return false;
        });

        clusterService.start("127.0.0.1", 9195, "/shenyu");

        assertTrue(attempts.await(5, TimeUnit.SECONDS));
        clusterService.shutdown();

        verify(selectMasterService, atLeast(2)).selectMaster("127.0.0.1", "9195", "/shenyu");
        verify(upstreamCheckService, never()).setup();
        verify(instanceCheckService, never()).setup();
        verify(upstreamCheckService, never()).close();
        verify(instanceCheckService, never()).close();
    }

    @Test
    public void testShutdown() {
        assertDoesNotThrow(() -> clusterService.shutdown());
    }
}
