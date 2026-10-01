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
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test for {@link ShenyuClusterService}.
 */
public final class ShenyuClusterServiceTest {

    @Test
    public void testMasterSelectionContinuesAfterFailure() throws InterruptedException {
        ClusterSelectMasterService selectMasterService = mock(ClusterSelectMasterService.class);
        UpstreamCheckService upstreamCheckService = mock(UpstreamCheckService.class);
        InstanceCheckService instanceCheckService = mock(InstanceCheckService.class);
        ClusterProperties clusterProperties = new ClusterProperties();
        clusterProperties.setSelectPeriod(1L);
        ShenyuClusterService clusterService = new ShenyuClusterService(selectMasterService, upstreamCheckService,
                instanceCheckService, clusterProperties);
        AtomicInteger attempts = new AtomicInteger();
        CountDownLatch attemptsFinished = new CountDownLatch(2);
        when(selectMasterService.selectMaster("127.0.0.1", "9195", "/shenyu")).thenAnswer(invocation -> {
            if (attempts.incrementAndGet() == 1) {
                throw new IllegalStateException("temporary lock failure");
            }
            return false;
        });
        when(selectMasterService.releaseMaster()).thenAnswer(invocation -> {
            attemptsFinished.countDown();
            return true;
        });

        ScheduledExecutorService executorService = (ScheduledExecutorService) ReflectionTestUtils.getField(clusterService, "executorService");
        try {
            clusterService.startSelectMasterTask("127.0.0.1", "9195", "/shenyu");

            assertTrue(attemptsFinished.await(5, TimeUnit.SECONDS));
            verify(selectMasterService, atLeast(2)).selectMaster("127.0.0.1", "9195", "/shenyu");
            verify(selectMasterService, times(2)).releaseMaster();
            verify(upstreamCheckService, times(1)).close();
            verify(instanceCheckService, times(1)).close();
        } finally {
            executorService.shutdownNow();
        }
    }
}
