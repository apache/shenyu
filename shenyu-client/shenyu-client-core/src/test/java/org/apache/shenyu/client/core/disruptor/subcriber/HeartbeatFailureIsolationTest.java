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

package org.apache.shenyu.client.core.disruptor.subcriber;

import org.apache.shenyu.common.utils.SystemInfoUtils;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.List;
import java.util.concurrent.RunnableScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

/**
 * Verify periodic heartbeats survive repository failures.
 */
public final class HeartbeatFailureIsolationTest {

    @Test
    @SuppressWarnings("unchecked")
    public void failureDoesNotSuppressOtherUrisOrSubsequentTicks() {
        ShenyuClientRegisterRepository repository = mock(ShenyuClientRegisterRepository.class);
        ShenyuClientURIExecutorSubscriber subscriber = new ShenyuClientURIExecutorSubscriber(repository);
        ScheduledThreadPoolExecutor executor = (ScheduledThreadPoolExecutor) ReflectionTestUtils.getField(subscriber, "executor");
        List<URIRegisterDTO> uris = (List<URIRegisterDTO>) ReflectionTestUtils.getField(subscriber, "uris");
        URIRegisterDTO failing = URIRegisterDTO.builder().host("localhost").port(18080).build();
        URIRegisterDTO healthy = URIRegisterDTO.builder().host("localhost").port(18081).build();
        doThrow(new IllegalStateException("admin unavailable")).when(repository).sendHeartbeat(failing);
        try (MockedStatic<SystemInfoUtils> ignored = mockStatic(SystemInfoUtils.class)) {
            uris.clear();
            uris.add(failing);
            uris.add(healthy);
            subscriber.start();
            RunnableScheduledFuture<?> task = (RunnableScheduledFuture<?>) executor.getQueue().iterator().next();
            for (int tick = 0; tick < 2; tick++) {
                executor.getQueue().remove(task);
                task.run();
                assertFalse(task.isDone(), "Periodic task must remain schedulable after a failed heartbeat");
            }
            verify(repository, times(2)).sendHeartbeat(failing);
            verify(repository, times(2)).sendHeartbeat(healthy);
        } finally {
            executor.shutdownNow();
            uris.clear();
        }
    }
}
