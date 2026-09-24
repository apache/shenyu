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

import org.apache.shenyu.client.core.shutdown.ShenyuClientShutdownHook;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.net.ServerSocket;
import java.time.Duration;
import java.util.List;
import java.util.Objects;
import java.util.Properties;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTimeoutPreemptively;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

/**
 * Readiness waits must not block subsequent URI registration indefinitely.
 */
public class UriReadinessTimeoutTest {

    private final ShenyuClientRegisterRepository repository = mock(ShenyuClientRegisterRepository.class);

    private ShenyuClientURIExecutorSubscriber subscriber;

    @AfterEach
    public void cleanup() {
        if (Objects.nonNull(subscriber)) {
            ((ScheduledThreadPoolExecutor) ReflectionTestUtils.getField(subscriber, "executor")).shutdownNow();
        }
    }

    @Test
    public void testUnreachableUriDoesNotBlockNextUri() throws Exception {
        subscriber = new ShenyuClientURIExecutorSubscriber(repository, 100);
        ShenyuClientShutdownHook.set(repository, new Properties());
        int closedPort;
        try (ServerSocket closed = new ServerSocket(0)) {
            closedPort = closed.getLocalPort();
        }
        try (ServerSocket ready = new ServerSocket(0)) {
            URIRegisterDTO unavailable = uri(closedPort);
            URIRegisterDTO available = uri(ready.getLocalPort());
            assertTimeoutPreemptively(Duration.ofSeconds(3), () -> subscriber.executor(List.of(unavailable, available)));
            verify(repository, never()).persistURI(unavailable);
            verify(repository).persistURI(available);
        }
    }

    @Test
    public void testInterruptionStopsWaitingAndPreservesFlag() throws Exception {
        subscriber = new ShenyuClientURIExecutorSubscriber(repository, 30000);
        int closedPort;
        try (ServerSocket closed = new ServerSocket(0)) {
            closedPort = closed.getLocalPort();
        }
        URIRegisterDTO unavailable = uri(closedPort);
        AtomicBoolean interrupted = new AtomicBoolean();
        CountDownLatch started = new CountDownLatch(1);
        Thread worker = new Thread(() -> {
            started.countDown();
            subscriber.executor(List.of(unavailable));
            interrupted.set(Thread.currentThread().isInterrupted());
        });
        worker.start();
        try {
            assertTrue(started.await(1, TimeUnit.SECONDS));
            worker.interrupt();
            worker.join(2000);
            assertFalse(worker.isAlive());
            assertTrue(interrupted.get());
            verify(repository, never()).persistURI(unavailable);
        } finally {
            worker.interrupt();
            worker.join(2000);
        }
    }

    private URIRegisterDTO uri(final int port) {
        return URIRegisterDTO.builder().host("127.0.0.1").port(port).rpcType("http").contextPath("/readiness").build();
    }
}
