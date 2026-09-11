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

package org.apache.shenyu.plugin.tcp.handler;

import org.apache.shenyu.protocol.tcp.BootstrapServer;
import org.apache.shenyu.protocol.tcp.TcpServerConfiguration;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.lang.reflect.Field;
import java.net.ServerSocket;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public final class TcpBootstrapFactoryTest {

    private static final String FIRST_SELECTOR = "first";

    private static final String SECOND_SELECTOR = "second";

    private final TcpBootstrapFactory factory = TcpBootstrapFactory.getSingleton();

    @BeforeEach
    public void setUp() {
        factory.clearCache();
    }

    @AfterEach
    public void tearDown() {
        factory.clearCache();
    }

    @Test
    public void shouldCreateServerOnlyOnceForConcurrentSameSelector() throws Exception {
        int threadCount = 16;
        TcpServerConfiguration configuration = configuration(FIRST_SELECTOR, getFreePort());
        ExecutorService executor = Executors.newFixedThreadPool(threadCount);
        CountDownLatch ready = new CountDownLatch(threadCount);
        CountDownLatch start = new CountDownLatch(1);
        List<Future<Boolean>> results = new ArrayList<>();
        try {
            for (int index = 0; index < threadCount; index++) {
                results.add(executor.submit(() -> {
                    ready.countDown();
                    start.await();
                    return factory.createBootstrapServerIfAbsent(configuration);
                }));
            }
            assertTrue(ready.await(5, TimeUnit.SECONDS));
            start.countDown();
            int createdCount = 0;
            for (Future<Boolean> result : results) {
                if (result.get(30, TimeUnit.SECONDS)) {
                    createdCount++;
                }
            }
            assertEquals(1, createdCount);
            assertNotNull(factory.getCache(FIRST_SELECTOR));
        } finally {
            start.countDown();
            executor.shutdownNow();
        }
    }

    @Test
    public void shouldAllowRetryAfterCreationFailure() throws IOException {
        TcpServerConfiguration configuration;
        try (ServerSocket occupiedPort = new ServerSocket(0)) {
            configuration = configuration(FIRST_SELECTOR, occupiedPort.getLocalPort());
            assertThrows(RuntimeException.class, () -> factory.createBootstrapServerIfAbsent(configuration));
            assertNull(factory.getCache(FIRST_SELECTOR));
        }

        configuration.setPort(getFreePort());
        assertTrue(factory.createBootstrapServerIfAbsent(configuration));
        assertNotNull(factory.getCache(FIRST_SELECTOR));
    }

    @Test
    public void shouldUnwrapFailureFromExistingCreation() throws Exception {
        IllegalStateException failure = new IllegalStateException("creation failed");
        CompletableFuture<BootstrapServer> failedCreation = new CompletableFuture<>();
        failedCreation.completeExceptionally(failure);
        ConcurrentMap<String, CompletableFuture<BootstrapServer>> creations = getCreations();
        creations.put(FIRST_SELECTOR, failedCreation);
        try {
            IllegalStateException actual = assertThrows(IllegalStateException.class,
                    () -> factory.createBootstrapServerIfAbsent(configuration(FIRST_SELECTOR, 0)));
            assertSame(failure, actual);
        } finally {
            creations.remove(FIRST_SELECTOR, failedCreation);
        }
    }

    @Test
    public void shouldNotBlockDifferentSelectorRemovalDuringShutdown() throws Exception {
        BootstrapServer blockingServer = mock(BootstrapServer.class);
        BootstrapServer secondServer = mock(BootstrapServer.class);
        CountDownLatch shutdownStarted = new CountDownLatch(1);
        CountDownLatch releaseShutdown = new CountDownLatch(1);
        doAnswer(invocation -> {
            shutdownStarted.countDown();
            assertTrue(releaseShutdown.await(5, TimeUnit.SECONDS));
            return null;
        }).when(blockingServer).shutdown();
        factory.cache(FIRST_SELECTOR, blockingServer);
        factory.cache(SECOND_SELECTOR, secondServer);

        ExecutorService executor = Executors.newFixedThreadPool(2);
        try {
            final Future<Boolean> firstResult = executor.submit(() -> factory.removeAndShutdown(FIRST_SELECTOR));
            assertTrue(shutdownStarted.await(5, TimeUnit.SECONDS));
            Future<Boolean> secondResult = executor.submit(() -> factory.removeAndShutdown(SECOND_SELECTOR));
            assertTrue(secondResult.get(5, TimeUnit.SECONDS));
            verify(secondServer).shutdown();
            releaseShutdown.countDown();
            assertTrue(firstResult.get(5, TimeUnit.SECONDS));
        } finally {
            releaseShutdown.countDown();
            executor.shutdownNow();
        }
    }

    private static TcpServerConfiguration configuration(final String selectorName, final int port) {
        TcpServerConfiguration configuration = new TcpServerConfiguration();
        configuration.setPluginSelectorName(selectorName);
        configuration.setPort(port);
        return configuration;
    }

    private static int getFreePort() throws IOException {
        try (ServerSocket socket = new ServerSocket(0)) {
            return socket.getLocalPort();
        }
    }

    @SuppressWarnings("unchecked")
    private ConcurrentMap<String, CompletableFuture<BootstrapServer>> getCreations() throws Exception {
        Field field = TcpBootstrapFactory.class.getDeclaredField("creations");
        field.setAccessible(true);
        return (ConcurrentMap<String, CompletableFuture<BootstrapServer>>) field.get(factory);
    }
}
