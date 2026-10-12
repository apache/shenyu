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
import org.apache.shenyu.protocol.tcp.TcpBootstrapServer;
import org.apache.shenyu.protocol.tcp.TcpServerConfiguration;
import org.apache.shenyu.protocol.tcp.UpstreamProvider;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedConstruction;

import java.io.IOException;
import java.lang.reflect.Field;
import java.net.ServerSocket;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
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
            assertFalse(UpstreamProvider.getSingleton().inCache(FIRST_SELECTOR));
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
    public void shouldApplyDifferentConfigurationAfterWaitingForCreation() throws Exception {
        TcpBootstrapFactory factorySpy = spy(factory);
        BootstrapServer original = mock(BootstrapServer.class);
        BootstrapServer replacement = mock(BootstrapServer.class);
        doReturn(replacement).when(factorySpy).createBootstrapServer(any(TcpServerConfiguration.class));
        CountDownLatch creationAwaited = new CountDownLatch(1);
        CompletableFuture<BootstrapServer> creation = spy(new CompletableFuture<BootstrapServer>());
        doAnswer(invocation -> {
            creationAwaited.countDown();
            return invocation.callRealMethod();
        }).when(creation).join();
        ConcurrentMap<String, CompletableFuture<BootstrapServer>> creations = getCreations();
        creations.put(FIRST_SELECTOR, creation);
        ExecutorService executor = Executors.newSingleThreadExecutor();
        try {
            final Future<Boolean> result = executor.submit(() -> factorySpy.createOrUpdateBootstrapServer(configuration(FIRST_SELECTOR, 9001)));
            assertTrue(creationAwaited.await(5, TimeUnit.SECONDS));
            factory.cache(configuration(FIRST_SELECTOR, 9000), original);
            creation.complete(original);
            assertTrue(result.get(5, TimeUnit.SECONDS));
            assertSame(replacement, factory.getCache(FIRST_SELECTOR));
            ArgumentCaptor<TcpServerConfiguration> captor = ArgumentCaptor.forClass(TcpServerConfiguration.class);
            verify(factorySpy).createBootstrapServer(captor.capture());
            assertEquals(9001, captor.getValue().getPort());
            verify(original).shutdown();
        } finally {
            creation.complete(original);
            creations.remove(FIRST_SELECTOR, creation);
            executor.shutdownNow();
            assertTrue(executor.awaitTermination(5, TimeUnit.SECONDS));
        }
    }

    @Test
    public void shouldSerializeReplacementAndRemovalForSameSelector() throws Exception {
        TcpBootstrapFactory factorySpy = spy(factory);
        BootstrapServer original = mock(BootstrapServer.class);
        BootstrapServer replacement = mock(BootstrapServer.class);
        factory.cache(configuration(FIRST_SELECTOR, 0), original);
        TcpServerConfiguration update = configuration(FIRST_SELECTOR, 0);
        update.getProps().setProperty("clientMaxConnections", "30");
        doReturn(replacement).when(factorySpy).createBootstrapServer(any(TcpServerConfiguration.class));
        CountDownLatch shutdownStarted = new CountDownLatch(1);
        CountDownLatch releaseShutdown = new CountDownLatch(1);
        doAnswer(invocation -> {
            shutdownStarted.countDown();
            assertTrue(releaseShutdown.await(5, TimeUnit.SECONDS));
            return null;
        }).when(original).shutdown();
        CountDownLatch removalStarted = new CountDownLatch(1);
        ExecutorService executor = Executors.newFixedThreadPool(2);
        try {
            final Future<Boolean> first = executor.submit(() -> factorySpy.createOrUpdateBootstrapServer(update));
            assertTrue(shutdownStarted.await(5, TimeUnit.SECONDS));
            final Future<Boolean> second = executor.submit(() -> {
                removalStarted.countDown();
                return factorySpy.removeAndShutdown(FIRST_SELECTOR);
            });
            assertTrue(removalStarted.await(5, TimeUnit.SECONDS));
            assertThrows(TimeoutException.class, () -> second.get(100, TimeUnit.MILLISECONDS));
            assertSame(original, factory.getCache(FIRST_SELECTOR));
            verify(original, times(1)).shutdown();
            releaseShutdown.countDown();
            assertTrue(first.get(5, TimeUnit.SECONDS));
            assertTrue(second.get(5, TimeUnit.SECONDS));
            assertNull(factory.getCache(FIRST_SELECTOR));
            verify(replacement).shutdown();
            verify(factorySpy, times(1)).createBootstrapServer(any(TcpServerConfiguration.class));
        } finally {
            releaseShutdown.countDown();
            executor.shutdownNow();
            assertTrue(executor.awaitTermination(5, TimeUnit.SECONDS));
        }
    }

    @Test
    public void shouldReplacePropertiesOnSamePort() throws IOException {
        TcpServerConfiguration configuration = configuration(FIRST_SELECTOR, getFreePort());
        configuration.setProps(null);
        factory.createOrUpdateBootstrapServer(configuration);
        BootstrapServer previous = factory.getCache(FIRST_SELECTOR);
        configuration.setProps(new Properties());
        configuration.getProps().setProperty("clientMaxConnections", "30");
        try (MockedConstruction<TcpBootstrapServer> servers = mockConstruction(TcpBootstrapServer.class)) {
            assertTrue(factory.createOrUpdateBootstrapServer(configuration));
            BootstrapServer replacement = factory.getCache(FIRST_SELECTOR);
            assertNotSame(previous, replacement);
            ArgumentCaptor<TcpServerConfiguration> captor = ArgumentCaptor.forClass(TcpServerConfiguration.class);
            verify(replacement).start(captor.capture());
            assertEquals("30", captor.getValue().getProps().getProperty("clientMaxConnections"));
            try (ServerSocket releasedPort = new ServerSocket(configuration.getPort())) {
                assertEquals(configuration.getPort(), releasedPort.getLocalPort());
            }
            assertFalse(factory.createOrUpdateBootstrapServer(configuration));
            assertEquals(1, servers.constructed().size());
            factory.removeAndShutdown(FIRST_SELECTOR);
            verify(replacement).shutdown();
        }
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    public void shouldRetainPreviousConfigurationAfterUpdateFailure(final boolean samePort) throws IOException {
        TcpServerConfiguration configuration = configuration(FIRST_SELECTOR, getFreePort());
        factory.createOrUpdateBootstrapServer(configuration);
        BootstrapServer previous = factory.getCache(FIRST_SELECTOR);
        UpstreamProvider upstreamProvider = UpstreamProvider.getSingleton();
        upstreamProvider.registerSelector("first-id", FIRST_SELECTOR);
        try (ServerSocket occupiedPort = new ServerSocket(0)) {
            TcpServerConfiguration update = configuration(FIRST_SELECTOR, samePort ? configuration.getPort() : occupiedPort.getLocalPort());
            if (samePort) {
                update.getProps().setProperty("workerGroupThreadCount", "invalid");
            }
            assertThrows(RuntimeException.class, () -> factory.createOrUpdateBootstrapServer(update));
            BootstrapServer retained = factory.getCache(FIRST_SELECTOR);
            assertNotNull(retained);
            if (samePort) {
                assertNotSame(previous, retained);
            } else {
                assertSame(previous, retained);
            }
            assertEquals(FIRST_SELECTOR, upstreamProvider.getSelectorName("first-id"));
            assertTrue(upstreamProvider.inCache(FIRST_SELECTOR));
            assertFalse(factory.createOrUpdateBootstrapServer(configuration));
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
        factory.cache(configuration(FIRST_SELECTOR, 0), blockingServer);
        factory.cache(configuration(SECOND_SELECTOR, 0), secondServer);

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
