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

package org.apache.shenyu.protocol.mqtt;

import io.netty.channel.ChannelFuture;
import io.netty.channel.EventLoopGroup;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.protocol.mqtt.repositories.ChannelRepository;
import org.apache.shenyu.protocol.mqtt.repositories.SubscribeRepository;
import org.apache.shenyu.protocol.mqtt.repositories.TopicRepository;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.net.BindException;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.time.Duration;
import java.util.concurrent.Callable;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link MqttBootstrapServer}.
 */
public final class MqttBootstrapServerTest {

    private MqttBootstrapServer server;

    @BeforeEach
    public void setUp() {
        server = new MqttBootstrapServer();
        MqttContext context = new MqttContext();
        context.setPort(0);
        context.setBossGroupThreadCount(1);
        context.setWorkerGroupThreadCount(1);
        context.setMaxPayloadSize(1024 * 1024);
        context.setUserName("test-user");
        context.setPassword("test-password");
        context.setLeakDetectorLevel("disabled");
    }

    @AfterEach
    public void tearDown() {
        server.shutdown();
        MqttContext context = new MqttContext();
        context.setPort(0);
        context.setBossGroupThreadCount(0);
        context.setWorkerGroupThreadCount(0);
        context.setMaxPayloadSize(0);
        context.setUserName(null);
        context.setPassword(null);
        context.setLeakDetectorLevel(null);
    }

    @Test
    public void initShouldRegisterAllRepositories() {
        server.init();

        assertNotNull(Singleton.INST.get(ChannelRepository.class));
        assertNotNull(Singleton.INST.get(SubscribeRepository.class));
        assertNotNull(Singleton.INST.get(TopicRepository.class));
    }

    @Test
    public void startAndShutdownShouldReleaseChannelAndEventLoops() {
        server.start();

        final ChannelFuture future = getField(server, "future", ChannelFuture.class);
        final EventLoopGroup bossGroup = getField(server, "bossGroup", EventLoopGroup.class);
        final EventLoopGroup workerGroup = getField(server, "workerGroup", EventLoopGroup.class);
        assertTrue(future.channel().isActive());

        server.shutdown();

        assertFalse(future.channel().isActive());
        await().atMost(Duration.ofSeconds(5)).until(bossGroup::isTerminated);
        await().atMost(Duration.ofSeconds(5)).until(workerGroup::isTerminated);
        assertNull(getField(server, "future", ChannelFuture.class));
        assertNull(getField(server, "bossGroup", EventLoopGroup.class));
        assertNull(getField(server, "workerGroup", EventLoopGroup.class));
    }

    @Test
    public void repeatedStartShouldKeepTheSameResources() {
        server.start();
        final ChannelFuture future = getField(server, "future", ChannelFuture.class);
        final EventLoopGroup bossGroup = getField(server, "bossGroup", EventLoopGroup.class);
        final EventLoopGroup workerGroup = getField(server, "workerGroup", EventLoopGroup.class);

        // Reuse the bound port so a second bind would fail instead of choosing another ephemeral port.
        new MqttContext().setPort(((InetSocketAddress) future.channel().localAddress()).getPort());
        server.start();

        assertSame(future, getField(server, "future", ChannelFuture.class));
        assertSame(bossGroup, getField(server, "bossGroup", EventLoopGroup.class));
        assertSame(workerGroup, getField(server, "workerGroup", EventLoopGroup.class));
        assertTrue(future.channel().isActive());
    }

    @Test
    public void failedBindShouldReleaseEventLoopsAndAllowRetry() throws Exception {
        final AtomicReference<EventLoopGroup> allocatedBoss = new AtomicReference<>();
        final AtomicReference<EventLoopGroup> allocatedWorker = new AtomicReference<>();
        server = new MqttBootstrapServer() {
            @Override
            public void shutdown() {
                allocatedBoss.set(getField(this, "bossGroup", EventLoopGroup.class));
                allocatedWorker.set(getField(this, "workerGroup", EventLoopGroup.class));
                super.shutdown();
            }
        };
        try (ServerSocket occupied = new ServerSocket(0)) {
            new MqttContext().setPort(occupied.getLocalPort());

            assertThrows(BindException.class, server::start);

            assertTrue(allocatedBoss.get().isTerminated());
            assertTrue(allocatedWorker.get().isTerminated());
            assertNull(getField(server, "future", ChannelFuture.class));
            assertNull(getField(server, "bossGroup", EventLoopGroup.class));
            assertNull(getField(server, "workerGroup", EventLoopGroup.class));
        }

        new MqttContext().setPort(0);
        server.start();
        assertTrue(getField(server, "future", ChannelFuture.class).channel().isActive());
    }

    @Test
    public void concurrentStartsShouldKeepTheSameResources() throws Exception {
        final CountDownLatch ready = new CountDownLatch(2);
        final CountDownLatch start = new CountDownLatch(1);
        Callable<ChannelFuture> task = () -> {
            ready.countDown();
            assertTrue(start.await(5, TimeUnit.SECONDS));
            server.start();
            return getField(server, "future", ChannelFuture.class);
        };
        ExecutorService executor = Executors.newFixedThreadPool(2);
        try {
            final Future<ChannelFuture> first = executor.submit(task);
            final Future<ChannelFuture> second = executor.submit(task);
            assertTrue(ready.await(5, TimeUnit.SECONDS));
            start.countDown();

            assertSame(first.get(10, TimeUnit.SECONDS), second.get(10, TimeUnit.SECONDS));
            assertTrue(server.isRunning());
            final EventLoopGroup bossGroup = getField(server, "bossGroup", EventLoopGroup.class);
            final EventLoopGroup workerGroup = getField(server, "workerGroup", EventLoopGroup.class);
            server.shutdown();
            assertTrue(bossGroup.isTerminated());
            assertTrue(workerGroup.isTerminated());
        } finally {
            start.countDown();
            executor.shutdownNow();
            assertTrue(executor.awaitTermination(10, TimeUnit.SECONDS));
        }
    }

    @Test
    public void startShouldReplaceClosedListenerAndReleaseOldEventLoops() {
        server.start();
        final ChannelFuture previous = getField(server, "future", ChannelFuture.class);
        final EventLoopGroup oldBoss = getField(server, "bossGroup", EventLoopGroup.class);
        final EventLoopGroup oldWorker = getField(server, "workerGroup", EventLoopGroup.class);
        new MqttContext().setPort(((InetSocketAddress) previous.channel().localAddress()).getPort());
        previous.channel().close().syncUninterruptibly();
        assertFalse(server.isRunning());

        server.start();

        assertTrue(server.isRunning());
        assertNotSame(previous, getField(server, "future", ChannelFuture.class));
        assertTrue(oldBoss.isTerminated());
        assertTrue(oldWorker.isTerminated());
    }

    private <T> T getField(final Object target, final String name, final Class<T> type) {
        try {
            Field field = MqttBootstrapServer.class.getDeclaredField(name);
            field.setAccessible(true);
            return type.cast(field.get(target));
        } catch (ReflectiveOperationException e) {
            throw new AssertionError(e);
        }
    }
}
