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
import java.time.Duration;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link MqttBootstrapServer}.
 */
public final class MqttBootstrapServerTest {

    @BeforeEach
    public void setUp() {
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
        MqttBootstrapServer server = new MqttBootstrapServer();

        server.init();

        assertNotNull(Singleton.INST.get(ChannelRepository.class));
        assertNotNull(Singleton.INST.get(SubscribeRepository.class));
        assertNotNull(Singleton.INST.get(TopicRepository.class));
    }

    @Test
    public void startAndShutdownShouldReleaseChannelAndEventLoops() throws Exception {
        MqttBootstrapServer server = new MqttBootstrapServer();

        server.start();

        ChannelFuture future = getField(server, "future", ChannelFuture.class);
        assertTrue(future.channel().isActive());

        server.shutdown();

        assertFalse(future.channel().isActive());
        EventLoopGroup bossGroup = getField(server, "bossGroup", EventLoopGroup.class);
        EventLoopGroup workerGroup = getField(server, "workerGroup", EventLoopGroup.class);
        await().atMost(Duration.ofSeconds(5)).until(bossGroup::isTerminated);
        await().atMost(Duration.ofSeconds(5)).until(workerGroup::isTerminated);
    }

    private <T> T getField(final Object target, final String name, final Class<T> type) throws Exception {
        Field field = MqttBootstrapServer.class.getDeclaredField(name);
        field.setAccessible(true);
        return type.cast(field.get(target));
    }
}
