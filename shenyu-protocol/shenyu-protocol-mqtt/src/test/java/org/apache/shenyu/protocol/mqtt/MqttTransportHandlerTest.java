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

import io.netty.channel.embedded.EmbeddedChannel;
import io.netty.handler.codec.mqtt.MqttConnectMessage;
import io.netty.handler.codec.mqtt.MqttConnectPayload;
import io.netty.handler.codec.mqtt.MqttConnectVariableHeader;
import io.netty.handler.codec.mqtt.MqttFixedHeader;
import io.netty.handler.codec.mqtt.MqttMessageType;
import io.netty.handler.codec.mqtt.MqttQoS;
import io.netty.handler.codec.mqtt.MqttTopicSubscription;
import io.netty.handler.codec.mqtt.MqttVersion;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.protocol.mqtt.repositories.ChannelRepository;
import org.apache.shenyu.protocol.mqtt.repositories.SubscribeRepository;
import org.apache.shenyu.protocol.mqtt.utils.MqttPacketIdGenerator;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.concurrent.TimeUnit;
import java.nio.charset.StandardCharsets;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link MqttTransportHandler}.
 */
public final class MqttTransportHandlerTest {

    private static final String TOPIC = "test/topic";

    private static final String CLIENT_ID = "test-client";

    private static final String USER_NAME = "test-user";

    private static final String PASSWORD = "test-password";

    private final SubscribeRepository subscribeRepository = new SubscribeRepository();

    private static ChannelRepository channelRepository;

    private EmbeddedChannel channel;

    @BeforeAll
    static void setUp() {
        channelRepository = new ChannelRepository();
        Singleton.INST.single(ChannelRepository.class, channelRepository);
        new MqttContext().setUserName(USER_NAME);
        new MqttContext().setPassword(PASSWORD);
    }

    @AfterAll
    static void tearDown() {
        new MqttContext().setUserName(null);
        new MqttContext().setPassword(null);
    }

    @Test
    public void duplicateConnectCleansUpChannelRepository() {
        EmbeddedChannel channel = new EmbeddedChannel(new MqttTransportHandler());

        channel.writeInbound(connectMessage());
        assertEquals(CLIENT_ID, channelRepository.get(channel));

        channel.writeInbound(connectMessage());
        channel.runPendingTasks();

        assertFalse(channel.isActive());
        assertNull(channelRepository.get(channel));
        channel.finishAndReleaseAll();
    }

    @Test
    public void abruptChannelCloseCleansUpChannelRepository() {
        EmbeddedChannel channel = new EmbeddedChannel(new MqttTransportHandler());

        channel.writeInbound(connectMessage());
        assertEquals(CLIENT_ID, channelRepository.get(channel));

        channel.close();
        channel.runPendingTasks();

        assertFalse(channel.isActive());
        assertNull(channelRepository.get(channel));
        channel.finishAndReleaseAll();
    }

    private MqttConnectMessage connectMessage() {
        MqttFixedHeader fixedHeader = new MqttFixedHeader(MqttMessageType.CONNECT, false, MqttQoS.AT_MOST_ONCE, false, 0);
        MqttConnectVariableHeader variableHeader = new MqttConnectVariableHeader(
                MqttVersion.MQTT_3_1_1.protocolName(), MqttVersion.MQTT_3_1_1.protocolLevel(),
                true, true, false, 0, false, false, 60);
        MqttConnectPayload payload = new MqttConnectPayload(CLIENT_ID, null, null,
                USER_NAME, PASSWORD.getBytes(StandardCharsets.UTF_8));
        return new MqttConnectMessage(fixedHeader, variableHeader, payload);
    }

    @BeforeEach
    public void setUp() {
        channel = new EmbeddedChannel();
        Singleton.INST.single(ChannelRepository.class, channelRepository);
        Singleton.INST.single(SubscribeRepository.class, subscribeRepository);
        channelRepository.add(channel, CLIENT_ID);
        subscribeRepository.add(channel, Collections.singletonList(new MqttTopicSubscription(TOPIC, MqttQoS.AT_LEAST_ONCE)));
        await().atMost(5, TimeUnit.SECONDS).untilAsserted(() -> {
            assertEquals(CLIENT_ID, channelRepository.get(channel));
            assertTrue(!subscribeRepository.get(TOPIC).isEmpty());
        });
    }

    @AfterEach
    public void tearDown() {
        channel.finishAndReleaseAll();
        channel.close();
    }

    @Test
    public void testOperationCompleteCleansRepositoriesOnClose() throws Exception {
        assertEquals(1, MqttPacketIdGenerator.next(channel));

        new MqttTransportHandler().operationComplete(channel.closeFuture());

        await().atMost(5, TimeUnit.SECONDS).untilAsserted(() -> {
            assertNull(channelRepository.get(channel));
            assertTrue(subscribeRepository.get(TOPIC).isEmpty());
        });
        assertEquals(1, MqttPacketIdGenerator.next(channel));
    }
}
