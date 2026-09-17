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
import io.netty.handler.codec.mqtt.MqttVersion;
import io.netty.channel.Channel;
import io.netty.channel.ChannelHandlerContext;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.protocol.mqtt.repositories.ChannelRepository;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.apache.shenyu.protocol.mqtt.repositories.SubscribeRepository;
import org.apache.shenyu.protocol.mqtt.repositories.WillRepository;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.nio.charset.StandardCharsets;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.when;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * Test cases for {@link MqttTransportHandler}.
 */
@ExtendWith(MockitoExtension.class)
public final class MqttTransportHandlerTest {

    private static final String CLIENT_ID = "test-client";

    private static final String USER_NAME = "test-user";

    private static final String PASSWORD = "test-password";

    private static ChannelRepository channelRepository;

    @Mock
    private ChannelHandlerContext ctx;

    @Mock
    private Channel channel;

    @Mock
    private SubscribeRepository subscribeRepository;

    private MqttTransportHandler handler;

    private WillRepository willRepository;

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
    public void setUp2() {
        handler = new MqttTransportHandler();
        willRepository = new WillRepository();
        Singleton.INST.single(WillRepository.class, willRepository);
        Singleton.INST.single(SubscribeRepository.class, subscribeRepository);
        when(ctx.channel()).thenReturn(channel);
    }

    @AfterEach
    public void tearDown2() {
        Singleton.INST.single(WillRepository.class, new WillRepository());
        Singleton.INST.single(SubscribeRepository.class, new SubscribeRepository());
    }

    @Test
    public void testChannelInactiveFiresWillAndRemovesIt() throws Exception {
        byte[] willMessage = "sudden disconnect".getBytes();
        WillRepository.WillEntry will = new WillRepository.WillEntry("status/offline", willMessage, 1, true);
        willRepository.add(channel, will);

        // publishWill uses subscribeRepository to get target channels
        when(subscribeRepository.getChannelsByTopic("status/offline")).thenReturn(java.util.Collections.emptyList());

        handler.channelInactive(ctx);

        // will should be removed after firing
        assertThat(willRepository.get(channel), nullValue());
    }

    @Test
    public void testChannelInactiveDoesNothingWhenNoWill() throws Exception {
        handler.channelInactive(ctx);

        assertThat(willRepository.get(channel), nullValue());
    }

    @Test
    public void testChannelInactiveAfterDisconnectClearsWill() throws Exception {
        byte[] willMessage = "graceful close".getBytes();
        WillRepository.WillEntry will = new WillRepository.WillEntry("status/clean", willMessage, 0, false);
        willRepository.add(channel, will);

        // simulate graceful disconnect: remove will first, then channelInactive
        willRepository.remove(channel);
        handler.channelInactive(ctx);

        assertThat(willRepository.get(channel), nullValue());
    }
}
