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

import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import io.netty.channel.embedded.EmbeddedChannel;
import io.netty.handler.codec.mqtt.MqttConnectMessage;
import io.netty.handler.codec.mqtt.MqttConnectPayload;
import io.netty.handler.codec.mqtt.MqttConnectVariableHeader;
import io.netty.handler.codec.mqtt.MqttFixedHeader;
import io.netty.handler.codec.mqtt.MqttMessageType;
import io.netty.handler.codec.mqtt.MqttPublishMessage;
import io.netty.handler.codec.mqtt.MqttPublishVariableHeader;
import io.netty.handler.codec.mqtt.MqttQoS;
import io.netty.handler.codec.mqtt.MqttTopicSubscription;
import io.netty.handler.codec.mqtt.MqttVersion;
import io.netty.channel.Channel;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelInboundHandlerAdapter;
import io.netty.util.CharsetUtil;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.protocol.mqtt.repositories.ChannelRepository;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.apache.shenyu.protocol.mqtt.repositories.SubscribeRepository;
import org.apache.shenyu.protocol.mqtt.repositories.WillRepository;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.apache.shenyu.protocol.mqtt.repositories.SubscribeRepository;
import org.apache.shenyu.protocol.mqtt.utils.MqttPacketIdGenerator;
import org.awaitility.core.ThrowingRunnable;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.Collections;
import java.util.concurrent.atomic.AtomicInteger;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.when;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link MqttTransportHandler}.
 */
@ExtendWith(MockitoExtension.class)
public final class MqttTransportHandlerTest {

    private static final String TOPIC = "test/topic";

    private static final String CLIENT_ID = "test-client";

    private static final String USER_NAME = "test-user";

    private static final String PASSWORD = "test-password";

    private static ChannelRepository channelRepository;

    private static final Duration TIMEOUT = Duration.ofSeconds(5);

    private static final Duration POLL_INTERVAL = Duration.ofMillis(10);

    /**
     * The repositories keep their state in static maps shared with the other test classes of this module,
     * so they are registered before every test and released again afterwards.
     */
    private static final ChannelRepository CHANNEL_REPOSITORY = new ChannelRepository();

    private static final SubscribeRepository SUBSCRIBE_REPOSITORY = new SubscribeRepository();

    private EmbeddedChannel registeredChannel;

    @Mock
    private ChannelHandlerContext ctx;

    @Mock
    private Channel channel;

    @Mock
    private SubscribeRepository subscribeRepository;

    private MqttTransportHandler handler;

    private WillRepository willRepository;

    @BeforeEach
    public void setUp() {
        Singleton.INST.single(ChannelRepository.class, CHANNEL_REPOSITORY);
        Singleton.INST.single(SubscribeRepository.class, SUBSCRIBE_REPOSITORY);
        new MqttContext().setUserName(USER_NAME);
        new MqttContext().setPassword(PASSWORD);

        registeredChannel = new EmbeddedChannel();
        CHANNEL_REPOSITORY.add(registeredChannel, CLIENT_ID);
        SUBSCRIBE_REPOSITORY.add(registeredChannel,
                Collections.singletonList(new MqttTopicSubscription(TOPIC, MqttQoS.AT_LEAST_ONCE)));
        awaitAssert(() -> {
            assertEquals(CLIENT_ID, CHANNEL_REPOSITORY.get(registeredChannel));
            assertTrue(SUBSCRIBE_REPOSITORY.get(TOPIC).containsKey(registeredChannel));
        });
    }

    @AfterEach
    public void tearDown() {
        MqttPacketIdGenerator.remove(registeredChannel);
        CHANNEL_REPOSITORY.remove(registeredChannel);
        SUBSCRIBE_REPOSITORY.remove(registeredChannel);
        awaitAssert(() -> assertFalse(SUBSCRIBE_REPOSITORY.get(TOPIC).containsKey(registeredChannel)));

        registeredChannel.finishAndReleaseAll();

        new MqttContext().setUserName(null);
        new MqttContext().setPassword(null);
    }

    @Test
    public void channelReadReleasesInboundPublishMessage() {
        EmbeddedChannel channel = new EmbeddedChannel(new MqttTransportHandler());
        MqttFixedHeader fixedHeader = new MqttFixedHeader(MqttMessageType.PUBLISH, false, MqttQoS.AT_MOST_ONCE, false, 0);
        MqttPublishVariableHeader variableHeader = new MqttPublishVariableHeader(TOPIC, 1);
        MqttPublishMessage message = new MqttPublishMessage(fixedHeader, variableHeader,
                Unpooled.copiedBuffer("hello", CharsetUtil.UTF_8));
        ByteBuf payload = message.payload();

        channel.writeInbound(message);

        assertEquals(0, payload.refCnt());
        channel.finishAndReleaseAll();
    }

    @Test
    public void duplicateConnectCleansUpChannelRepository() {
        EmbeddedChannel channel = new EmbeddedChannel(new MqttTransportHandler());

        channel.writeInbound(connectMessage());
        assertEquals(CLIENT_ID, CHANNEL_REPOSITORY.get(channel));

        channel.writeInbound(connectMessage());
        channel.runPendingTasks();

        assertFalse(channel.isActive());
        assertNull(CHANNEL_REPOSITORY.get(channel));
        channel.finishAndReleaseAll();
    }

    @Test
    public void abruptChannelCloseCleansUpChannelRepository() {
        EmbeddedChannel channel = new EmbeddedChannel(new MqttTransportHandler());

        channel.writeInbound(connectMessage());
        assertEquals(CLIENT_ID, CHANNEL_REPOSITORY.get(channel));

        channel.close();
        channel.runPendingTasks();

        assertFalse(channel.isActive());
        assertNull(CHANNEL_REPOSITORY.get(channel));
        channel.finishAndReleaseAll();
    }

    @Test
    public void nonMqttMessageClosesConnectedChannel() {
        EmbeddedChannel channel = new EmbeddedChannel(new MqttTransportHandler());

        channel.writeInbound(connectMessage());
        assertEquals(CLIENT_ID, CHANNEL_REPOSITORY.get(channel));

        channel.writeInbound("not-a-mqtt-message");

        assertFalse(channel.isActive());
        assertNull(CHANNEL_REPOSITORY.get(channel));
        channel.finishAndReleaseAll();
    }

    @Test
    public void testOperationCompleteCleansRepositoriesOnClose() throws Exception {
        assertEquals(1, MqttPacketIdGenerator.next(registeredChannel));

        new MqttTransportHandler().operationComplete(registeredChannel.closeFuture());

        awaitAssert(() -> assertNull(CHANNEL_REPOSITORY.get(registeredChannel)));
        awaitAssert(() -> assertFalse(SUBSCRIBE_REPOSITORY.get(TOPIC).containsKey(registeredChannel)));
        assertEquals(1, MqttPacketIdGenerator.next(registeredChannel));
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

    /**
     * The repositories mutate their state asynchronously on the common pool,
     * so assertions are retried until the mutation becomes visible.
     *
     * @param assertion assertion to retry
     */
    private void awaitAssert(final ThrowingRunnable assertion) {
        await().atMost(TIMEOUT).pollInterval(POLL_INTERVAL).untilAsserted(assertion);
    }

    @BeforeAll
    static void setUpAll() {
        channelRepository = new ChannelRepository();
        Singleton.INST.single(ChannelRepository.class, channelRepository);
        new MqttContext().setUserName(USER_NAME);
        new MqttContext().setPassword(PASSWORD);
    }

    @AfterAll
    static void tearDownAll() {
        new MqttContext().setUserName(null);
        new MqttContext().setPassword(null);
    }

    @Test
    public void channelInactiveIsPropagatedOnlyOnce() {
        AtomicInteger fired = new AtomicInteger();
        EmbeddedChannel channel = new EmbeddedChannel(new MqttTransportHandler(), new ChannelInboundHandlerAdapter() {
            @Override
            public void channelInactive(final ChannelHandlerContext context) throws Exception {
                fired.incrementAndGet();
                super.channelInactive(context);
            }
        });

        channel.close();
        channel.runPendingTasks();

        assertEquals(1, fired.get());
        channel.finishAndReleaseAll();
    }

    @BeforeEach
    public void setUpEach() {
        handler = new MqttTransportHandler();
        willRepository = new WillRepository();
        Singleton.INST.single(WillRepository.class, willRepository);
        Singleton.INST.single(SubscribeRepository.class, subscribeRepository);
        // shared stub: only the tests driving the handler with a mock context use it
        lenient().when(ctx.channel()).thenReturn(channel);
    }

    @AfterEach
    public void tearDownEach() {
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
