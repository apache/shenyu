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
import io.netty.channel.Channel;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelInboundHandlerAdapter;
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
import io.netty.util.CharsetUtil;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.protocol.mqtt.repositories.ChannelRepository;
import org.apache.shenyu.protocol.mqtt.repositories.SubscribeRepository;
import org.apache.shenyu.protocol.mqtt.repositories.WillRepository;
import org.apache.shenyu.protocol.mqtt.utils.MqttPacketIdGenerator;
import org.awaitility.core.ThrowingRunnable;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.Collections;
import java.util.concurrent.atomic.AtomicInteger;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link MqttTransportHandler}.
 */
@ExtendWith(MockitoExtension.class)
public final class MqttTransportHandlerTest {

    private static final String TOPIC = "test/topic";

    private static final String WILL_TOPIC = "status/offline";

    private static final String CLIENT_ID = "test-client";

    private static final String USER_NAME = "test-user";

    private static final String PASSWORD = "test-password";

    private static final Duration TIMEOUT = Duration.ofSeconds(5);

    private static final Duration POLL_INTERVAL = Duration.ofMillis(10);

    /**
     * The repositories keep their state in static maps shared with the other test classes of this module,
     * so they are registered before every test and released again afterwards.
     */
    private static final ChannelRepository CHANNEL_REPOSITORY = new ChannelRepository();

    private static final SubscribeRepository SUBSCRIBE_REPOSITORY = new SubscribeRepository();

    @Mock
    private ChannelHandlerContext ctx;

    @Mock
    private Channel channel;

    private MqttTransportHandler handler;

    private WillRepository willRepository;

    private EmbeddedChannel registeredChannel;

    @BeforeEach
    public void setUp() {
        handler = new MqttTransportHandler();
        willRepository = new WillRepository();
        Singleton.INST.single(WillRepository.class, willRepository);
        Singleton.INST.single(ChannelRepository.class, CHANNEL_REPOSITORY);
        Singleton.INST.single(SubscribeRepository.class, SUBSCRIBE_REPOSITORY);
        new MqttContext().setUserName(USER_NAME);
        new MqttContext().setPassword(PASSWORD);
        // only the will tests drive the handler through a mocked context
        lenient().when(ctx.channel()).thenReturn(channel);

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
        willRepository.remove(channel);
        SUBSCRIBE_REPOSITORY.remove(channel);
        MqttPacketIdGenerator.remove(registeredChannel);
        CHANNEL_REPOSITORY.remove(registeredChannel);
        SUBSCRIBE_REPOSITORY.remove(registeredChannel);
        awaitAssert(() -> {
            assertFalse(SUBSCRIBE_REPOSITORY.get(TOPIC).containsKey(registeredChannel));
            assertTrue(SUBSCRIBE_REPOSITORY.get(WILL_TOPIC).isEmpty());
        });

        registeredChannel.finishAndReleaseAll();
        new MqttContext().setUserName(null);
        new MqttContext().setPassword(null);
    }

    @Test
    public void channelReadReleasesInboundPublishMessage() {
        EmbeddedChannel publisherChannel = new EmbeddedChannel(new MqttTransportHandler());
        MqttFixedHeader fixedHeader = new MqttFixedHeader(MqttMessageType.PUBLISH, false, MqttQoS.AT_MOST_ONCE, false, 0);
        MqttPublishVariableHeader variableHeader = new MqttPublishVariableHeader(TOPIC, 1);
        MqttPublishMessage message = new MqttPublishMessage(fixedHeader, variableHeader,
                Unpooled.copiedBuffer("hello", CharsetUtil.UTF_8));
        ByteBuf payload = message.payload();

        publisherChannel.writeInbound(message);

        assertEquals(0, payload.refCnt());
        publisherChannel.finishAndReleaseAll();
    }

    @Test
    public void duplicateConnectCleansUpChannelRepository() {
        EmbeddedChannel sessionChannel = new EmbeddedChannel(new MqttTransportHandler());

        sessionChannel.writeInbound(connectMessage());
        assertEquals(CLIENT_ID, CHANNEL_REPOSITORY.get(sessionChannel));

        sessionChannel.writeInbound(connectMessage());
        sessionChannel.runPendingTasks();

        assertFalse(sessionChannel.isActive());
        assertNull(CHANNEL_REPOSITORY.get(sessionChannel));
        sessionChannel.finishAndReleaseAll();
    }

    @Test
    public void abruptChannelCloseCleansUpChannelRepository() {
        EmbeddedChannel sessionChannel = new EmbeddedChannel(new MqttTransportHandler());

        sessionChannel.writeInbound(connectMessage());
        assertEquals(CLIENT_ID, CHANNEL_REPOSITORY.get(sessionChannel));

        sessionChannel.close();
        sessionChannel.runPendingTasks();

        assertFalse(sessionChannel.isActive());
        assertNull(CHANNEL_REPOSITORY.get(sessionChannel));
        sessionChannel.finishAndReleaseAll();
    }

    @Test
    public void nonMqttMessageClosesConnectedChannel() {
        EmbeddedChannel sessionChannel = new EmbeddedChannel(new MqttTransportHandler());

        sessionChannel.writeInbound(connectMessage());
        assertEquals(CLIENT_ID, CHANNEL_REPOSITORY.get(sessionChannel));

        sessionChannel.writeInbound("not-a-mqtt-message");
        sessionChannel.runPendingTasks();

        assertFalse(sessionChannel.isActive());
        assertNull(CHANNEL_REPOSITORY.get(sessionChannel));
        sessionChannel.finishAndReleaseAll();
    }

    @Test
    public void testOperationCompleteCleansRepositoriesOnClose() throws Exception {
        assertEquals(1, MqttPacketIdGenerator.next(registeredChannel));

        new MqttTransportHandler().operationComplete(registeredChannel.closeFuture());

        awaitAssert(() -> assertNull(CHANNEL_REPOSITORY.get(registeredChannel)));
        awaitAssert(() -> assertFalse(SUBSCRIBE_REPOSITORY.get(TOPIC).containsKey(registeredChannel)));
        assertEquals(1, MqttPacketIdGenerator.next(registeredChannel));
    }

    @Test
    public void channelInactiveIsPropagatedOnlyOnce() {
        AtomicInteger fired = new AtomicInteger();
        EmbeddedChannel sessionChannel = new EmbeddedChannel(new MqttTransportHandler(), new ChannelInboundHandlerAdapter() {
            @Override
            public void channelInactive(final ChannelHandlerContext context) throws Exception {
                fired.incrementAndGet();
                super.channelInactive(context);
            }
        });

        sessionChannel.close();
        sessionChannel.runPendingTasks();

        assertEquals(1, fired.get());
        sessionChannel.finishAndReleaseAll();
    }

    @Test
    public void testChannelInactiveFiresWillAndRemovesIt() throws Exception {
        willRepository.add(channel, new WillRepository.WillEntry(WILL_TOPIC, "sudden disconnect".getBytes(), 1, true));
        SUBSCRIBE_REPOSITORY.add(channel,
                Collections.singletonList(new MqttTopicSubscription(WILL_TOPIC, MqttQoS.AT_LEAST_ONCE)));
        awaitAssert(() -> assertTrue(SUBSCRIBE_REPOSITORY.get(WILL_TOPIC).containsKey(channel)));
        when(channel.isActive()).thenReturn(true);

        handler.channelInactive(ctx);

        ArgumentCaptor<MqttPublishMessage> published = ArgumentCaptor.forClass(MqttPublishMessage.class);
        verify(channel).writeAndFlush(published.capture());
        assertEquals(WILL_TOPIC, published.getValue().variableHeader().topicName());
        assertEquals(MqttQoS.AT_LEAST_ONCE, published.getValue().fixedHeader().qosLevel());
        assertTrue(published.getValue().fixedHeader().isRetain());
        assertNull(willRepository.get(channel));
    }

    @Test
    public void testChannelInactiveDoesNothingWhenNoWill() throws Exception {
        handler.channelInactive(ctx);

        assertNull(willRepository.get(channel));
    }

    @Test
    public void testChannelInactiveAfterDisconnectClearsWill() throws Exception {
        willRepository.add(channel, new WillRepository.WillEntry("status/clean", "graceful close".getBytes(), 0, false));

        // a graceful disconnect removes the will first, so channelInactive must not publish it
        willRepository.remove(channel);
        handler.channelInactive(ctx);

        assertNull(willRepository.get(channel));
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
}
