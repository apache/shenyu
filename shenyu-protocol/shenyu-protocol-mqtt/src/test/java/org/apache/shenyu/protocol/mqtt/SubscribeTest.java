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

import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelInboundHandlerAdapter;
import io.netty.channel.embedded.EmbeddedChannel;
import io.netty.handler.codec.mqtt.MqttFixedHeader;
import io.netty.handler.codec.mqtt.MqttMessageBuilders;
import io.netty.handler.codec.mqtt.MqttMessageIdVariableHeader;
import io.netty.handler.codec.mqtt.MqttMessageType;
import io.netty.handler.codec.mqtt.MqttPublishMessage;
import io.netty.handler.codec.mqtt.MqttQoS;
import io.netty.handler.codec.mqtt.MqttSubAckMessage;
import io.netty.handler.codec.mqtt.MqttSubscribeMessage;
import io.netty.handler.codec.mqtt.MqttSubscribePayload;
import io.netty.handler.codec.mqtt.MqttTopicSubscription;
import io.netty.util.CharsetUtil;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.protocol.mqtt.repositories.ChannelRepository;
import org.apache.shenyu.protocol.mqtt.repositories.SessionRepository;
import org.apache.shenyu.protocol.mqtt.repositories.SubscribeRepository;
import org.apache.shenyu.protocol.mqtt.repositories.TopicRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.BooleanSupplier;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

/**
 * Test case for {@link Subscribe}.
 */
class SubscribeTest {

    private static final String CLIENT_ID = "subscribe-test-client";

    private SubscribeRepository subscribeRepository;

    private TopicRepository topicRepository;

    private final Subscribe subscribe = new Subscribe();

    /**
     * SUBSCRIBE resolves the client session through these repositories, so they must be
     * present even when a test only exercises topic routing.
     */
    @BeforeEach
    void setUp() {
        subscribeRepository = new SubscribeRepository();
        topicRepository = new EmptyTopicRepository();
        Singleton.INST.single(ChannelRepository.class, new ChannelRepository());
        Singleton.INST.single(SessionRepository.class, new SessionRepository());
        Singleton.INST.single(SubscribeRepository.class, subscribeRepository);
        Singleton.INST.single(TopicRepository.class, topicRepository);
    }

    @Test
    void testSubscribeRejectsInvalidTopicFilter() throws InterruptedException {
        EmbeddedChannel channel = new EmbeddedChannel();
        channel.pipeline().addLast(new ChannelInboundHandlerAdapter());

        SubscribeRepository repository = new SubscribeRepository();
        Singleton.INST.single(SubscribeRepository.class, repository);
        Singleton.INST.single(TopicRepository.class, new TopicRepository());
        new MessageType().setConnected(channel, true);
        Singleton.INST.get(ChannelRepository.class).add(channel, CLIENT_ID);

        ChannelHandlerContext ctx = channel.pipeline().firstContext();
        MqttSubscribeMessage msg = MqttMessageBuilders.subscribe()
                .messageId(1)
                .addSubscription(MqttQoS.AT_MOST_ONCE, "sport/#")
                .addSubscription(MqttQoS.AT_MOST_ONCE, "bad#filter")
                .build();

        new Subscribe().subscribe(ctx, msg);
        awaitUntil(() -> repository.get("sport/#").containsKey(channel));
        assertTrue(repository.get("bad#filter").isEmpty());

        MqttSubAckMessage subAck = channel.readOutbound();
        assertNotNull(subAck);
        List<Integer> granted = subAck.payload().grantedQoSLevels();
        assertEquals(2, granted.size());
        assertEquals(0, granted.get(0).intValue());
        assertEquals(0x80, granted.get(1).intValue());

        repository.remove(Collections.singletonList("sport/#"), channel);
        awaitUntil(() -> repository.get("sport/#").isEmpty());
        channel.finishAndReleaseAll();
    }

    @Test
    void testMultiLevelWildcardReceivesRetainedMessages() throws InterruptedException {
        EmbeddedChannel channel = new EmbeddedChannel();
        channel.pipeline().addLast(new ChannelInboundHandlerAdapter());

        Singleton.INST.single(SubscribeRepository.class, new SubscribeRepository());
        TopicRepository topicRepository = new TopicRepository();
        Singleton.INST.single(TopicRepository.class, topicRepository);
        new MessageType().setConnected(channel, true);
        Singleton.INST.get(ChannelRepository.class).add(channel, CLIENT_ID);

        topicRepository.add("retained-hash/tennis/player1", "retained-1".getBytes(CharsetUtil.UTF_8));
        topicRepository.add("retained-hash/tennis/player2", "retained-2".getBytes(CharsetUtil.UTF_8));
        topicRepository.add("retained-hash/weather/today", "retained-3".getBytes(CharsetUtil.UTF_8));
        awaitUntil(() -> Objects.nonNull(topicRepository.get("retained-hash/tennis/player1"))
                && Objects.nonNull(topicRepository.get("retained-hash/tennis/player2")));

        try {
            ChannelHandlerContext ctx = channel.pipeline().firstContext();
            MqttSubscribeMessage msg = MqttMessageBuilders.subscribe()
                    .messageId(11)
                    .addSubscription(MqttQoS.AT_MOST_ONCE, "retained-hash/#")
                    .build();

            new Subscribe().subscribe(ctx, msg);

            Map<String, String> retained = readRetainedMessages(channel, 2);
            assertEquals("retained-1", retained.get("retained-hash/tennis/player1"));
            assertEquals("retained-2", retained.get("retained-hash/tennis/player2"));
            assertNotNull(channel.readOutbound());
        } finally {
            topicRepository.remove("retained-hash/tennis/player1");
            topicRepository.remove("retained-hash/tennis/player2");
            topicRepository.remove("retained-hash/weather/today");
            channel.finishAndReleaseAll();
        }
    }

    @Test
    void testSingleLevelWildcardReceivesRetainedMessageOfOneLevel() throws InterruptedException {
        EmbeddedChannel channel = new EmbeddedChannel();
        channel.pipeline().addLast(new ChannelInboundHandlerAdapter());

        Singleton.INST.single(SubscribeRepository.class, new SubscribeRepository());
        TopicRepository topicRepository = new TopicRepository();
        Singleton.INST.single(TopicRepository.class, topicRepository);
        new MessageType().setConnected(channel, true);
        Singleton.INST.get(ChannelRepository.class).add(channel, CLIENT_ID);

        topicRepository.add("retained-plus/tennis", "retained-level".getBytes(CharsetUtil.UTF_8));
        topicRepository.add("retained-plus/tennis/player1", "retained-deeper".getBytes(CharsetUtil.UTF_8));
        awaitUntil(() -> Objects.nonNull(topicRepository.get("retained-plus/tennis")));

        try {
            ChannelHandlerContext ctx = channel.pipeline().firstContext();
            MqttSubscribeMessage msg = MqttMessageBuilders.subscribe()
                    .messageId(12)
                    .addSubscription(MqttQoS.AT_MOST_ONCE, "retained-plus/+")
                    .build();

            new Subscribe().subscribe(ctx, msg);

            Map<String, String> retained = readRetainedMessages(channel, 1);
            assertEquals("retained-level", retained.get("retained-plus/tennis"));
            assertNotNull(channel.readOutbound());
        } finally {
            topicRepository.remove("retained-plus/tennis");
            topicRepository.remove("retained-plus/tennis/player1");
            channel.finishAndReleaseAll();
        }
    }

    @Test
    void testOverlappingFiltersReceiveRetainedMessageOnce() throws InterruptedException {
        EmbeddedChannel channel = new EmbeddedChannel();
        channel.pipeline().addLast(new ChannelInboundHandlerAdapter());

        Singleton.INST.single(SubscribeRepository.class, new SubscribeRepository());
        TopicRepository topicRepository = new TopicRepository();
        Singleton.INST.single(TopicRepository.class, topicRepository);
        new MessageType().setConnected(channel, true);
        Singleton.INST.get(ChannelRepository.class).add(channel, CLIENT_ID);

        topicRepository.add("retained-once/tennis", "retained-level".getBytes(CharsetUtil.UTF_8));
        awaitUntil(() -> Objects.nonNull(topicRepository.get("retained-once/tennis")));

        try {
            ChannelHandlerContext ctx = channel.pipeline().firstContext();
            MqttSubscribeMessage msg = MqttMessageBuilders.subscribe()
                    .messageId(13)
                    .addSubscription(MqttQoS.AT_MOST_ONCE, "retained-once/#")
                    .addSubscription(MqttQoS.AT_MOST_ONCE, "retained-once/+")
                    .build();

            new Subscribe().subscribe(ctx, msg);

            Map<String, String> retained = readRetainedMessages(channel, 1);
            assertEquals("retained-level", retained.get("retained-once/tennis"));
            MqttSubAckMessage subAck = channel.readOutbound();
            assertNotNull(subAck);
            assertEquals(2, subAck.payload().grantedQoSLevels().size());
        } finally {
            topicRepository.remove("retained-once/tennis");
            channel.finishAndReleaseAll();
        }
    }

    @Test
    void testSubAckGrantsRequestedQoS() {
        EmbeddedChannel channel = connectedChannel();
        MqttSubscribeMessage msg = subscribeMessage(10,
                new MqttTopicSubscription("topic/qos2", MqttQoS.EXACTLY_ONCE),
                new MqttTopicSubscription("topic/qos0", MqttQoS.AT_MOST_ONCE),
                new MqttTopicSubscription("topic/qos1", MqttQoS.AT_LEAST_ONCE));

        subscribe.subscribe(channel.pipeline().lastContext(), msg);

        MqttSubAckMessage subAck = channel.readOutbound();
        assertEquals(10, subAck.variableHeader().messageId());
        assertEquals(Arrays.asList(2, 0, 1), subAck.payload().grantedQoSLevels());

        subscribeRepository.remove(Arrays.asList("topic/qos2", "topic/qos0", "topic/qos1"));
    }

    @Test
    void testSubAckExcludesFailureSubscriptions() {
        EmbeddedChannel channel = connectedChannel();
        MqttSubscribeMessage msg = subscribeMessage(20,
                new MqttTopicSubscription("topic/qos1", MqttQoS.AT_LEAST_ONCE),
                new MqttTopicSubscription("topic/failure", MqttQoS.FAILURE),
                new MqttTopicSubscription("topic/qos0", MqttQoS.AT_MOST_ONCE));

        subscribe.subscribe(channel.pipeline().lastContext(), msg);

        MqttSubAckMessage subAck = channel.readOutbound();
        assertEquals(Arrays.asList(1, 0), subAck.payload().grantedQoSLevels());
        assertTrue(subscribeRepository.get("topic/failure").isEmpty());

        subscribeRepository.remove(Arrays.asList("topic/qos1", "topic/qos0"));
    }

    @Test
    void testSubscribeBeforeConnectClosesChannel() {
        EmbeddedChannel channel = new EmbeddedChannel(new ChannelInboundHandlerAdapter());
        MqttSubscribeMessage msg = subscribeMessage(30,
                new MqttTopicSubscription("topic/qos0", MqttQoS.AT_MOST_ONCE));

        subscribe.subscribe(channel.pipeline().lastContext(), msg);

        channel.runPendingTasks();
        assertFalse(channel.isActive());
        assertNull(channel.readOutbound());
    }

    private EmbeddedChannel connectedChannel() {
        EmbeddedChannel channel = new EmbeddedChannel(new ChannelInboundHandlerAdapter());
        new MessageType().setConnected(channel, true);
        Singleton.INST.get(ChannelRepository.class).add(channel, CLIENT_ID);
        return channel;
    }

    private MqttSubscribeMessage subscribeMessage(final int packetId, final MqttTopicSubscription... subscriptions) {
        MqttFixedHeader fixedHeader = new MqttFixedHeader(MqttMessageType.SUBSCRIBE, false, MqttQoS.AT_LEAST_ONCE, false, 0);
        MqttMessageIdVariableHeader variableHeader = MqttMessageIdVariableHeader.from(packetId);
        return new MqttSubscribeMessage(fixedHeader, variableHeader, new MqttSubscribePayload(Arrays.asList(subscriptions)));
    }

    private Map<String, String> readRetainedMessages(final EmbeddedChannel channel, final int expected) {
        Map<String, String> messages = new HashMap<>();
        for (int i = 0; i < expected; i++) {
            MqttPublishMessage publish = channel.readOutbound();
            assertNotNull(publish);
            assertTrue(publish.fixedHeader().isRetain());
            messages.put(publish.variableHeader().topicName(), publish.payload().toString(CharsetUtil.UTF_8));
            publish.release();
        }
        return messages;
    }

    private void awaitUntil(final BooleanSupplier condition) throws InterruptedException {
        long deadline = System.currentTimeMillis() + 5000;
        while (!condition.getAsBoolean()) {
            if (System.currentTimeMillis() >= deadline) {
                fail("condition not met within timeout");
            }
            Thread.sleep(10);
        }
    }

    private static final class EmptyTopicRepository extends TopicRepository {

        @Override
        public Map<String, byte[]> getMatchingTopics(final String topicFilter) {
            return Collections.emptyMap();
        }
    }
}
