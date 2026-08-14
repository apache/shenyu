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
import io.netty.handler.codec.mqtt.MqttQoS;
import io.netty.handler.codec.mqtt.MqttTopicSubscription;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.protocol.mqtt.repositories.ChannelRepository;
import org.apache.shenyu.protocol.mqtt.repositories.SubscribeRepository;
import org.apache.shenyu.protocol.mqtt.utils.MqttPacketIdGenerator;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.concurrent.TimeUnit;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for MqttTransportHandler.
 */
public class MqttTransportHandlerTest {

    private static final String TOPIC = "test/topic";

    private static final String CLIENT_ID = "test-client";

    private final ChannelRepository channelRepository = new ChannelRepository();

    private final SubscribeRepository subscribeRepository = new SubscribeRepository();

    private EmbeddedChannel channel;

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
