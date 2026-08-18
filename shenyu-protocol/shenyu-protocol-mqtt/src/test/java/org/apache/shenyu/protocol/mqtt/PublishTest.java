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

import io.netty.buffer.Unpooled;
import io.netty.channel.ChannelHandlerContext;
import io.netty.handler.codec.mqtt.MqttFixedHeader;
import io.netty.handler.codec.mqtt.MqttMessageType;
import io.netty.handler.codec.mqtt.MqttPublishMessage;
import io.netty.handler.codec.mqtt.MqttPublishVariableHeader;
import io.netty.handler.codec.mqtt.MqttQoS;
import io.netty.util.CharsetUtil;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.protocol.mqtt.repositories.SubscribeRepository;
import org.apache.shenyu.protocol.mqtt.repositories.TopicRepository;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.time.Duration;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;

/**
 * Test cases for {@link Publish}.
 */
public final class PublishTest {

    private static final String RETAINED_TOPIC = "test/retained";

    private static final String NON_RETAINED_TOPIC = "test/non-retained";

    private static final String CLEARED_TOPIC = "test/cleared";

    private static TopicRepository topicRepository;

    @BeforeAll
    static void setUp() {
        topicRepository = new TopicRepository();
        Singleton.INST.single(TopicRepository.class, topicRepository);
        Singleton.INST.single(SubscribeRepository.class, new SubscribeRepository());
    }

    @Test
    public void retainedPublishStoresMessage() {
        new Publish().publish(mock(ChannelHandlerContext.class), publishMessage(RETAINED_TOPIC, "hello", true));
        await().atMost(Duration.ofSeconds(5))
                .until(() -> "hello".equals(topicRepository.get(RETAINED_TOPIC)));
    }

    @Test
    public void nonRetainedPublishDoesNotStoreMessage() {
        new Publish().publish(mock(ChannelHandlerContext.class), publishMessage(NON_RETAINED_TOPIC, "hello", false));
        assertNull(topicRepository.get(NON_RETAINED_TOPIC));
    }

    @Test
    public void zeroByteRetainedPublishClearsRetainedMessage() {
        Publish publish = new Publish();
        publish.publish(mock(ChannelHandlerContext.class), publishMessage(CLEARED_TOPIC, "hello", true));
        await().atMost(Duration.ofSeconds(5))
                .until(() -> "hello".equals(topicRepository.get(CLEARED_TOPIC)));
        publish.publish(mock(ChannelHandlerContext.class), publishMessage(CLEARED_TOPIC, "", true));
        assertNull(topicRepository.get(CLEARED_TOPIC));
    }

    private MqttPublishMessage publishMessage(final String topic, final String payload, final boolean retain) {
        MqttFixedHeader fixedHeader = new MqttFixedHeader(MqttMessageType.PUBLISH, false, MqttQoS.AT_MOST_ONCE, retain, 0);
        MqttPublishVariableHeader variableHeader = new MqttPublishVariableHeader(topic, 1);
        return new MqttPublishMessage(fixedHeader, variableHeader, Unpooled.copiedBuffer(payload, CharsetUtil.UTF_8));
    }
}
