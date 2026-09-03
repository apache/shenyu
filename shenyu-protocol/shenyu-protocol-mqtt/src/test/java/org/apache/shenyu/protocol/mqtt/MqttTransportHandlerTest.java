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
import io.netty.util.IllegalReferenceCountException;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.protocol.mqtt.repositories.SubscribeRepository;
import org.apache.shenyu.protocol.mqtt.repositories.TopicRepository;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.time.Duration;

import static org.awaitility.Awaitility.await;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

/**
 * Test cases for {@link MqttTransportHandler}.
 */
public final class MqttTransportHandlerTest {

    @BeforeAll
    static void setUp() {
        Singleton.INST.single(TopicRepository.class, new TopicRepository());
        Singleton.INST.single(SubscribeRepository.class, new SubscribeRepository());
    }

    @Test
    public void channelReadReleasesInboundMessage() throws Exception {
        MqttFixedHeader fixedHeader = new MqttFixedHeader(MqttMessageType.PUBLISH, false, MqttQoS.AT_MOST_ONCE, false, 0);
        MqttPublishVariableHeader variableHeader = new MqttPublishVariableHeader("test/topic", 1);
        MqttPublishMessage msg = new MqttPublishMessage(fixedHeader, variableHeader, Unpooled.copiedBuffer("hello", CharsetUtil.UTF_8));
        new MqttTransportHandler().channelRead(mock(ChannelHandlerContext.class), msg);
        await().atMost(Duration.ofSeconds(5))
                .until(() -> {
                    try {
                        msg.payload().refCnt();
                        return false;
                    } catch (IllegalReferenceCountException e) {
                        // refCnt() throws once the payload has been fully released.
                        return true;
                    }
                });
    }

    @Test
    public void channelReadClosesChannelForNonMqttMessage() throws Exception {
        ChannelHandlerContext ctx = mock(ChannelHandlerContext.class);
        new MqttTransportHandler().channelRead(ctx, new Object());
        verify(ctx).close();
    }
}
