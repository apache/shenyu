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

import io.netty.channel.Channel;
import io.netty.channel.ChannelFuture;
import io.netty.channel.ChannelHandlerContext;
import io.netty.handler.codec.mqtt.MqttFixedHeader;
import io.netty.handler.codec.mqtt.MqttMessageIdVariableHeader;
import io.netty.handler.codec.mqtt.MqttMessageType;
import io.netty.handler.codec.mqtt.MqttQoS;
import io.netty.handler.codec.mqtt.MqttSubAckMessage;
import io.netty.handler.codec.mqtt.MqttSubscribeMessage;
import io.netty.handler.codec.mqtt.MqttSubscribePayload;
import io.netty.handler.codec.mqtt.MqttTopicSubscription;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.protocol.mqtt.repositories.SubscribeRepository;
import org.apache.shenyu.protocol.mqtt.repositories.TopicRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link Subscribe}.
 */
@ExtendWith(MockitoExtension.class)
class SubscribeTest {

    @Mock
    private ChannelHandlerContext ctx;

    @Mock
    private Channel channel;

    @Mock
    private SubscribeRepository subscribeRepository;

    @Mock
    private TopicRepository topicRepository;

    private final Subscribe subscribe = new Subscribe();

    @BeforeEach
    void setUp() {
        when(ctx.channel()).thenReturn(channel);
        Singleton.INST.single(SubscribeRepository.class, subscribeRepository);
        Singleton.INST.single(TopicRepository.class, topicRepository);
    }

    @Test
    void testSubAckGrantsRequestedQoS() {
        MqttSubscribeMessage msg = subscribeMessage(10,
                new MqttTopicSubscription("topic/qos2", MqttQoS.EXACTLY_ONCE),
                new MqttTopicSubscription("topic/qos0", MqttQoS.AT_MOST_ONCE),
                new MqttTopicSubscription("topic/qos1", MqttQoS.AT_LEAST_ONCE));

        subscribe.subscribe(ctx, msg);

        MqttSubAckMessage subAck = capturedSubAck();
        assertEquals(10, subAck.variableHeader().messageId());
        assertEquals(Arrays.asList(2, 0, 1), subAck.payload().grantedQoSLevels());
    }

    @Test
    void testSubAckExcludesFailureSubscriptions() {
        MqttSubscribeMessage msg = subscribeMessage(20,
                new MqttTopicSubscription("topic/qos1", MqttQoS.AT_LEAST_ONCE),
                new MqttTopicSubscription("topic/failure", MqttQoS.FAILURE),
                new MqttTopicSubscription("topic/qos0", MqttQoS.AT_MOST_ONCE));

        subscribe.subscribe(ctx, msg);

        MqttSubAckMessage subAck = capturedSubAck();
        assertEquals(Arrays.asList(1, 0), subAck.payload().grantedQoSLevels());
        verify(topicRepository, never()).get("topic/failure");
    }

    @Test
    void testSubscribeWhenConnectedClosesChannel() {
        ChannelFuture closeFuture = mock(ChannelFuture.class);
        when(channel.close()).thenReturn(closeFuture);
        subscribe.setConnected(true);

        subscribe.subscribe(ctx, subscribeMessage(30,
                new MqttTopicSubscription("topic/qos0", MqttQoS.AT_MOST_ONCE)));

        verify(channel).close();
        verify(channel, never()).writeAndFlush(any());
    }

    private MqttSubAckMessage capturedSubAck() {
        ArgumentCaptor<Object> captor = ArgumentCaptor.forClass(Object.class);
        verify(channel).writeAndFlush(captor.capture());
        return (MqttSubAckMessage) captor.getValue();
    }

    private MqttSubscribeMessage subscribeMessage(final int packetId, final MqttTopicSubscription... subscriptions) {
        MqttFixedHeader fixedHeader = new MqttFixedHeader(MqttMessageType.SUBSCRIBE, false, MqttQoS.AT_LEAST_ONCE, false, 0);
        MqttMessageIdVariableHeader variableHeader = MqttMessageIdVariableHeader.from(packetId);
        return new MqttSubscribeMessage(fixedHeader, variableHeader, new MqttSubscribePayload(Arrays.asList(subscriptions)));
    }
}
