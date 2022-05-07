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
import io.netty.channel.Channel;
import io.netty.channel.ChannelHandlerContext;
import io.netty.handler.codec.mqtt.MqttFixedHeader;
import io.netty.handler.codec.mqtt.MqttQoS;
import io.netty.handler.codec.mqtt.MqttSubscribeMessage;
import io.netty.handler.codec.mqtt.MqttTopicSubscription;
import io.netty.handler.codec.mqtt.MqttMessageType;
import io.netty.handler.codec.mqtt.MqttSubAckPayload;
import io.netty.handler.codec.mqtt.MqttSubAckMessage;
import io.netty.handler.codec.mqtt.MqttPublishVariableHeader;
import io.netty.handler.codec.mqtt.MqttPublishMessage;
import io.netty.util.CharsetUtil;
import org.apache.logging.log4j.util.Strings;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.protocol.mqtt.repositories.SubscribeRepository;
import org.apache.shenyu.protocol.mqtt.repositories.TopicRepository;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import static io.netty.channel.ChannelFutureListener.FIRE_EXCEPTION_ON_FAILURE;
import static io.netty.handler.codec.mqtt.MqttMessageIdVariableHeader.from;
import static io.netty.handler.codec.mqtt.MqttQoS.AT_MOST_ONCE;
import static io.netty.handler.codec.mqtt.MqttQoS.FAILURE;

/**
 *  Subscribe to named topics.
 */
public class Subscribe extends MessageType {

    @Override
    public void subscribe(final ChannelHandlerContext ctx, final MqttSubscribeMessage msg) {
        Channel channel = ctx.channel();

        if (isConnected()) {
            channel.close().addListener(FIRE_EXCEPTION_ON_FAILURE);
            return;
        }
        List<MqttTopicSubscription> mqttTopicSubscriptions = msg.payload().topicSubscriptions();
        int packetId = msg.variableHeader().messageId();

        //// todo Regular match
        List<String> ackTopics = mqttTopicSubscriptions
                .stream()
                .filter(topicSub -> topicSub.qualityOfService() != FAILURE)
                .map(MqttTopicSubscription::topicName)
                .collect(Collectors.toList());

        Singleton.INST.get(SubscribeRepository.class).add(ctx.channel(), mqttTopicSubscriptions);

        for (String ackTopic : ackTopics) {
            String message = Singleton.INST.get(TopicRepository.class).get(ackTopic);
            if (Strings.isNotEmpty(message)) {
                sendSubMessage(ackTopic, message, packetId, channel);
            }
        }

        sendSubAckMessage(packetId, ackTopics, channel);
    }

    /**
     * call back request of message.
     * @param packetId packetId
     * @param ackTopics ackTopics
     * @param channel channel
     */
    private void sendSubAckMessage(final int packetId, final List<String> ackTopics, final Channel channel) {

        List<Integer> qos = new ArrayList<>();
        for (int i = 0; i < ackTopics.size(); i++) {
            // default qos 0
            qos.add(MqttQoS.AT_MOST_ONCE.value());
        }

        MqttFixedHeader fixedHeader = new MqttFixedHeader(MqttMessageType.SUBACK, false, AT_MOST_ONCE,
                false, 0);
        MqttSubAckPayload payload = new MqttSubAckPayload(qos);
        MqttSubAckMessage mqttSubAckMessage = new MqttSubAckMessage(fixedHeader, from(packetId), payload);
        channel.writeAndFlush(mqttSubAckMessage);
    }

    /**
     * send subscribe message.
     * @param topic topic
     * @param message message
     * @param packetId packetId
     * @param channel channel
     */
    private void sendSubMessage(final String topic, final String message, final int packetId, final Channel channel) {
        MqttFixedHeader fixedHeader = new MqttFixedHeader(MqttMessageType.PUBLISH, false, AT_MOST_ONCE, true, 0);
        MqttPublishVariableHeader varHeader = new MqttPublishVariableHeader(topic, packetId);
        MqttPublishMessage mqttPublishMessage = new MqttPublishMessage(fixedHeader, varHeader, Unpooled.copiedBuffer(message, CharsetUtil.UTF_8));
        channel.writeAndFlush(mqttPublishMessage);
    }
}
