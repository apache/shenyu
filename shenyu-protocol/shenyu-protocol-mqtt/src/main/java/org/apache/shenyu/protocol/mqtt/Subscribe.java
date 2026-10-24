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
import io.netty.handler.codec.mqtt.MqttSubscribeMessage;
import io.netty.handler.codec.mqtt.MqttTopicSubscription;
import io.netty.handler.codec.mqtt.MqttMessageType;
import io.netty.handler.codec.mqtt.MqttSubAckPayload;
import io.netty.handler.codec.mqtt.MqttSubAckMessage;
import io.netty.handler.codec.mqtt.MqttPublishVariableHeader;
import io.netty.handler.codec.mqtt.MqttPublishMessage;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.protocol.mqtt.repositories.ChannelRepository;
import org.apache.shenyu.protocol.mqtt.repositories.MqttSession;
import org.apache.shenyu.protocol.mqtt.repositories.SessionRepository;
import org.apache.shenyu.protocol.mqtt.repositories.SubscribeRepository;
import org.apache.shenyu.protocol.mqtt.repositories.TopicRepository;

import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
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

        if (!isConnected(channel)) {
            channel.close().addListener(FIRE_EXCEPTION_ON_FAILURE);
            return;
        }
        List<MqttTopicSubscription> mqttTopicSubscriptions = msg.payload().topicSubscriptions();
        int packetId = msg.variableHeader().messageId();

        // MQTT-4.7.1-1: invalid topic filters must be rejected with 0x80 and never stored.
        List<MqttTopicSubscription> validSubscriptions = mqttTopicSubscriptions
                .stream()
                .filter(topicSub -> topicSub.qualityOfService() != FAILURE)
                .filter(topicSub -> TopicMatcher.isValidFilter(topicSub.topicName()))
                .collect(Collectors.toList());

        Singleton.INST.get(SubscribeRepository.class).add(ctx.channel(), validSubscriptions);

        String clientId = Singleton.INST.get(ChannelRepository.class).get(ctx.channel());
        MqttSession session = Singleton.INST.get(SessionRepository.class).get(clientId);
        if (Objects.nonNull(session)) {
            validSubscriptions.forEach(subscription -> session.addTopic(subscription.topicName(), subscription.qualityOfService()));
        }

        // MQTT-3.3.1-6: a subscription must also receive the retained messages whose
        // concrete topic name matches its filter, so wildcard filters are matched as well.
        Set<String> deliveredRetainedTopics = new HashSet<>();
        for (MqttTopicSubscription subscription : validSubscriptions) {
            Singleton.INST.get(TopicRepository.class).getMatchingTopics(subscription.topicName())
                    .forEach((retainedTopic, retainedMessage) -> {
                        if (deliveredRetainedTopics.add(retainedTopic)) {
                            sendSubMessage(retainedTopic, retainedMessage, packetId, channel);
                        }
                    });
        }

        sendSubAckMessage(packetId, mqttTopicSubscriptions, channel);
    }

    /**
     * call back request of message.
     * @param packetId packetId
     * @param mqttTopicSubscriptions mqttTopicSubscriptions
     * @param channel channel
     */
    private void sendSubAckMessage(final int packetId, final List<MqttTopicSubscription> mqttTopicSubscriptions,
                                   final Channel channel) {

        List<Integer> qos = mqttTopicSubscriptions.stream()
                .filter(subscription -> subscription.qualityOfService() != FAILURE)
                .map(subscription -> TopicMatcher.isValidFilter(subscription.topicName())
                        ? subscription.qualityOfService().value()
                        : FAILURE.value())
                .collect(Collectors.toList());

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
    private void sendSubMessage(final String topic, final byte[] message, final int packetId, final Channel channel) {
        MqttFixedHeader fixedHeader = new MqttFixedHeader(MqttMessageType.PUBLISH, false, AT_MOST_ONCE, true, 0);
        MqttPublishVariableHeader varHeader = new MqttPublishVariableHeader(topic, packetId);
        MqttPublishMessage mqttPublishMessage = new MqttPublishMessage(fixedHeader, varHeader, Unpooled.wrappedBuffer(message));
        channel.writeAndFlush(mqttPublishMessage);
    }
}
