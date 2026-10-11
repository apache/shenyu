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
import io.netty.handler.codec.mqtt.MqttFixedHeader;
import io.netty.handler.codec.mqtt.MqttMessage;
import io.netty.handler.codec.mqtt.MqttMessageIdVariableHeader;
import io.netty.handler.codec.mqtt.MqttPublishMessage;
import io.netty.handler.codec.mqtt.MqttQoS;
import io.netty.handler.codec.mqtt.MqttPubAckMessage;
import io.netty.handler.codec.mqtt.MqttMessageType;
import io.netty.handler.codec.mqtt.MqttPublishVariableHeader;
import io.netty.util.ReferenceCountUtil;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.protocol.mqtt.repositories.SubscribeRepository;
import org.apache.shenyu.protocol.mqtt.repositories.TopicRepository;
import org.apache.shenyu.protocol.mqtt.utils.MqttPacketIdGenerator;

import java.util.Map;
import java.util.concurrent.CompletableFuture;

import static io.netty.channel.ChannelFutureListener.FIRE_EXCEPTION_ON_FAILURE;
import static io.netty.handler.codec.mqtt.MqttMessageType.PUBACK;
import static io.netty.handler.codec.mqtt.MqttMessageType.PUBREC;

/**
 * Publish message.
 */
public class Publish extends MessageType {

    @Override
    public void publish(final ChannelHandlerContext ctx, final MqttPublishMessage msg) {
        if (!isConnected(ctx.channel())) {
            ctx.channel().close().addListener(FIRE_EXCEPTION_ON_FAILURE);
            return;
        }
        String topic = msg.variableHeader().topicName();
        ByteBuf payload = msg.payload();
        MqttQoS mqttQoS = msg.fixedHeader().qosLevel();
        if (msg.fixedHeader().isRetain()) {
            if (payload.isReadable()) {
                byte[] message = new byte[payload.readableBytes()];
                payload.getBytes(payload.readerIndex(), message);
                Singleton.INST.get(TopicRepository.class).add(topic, message);
            } else {
                Singleton.INST.get(TopicRepository.class).remove(topic);
            }
        }
        int packetId = msg.variableHeader().packetId();
        // The inbound message is released by MqttTransportHandler once publish returns, retain the payload for the asynchronous send.
        payload.retain();
        CompletableFuture.runAsync(() -> {
            try {
                send(topic, payload, mqttQoS);
            } finally {
                ReferenceCountUtil.safeRelease(payload);
            }
        });

        switch (mqttQoS.value()) {
            case 0:
                break;

            case 1:
                qos1(ctx, packetId);
                break;

            case 2:
                qos2(ctx, packetId);
                break;
            default:
                break;
        }

    }

    /**
     * send PUBACK to the publisher for a qos1 publish.
     */
    private void qos1(final ChannelHandlerContext ctx, final int packetId) {
        MqttFixedHeader mqttFixedHeader = new MqttFixedHeader(PUBACK, false, MqttQoS.AT_MOST_ONCE, false, 0);
        MqttMessageIdVariableHeader mqttMsgIdVariableHeader = MqttMessageIdVariableHeader.from(packetId);

        MqttPubAckMessage mqttPubAckMessage = new MqttPubAckMessage(mqttFixedHeader, mqttMsgIdVariableHeader);
        ctx.writeAndFlush(mqttPubAckMessage);
    }

    /**
     * send PUBREC to the publisher for a qos2 publish.
     */
    private void qos2(final ChannelHandlerContext ctx, final int packetId) {
        MqttFixedHeader mqttFixedHeader = new MqttFixedHeader(PUBREC, false, MqttQoS.AT_MOST_ONCE, false, 0);
        MqttMessageIdVariableHeader mqttMsgIdVariableHeader = MqttMessageIdVariableHeader.from(packetId);

        MqttMessage mqttPubRecMessage = new MqttMessage(mqttFixedHeader, mqttMsgIdVariableHeader);
        ctx.writeAndFlush(mqttPubRecMessage);
    }

    private void send(final String topic, final ByteBuf payload, final MqttQoS publishQoS) {
        Map<Channel, MqttQoS> subscribers = Singleton.INST.get(SubscribeRepository.class).get(topic);
        //// todo thread pool
        subscribers.entrySet().parallelStream().forEach(entry -> {
            Channel channel = entry.getKey();
            if (channel.isActive()) {
                MqttQoS qos = minQoS(publishQoS, entry.getValue());
                int packetId = MqttQoS.AT_MOST_ONCE == qos ? 0 : MqttPacketIdGenerator.next(channel);
                MqttFixedHeader mqttFixedHeader = new MqttFixedHeader(MqttMessageType.PUBLISH, false, qos, false, 0);
                MqttPublishVariableHeader mqttPublishVariableHeader = new MqttPublishVariableHeader(topic, packetId);
                MqttPublishMessage mqttPublishMessage = new MqttPublishMessage(mqttFixedHeader, mqttPublishVariableHeader, Unpooled.wrappedBuffer(payload.retainedDuplicate()));
                channel.writeAndFlush(mqttPublishMessage);
            }
        });
    }

    private static MqttQoS minQoS(final MqttQoS publishQoS, final MqttQoS grantedQoS) {
        return publishQoS.value() <= grantedQoS.value() ? publishQoS : grantedQoS;
    }
}
