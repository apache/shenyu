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
import io.netty.handler.codec.mqtt.MqttMessageIdVariableHeader;
import io.netty.handler.codec.mqtt.MqttPublishMessage;
import io.netty.handler.codec.mqtt.MqttQoS;
import io.netty.handler.codec.mqtt.MqttPubAckMessage;
import io.netty.handler.codec.mqtt.MqttMessageType;
import io.netty.handler.codec.mqtt.MqttPublishVariableHeader;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.protocol.mqtt.repositories.SubscribeRepository;
import org.apache.shenyu.protocol.mqtt.repositories.TopicRepository;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import static io.netty.handler.codec.mqtt.MqttMessageType.PUBACK;

/**
 * Publish message.
 */
public class Publish extends MessageType {

    @Override
    public void publish(final ChannelHandlerContext ctx, final MqttPublishMessage msg) {
        if (isConnected()) {
            return;
        }
        String topic = msg.variableHeader().topicName();
        ByteBuf payload = msg.payload();
        String message = byteBufToString(payload);
        //// todo qos
        MqttQoS mqttQoS = msg.fixedHeader().qosLevel();
        Singleton.INST.get(TopicRepository.class).add(topic, message);
        int packetId = msg.variableHeader().packetId();
        CompletableFuture.runAsync(() -> send(topic, payload, packetId));

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
     * todo qos0.
     */
    private void qos0() {

    }

    /**
     * todo qos1.
     */
    private void qos1(final ChannelHandlerContext ctx, final int packetId) {
        MqttFixedHeader mqttFixedHeader = new MqttFixedHeader(PUBACK, false, MqttQoS.AT_LEAST_ONCE, false, 0);
        MqttMessageIdVariableHeader mqttMsgIdVariableHeader = MqttMessageIdVariableHeader.from(packetId);

        MqttPubAckMessage mqttPubAckMessage = new MqttPubAckMessage(mqttFixedHeader, mqttMsgIdVariableHeader);
        ctx.writeAndFlush(mqttPubAckMessage);
    }

    /**
     * todo qos2.
     */
    private void qos2(final ChannelHandlerContext ctx, final int packetId) {
        MqttFixedHeader mqttFixedHeader = new MqttFixedHeader(PUBACK, false, MqttQoS.EXACTLY_ONCE, false, 0);
        MqttMessageIdVariableHeader mqttMsgIdVariableHeader = MqttMessageIdVariableHeader.from(packetId);

        MqttPubAckMessage mqttPubAckMessage = new MqttPubAckMessage(mqttFixedHeader, mqttMsgIdVariableHeader);
        ctx.writeAndFlush(mqttPubAckMessage);
    }

    private String byteBufToString(final ByteBuf byteBuf) {
        if (byteBuf.hasArray()) {
            return new String(byteBuf.array(), byteBuf.arrayOffset() + byteBuf.readerIndex(), byteBuf.readableBytes());
        } else {
            byte[] bytes = new byte[byteBuf.readableBytes()];
            byteBuf.getBytes(byteBuf.readerIndex(), bytes);
            return new String(bytes, 0, byteBuf.readableBytes());
        }
    }

    private void send(final String topic, final ByteBuf payload, final int packetId) {
        List<Channel> channels = Singleton.INST.get(SubscribeRepository.class).get(topic);
        //// todo thread pool
        channels.parallelStream().forEach(channel -> {
            if (channel.isActive()) {
                MqttFixedHeader mqttFixedHeader = new MqttFixedHeader(MqttMessageType.PUBLISH, false, MqttQoS.AT_MOST_ONCE, false, 0);
                MqttPublishVariableHeader mqttPublishVariableHeader = new MqttPublishVariableHeader(topic, packetId);
                MqttPublishMessage mqttPublishMessage = new MqttPublishMessage(mqttFixedHeader, mqttPublishVariableHeader, Unpooled.wrappedBuffer(payload));
                channel.writeAndFlush(mqttPublishMessage);
            }
        });
    }
}
