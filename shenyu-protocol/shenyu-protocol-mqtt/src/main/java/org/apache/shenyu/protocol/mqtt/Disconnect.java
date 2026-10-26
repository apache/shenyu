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
import io.netty.channel.ChannelHandlerContext;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.protocol.mqtt.repositories.ChannelRepository;
import org.apache.shenyu.protocol.mqtt.repositories.InflightRepository;
import org.apache.shenyu.protocol.mqtt.repositories.MqttSession;
import org.apache.shenyu.protocol.mqtt.repositories.SessionRepository;
import org.apache.shenyu.protocol.mqtt.repositories.SubscribeRepository;
import org.apache.shenyu.protocol.mqtt.repositories.WillRepository;
import org.apache.shenyu.protocol.mqtt.utils.MqttPacketIdGenerator;

import java.util.Objects;

/**
 * The DISCONNECT message is sent from the client to the server to indicate
 * that it is about to close its TCP/IP connection.
 *
 * <p>This allows for a clean disconnection, rather than just dropping the line.
 * If the client had connected with the clean session flag set,
 * then all previously maintained information about the client will be discarded.
 *
 * <p>A server should not rely on the client to close the TCP/IP connection after receiving a DISCONNECT.
 */
public class Disconnect extends MessageType {

    @Override
    public void disconnect(final ChannelHandlerContext ctx) {
        // MQTT-3.1.2-8: a graceful DISCONNECT discards the stored will.
        Singleton.INST.get(WillRepository.class).remove(ctx.channel());
        cleanChannel(ctx.channel());
        ctx.close();
    }

    /**
     * Discard clean session state and release resources for a disconnected channel.
     * @param channel disconnected channel
     */
    static void cleanChannel(final Channel channel) {
        final ChannelRepository channelRepository = Singleton.INST.get(ChannelRepository.class);
        String clientId = channelRepository.get(channel);
        if (Objects.nonNull(clientId)) {
            // fenced on ownership: a replaced connection closing late must not
            // discard the session of the connection that took over (#7918)
            if (channelRepository.isOwner(clientId, channel)) {
                MqttSession session = Singleton.INST.get(SessionRepository.class).get(clientId);
                if (Objects.nonNull(session) && session.isCleanSession()) {
                    // A clean-session disconnect discards all stored session state,
                    // including the inbound packet ids still being acknowledged (#7919)
                    Singleton.INST.get(SessionRepository.class).remove(clientId);
                    Singleton.INST.get(InflightRepository.class).remove(clientId);
                }
            }
        }
        Singleton.INST.get(SubscribeRepository.class).remove(channel);
        channelRepository.remove(channel);
        MqttPacketIdGenerator.remove(channel);
    }
}
