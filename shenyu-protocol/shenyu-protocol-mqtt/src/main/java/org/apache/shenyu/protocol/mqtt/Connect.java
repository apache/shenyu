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
import io.netty.handler.codec.mqtt.MqttConnAckMessage;
import io.netty.handler.codec.mqtt.MqttConnectMessage;
import io.netty.handler.codec.mqtt.MqttConnectReturnCode;
import io.netty.handler.codec.mqtt.MqttMessageBuilders;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.protocol.mqtt.repositories.ChannelRepository;

/**
 * Client requests a connection to a server.
 */
public class Connect extends MessageType {

    @Override
    public void connect(final ChannelHandlerContext ctx, final MqttConnectMessage msg) {

        String clientId = msg.payload().clientIdentifier();
        if (StringUtils.isEmpty(clientId)) {
            ctx.writeAndFlush(wrong(MqttConnectReturnCode.CONNECTION_REFUSED_IDENTIFIER_REJECTED));
            return;
        }

        String userName = msg.payload().userName();
        byte[] passwordInBytes = msg.payload().passwordInBytes();

        if (!MqttContext.isValid(userName, passwordInBytes)) {
            ctx.writeAndFlush(wrong(MqttConnectReturnCode.CONNECTION_REFUSED_BAD_USER_NAME_OR_PASSWORD));
            return;
        }

        // record connect
        Singleton.INST.get(ChannelRepository.class).add(ctx.channel(), clientId);
        MqttConnAckMessage ackMessage = MqttMessageBuilders.connAck()
                .returnCode(MqttConnectReturnCode.CONNECTION_ACCEPTED)
                .sessionPresent(true)
                .build();
        ctx.writeAndFlush(ackMessage);
        setConnected(true);
    }

    private MqttConnAckMessage wrong(final MqttConnectReturnCode returnCode) {
        return MqttMessageBuilders.connAck()
                .returnCode(returnCode)
                .sessionPresent(false)
                .build();
    }
}
