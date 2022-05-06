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
import io.netty.handler.codec.mqtt.MqttVersion;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.protocol.mqtt.repositories.ChannelRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static io.netty.channel.ChannelFutureListener.CLOSE_ON_FAILURE;
import static io.netty.handler.codec.mqtt.MqttConnectReturnCode.CONNECTION_REFUSED_BAD_USER_NAME_OR_PASSWORD;
import static io.netty.handler.codec.mqtt.MqttConnectReturnCode.CONNECTION_REFUSED_IDENTIFIER_REJECTED;
import static io.netty.handler.codec.mqtt.MqttConnectReturnCode.CONNECTION_REFUSED_UNACCEPTABLE_PROTOCOL_VERSION;

/**
 * Client requests a connection to a server.
 */
public class Connect extends MessageType {

    private static final Logger LOG = LoggerFactory.getLogger(Connect.class);

    @Override
    public void connect(final ChannelHandlerContext ctx, final MqttConnectMessage msg) {

        String clientId = msg.payload().clientIdentifier();
        if (StringUtils.isEmpty(clientId)) {
            LOG.info("MQTT clientId can not be empty.");
            close(ctx, CONNECTION_REFUSED_IDENTIFIER_REJECTED);
            return;
        }

        if (!allowedProtocolVersion(msg)) {
            LOG.info("MQTT protocol version is not supported. clientId: {}", clientId);
            close(ctx, CONNECTION_REFUSED_UNACCEPTABLE_PROTOCOL_VERSION);
        }

        String userName = msg.payload().userName();
        byte[] passwordInBytes = msg.payload().passwordInBytes();

        if (!MqttContext.isValid(userName, passwordInBytes)) {
            LOG.info("MQTT client verification failed, please check the username and password.");
            close(ctx, CONNECTION_REFUSED_BAD_USER_NAME_OR_PASSWORD);
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

    private void close(final ChannelHandlerContext ctx, final MqttConnectReturnCode returnCode) {
        ctx.writeAndFlush(wrong(returnCode));
        ctx.close().addListener(CLOSE_ON_FAILURE);
    }

    private MqttConnAckMessage wrong(final MqttConnectReturnCode returnCode) {
        return MqttMessageBuilders.connAck()
                .returnCode(returnCode)
                .sessionPresent(false)
                .build();
    }

    private boolean allowedProtocolVersion(final MqttConnectMessage msg) {
        return msg.variableHeader().version() == MqttVersion.MQTT_3_1.protocolLevel();
    }
}
