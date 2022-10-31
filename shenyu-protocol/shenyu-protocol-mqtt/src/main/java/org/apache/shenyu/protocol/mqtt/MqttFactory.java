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
import io.netty.handler.codec.mqtt.MqttConnectMessage;
import io.netty.handler.codec.mqtt.MqttMessage;
import io.netty.handler.codec.mqtt.MqttPublishMessage;
import io.netty.handler.codec.mqtt.MqttSubscribeMessage;
import io.netty.handler.codec.mqtt.MqttUnsubscribeMessage;

/**
 * mqtt factory.
 */
public class MqttFactory {

    private final MessageType messageType = new MessageType();

    private final MqttMessage msg;

    private final ChannelHandlerContext ctx;

    public MqttFactory(final MqttMessage msg, final ChannelHandlerContext ctx) {
        this.msg = msg;
        this.ctx = ctx;
    }

    /**
     * connect.
     */
    public void connect() {
        if (msg.fixedHeader() == null) {
            return;
        }
        switch (msg.fixedHeader().messageType()) {
            case CONNECT:
                messageType.connect(ctx, (MqttConnectMessage) msg);
                break;
            case PUBLISH:
                messageType.publish(ctx, (MqttPublishMessage) msg);
                break;
            case SUBSCRIBE:
                messageType.subscribe(ctx, (MqttSubscribeMessage) msg);
                break;
            case UNSUBSCRIBE:
                messageType.unsubscribe(ctx, (MqttUnsubscribeMessage) msg);
                break;
            case PINGREQ:
                messageType.pingReq(ctx);
                break;
            case PUBACK:
            case DISCONNECT:
            default:
                break;
        }
    }
}
