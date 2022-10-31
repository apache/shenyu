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
import io.netty.handler.codec.mqtt.MqttPublishMessage;
import io.netty.handler.codec.mqtt.MqttSubscribeMessage;
import io.netty.handler.codec.mqtt.MqttUnsubscribeMessage;

/**
 * Command messages.
 */
public interface AbstractMessageType {

    /**
     * Client request to connect to Server.
     * @param ctx ChannelHandlerContext
     * @param msg msg
     */
    default void connect(final ChannelHandlerContext ctx, final MqttConnectMessage msg) {

    }

    /**
     * Publish message.
     * @param ctx ctx
     * @param msg msg
     */
    default void publish(final ChannelHandlerContext ctx, final MqttPublishMessage msg) {

    }

    /**
     * Client Subscribe request.
     * @param ctx ctx
     * @param msg msg
     */
    default void subscribe(final ChannelHandlerContext ctx, final MqttSubscribeMessage msg) {

    }

    /**
     * Client Unsubscribe request.
     * @param ctx ctx
     * @param msg msg
     */
    default void unsubscribe(final ChannelHandlerContext ctx, final MqttUnsubscribeMessage msg) {

    }

    /**
     * PING Request.
     * @param ctx ctx
     */
    default void pingReq(final ChannelHandlerContext ctx) {

    }

    /**
     * PING Response.
     * @param ctx ctx
     */
    default void pingResp(final ChannelHandlerContext ctx) {

    }

    /**
     * Client is Disconnecting.
     * @param ctx ctx
     */
    default void disconnect(final ChannelHandlerContext ctx) {

    }

}
