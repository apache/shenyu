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
public class MessageType implements AbstractMessageType {

    private volatile boolean connected;

    /**
     * isConnected.
     * @return connected
     */
    boolean isConnected() {
        return connected;
    }

    /**
     * set connected.
     * @param connected connected
     */
    void setConnected(final boolean connected) {
        this.connected = connected;
    }

    @Override
    public void connect(final ChannelHandlerContext ctx, final MqttConnectMessage msg) {
        //// todo polymorphism connect
        new Connect().connect(ctx, msg);
    }

    @Override
    public void publish(final ChannelHandlerContext ctx, final MqttPublishMessage msg) {
        //// todo polymorphism publish
        new Publish().publish(ctx, msg);
    }

    @Override
    public void subscribe(final ChannelHandlerContext ctx, final MqttSubscribeMessage msg) {
        //// todo polymorphism subscribe
        new Subscribe().subscribe(ctx, msg);
    }

    @Override
    public void unsubscribe(final ChannelHandlerContext ctx, final MqttUnsubscribeMessage msg) {
        //// todo polymorphism unsubscribe
        new Unsubscribe().unsubscribe(ctx, msg);
    }

    @Override
    public void pingReq(final ChannelHandlerContext ctx) {
        //// todo polymorphism pingReq
        new PingReq().pingReq(ctx);
    }

    @Override
    public void pingResp(final ChannelHandlerContext ctx) {
        //// todo polymorphism pingResp
        new PingResp().pingResp(ctx);
    }

    @Override
    public void disconnect(final ChannelHandlerContext ctx) {
        //// todo polymorphism disconnect
        new Disconnect().disconnect(ctx);
    }
}
