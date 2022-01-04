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
        //// todo Last words
        //// todo Clean session
        cleanChannel(ctx.channel());
        ctx.close();
    }

    private void cleanChannel(final Channel channel) {
        //// todo ttl
        Singleton.INST.get(ChannelRepository.class).remove(channel);
    }
}
