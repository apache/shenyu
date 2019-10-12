/*
 *
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 */

package org.dromara.soul.remoting.netty;

import java.net.SocketAddress;
import org.dromara.soul.remoting.api.Channel;
import org.dromara.soul.remoting.api.ChannelFuture;

/**
 * NettyChannel
 * CreateDate: 2019/10/12 15:57
 *
 * @author sixh
 */
public class NettyChannel implements Channel {
    private io.netty.channel.Channel channel;

    public NettyChannel(io.netty.channel.Channel channel) {
        this.channel = channel;
    }

    @Override
    public boolean isOpen() {
        return this.channel.isOpen();
    }

    @Override
    public SocketAddress localAddress() {
        return channel.localAddress();
    }

    @Override
    public String getId() {
        return this.channel.id().asShortText();
    }

    @Override
    public SocketAddress remoteAddress() {
        return channel.remoteAddress();
    }

    @Override
    public ChannelFuture send(Object message) {
        return new NettyChannelFuture(channel.writeAndFlush(message));
    }

    @Override
    public boolean isClose() {
        return !isOpen();
    }

    @Override
    public ChannelFuture close() {
        return new NettyChannelFuture(channel.close());
    }
}
