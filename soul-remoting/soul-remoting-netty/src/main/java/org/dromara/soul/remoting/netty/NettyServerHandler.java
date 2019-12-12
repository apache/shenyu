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

import io.netty.channel.ChannelDuplexHandler;
import io.netty.channel.ChannelHandler;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelPromise;
import java.util.Collection;
import java.util.concurrent.TimeUnit;
import org.dromara.soul.common.Attribute;
import org.dromara.soul.remoting.api.Channel;
import org.dromara.soul.remoting.api.ChannelCache;
import org.dromara.soul.remoting.api.ChannelCacheListener;
import org.dromara.soul.remoting.api.RemotingConst;

/**
 * NettyServerHandler
 * CreateDate: 2019/10/12 16:20
 *
 * @author sixh
 */
@ChannelHandler.Sharable
public class NettyServerHandler extends ChannelDuplexHandler implements ChannelCacheListener {
    private org.dromara.soul.remoting.api.ChannelHandler channelHandler;
    private final ChannelCache channelCache;

    private final Attribute attribute;

    public NettyServerHandler(Attribute attribute, org.dromara.soul.remoting.api.ChannelHandler channelHandler) {
        if (attribute == null) {
            throw new IllegalArgumentException("attribute is null");
        }
        if (channelHandler == null) {
            throw new IllegalArgumentException("handler is null");
        }
        this.channelHandler = channelHandler;
        this.attribute = attribute;
        //Timeout handling.
        Integer timeOut = this.attribute.getProperty(RemotingConst.NET_TIMEOUT_KEY, 3);
        channelCache = new ChannelCache(timeOut, TimeUnit.SECONDS, "nettyChannelCache", this);
    }

    @Override
    public void channelActive(ChannelHandlerContext ctx) throws Exception {
        super.channelActive(ctx);
        Channel channel = new NettyChannel(ctx.channel());
        channelCache.put(channel.getId(), channel);
        channelHandler.connected(channel);
    }

    @Override
    public void channelInactive(ChannelHandlerContext ctx) throws Exception {
        super.channelInactive(ctx);
        Channel channel = new NettyChannel(ctx.channel());
        channelCache.remove(channel.getId());
        channelHandler.disconnected(channel);
    }

    @Override
    public void channelRead(ChannelHandlerContext ctx, Object msg) throws Exception {
        super.channelRead(ctx, msg);
        Channel channel = new NettyChannel(ctx.channel());
        channelHandler.received(channel, msg);
    }

    @Override
    public void write(ChannelHandlerContext ctx,
                      Object msg, ChannelPromise promise) throws Exception {
        super.write(ctx, msg, promise);
        Channel channel = new NettyChannel(ctx.channel());
        channelHandler.sent(channel, msg);
    }

    @Override
    public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) throws Exception {
        super.exceptionCaught(ctx, cause);
        Channel channel = new NettyChannel(ctx.channel());
        channelHandler.exceptionCaught(channel, cause);
    }

    public Collection<Channel> getChannels() {
        return channelCache.getAll();
    }

    @Override
    public void timeout(Channel channel) {
        channelHandler.timeout(channel);
        channel.close();
    }
}
