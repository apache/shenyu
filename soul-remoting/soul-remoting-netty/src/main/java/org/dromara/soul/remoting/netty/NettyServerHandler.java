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

    public NettyServerHandler(Attribute attribute, org.dromara.soul.remoting.api.ChannelHandler channelHandler) {
        this.channelHandler = channelHandler;
        channelCache = new ChannelCache(1, TimeUnit.SECONDS, "nettyChannelCache", this);
    }

    @Override
    public void channelActive(ChannelHandlerContext ctx) throws Exception {
        super.channelActive(ctx);
    }

    @Override
    public void channelInactive(ChannelHandlerContext ctx) throws Exception {
        super.channelInactive(ctx);
    }

    @Override
    public void disconnect(ChannelHandlerContext ctx, ChannelPromise promise) throws Exception {
        super.disconnect(ctx, promise);
    }

    @Override
    public void channelRead(ChannelHandlerContext ctx, Object msg) throws Exception {
        super.channelRead(ctx, msg);
    }

    @Override
    public void write(ChannelHandlerContext ctx,
                      Object msg, ChannelPromise promise) throws Exception {
        super.write(ctx, msg, promise);
    }

    @Override
    public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) throws Exception {
        super.exceptionCaught(ctx, cause);
    }

    public Collection<Channel> getChannels() {
        return channelCache.getAll();
    }

    @Override
    public void timeout(Channel channel) {
        channelHandler.timeout(channel);
    }
}
