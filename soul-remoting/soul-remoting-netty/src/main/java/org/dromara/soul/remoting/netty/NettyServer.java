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

import io.netty.bootstrap.ServerBootstrap;
import io.netty.buffer.PooledByteBufAllocator;
import io.netty.channel.ChannelFuture;
import io.netty.channel.ChannelOption;
import io.netty.channel.EventLoopGroup;
import io.netty.channel.WriteBufferWaterMark;
import io.netty.channel.epoll.Epoll;
import io.netty.channel.epoll.EpollChannelOption;
import io.netty.channel.epoll.EpollEventLoopGroup;
import io.netty.channel.epoll.EpollServerSocketChannel;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.nio.NioServerSocketChannel;
import io.netty.handler.logging.LogLevel;
import io.netty.handler.logging.LoggingHandler;
import org.dromara.soul.common.Attribute;
import org.dromara.soul.common.concurrent.SoulThreadFactory;
import org.dromara.soul.remoting.api.AbstractNetServer;
import org.dromara.soul.remoting.api.Channel;
import org.dromara.soul.remoting.api.ChannelHandler;

import java.util.Collection;

/**
 * NettyServer starter.
 *
 * @author xiaoyu
 * @author sixh
 */
public class NettyServer extends AbstractNetServer {

    private ServerBootstrap bootstrap;

    private EventLoopGroup bossGroup;

    private EventLoopGroup workerGroup;

    private Channel channel;

    private NettyServerHandler serverHandler;

    NettyServer(Attribute attribute, ChannelHandler handler) {
        super(attribute, handler);
    }

    @Override
    public void bind() {
        serverHandler = new NettyServerHandler(getAttribute(), this);
        bootstrap = new ServerBootstrap();
        bossGroup = createEventLoopGroup();
        if (bossGroup instanceof EpollEventLoopGroup) {
            groupsEpoll(bootstrap, serverHandler);
        } else {
            groupsNio(bootstrap, serverHandler);
        }
        ChannelFuture future = bootstrap.bind(getHost(), getPort());
        io.netty.channel.Channel channel = future.syncUninterruptibly().channel();
        this.channel = new NettyChannel(channel);
    }

    @Override
    public Collection<Channel> getChannels() {
        return serverHandler.getChannels();
    }

    @Override
    protected void close() {
        if (channel != null) {
            channel.close();
        }
        if (getChannels() != null) {
            for (Channel channel1 : getChannels()) {
                channel1.close();
            }
        }
        if (bootstrap != null) {
            bossGroup.shutdownGracefully();
            workerGroup.shutdownGracefully();
        }
    }

    private void groupsEpoll(final ServerBootstrap bootstrap, final NettyServerHandler serverHandler) {
        workerGroup = new EpollEventLoopGroup(getIoThreads(), SoulThreadFactory.create("netty-nio-ServerWork", false));
        bootstrap.group(bossGroup, workerGroup)
                .channel(EpollServerSocketChannel.class)
                .option(EpollChannelOption.SO_BACKLOG, 128)
                .option(EpollChannelOption.WRITE_BUFFER_WATER_MARK, new WriteBufferWaterMark(8 * 1024 * 1024, 16 * 1024 * 1024))
                .option(EpollChannelOption.ALLOCATOR, PooledByteBufAllocator.DEFAULT)
                .childOption(EpollChannelOption.ALLOCATOR, PooledByteBufAllocator.DEFAULT)
                .childOption(EpollChannelOption.TCP_NODELAY, true)
                .handler(new LoggingHandler(LogLevel.INFO))
                .childHandler(new NettyServerInitializer(serverHandler));
    }

    private void groupsNio(final ServerBootstrap bootstrap, final NettyServerHandler serverHandler) {
        workerGroup = new NioEventLoopGroup(getIoThreads(), SoulThreadFactory.create("netty-nio-ServerWork", false));
        bootstrap.group(bossGroup, workerGroup)
                .channel(NioServerSocketChannel.class)
                .option(ChannelOption.SO_BACKLOG, 128)
                .option(ChannelOption.WRITE_BUFFER_WATER_MARK, new WriteBufferWaterMark(8 * 1024 * 1024, 16 * 1024 * 1024))
                .option(ChannelOption.ALLOCATOR, PooledByteBufAllocator.DEFAULT)
                .childOption(ChannelOption.ALLOCATOR, PooledByteBufAllocator.DEFAULT)
                .childOption(ChannelOption.TCP_NODELAY, true)
                .handler(new LoggingHandler(LogLevel.INFO))
                .childHandler(new NettyServerInitializer(serverHandler));
    }


    private EventLoopGroup createEventLoopGroup() {
        return Epoll.isAvailable() ?
                new EpollEventLoopGroup(1, SoulThreadFactory.create("netty-epoll-ServerBoss", false)) :
                new NioEventLoopGroup(1, SoulThreadFactory.create("netty-nio-ServerBoss", false));
    }
}
