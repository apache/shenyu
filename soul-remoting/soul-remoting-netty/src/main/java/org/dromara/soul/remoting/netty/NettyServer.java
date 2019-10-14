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
import io.netty.channel.ChannelInitializer;
import io.netty.channel.ChannelOption;
import io.netty.channel.EventLoopGroup;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.nio.NioServerSocketChannel;
import io.netty.channel.socket.nio.NioSocketChannel;
import io.netty.handler.codec.http.HttpObjectAggregator;
import io.netty.handler.codec.http.HttpServerCodec;
import io.netty.handler.codec.http.websocketx.extensions.compression.WebSocketServerCompressionHandler;
import io.netty.handler.stream.ChunkedWriteHandler;
import java.util.Collection;
import org.dromara.soul.common.Attribute;
import org.dromara.soul.common.concurrent.SoulThreadFactory;
import org.dromara.soul.remoting.api.AbstractNetServer;
import org.dromara.soul.remoting.api.Channel;
import org.dromara.soul.remoting.api.ChannelHandler;

/**
 * NettyServer
 * CreateDate: 2019/10/11 16:23
 *
 * @author sixh
 */
public class NettyServer extends AbstractNetServer {

    private EventLoopGroup boosGroup;
    private EventLoopGroup workGroup;
    private ServerBootstrap bootstrap;
    private Channel channel;
    private NettyServerHandler serverHandler;

    public NettyServer(Attribute attribute, ChannelHandler handler) {
        super(attribute, handler);
    }

    @Override
    public void bind() {
        boosGroup = new NioEventLoopGroup(1, SoulThreadFactory.create("nettyServerBoss", false));
        workGroup = new NioEventLoopGroup(getIoThreads(), SoulThreadFactory.create("nettyServerWork", false));
        serverHandler = new NettyServerHandler(getAttribute(), this);
        bootstrap = new ServerBootstrap().group(boosGroup, workGroup)
                .channel(NioServerSocketChannel.class)
                .childOption(ChannelOption.TCP_NODELAY, Boolean.TRUE)
                .childOption(ChannelOption.SO_REUSEADDR, Boolean.TRUE)
                .childOption(ChannelOption.ALLOCATOR, PooledByteBufAllocator.DEFAULT)
                .childHandler(new ChannelInitializer<NioSocketChannel>() {
                    @Override
                    protected void initChannel(NioSocketChannel channel) throws Exception {
                        channel.pipeline().addLast("http", new HttpServerCodec());
                        channel.pipeline().addLast("websocket", new WebSocketServerCompressionHandler());
                        channel.pipeline().addLast("http-aggregator", new HttpObjectAggregator(1024 * 1024 * 64));
                        channel.pipeline().addLast("chunkedWriter", new ChunkedWriteHandler());
                        channel.pipeline().addLast(serverHandler);
                    }
                });
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
            boosGroup.shutdownGracefully();
            workGroup.shutdownGracefully();
        }
    }
}
